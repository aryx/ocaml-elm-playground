(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* From Playground shapes to pixels. The pipeline, for each shape:
 *
 *   shape, in its own local coordinates
 *     (e.g. [rectangle red 100. 50.] is the box from (-50, -25) to (50, 25))
 *       |
 *       | shape transform: its scale, rotate, and move
 *       v
 *   Elm world coordinates (origin at the center of the window, y up)
 *       |
 *       | screen transform: y flip + move the origin to the center
 *       v
 *   pixel coordinates (origin at the top-left corner, y down)
 *       |
 *       | rasterization: which pixels does the shape cover?
 *       v
 *   pixels in the framebuffer
 *
 * The two transforms are Affine matrices, multiplied into one before
 * any point is transformed; a [group] just multiplies in one more.
 *
 * Phase 1 of docs/claude_notes/plan_software_2d.md: rasterization is a
 * placeholder, every shape is drawn as the axis-aligned box around its
 * transformed corners, in its color. That's enough to check the whole
 * pipeline (positions, sizes, rotations, groups, fading) before the
 * real algorithms (polygon filling, circles, images, text) arrive.
 *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

(* Playground colors to 0xRRGGBB ints; e.g. Hex "#cc0000" -> 0xcc0000,
 * Rgb (255, 128, 0) -> 0xff8000 *)
let rgb_of_color (color : Color.t) : int =
  match color with
  | Rgb (r, g, b) -> (r lsl 16) lor (g lsl 8) lor b
  | Hex s when String.length s = 7 && s.[0] = '#' -> int_of_string ("0x" ^ String.sub s 1 6)
  | Hex s -> failwith (Printf.sprintf "wrong color format: %s" s)

(* Images don't have a color; until phase 4 draws their real pixels,
 * show where they are with a light gray box *)
let image_placeholder_rgb = 0xc0c0c0

(*****************************************************************************)
(* Transforms *)
(*****************************************************************************)

(* Elm world coordinates -> pixel coordinates. For a 1000x1000
 * framebuffer:
 *   Elm (0, 0), the center        -> pixel (500, 500)
 *   Elm (0, 100), above center    -> pixel (500, 400)
 *   Elm (-500, 500), top-left     -> pixel (0, 0)
 * i.e. first flip y (scale 1 -1), then move the origin to the center. *)
let screen_transform (fb : Framebuffer.t) : Affine.t =
  Affine.compose
    (Affine.translate (float fb.width /. 2.) (float fb.height /. 2.))
    (Affine.scale 1. (-1.))

(* A shape's own scale, then rotation, then move -- the same order as
 * the web backend's SVG "translate(x, y) rotate(a) scale(s)", which
 * also applies right to left. Scaling or rotating *after* moving would
 * scale or rotate the shape's position around the window's center too.
 * Playground angles are in degrees, counterclockwise. *)
let shape_transform (shape : Playground.shape) : Affine.t =
  let radians = shape.angle *. Float.pi /. 180. in
  Affine.compose
    (Affine.translate shape.x shape.y)
    (Affine.compose (Affine.rotate radians) (Affine.scale shape.scale shape.scale))

(*****************************************************************************)
(* Phase 1 placeholder: bounding boxes *)
(*****************************************************************************)

(* The box around a form, in its local coordinates, as
 * (xmin, ymin, xmax, ymax); None when there's nothing to draw *)
let local_bounds (form : Playground.form) : (float * float * float * float) option =
  let centered w h = Some (-.w /. 2., -.h /. 2., w /. 2., h /. 2.) in
  match form with
  | Circle (_, r) | Ngon (_, _, r) -> centered (2. *. r) (2. *. r)
  | Oval (_, w, h) | Rectangle (_, w, h) | Image (w, h, _) -> centered w h
  | Polygon (_, []) -> None
  | Polygon (_, points) ->
      let xs = List.map fst points and ys = List.map snd points in
      let min_of = List.fold_left min infinity and max_of = List.fold_left max neg_infinity in
      Some (min_of xs, min_of ys, max_of xs, max_of ys)
  | Words (_, str) ->
      (* rough: in a sans-serif font, a character is about half as wide
       * as the font is tall; phase 5 draws the real letters *)
      let size = Playground.words_font_size in
      centered (0.5 *. size *. float (String.length str)) size
  | Group _ -> None

(* Fill the pixels covered by the box [bounds] once transformed by [m].
 *
 * Which pixels does a region "cover"? The standard rule (used by
 * OpenGL, Direct3D, and most 2D libraries): a pixel belongs to a shape
 * if its *center* does. Pixel (px, py) is the unit square from (px, py)
 * to (px+1, py+1), so its center is (px + 0.5, py + 0.5). For example a
 * box from x = 10.2 to x = 12.7 covers the pixels whose centers 10.5,
 * 11.5, 12.5 are inside it: px = 10, 11, 12. In general, the first
 * covered pixel is ceil(xmin - 0.5) and the first one after is
 * ceil(xmax - 0.5), so exactly one of two boxes sharing an edge gets
 * the pixels on that edge: no gap, and no pixel painted twice. *)
let fill_transformed_box (fb : Framebuffer.t) (m : Affine.t) (xmin, ymin, xmax, ymax) ~rgb ~alpha =
  let corners =
    List.map (Affine.apply m) [ (xmin, ymin); (xmax, ymin); (xmax, ymax); (xmin, ymax) ]
  in
  let xs = List.map fst corners and ys = List.map snd corners in
  let first_pixel v = int_of_float (Float.ceil (v -. 0.5)) in
  let x0 = first_pixel (List.fold_left min infinity xs) in
  let x1 = first_pixel (List.fold_left max neg_infinity xs) in
  let y0 = first_pixel (List.fold_left min infinity ys) in
  let y1 = first_pixel (List.fold_left max neg_infinity ys) in
  (* clip rows here (spans clip columns themselves), so a shape far off
   * screen doesn't loop over millions of invisible rows *)
  for y = max y0 0 to min y1 fb.height - 1 do
    Framebuffer.fill_span fb ~y ~x0 ~x1 ~rgb ~alpha
  done

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

(* [m] is the transform from the coordinates [shape] lives in (the
 * window's, or its enclosing group's) to pixel coordinates *)
let rec render_shape (fb : Framebuffer.t) (m : Affine.t) (shape : Playground.shape) : unit =
  let m = Affine.compose m (shape_transform shape) in
  match shape.form with
  | Group shapes ->
      (* TODO: alpha, like Shape_render_native; doing it right needs an
       * offscreen layer (fading each child separately would let
       * overlapping children show through each other) *)
      List.iter (render_shape fb m) shapes
  | Circle (color, _)
  | Oval (color, _, _)
  | Rectangle (color, _, _)
  | Ngon (color, _, _)
  | Polygon (color, _)
  | Words (color, _) ->
      Option.iter
        (fun bounds -> fill_transformed_box fb m bounds ~rgb:(rgb_of_color color) ~alpha:shape.alpha)
        (local_bounds shape.form)
  | Image _ ->
      Option.iter
        (fun bounds -> fill_transformed_box fb m bounds ~rgb:image_placeholder_rgb ~alpha:shape.alpha)
        (local_bounds shape.form)

let render (fb : Framebuffer.t) (shapes : Playground.shape list) : unit =
  List.iter (render_shape fb (screen_transform fb)) shapes
