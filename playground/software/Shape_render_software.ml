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
 * Rectangles, polygons, and ngons are polygons: their corners go
 * through the transform, then Fill.polygon fills them. The other forms
 * (circles, ovals, words, images) are, until their phase of
 * docs/claude_notes/plan_software_2d.md, drawn as the box around them,
 * which is also what the "b" key shows for every form.
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
(* Polygons *)
(*****************************************************************************)

(* The corners of a form that is a polygon, in its local coordinates;
 * None for the other forms. *)
let local_polygon (form : Playground.form) : (float * float) list option =
  match form with
  | Rectangle (_, w, h) ->
      let x = w /. 2. and y = h /. 2. in
      Some [ (-.x, y); (x, y); (x, -.y); (-.x, -.y) ]
  | Polygon (_, points) -> Some points
  | Ngon (_, n, r) ->
      (* n corners on the circle of radius r, the first one at the top
       * (90 degrees), then every 360/n degrees clockwise, like
       * elm-playground; e.g. for a triangle, at 90, -30, and -150
       * degrees: (0, r), (0.87r, -0.5r), (-0.87r, -0.5r) *)
      Some
        (List.init n (fun i ->
             let degrees = 90. -. (360. *. float i /. float n) in
             let radians = degrees *. Float.pi /. 180. in
             (r *. cos radians, r *. sin radians)))
  | Circle _ | Oval _ | Image _ | Words _ | Group _ -> None

(*****************************************************************************)
(* Placeholder: bounding boxes *)
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

(* Fill the axis-aligned box around the local box [bounds] once
 * transformed by [m]: once rotated, the box's corners are no longer
 * axis-aligned, so take their min and max x and y *)
let fill_transformed_box (fb : Framebuffer.t) (m : Affine.t) (xmin, ymin, xmax, ymax) ~rgb ~alpha =
  let corners =
    List.map (Affine.apply m) [ (xmin, ymin); (xmax, ymin); (xmax, ymax); (xmin, ymax) ]
  in
  let xs = List.map fst corners and ys = List.map snd corners in
  let x0 = List.fold_left min infinity xs and x1 = List.fold_left max neg_infinity xs in
  let y0 = List.fold_left min infinity ys and y1 = List.fold_left max neg_infinity ys in
  Fill.polygon fb [ (x0, y0); (x1, y0); (x1, y1); (x0, y1) ] ~rgb ~alpha

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

type options = { alpha_blending : bool; bounding_boxes : bool }

let default_options = { alpha_blending = true; bounding_boxes = false }

(* The opacity to draw with. Without blending, there's no "partly
 * there": e.g. [fade 0.2] draws fully opaque, only [fade 0.] hides *)
let effective_alpha (options : options) (alpha : float) : float =
  if options.alpha_blending then alpha else if alpha > 0. then 1. else 0.

(* The color to draw a (non-group) form with *)
let form_rgb (form : Playground.form) : int =
  match form with
  | Circle (color, _)
  | Oval (color, _, _)
  | Rectangle (color, _, _)
  | Ngon (color, _, _)
  | Polygon (color, _)
  | Words (color, _) ->
      rgb_of_color color
  | Image _ | Group _ -> image_placeholder_rgb

(* [m] is the transform from the coordinates [shape] lives in (the
 * window's, or its enclosing group's) to pixel coordinates *)
let rec render_shape (options : options) (fb : Framebuffer.t) (m : Affine.t) (shape : Playground.shape) : unit =
  let m = Affine.compose m (shape_transform shape) in
  let alpha = effective_alpha options shape.alpha in
  let rgb = form_rgb shape.form in
  match shape.form, local_polygon shape.form with
  | Group shapes, _ ->
      (* TODO: alpha, like Shape_render_native; doing it right needs an
       * offscreen layer (fading each child separately would let
       * overlapping children show through each other) *)
      List.iter (render_shape options fb m) shapes
  | _, Some corners when not options.bounding_boxes ->
      Fill.polygon fb (List.map (Affine.apply m) corners) ~rgb ~alpha
  | form, _ -> Option.iter (fun bounds -> fill_transformed_box fb m bounds ~rgb ~alpha) (local_bounds form)

let render ?(options = default_options) (fb : Framebuffer.t) (shapes : Playground.shape list) : unit =
  List.iter (render_shape options fb (screen_transform fb)) shapes
