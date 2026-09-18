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
 * through the transform, then Fill.polygon fills them. Circles use the
 * midpoint circle algorithm (Circle), unless the transform stretches
 * them into ellipses, which, like ovals, become polygons with many
 * sides. Words and images are, until their phase of
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
(* Forms as polygons, circles, and boxes *)
(*****************************************************************************)
(* Every form becomes, in pixel coordinates, either a polygon or (for
 * circles that stay circles) a center and a radius *)

let rectangle_corners w h =
  let x = w /. 2. and y = h /. 2. in
  [ (-.x, y); (x, y); (x, -.y); (-.x, -.y) ]

(* n corners on the circle of radius r, the first one at the top (90
 * degrees), then every 360/n degrees clockwise, like elm-playground;
 * e.g. for a triangle, at 90, -30, and -150 degrees:
 * (0, r), (0.87r, -0.5r), (-0.87r, -0.5r) *)
let ngon_corners n r =
  List.init n (fun i ->
      let degrees = 90. -. (360. *. float i /. float n) in
      let radians = degrees *. Float.pi /. 180. in
      (r *. cos radians, r *. sin radians))

(* A circle stays a circle when [m] only moves, rotates, flips, and
 * scales by the same amount in every direction: its two columns (where
 * the x and y axes go) must be perpendicular (dot product 0) and of
 * the same length. Then the center is where (0, 0) goes and the radius
 * is scaled by that length. Otherwise (e.g. a group scaled only
 * horizontally... not possible in Playground today, but free to
 * support) it's an ellipse. *)
let circle_in_pixels (m : Affine.t) (r : float) : ((float * float) * float) option =
  let scale_x = Float.hypot m.a m.b and scale_y = Float.hypot m.c m.d in
  let perpendicular = Float.abs ((m.a *. m.c) +. (m.b *. m.d)) < 1e-9 *. scale_x *. scale_y in
  if perpendicular && Float.abs (scale_x -. scale_y) < 1e-9 *. scale_x then
    Some (Affine.apply m (0., 0.), r *. scale_x)
  else None

(* An ellipse (or a circle that doesn't stay one) as a polygon in pixel
 * coordinates, with as many sides as its size on screen needs *)
let ellipse_polygon (m : Affine.t) ~rx ~ry : (float * float) list =
  let scale = Float.max (Float.hypot m.a m.b) (Float.hypot m.c m.d) in
  let segments = Circle.segments_for_radius (Float.max rx ry *. scale) in
  List.map (Affine.apply m) (Circle.ellipse_points ~rx ~ry ~segments)

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

(* The axis-aligned box, in pixel coordinates, around the local box
 * [bounds] once transformed by [m]: once rotated, the box's corners are
 * no longer axis-aligned, so take their min and max x and y *)
let box_polygon (m : Affine.t) (xmin, ymin, xmax, ymax) : (float * float) list =
  let corners =
    List.map (Affine.apply m) [ (xmin, ymin); (xmax, ymin); (xmax, ymax); (xmin, ymax) ]
  in
  let xs = List.map fst corners and ys = List.map snd corners in
  let x0 = List.fold_left min infinity xs and x1 = List.fold_left max neg_infinity xs in
  let y0 = List.fold_left min infinity ys and y1 = List.fold_left max neg_infinity ys in
  [ (x0, y0); (x1, y0); (x1, y1); (x0, y1) ]

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

type options = { alpha_blending : bool; bounding_boxes : bool; wireframe : bool }

let default_options = { alpha_blending = true; bounding_boxes = false; wireframe = false }

(* The opacity to draw with. Without blending, there's no "partly
 * there": e.g. [fade 0.2] draws fully opaque, only [fade 0.] hides *)
let effective_alpha (options : options) (alpha : float) : float =
  if options.alpha_blending then alpha else if alpha > 0. then 1. else 0.

(*****************************************************************************)
(* Drawing polygons and circles: filled, or wireframe *)
(*****************************************************************************)

let fill_polygon fb points ~rgb ~alpha = Fill.polygon fb points ~rgb ~alpha

(* wireframe: a line from each corner to the next, and from the last
 * back to the first *)
let outline_polygon fb points ~rgb ~alpha =
  match points with
  | [] -> ()
  | first :: _ ->
      let rec loop = function
        | p :: (q :: _ as rest) ->
            Line.draw fb p q ~rgb ~alpha;
            loop rest
        | [ last ] -> Line.draw fb last first ~rgb ~alpha
        | [] -> ()
      in
      loop points

(* The midpoint circle algorithm works on the pixel grid: its center is
 * a pixel (the one containing the real center) and its radius a whole
 * number of pixels, so the circle can be up to half a pixel off --
 * one reason why modern renderers prefer polygons, whose corners can
 * be anywhere between pixels. *)
let fill_circle fb ((cx, cy), r) ~rgb ~alpha =
  let pixel v = int_of_float (Float.floor v) in
  Circle.fill fb ~cx:(pixel cx) ~cy:(pixel cy) ~r:(int_of_float (Float.round r)) ~rgb ~alpha

let outline_circle fb ((cx, cy), r) ~rgb ~alpha =
  let pixel v = int_of_float (Float.floor v) in
  Circle.outline fb ~cx:(pixel cx) ~cy:(pixel cy) ~r:(int_of_float (Float.round r)) ~rgb ~alpha

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

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

(* A (non-group) form, [m] taking its local coordinates to pixels *)
let render_form (options : options) (fb : Framebuffer.t) (m : Affine.t) (form : Playground.form) ~rgb ~alpha =
  let draw_polygon = if options.wireframe then outline_polygon else fill_polygon in
  let draw_circle = if options.wireframe then outline_circle else fill_circle in
  let polygon local_corners = draw_polygon fb (List.map (Affine.apply m) local_corners) ~rgb ~alpha in
  let box () = Option.iter (fun b -> draw_polygon fb (box_polygon m b) ~rgb ~alpha) (local_bounds form) in
  if options.bounding_boxes then box ()
  else
    match form with
    | Rectangle (_, w, h) -> polygon (rectangle_corners w h)
    | Polygon (_, points) -> polygon points
    | Ngon (_, n, r) -> polygon (ngon_corners n r)
    | Circle (_, r) -> (
        match circle_in_pixels m r with
        | Some circle -> draw_circle fb circle ~rgb ~alpha
        | None -> draw_polygon fb (ellipse_polygon m ~rx:r ~ry:r) ~rgb ~alpha)
    | Oval (_, w, h) -> draw_polygon fb (ellipse_polygon m ~rx:(w /. 2.) ~ry:(h /. 2.)) ~rgb ~alpha
    (* until phases 4 (images) and 5 (text) *)
    | Words _ | Image _ -> box ()
    | Group _ -> ()

(* [m] is the transform from the coordinates [shape] lives in (the
 * window's, or its enclosing group's) to pixel coordinates *)
let rec render_shape (options : options) (fb : Framebuffer.t) (m : Affine.t) (shape : Playground.shape) : unit =
  let m = Affine.compose m (shape_transform shape) in
  match shape.form with
  | Group shapes ->
      (* TODO: alpha, like Shape_render_native; doing it right needs an
       * offscreen layer (fading each child separately would let
       * overlapping children show through each other) *)
      List.iter (render_shape options fb m) shapes
  | form ->
      render_form options fb m form ~rgb:(form_rgb form) ~alpha:(effective_alpha options shape.alpha)

let render ?(options = default_options) (fb : Framebuffer.t) (shapes : Playground.shape list) : unit =
  List.iter (render_shape options fb (screen_transform fb)) shapes
