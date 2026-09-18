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
 * sides. Images are drawn pixel by pixel (Blit), words with the lines
 * of a vector font (Hershey). The "b" key draws every form as the box
 * around it instead.
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

(* Images don't have a color; the "b" key shows their box in light
 * gray *)
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
      let size = Playground.words_font_size in
      let _strokes, width = Hershey.layout str in
      centered (width *. size /. Hershey.units_per_em) size
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
(* Images *)
(*****************************************************************************)

(* Image_decode's images (stb_image's buffers) as Blit's: the same
 * bytes when laid out the same way, which is always the case in
 * practice (no offset, rows one after the other); else a copy *)
let blit_image (img : Image_decode.image) : Blit.image =
  if img.offset = 0 && img.stride = img.width * 4 then
    { width = img.width; height = img.height; rgba = img.data }
  else begin
    let rgba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (img.width * img.height * 4) in
    for j = 0 to img.height - 1 do
      for k = 0 to (img.width * 4) - 1 do
        rgba.{(j * img.width * 4) + k} <- img.data.{img.offset + (j * img.stride) + k}
      done
    done;
    { width = img.width; height = img.height; rgba }
  end

(* [image w h src] shows the image as a w x h box centered on (0, 0):
 * this maps its pixel (u, v) (top-left origin, y down, u from 0 to its
 * width in pixels) to that box (y up), e.g. for a 35x35 image in a
 * 70x70 box: pixel (0, 0) -> (-35, 35), pixel (35, 35) -> (35, -35) *)
let image_to_local ~w ~h (image : Blit.image) : Affine.t =
  Affine.compose
    (Affine.translate (-.w /. 2.) (h /. 2.))
    (Affine.scale (w /. float image.width) (-.h /. float image.height))

(*****************************************************************************)
(* Text *)
(*****************************************************************************)

(* The pen's width, in font units: 1/12 of the em, a regular weight *)
let pen_width = Hershey.units_per_em /. 12.

(* Hershey font units to the local coordinates of a [words] shape:
 * centered on (0, 0) like in the other backends (the web's
 * text-anchor="middle" and dominant-baseline="central"): move left by
 * half the text's width (Hershey's y = 0 already is the middle of the
 * em), flip y (the font's y goes down), and scale the em to the font
 * size; e.g. at font size 10, "A" is 18 * 10/30 = 6 units wide *)
let text_to_local ~width : Affine.t =
  let s = Playground.words_font_size /. Hershey.units_per_em in
  Affine.compose (Affine.scale s (-.s)) (Affine.translate (-.width /. 2.) 0.)

(* By how much [m] scales lengths (on average, if it stretches more in
 * one direction): the square root of how much it scales areas *)
let length_scale (m : Affine.t) : float = sqrt (Float.abs ((m.a *. m.d) -. (m.b *. m.c)))

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

type options = {
  alpha_blending : bool;
  bounding_boxes : bool;
  wireframe : bool;
  bilinear : bool;
  antialiasing : bool;
}

let default_options =
  { alpha_blending = true; bounding_boxes = false; wireframe = false; bilinear = true; antialiasing = true }

(* The opacity to draw with. Without blending, there's no "partly
 * there": e.g. [fade 0.2] draws fully opaque, only [fade 0.] hides *)
let effective_alpha (options : options) (alpha : float) : float =
  if options.alpha_blending then alpha else if alpha > 0. then 1. else 0.

(*****************************************************************************)
(* Drawing polygons and circles: filled, or wireframe *)
(*****************************************************************************)

(* With [~aa] (antialiasing), each function below uses the antialiased
 * version of its algorithm: Fill.polygons_aa instead of Fill.polygon,
 * Line.draw_aa (Wu) instead of Line.draw (Bresenham) *)

let fill_polygon ~aa fb points ~rgb ~alpha =
  if aa then Fill.polygons_aa fb [ points ] ~rgb ~alpha else Fill.polygon fb points ~rgb ~alpha

let line ~aa = if aa then Line.draw_aa else Line.draw

(* wireframe: a line from each corner to the next, and from the last
 * back to the first *)
let outline_polygon ~aa fb points ~rgb ~alpha =
  match points with
  | [] -> ()
  | first :: _ ->
      let rec loop = function
        | p :: (q :: _ as rest) ->
            line ~aa fb p q ~rgb ~alpha;
            loop rest
        | [ last ] -> line ~aa fb last first ~rgb ~alpha
        | [] -> ()
      in
      loop points

(* The midpoint circle algorithm works on the pixel grid: its center is
 * a pixel (the one containing the real center) and its radius a whole
 * number of pixels, so the circle can be up to half a pixel off --
 * one reason why modern renderers prefer polygons, whose corners can
 * be anywhere between pixels. It also only decides "in or out" for
 * each pixel, so antialiased circles are polygons. *)
let circle_polygon ((cx, cy), r) =
  Circle.ellipse_points ~rx:r ~ry:r ~segments:(Circle.segments_for_radius r)
  |> List.map (fun (x, y) -> (cx +. x, cy +. y))

let fill_circle ~aa fb (((cx, cy), r) as circle) ~rgb ~alpha =
  if aa then Fill.polygons_aa fb [ circle_polygon circle ] ~rgb ~alpha
  else
    let pixel v = int_of_float (Float.floor v) in
    Circle.fill fb ~cx:(pixel cx) ~cy:(pixel cy) ~r:(int_of_float (Float.round r)) ~rgb ~alpha

let outline_circle ~aa fb (((cx, cy), r) as circle) ~rgb ~alpha =
  if aa then outline_polygon ~aa fb (circle_polygon circle) ~rgb ~alpha
  else
    let pixel v = int_of_float (Float.floor v) in
    Circle.outline fb ~cx:(pixel cx) ~cy:(pixel cy) ~r:(int_of_float (Float.round r)) ~rgb ~alpha

(* The current frame of an animated GIF, e.g. Mario's walk, like
 * browsers do: the animation runs on its own clock *)
let draw_image options fb m ~w ~h src ~alpha =
  match Image_decode.image_of_url_at ~time:(Unix.gettimeofday ()) src with
  | None -> ()
  | Some img ->
      let image = blit_image img in
      let sample = if options.bilinear then Blit.sample_bilinear else Blit.sample_nearest in
      Blit.draw fb image (Affine.compose m (image_to_local ~w ~h image)) ~sample ~alpha

(* A line through points, 1 pixel wide *)
let thin_polyline ~aa fb points ~rgb ~alpha =
  let rec loop = function
    | p :: (q :: _ as rest) ->
        line ~aa fb p q ~rgb ~alpha;
        loop rest
    | [ _ ] | [] -> ()
  in
  loop points

(* Text: Hershey's strokes, drawn 1 pixel wide when the pen would be
 * thinner than that anyway (or in wireframe), else as thick strokes *)
let draw_words options fb m str ~rgb ~alpha =
  let strokes, width = Hershey.layout str in
  let m = Affine.compose m (text_to_local ~width) in
  let lines = List.map (List.map (Affine.apply m)) strokes in
  let pen = pen_width *. length_scale m in
  let aa = options.antialiasing in
  if options.wireframe || pen < 1.5 then List.iter (fun l -> thin_polyline ~aa fb l ~rgb ~alpha) lines
  else if aa then Fill.polygons_aa fb (Stroke.contours lines ~width:pen) ~rgb ~alpha
  else Stroke.polylines fb lines ~width:pen ~rgb ~alpha

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
  let aa = options.antialiasing in
  let draw_polygon = if options.wireframe then outline_polygon ~aa else fill_polygon ~aa in
  let draw_circle = if options.wireframe then outline_circle ~aa else fill_circle ~aa in
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
    | Image (w, h, _) when options.wireframe -> polygon (rectangle_corners w h)
    | Image (w, h, src) -> draw_image options fb m ~w ~h src ~alpha
    | Words (_, str) -> draw_words options fb m str ~rgb ~alpha
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
