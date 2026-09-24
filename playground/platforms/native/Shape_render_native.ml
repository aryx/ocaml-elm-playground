(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Basics
open Color

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let spf = Printf.sprintf

let ( =~ ) s re = Str.string_match (Str.regexp re) s 0
let matched (i : int) (s : string) : string = Str.matched_group i s
let matched3 s = (matched 1 s, matched 2 s, matched 3 s)

let set_color cr color alpha =
  let (r, g, b) =
    match color with
    | Rgb (r, g, b) -> (float r / 255., float g / 255., float b / 255.)
    | Hex s ->
        let s = String.lowercase_ascii s in
        if s =~ "^#\\([a-f0-9][a-f0-9]\\)\\([a-f0-9][a-f0-9]\\)\\([a-f0-9][a-f0-9]\\)$" then
          let (a, b, c) = matched3 s in
          let f x = ("0x" ^ x |> int_of_string |> float) / 255. in
          (f a, f b, f c)
        else failwith (spf "wrong color format: %s" s)
  in
  Cairo.set_source_rgba cr r g b (clamp 0. 1. alpha)

(* Cairo (0,0) is at the top left of the screen, in which as y goes up,
 * the coordinates are down on the physical screen. Elm uses a better
 * default where if your y goes up, then it's upper on the screen.
 * Here we convert the Elm coordinate system to Cairo. Note that
 * it's hard to use one of the rotate/translate Cairo function to emulate
 * that as only y need to change (maybe need to create a special matrix?)
 *)
let convert (x, y) = (x, -.y)

let render_transform cr x y angle s =
  Cairo.translate cr x y;
  Cairo.rotate cr (-.Basics.degrees_to_radians angle);
  Cairo.scale cr s s

let rec ngon_points cr i n radius =
  if i == n then ()
  else begin
    let a = turns ((float i / float n) - 0.25) in
    let x = radius * cos a in
    let y = radius * sin a in
    (if i = 0 then Cairo.move_to cr x y else Cairo.line_to cr x y);
    ngon_points cr (Stdlib.( + ) i 1) n radius
  end

let render_ngon cr hook color n radius x y angle s alpha =
  let (x, y) = convert (x, y) in
  Cairo.save cr;
  hook cr;
  set_color cr color alpha;
  render_transform cr x y angle s;
  ngon_points cr 0 n radius;
  Cairo.fill cr;
  Cairo.restore cr

let render_oval cr hook color w h x y _angle _s alpha =
  let x = x - (w / 2.) in
  let y = y + (h / 2.) in
  let (x, y) = convert (x, y) in
  Cairo.save cr;
  hook cr;
  set_color cr color alpha;
  (* code in cairo.mli to draw ellipsis *)
  Cairo.translate cr (x +. (w /. 2.)) (y +. (h /. 2.));
  Cairo.scale cr (w /. 2.) (h /. 2.);
  Cairo.arc cr 0. 0. 1. 0. pi2;
  Cairo.fill cr;
  Cairo.restore cr

let render_circle cr hook color radius x y angle s alpha =
  let (x, y) = convert (x, y) in
  Cairo.save cr;
  hook cr;
  set_color cr color alpha;
  render_transform cr x y angle s;
  Cairo.arc cr 0. 0. radius 0. pi2;
  Cairo.fill cr;
  Cairo.restore cr

let render_polygon cr hook color points x y angle s alpha =
  let (x, y) = convert (x, y) in
  Cairo.save cr;
  hook cr;
  set_color cr color alpha;
  render_transform cr x y angle s;
  (match points with
  | [] -> failwith "not enough points in polygon"
  | (x, y) :: xs ->
      let (x, y) = convert (x, y) in
      Cairo.move_to cr x y;
      xs
      |> List.iter (fun (x, y) ->
             let (x, y) = convert (x, y) in
             Cairo.line_to cr x y);
      Cairo.line_to cr x y;
      Cairo.fill cr);
  Cairo.restore cr

let render_rectangle cr hook color w h x y angle s alpha =
  render_polygon cr hook color
    [ (-.w / 2., h /. 2.); (w / 2., h /. 2.); (w / 2., -.h /. 2.); (-.w / 2., -.h /. 2.) ]
    x y angle s alpha

let render_words cr hook color str x y angle s alpha =
  let (x, y) = convert (x, y) in
  Cairo.save cr;
  hook cr;
  set_color cr color alpha;
  render_transform cr x y angle s;
  (* claude: same font as the web backend (see
   * Playground.words_font_size), instead of Cairo's default 10 *)
  Cairo.select_font_face cr Playground.words_font_family;
  Cairo.set_font_size cr Playground.words_font_size;
  (* claude: center the text on (x, y) the same way the web backend does
   * with text-anchor="middle" and dominant-baseline="central": use the
   * advance width (not the width of the inked part), and the font's
   * ascent/descent (not the inked height of this particular string,
   * which made e.g. "aaa" and "Ag" sit at different heights). The
   * baseline goes (ascent - descent) / 2 below the center, which puts
   * the middle of the font's box on y. *)
  let (text_ext : Cairo.text_extents) = Cairo.text_extents cr str in
  let (font_ext : Cairo.font_extents) = Cairo.font_extents cr in
  Cairo.move_to cr (-.text_ext.x_advance / 2.) ((font_ext.ascent - font_ext.descent) / 2.);
  Cairo.show_text cr str;
  Cairo.restore cr

(* claude: a surface in a w x h box: an image's, or a bitmap's *)
let render_surface ~smooth_images cr hook w h surface x y angle s _alpha =
  let (x, y) = convert (x, y) in
  (* claude: Cairo.set_source_surface only positions a surface's origin,
   * it never resizes its pixel content to fill (w,h) -- so without this
   * scale, an image whose native decoded size differs from the
   * requested (w,h) (e.g. Mario's 35x35 GIF sprites drawn via
   * "image 70. 70. ...") is drawn at its native size, anchored at the
   * top-left of the (w,h) box instead of filling/centering it. *)
  let surface_w = float (Cairo.Image.get_width surface) in
  let surface_h = float (Cairo.Image.get_height surface) in
  Cairo.save cr;
  hook cr;
  render_transform cr x y angle s;
  Cairo.scale cr (w /. surface_w) (h /. surface_h);
  Cairo.set_source_surface cr surface ~x:(-.surface_w /. 2.) ~y:(-.surface_h /. 2.);
  (* claude: Playground.rendering's smooth_images; NEAREST keeps pixel
   * art sprites sharp when enlarged *)
  if not smooth_images then Cairo.Pattern.set_filter (Cairo.get_source cr) Cairo.Pattern.NEAREST;
  Cairo.paint cr;
  Cairo.restore cr

let render_image ~smooth_images cr hook w h src x y angle s alpha =
  (* claude: surface_of_url_at to animate animated GIFs (e.g., Mario's
   * walk sprites), like browsers do on the web *)
  match Image_native.surface_of_url_at ~time:(Unix.gettimeofday ()) src with
  | None -> ()
  | Some surface -> render_surface ~smooth_images cr hook w h surface x y angle s alpha

(*****************************************************************************)
(* Render playground *)
(*****************************************************************************)
open Playground

(* ugly, to handle Group *)
type hook = Cairo.context -> unit

let empty_hook = fun _cr -> ()

let rec (render_shape : smooth_images:bool -> Cairo.context -> hook -> shape -> unit) =
 fun ~smooth_images cr hook { x; y; angle; scale; alpha; form } ->
  match form with
  | Circle (color, radius) -> render_circle cr hook color radius x y angle scale alpha
  | Oval (color, width, height) -> render_oval cr hook color width height x y angle scale alpha
  | Rectangle (color, width, height) -> render_rectangle cr hook color width height x y angle scale alpha
  | Ngon (color, n, radius) -> render_ngon cr hook color n radius x y angle scale alpha
  | Polygon (color, points) -> render_polygon cr hook color points x y angle scale alpha
  | Words (color, str) -> render_words cr hook color str x y angle scale alpha
  | Image (w, h, src) -> render_image ~smooth_images cr hook w h src x y angle scale alpha
  | Bitmap (w, h, img) -> render_surface ~smooth_images cr hook w h (Image_native.surface_of_bitmap img) x y angle scale alpha
  | Group xs ->
      (* TODO: alpha *)
      let hook =
       fun cr' ->
        hook cr';
        let (x, y) = convert (x, y) in
        render_transform cr' x y angle scale
      in
      List.iter (render_shape ~smooth_images cr hook) xs

let render ?(smooth_images = true) (cr : Cairo.context) (shapes : shape list) : unit =
  List.iter (render_shape ~smooth_images cr empty_hook) shapes
