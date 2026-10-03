(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Web_render.mli *)

open Basics
open Playground
open Color
module V = Web_vdom
module Html = Web_vdom.Html
module Svg = Web_vdom.Svg

(* claude: JavaScript's own String(x), the shortest string that reads
 * back as x: every number of every shape goes through this each frame,
 * and sprintf "%f" (OCaml's formatting, emulated in JavaScript) made
 * turning TinyMinimoog's 800 shapes into a virtual DOM the frame's
 * largest cost, 9.8 ms of 21 *)
let js_string = Ojs.get_prop_ascii Ojs.global "String"
let string_of_number (x : float) : string = Ojs.string_of_js (Ojs.apply js_string [| Ojs.float_to_js x |])

(*****************************************************************************)
(* Render *)
(*****************************************************************************)

let render_color color =
  match color with
  | Hex str -> str
  | Rgb (r,g,b) -> "rgb(" ^ string_of_int r ^ "," ^ string_of_int g ^ "," ^ string_of_int b ^ ")"

(* claude: the strings joined with ^, not sprintf, for the same reason
 * as [string_of_number] *)
let render_transform x y a s =
  let translate = "translate(" ^ string_of_number x ^ ", " ^ string_of_number (-. y) ^ ")" in
  let rotate = if a = 0. then "" else " rotate(" ^ string_of_number (-. a) ^ ")" in
  let scale = if s = 1. then "" else " scale(" ^ string_of_number s ^ ")" in
  translate ^ rotate ^ scale

let render_rect_transform width height x y angle s =
  render_transform x y angle s ^
  " translate(" ^ string_of_number (-. width / 2.) ^ ", " ^ string_of_number (-. height / 2.) ^ ")"


let render_alpha alpha =
  if alpha = 1.
  then []
  else [Svg.Attributes.opacity (string_of_number (clamp 0. 1. alpha))]

let render_circle color radius x y angle s alpha =
  Svg.circle 
    (Svg.Attributes.r (string_of_number radius) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_oval color width height x y angle s alpha = 
  Svg.ellipse
    (Svg.Attributes.rx (string_of_number (width / 2.)) ::
     Svg.Attributes.ry (string_of_number (height / 2.)) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_rectangle color w h x y angle s alpha = 
  Svg.rect
    (Svg.Attributes.width (string_of_number (w)) ::
     Svg.Attributes.height (string_of_number (h)) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_rect_transform w h x y angle s)::
     render_alpha alpha
    )
    []

let rec to_ngon_points i n radius str =
  if i == n 
  then str
  else
    let a = turns (float_of_int i / float_of_int n - 0.25) in
    let x = radius * cos a in
    let y = radius * sin a in
    to_ngon_points (Stdlib.(+) i 1) n radius
      (str ^ string_of_number x ^ "," ^ string_of_number y ^ " ")

let render_ngon color n radius x y angle s alpha = 
  Svg.polygon
    (Svg.Attributes.points (to_ngon_points 0 n radius "") ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

(* claude: same as renderWords in elm-playground: the text is centered on (x, y)
 * horizontally (text-anchor) and vertically (dominant-baseline). *)
let render_words color str x y angle s alpha =
  Svg.text_
    (Svg.Attributes.textAnchor "middle" ::
     Svg.Attributes.dominantBaseline "central" ::
     (* claude: same font as the native backend, instead of the browser
      * default (see Playground.words_font_size) *)
     Svg.Attributes.fontSize (string_of_number words_font_size) ::
     Svg.Attributes.fontFamily words_font_family ::
     Svg.Attributes.textContent str ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

(* claude: Same as renderPolygon in elm-playground: the points are relative to
 * (x, y), and their y is negated since the svg y axis goes down (see
 * render_transform). *)
(* claude: List.map in constant stack (OCaml 4.14's recurses as deep as
 * the list, and a browser's stack is small): a code map's street drew a
 * polygon of 8,800 points and a picture of thousands of shapes, the
 * page frozen, an exception every frame *)
let map_list (f : 'a -> 'b) (l : 'a list) : 'b list = List.rev (List.rev_map f l)

let render_polygon color points x y angle s alpha =
  let points_str =
    points
    |> map_list (fun (px, py) -> string_of_number px ^ "," ^ string_of_number (-. py))
    |> String.concat " "
  in
  Svg.polygon
    (Svg.Attributes.points points_str ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_image w h src x y angle s alpha =
  Svg.image
    (Svg.Attributes.href src:: (* was xlinkHref but require attributeNS *)
     Svg.Attributes.width (string_of_number w)::
     Svg.Attributes.height (string_of_number h)::
     Svg.Attributes.fill (render_color yellow) ::
     Svg.Attributes.transform (render_rect_transform w h x y angle s)::
     render_alpha alpha
    )
    []


(* claude: a bitmap as an image's URL, its pixels in it: a PNG as a
 * data: URL -- SVG has nothing else for pixels in memory (and the vdom
 * no canvas). The ones made are kept, in a table, by the image itself,
 * up to 16 million pixels in all (past that, all dropped, and made
 * again as they come), as Cairo's platform keeps its surfaces
 * (Image_native.surface_of_bitmap): a picture shown in many frames is
 * encoded once; a video, a PNG per new frame, slow.
 *
 * Before: the last 32, in a list. Enough for a page's pictures
 * (TinyMosaic), not for a game whose soldiers are a picture a limb and
 * whose map is tiles (mini-soldat): 64 pictures in a frame. And a list
 * of 32 kept by age does not keep 32 of the 64: it keeps none from a
 * frame to the next. The 33rd picture pushes the 1st out; the 1st,
 * wanted again, is made again and pushes the 2nd out; and so on round.
 * With 33 pictures in a frame, 33 PNG a frame; with mini-soldat's 64,
 * 9 frames a second where the table gives 60.
 *
 *   let last_bitmaps : (Rgba_image.t * string) list ref = ref []
 *   let bitmap_url img =
 *     match List.find_opt (fun (i, _) -> i == img) !last_bitmaps with
 *     | Some (_, url) -> url
 *     | None ->
 *         let url = png_data_url img in
 *         last_bitmaps := List.filteri (fun k _ -> k < 32) ((img, url) :: !last_bitmaps);
 *         url
 *)
module Bitmaps = Hashtbl.Make (struct
  type t = Rgba_image.t

  let equal = ( == )

  (* its size, and 16 bytes spread over its pixels: as Image_native's *)
  let hash (img : Rgba_image.t) : int =
    let n = Bigarray.Array1.dim img.rgba in
    let h = ref ((img.width *.. 31) +.. img.height) in
    if n > 0 then
      for i = 0 to 15 do
        h := (!h *.. 31) +.. Bigarray.Array1.unsafe_get img.rgba (i *.. (n -.. 1) /.. 15)
      done;
    !h land max_int
end)

let bitmaps : string Bitmaps.t = Bitmaps.create 512
let bitmaps_pixels = ref 0

(* as many pixels as Cairo's platform keeps: a PNG's text is smaller
 * than the surface it would be there *)
let bitmaps_budget = 16_000_000

(* claude: the PNG encoded by the browser, in native code: the pixels
 * put on a canvas (putImageData, the Rgba_image's own bytes, no copy:
 * a Bigarray is a typed array in JavaScript) and the canvas asked for
 * a PNG (toDataURL). 28 ms for 1738 by 838 where Png.encode compiled to
 * JavaScript took 1 s (row filters, deflate, base64): tinybox's code
 * map, a new picture every frame it moves, went from 1 frame a second.
 * The same lossless PNG. One canvas, kept, resized to each picture.
 *
 *   old: "data:image/png;base64," ^ String_base64.encode (Png.encode img)
 *)
let canvas : Ojs.t option ref = ref None

let png_data_url (img : Rgba_image.t) : string =
  let c =
    match !canvas with
    | Some c -> c
    | None ->
        let c = Ojs.call (Ojs.get_prop_ascii Ojs.global "document") "createElement" [| Ojs.string_to_js "canvas" |] in
        canvas := Some c;
        c
  in
  Ojs.set_prop_ascii c "width" (Ojs.int_to_js img.width);
  Ojs.set_prop_ascii c "height" (Ojs.int_to_js img.height);
  let ctx = Ojs.call c "getContext" [| Ojs.string_to_js "2d" |] in
  let u8 : Ojs.t =
    (* an Ojs.t and a Js.t are the same JavaScript value, the two
     * libraries' types for it *)
    Obj.magic
      (Js_of_ocaml.Typed_array.from_genarray Js_of_ocaml.Typed_array.Int8_unsigned (Bigarray.genarray_of_array1 img.rgba))
  in
  let clamped =
    Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Uint8ClampedArray")
      [| Ojs.get_prop_ascii u8 "buffer"; Ojs.get_prop_ascii u8 "byteOffset"; Ojs.get_prop_ascii u8 "length" |]
  in
  let data = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "ImageData") [| clamped; Ojs.int_to_js img.width; Ojs.int_to_js img.height |] in
  ignore (Ojs.call ctx "putImageData" [| data; Ojs.int_to_js 0; Ojs.int_to_js 0 |]);
  Ojs.string_of_js (Ojs.call c "toDataURL" [| Ojs.string_to_js "image/png" |])

let bitmap_url (img : Rgba_image.t) : string =
  match Bitmaps.find_opt bitmaps img with
  | Some url -> url
  | None ->
      let pixels = img.width *.. img.height in
      if !bitmaps_pixels +.. pixels > bitmaps_budget then begin
        Bitmaps.reset bitmaps;
        bitmaps_pixels := 0
      end;
      let url = png_data_url img in
      Bitmaps.replace bitmaps img url;
      bitmaps_pixels := !bitmaps_pixels +.. pixels;
      url

let rec (render_shape: shape -> 'msg Svg.t) =
  fun { x; y; angle; scale; alpha; form} ->
  match form with
  | Circle (color, radius) -> 
     render_circle color radius x y angle scale alpha
  | Oval (color, width, height) ->
     render_oval color width height x y angle scale alpha
  | Rectangle (color, width, height) ->
     render_rectangle color width height x y angle scale alpha
  | Ngon (color, n, radius) ->
     render_ngon color n radius x y angle scale alpha
  | Polygon (color, points) -> 
     render_polygon color points x y angle scale alpha
  | Words (color, str) ->
     render_words color str x y angle scale alpha
  | Image (w, h, src) ->
     render_image w h src x y angle scale alpha
  | Bitmap (w, h, img) ->
     render_image w h (bitmap_url img) x y angle scale alpha
  (* claude: was a failwith "Todo". Same as renderGroup in
   * elm-playground: an svg <g> whose transform and opacity apply to all
   * the shapes in the group, which are positioned relative to the group
   * (x, y). *)
  | Group shapes ->
      Svg.g
        (Svg.Attributes.transform (render_transform x y angle scale) ::
         render_alpha alpha
        )
        (map_list render_shape shapes)


let (render: rendering:rendering -> screen -> shape list -> 'msg Svg.t) =
 fun ~rendering screen shapes ->
    let w = screen.width |> string_of_number in
    let h = screen.height |> string_of_number  in
    let x = screen.left |> string_of_number  in
    let y = screen.bottom |> string_of_number in

    Svg.svg
      ([Svg.Attributes.viewBox (x ^ " " ^ y ^ " " ^ w ^ " " ^ h);
       Html.style "position" "fixed";
       Html.style "top" "0";
       Html.style "left" "0";
       Svg.Attributes.width "100%";
       Svg.Attributes.height "100%";
      ] @
      (* claude: Playground.rendering, the browser's own switches; both
       * are inherited by all the shapes inside the <svg> *)
      (if rendering.antialiasing then [] else [V.attr "shape-rendering" "crispEdges"]) @
      (if rendering.smooth_images then [] else [Html.style "image-rendering" "pixelated"]))
      (map_list render_shape shapes)
