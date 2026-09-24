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
(* Cairo surfaces for the images Image_decode downloads and decodes. *)

(*****************************************************************************)
(* Conversion *)
(*****************************************************************************)

(* claude: Image_decode decodes to an interleaved, row-major, top-to-bottom
 * RGBA8 buffer (see Rgba_image.mli). Cairo.Image.create_for_data32
 * wants a (height, width) Bigarray.Array2.t of int32, each int32 being a
 * premultiplied-alpha ARGB32 pixel (alpha in the top byte); this is the
 * same layout/orientation, so we just need to repack/premultiply. *)
let cairo_surface_of_image (img : Image_decode.image) : Cairo.Surface.t =
  let w = img.width and h = img.height in
  let data = img.rgba in
  let pixels =
    Bigarray.Array2.create Bigarray.int32 Bigarray.c_layout h w in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let o = (y * w + x) * 4 in
      let r = Bigarray.Array1.unsafe_get data o in
      let g = Bigarray.Array1.unsafe_get data (o + 1) in
      let b = Bigarray.Array1.unsafe_get data (o + 2) in
      let a = Bigarray.Array1.unsafe_get data (o + 3) in
      let premultiply c = c * a / 255 in
      let pixel =
        (a lsl 24) lor (premultiply r lsl 16)
        lor (premultiply g lsl 8) lor (premultiply b)
      in
      Bigarray.Array2.unsafe_set pixels y x (Int32.of_int pixel)
    done
  done;
  Cairo.Image.create_for_data32 pixels

(*****************************************************************************)
(* Surface caches *)
(*****************************************************************************)
(* Image_decode already caches the decoded pixels; these caches just
 * avoid redoing the (per-pixel) conversion above every frame. *)

let hsurfaces : (string, Cairo.Surface.t option) Hashtbl.t = Hashtbl.create 101

let surface_of_url src =
  match Hashtbl.find_opt hsurfaces src with
  | Some surface_opt -> surface_opt
  | None ->
    let surface_opt = Option.map cairo_surface_of_image (Image_decode.image_of_url src) in
    Hashtbl.add hsurfaces src surface_opt;
    surface_opt

let hanimations : (string, Cairo.Surface.t Image_decode.animation option) Hashtbl.t =
  Hashtbl.create 101

let animation_of_url src =
  match Hashtbl.find_opt hanimations src with
  | Some anim_opt -> anim_opt
  | None ->
    let anim_opt =
      Option.map (Image_decode.map_animation cairo_surface_of_image)
        (Image_decode.animation_of_url src)
    in
    Hashtbl.add hanimations src anim_opt;
    anim_opt

let surface_of_url_at ~(time : float) (src : string) : Cairo.Surface.t option =
  match animation_of_url src with
  | None -> surface_of_url src
  | Some anim -> Some (Image_decode.frame_at ~time anim)

(* claude: a bitmap's surface, the last one kept: a video's frame stays
 * on the screen for 2 or 3 of the app's frames, and a picture for all
 * of them; a new image (not the same one, ==) is converted again *)
let last_bitmap : (Rgba_image.t * Cairo.Surface.t) option ref = ref None

let surface_of_bitmap (img : Rgba_image.t) : Cairo.Surface.t =
  match !last_bitmap with
  | Some (i, surface) when i == img -> surface
  | _ ->
      let surface = cairo_surface_of_image img in
      last_bitmap := Some (img, surface);
      surface

(*****************************************************************************)
(* Preloading *)
(*****************************************************************************)

let preload = Image_decode.preload

(* claude: also convert the preloaded images now, so not even the
 * conversion to Cairo surfaces happens mid-game *)
let load_queued () =
  Image_decode.load_queued ()
  |> List.iter (fun src ->
         ignore (surface_of_url src : Cairo.Surface.t option);
         ignore (animation_of_url src : Cairo.Surface.t Image_decode.animation option))
