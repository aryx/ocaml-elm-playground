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

(* claude: a bitmap's surface, kept while the image is shown: a video's
 * frame stays on the screen for 2 or 3 of the app's frames, and a
 * picture for all of them; a new image (not the same one, ==) is
 * converted, and kept in its turn.
 *
 * Kept by the image itself (==), found through a table: the hash is of
 * its size and a few of its bytes (an address cannot be one, the
 * collector moves it), then the images of that hash are looked at one
 * by one. And kept up to so many pixels in all rather than so many
 * images: a page of text drawn a picture a letter (mini-chrome) shows
 * hundreds of small ones in every frame, a video one large new one
 * every few frames; past the budget, everything is dropped and what is
 * still shown converted again, once.
 *
 * Before: the last 32, in a list. Enough for a page's pictures
 * (TinyMosaic), not for its letters: a window of Wikipedia has more
 * than 32 different ones (sizes, colours, weights), so most were
 * converted again at each frame, a Bigarray and a Cairo surface each:
 * 340 ms a frame, where the same letters as 60,000 rectangles took 145.
 *
 *   let last_bitmaps : (Rgba_image.t * Cairo.Surface.t) list ref = ref []
 *   let surface_of_bitmap img =
 *     match List.find_opt (fun (i, _) -> i == img) !last_bitmaps with
 *     | Some (_, surface) -> surface
 *     | None ->
 *         let surface = cairo_surface_of_image img in
 *         last_bitmaps := List.filteri (fun k _ -> k < 32) ((img, surface) :: !last_bitmaps);
 *         surface
 *)
module Bitmaps = Hashtbl.Make (struct
  type t = Rgba_image.t

  let equal = ( == )

  (* its size, and 16 bytes spread over its pixels *)
  let hash (img : Rgba_image.t) : int =
    let n = Bigarray.Array1.dim img.rgba in
    let h = ref ((img.width * 31) + img.height) in
    if n > 0 then
      for i = 0 to 15 do
        h := (!h * 31) + Bigarray.Array1.unsafe_get img.rgba (i * (n - 1) / 15)
      done;
    !h land max_int
end)

let bitmaps : Cairo.Surface.t Bitmaps.t = Bitmaps.create 512
let bitmaps_pixels = ref 0

(* 64 MB of surfaces: 50 frames of a 640 by 480 video, or every letter
 * of every size a page has *)
let bitmaps_budget = 16_000_000

let surface_of_bitmap (img : Rgba_image.t) : Cairo.Surface.t =
  match Bitmaps.find_opt bitmaps img with
  | Some surface -> surface
  | None ->
      let pixels = img.width * img.height in
      if !bitmaps_pixels + pixels > bitmaps_budget then begin
        Bitmaps.reset bitmaps;
        bitmaps_pixels := 0
      end;
      let surface = cairo_surface_of_image img in
      Bitmaps.replace bitmaps img surface;
      bitmaps_pixels := !bitmaps_pixels + pixels;
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
