(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Blit.mli for the ideas, with pictures *)

type image = {
  width : int;
  height : int;
  rgba : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

type color = { rgb : int; a : float }

(*****************************************************************************)
(* Filtering *)
(*****************************************************************************)

(* The 4 bytes of pixel (i, j), as a color *)
let pixel (image : image) i j : color =
  let o = ((j * image.width) + i) * 4 in
  let byte k = image.rgba.{o + k} in
  { rgb = (byte 0 lsl 16) lor (byte 1 lsl 8) lor byte 2; a = float (byte 3) /. 255. }

let clamp lo hi v = max lo (min hi v)

let sample_nearest (image : image) (u, v) : color =
  let i = clamp 0 (image.width - 1) (int_of_float (Float.floor u)) in
  let j = clamp 0 (image.height - 1) (int_of_float (Float.floor v)) in
  pixel image i j

(* Mixing colors channel by channel, the alpha too; e.g.
 * lerp red blue 0.25 = 75% red + 25% blue. (Mixing a transparent pixel
 * would really need premultiplied alpha, see Porter & Duff, to not
 * darken the edges of sprites: a known simplification here.) *)
let lerp (c0 : color) (c1 : color) (t : float) : color =
  let mix shift =
    let x0 = (c0.rgb lsr shift) land 0xFF and x1 = (c1.rgb lsr shift) land 0xFF in
    int_of_float ((float x0 *. (1. -. t)) +. (float x1 *. t) +. 0.5)
  in
  { rgb = (mix 16 lsl 16) lor (mix 8 lsl 8) lor mix 0; a = (c0.a *. (1. -. t)) +. (c1.a *. t) }

let sample_bilinear (image : image) (u, v) : color =
  (* the centers of pixels i and i+1 are at i + 0.5 and i + 1.5, so
   * the pixel whose center is just left of u is floor (u - 0.5), and
   * tx how far u is towards the next center *)
  let x = u -. 0.5 and y = v -. 0.5 in
  let i = int_of_float (Float.floor x) and j = int_of_float (Float.floor y) in
  let tx = x -. float i and ty = y -. float j in
  let at i j = pixel image (clamp 0 (image.width - 1) i) (clamp 0 (image.height - 1) j) in
  let top = lerp (at i j) (at (i + 1) j) tx in
  let bottom = lerp (at i (j + 1)) (at (i + 1) (j + 1)) tx in
  lerp top bottom ty

(*****************************************************************************)
(* Inverse mapping *)
(*****************************************************************************)

let draw (fb : Framebuffer.t) (image : image) (m : Affine.t) ~sample ~alpha =
  let w = float image.width and h = float image.height in
  (* 1. the framebuffer pixels the image may cover: the box around its
   * transformed corners, clipped to the framebuffer *)
  let corners = List.map (Affine.apply m) [ (0., 0.); (w, 0.); (w, h); (0., h) ] in
  let xs = List.map fst corners and ys = List.map snd corners in
  let first_pixel v = int_of_float (Float.ceil (v -. 0.5)) in
  let x0 = max 0 (first_pixel (List.fold_left min infinity xs)) in
  let x1 = min fb.width (first_pixel (List.fold_left max neg_infinity xs)) in
  let y0 = max 0 (first_pixel (List.fold_left min infinity ys)) in
  let y1 = min fb.height (first_pixel (List.fold_left max neg_infinity ys)) in
  (* 2. for each, where does its center come from in the image? *)
  let inverse = Affine.invert m in
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      let ((u, v) as p) = Affine.apply inverse (float x +. 0.5, float y +. 0.5) in
      (* 3. from inside the image (not just its box): take its color *)
      if u >= 0. && u < w && v >= 0. && v < h then begin
        let c = sample image p in
        Framebuffer.plot fb ~x ~y ~rgb:c.rgb ~alpha:(alpha *. c.a)
      end
    done
  done
