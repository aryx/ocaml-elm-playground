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

type filter = Nearest | Bilinear

(* The framebuffer pixels the image may cover: the box around its
 * transformed corners, clipped to the framebuffer, as
 * (x0, x1, y0, y1), x1 and y1 excluded *)
let covered_box (fb : Framebuffer.t) (image : image) (m : Affine.t) =
  let w = float image.width and h = float image.height in
  let corners = List.map (Affine.apply m) [ (0., 0.); (w, 0.); (w, h); (0., h) ] in
  let xs = List.map fst corners and ys = List.map snd corners in
  let first_pixel v = int_of_float (Float.ceil (v -. 0.5)) in
  ( max 0 (first_pixel (List.fold_left min infinity xs)),
    min fb.width (first_pixel (List.fold_left max neg_infinity xs)),
    max 0 (first_pixel (List.fold_left min infinity ys)),
    min fb.height (first_pixel (List.fold_left max neg_infinity ys)) )

(* The original, simple version: for each covered pixel, where does its
 * center come from in the image? (a matrix product), then the color
 * there (the filter, as a function returning a color) *)
let draw_simple (fb : Framebuffer.t) (image : image) (m : Affine.t) ~filter ~alpha =
  let sample = match filter with Nearest -> sample_nearest | Bilinear -> sample_bilinear in
  let w = float image.width and h = float image.height in
  let x0, x1, y0, y1 = covered_box fb image m in
  let inverse = Affine.invert m in
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      let ((u, v) as p) = Affine.apply inverse (float x +. 0.5, float y +. 0.5) in
      (* from inside the image (not just its box): take its color *)
      if u >= 0. && u < w && v >= 0. && v < h then begin
        let c = sample image p in
        Framebuffer.plot fb ~x ~y ~rgb:c.rgb ~alpha:(alpha *. c.a)
      end
    done
  done

(* claude: optimization (Opti.enabled), the same pixels as draw_simple
 * but without its per-pixel overhead, about 10 small allocations per
 * pixel (the (u, v) pair from Affine.apply, a [color] record per texel
 * read and per lerp):
 *
 * - forward differencing: one pixel to the right on screen, (x+1, y),
 *   is always the same step in the image, the inverse matrix's first
 *   column (inverse.a, inverse.b), so compute (u, v) once per row and
 *   then just add that step -- the same idea as Fill's edge coherence;
 * - the filter inlined, on plain local numbers (which OCaml keeps
 *   unboxed), instead of a function returning records. *)
let draw_fast (fb : Framebuffer.t) (image : image) (m : Affine.t) ~filter ~alpha =
  let w = float image.width and h = float image.height in
  let x0, x1, y0, y1 = covered_box fb image m in
  let inverse = Affine.invert m in
  let data = image.rgba and iw = image.width and ih = image.height in
  let byte i j k = Bigarray.Array1.unsafe_get data ((((j * iw) + i) * 4) + k) in
  for y = y0 to y1 - 1 do
    (* (u, v) for the first pixel of the row, then one step per pixel *)
    let u0, v0 = Affine.apply inverse (float x0 +. 0.5, float y +. 0.5) in
    let u = ref u0 and v = ref v0 in
    for x = x0 to x1 - 1 do
      let u' = !u and v' = !v in
      if u' >= 0. && u' < w && v' >= 0. && v' < h then begin
        match filter with
        | Nearest ->
            let i = int_of_float u' and j = int_of_float v' in
            let a = byte i j 3 in
            if a > 0 then
              Framebuffer.plot fb ~x ~y
                ~rgb:((byte i j 0 lsl 16) lor (byte i j 1 lsl 8) lor byte i j 2)
                ~alpha:(alpha *. float a /. 255.)
        | Bilinear ->
            (* as sample_bilinear: the 4 texels around (u, v), weighted *)
            let fx = u' -. 0.5 and fy = v' -. 0.5 in
            let i = int_of_float (Float.floor fx) and j = int_of_float (Float.floor fy) in
            let tx = fx -. float i and ty = fy -. float j in
            let i0 = max 0 i and i1 = min (iw - 1) (i + 1) in
            let j0 = max 0 j and j1 = min (ih - 1) (j + 1) in
            let mix k =
              let top = (float (byte i0 j0 k) *. (1. -. tx)) +. (float (byte i1 j0 k) *. tx) in
              let bottom = (float (byte i0 j1 k) *. (1. -. tx)) +. (float (byte i1 j1 k) *. tx) in
              (top *. (1. -. ty)) +. (bottom *. ty)
            in
            let a = mix 3 in
            if a > 0. then begin
              let channel k = int_of_float (mix k +. 0.5) in
              Framebuffer.plot fb ~x ~y
                ~rgb:((channel 0 lsl 16) lor (channel 1 lsl 8) lor channel 2)
                ~alpha:(alpha *. a /. 255.)
            end
      end;
      u := u' +. inverse.a;
      v := v' +. inverse.b
    done
  done

let draw fb image m ~filter ~alpha =
  if !Opti.enabled then draw_fast fb image m ~filter ~alpha else draw_simple fb image m ~filter ~alpha
