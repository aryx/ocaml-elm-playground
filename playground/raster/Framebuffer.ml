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
(* Types *)
(*****************************************************************************)

type pixels = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

type t = { width : int; height : int; pixels : pixels }

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

(* 0xRRGGBB -> 0xFFRRGGBB, the opaque pixel value stored in memory *)
let pixel_of_rgb (rgb : int) : int32 = Int32.of_int (0xFF000000 lor rgb)

(* 0xAARRGGBB -> 0xRRGGBB (dropping the alpha byte) *)
let rgb_of_pixel (pixel : int32) : int = Int32.to_int pixel land 0xFFFFFF

let red rgb = (rgb lsr 16) land 0xFF
let green rgb = (rgb lsr 8) land 0xFF
let blue rgb = rgb land 0xFF

let blend ~(src : int) ~(dst : int) ~(alpha : float) : int =
  (* each channel is a weighted average of the two colors, e.g. with
   * alpha = 0.25, a quarter of src and three quarters of dst *)
  let mix s d =
    int_of_float ((float s *. alpha) +. (float d *. (1. -. alpha)) +. 0.5)
  in
  (mix (red src) (red dst) lsl 16)
  lor (mix (green src) (green dst) lsl 8)
  lor mix (blue src) (blue dst)

(*****************************************************************************)
(* Creation *)
(*****************************************************************************)

let of_pixels (pixels : pixels) : t =
  { width = Bigarray.Array2.dim2 pixels; height = Bigarray.Array2.dim1 pixels; pixels }

let create ~width ~height : t =
  let pixels = Bigarray.Array2.create Bigarray.int32 Bigarray.c_layout height width in
  Bigarray.Array2.fill pixels (pixel_of_rgb 0xFFFFFF);
  of_pixels pixels

let clear (fb : t) ~rgb = Bigarray.Array2.fill fb.pixels (pixel_of_rgb rgb)

let get_rgb (fb : t) ~x ~y = rgb_of_pixel fb.pixels.{y, x}

(*****************************************************************************)
(* Spans *)
(*****************************************************************************)

let fill_span (fb : t) ~y ~x0 ~x1 ~rgb ~alpha =
  (* clipping: keep only the part of the span that is inside the
   * framebuffer; e.g. on a 1000-pixel-wide framebuffer, the span
   * [-20, 30) becomes [0, 30), and [990, 1200) becomes [990, 1000) *)
  let x0 = max x0 0 and x1 = min x1 fb.width in
  if y >= 0 && y < fb.height && x0 < x1 && alpha > 0. then
    if alpha >= 1. then
      (* opaque: no need to look at what's there, just overwrite; this
       * is by far the most common case, so it gets a fast path, filling
       * the whole run at once *)
      let row = Bigarray.Array2.slice_left fb.pixels y in
      Bigarray.Array1.fill (Bigarray.Array1.sub row x0 (x1 - x0)) (pixel_of_rgb rgb)
    else
      for x = x0 to x1 - 1 do
        let dst = rgb_of_pixel fb.pixels.{y, x} in
        fb.pixels.{y, x} <- pixel_of_rgb (blend ~src:rgb ~dst ~alpha)
      done

let plot (fb : t) ~x ~y ~rgb ~alpha = fill_span fb ~y ~x0:x ~x1:(x + 1) ~rgb ~alpha
