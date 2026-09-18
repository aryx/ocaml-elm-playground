(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Matting.mli. *)

type rgba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* the channel at bit [shift] of a 0xRRGGBB color: 16 red, 8 green, 0 blue.
 * claude: at the top level, not inside premultiplied_rgba's loop,
 * where OCaml would allocate a closure for it at each of the million
 * pixels of a 1000x1000 image (~11ms of the first version's 64) *)
let channel (rgb : int) (shift : int) : int = (rgb lsr shift) land 0xFF

let premultiplied_rgba ~(width : int) ~(height : int) (draw : Framebuffer.t -> unit) : rgba =
  let over_black = Framebuffer.create ~width ~height in
  Framebuffer.clear over_black ~rgb:0x000000;
  draw over_black;
  let over_white = Framebuffer.create ~width ~height in
  Framebuffer.clear over_white ~rgb:0xFFFFFF;
  draw over_white;
  (* all transparent to start with: most pixels stay so (nothing drawn
   * there), and filling with zeros is fast *)
  let out = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (width * height * 4) in
  Bigarray.Array1.fill out 0;
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let b = Framebuffer.get_rgb over_black ~x ~y and w = Framebuffer.get_rgb over_white ~x ~y in
      (* claude: only where something was drawn: black and white left
       * as they were is transparent, already in [out]. Skipping them
       * took a HUD of a few words on a 1000x1000 window from 27ms to
       * 20ms: they're nearly all the pixels. *)
      if not (b = 0x000000 && w = 0xFFFFFF) then begin
        (* a = 1 - (W - B)/255, from each channel; they agree up to
         * rounding, so their average *)
        let diff_sum =
          channel w 16 - channel b 16 + (channel w 8 - channel b 8) + (channel w 0 - channel b 0)
        in
        let alpha = 255 - ((diff_sum + 1) / 3) in
        let i = ((y * width) + x) * 4 in
        out.{i} <- channel b 16;
        out.{i + 1} <- channel b 8;
        out.{i + 2} <- channel b 0;
        (* claude: Int.max/Int.min, not max/min. The first version,
         * [max 0 (min 255 alpha)], took ~25ms of its 64 on the million
         * pixels of a 1000x1000 window, i.e. ~12ns per call, for what
         * should be one machine comparison. Why: Stdlib's [max] is
         *
         *   let max a b = if a >= b then a else b    (* 'a -> 'a -> 'a *)
         *
         * compiled once, for every type, so its [>=] can't know it's
         * comparing ints: it calls the C function caml_greaterequal,
         * which inspects both values at runtime (int? pointer? to a
         * string, a float, a tuple?) before comparing. ocamlopt
         * replaces a comparison by a single instruction only where the
         * types are known at that very spot, e.g. [alpha >= 0] written
         * here, with [alpha : int]; calling a polymorphic function hides
         * the type (unless the compiler inlines it first: flambda, an
         * optional optimizing variant of ocamlopt, can; the standard
         * one, used here, doesn't). Int.max/Int.min (OCaml >= 4.13) are the same code
         * with ints, so their comparisons are single instructions. The
         * same trap: [compare], [=], [<] passed around as values, or
         * used inside a function polymorphic in the compared values'
         * type (e.g. [List.sort compare] on ints: [Int.compare] is
         * faster). *)
        out.{i + 3} <- Int.max 0 (Int.min 255 alpha)
      end
    done
  done;
  out
