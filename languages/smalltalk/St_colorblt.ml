(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_colorblt.mli *)

module M = St_memory

type oop = M.oop

(* a Form's bits, width, height, bytes per row, and bits per pixel *)
type form = { bits : Bytes.t; w : int; h : int; stride : int; depth : int }

let stride ~(depth : int) (w : int) : int =
  match depth with 1 -> (w + 15) / 16 * 2 | 8 -> (w + 3) / 4 * 4 | _ -> 4 * w

(*****************************************************************************)
(* A pixel *)
(*****************************************************************************)

let byte (f : form) (i : int) : int = Char.code (Bytes.unsafe_get f.bits i)

let get (f : form) (x : int) (y : int) : int =
  match f.depth with
  | 1 -> (byte f ((y * f.stride) + (x lsr 3)) lsr (7 - (x land 7))) land 1
  | 8 -> byte f ((y * f.stride) + x)
  | _ ->
      let i = (y * f.stride) + (4 * x) in
      (byte f i lsl 24) lor (byte f (i + 1) lsl 16) lor (byte f (i + 2) lsl 8) lor byte f (i + 3)

let put (f : form) (x : int) (y : int) (v : int) : unit =
  let set i b = Bytes.unsafe_set f.bits i (Char.unsafe_chr (b land 255)) in
  match f.depth with
  | 1 ->
      let i = (y * f.stride) + (x lsr 3) and bit = 128 lsr (x land 7) in
      set i (if v land 1 = 1 then byte f i lor bit else byte f i land lnot bit)
  | 8 -> set ((y * f.stride) + x) v
  | _ ->
      let i = (y * f.stride) + (4 * x) in
      set i (v lsr 24);
      set (i + 1) (v lsr 16);
      set (i + 2) (v lsr 8);
      set (i + 3) v

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let combine ~(rule : int) ~(depth : int) (s : int) (d : int) : int =
  match rule with
  | 24 when depth = 32 ->
      let a = (s lsr 24) land 255 in
      let mix shift = (((((s lsr shift) land 255) * a) + (((d lsr shift) land 255) * (255 - a)) + 127) / 255) lsl shift in
      let alpha = a + (((((d lsr 24) land 255) * (255 - a)) + 127) / 255) in
      (alpha lsl 24) lor mix 16 lor mix 8 lor mix 0
  | 25 -> if s = 0 then d else s
  | _ when rule >= 0 && rule < 16 ->
      (* St_bitblt's truth table, on every bit of the pixel at once *)
      let on bit v = if rule land bit <> 0 then v else 0 in
      on 8 (lnot s land lnot d) lor on 4 (lnot s land d) lor on 2 (s land lnot d) lor on 1 (s land d)
  | _ -> d

(*****************************************************************************)
(* A pixel at a time *)
(*****************************************************************************)

(* the definition, as St_colorblt.mli states it. What [blit] must
 * equal. *)
let blit_pixels ~(dest : form) ~(source : form option) ~(map : int array option) ~(halftone : form option) ~(rule : int)
    ~(dx : int) ~(dy : int) ~(sx : int) ~(sy : int) ((x0, y0, x1, y1) : int * int * int * int) : unit =
  let ones = if dest.depth = 32 then -1 else (1 lsl dest.depth) - 1 in
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      let s =
        match source with
        | None -> ones
        | Some f -> (
            let p = get f (sx + x - dx) (sy + y - dy) in
            match map with None -> p | Some table -> table.(p))
      in
      let s = match halftone with None -> s | Some f -> s land get f (x mod f.w) (y mod f.h) in
      put dest x y (combine ~rule ~depth:dest.depth s (get dest x y))
    done
  done

(*****************************************************************************)
(* A row at a time *)
(*****************************************************************************)

(* claude: a screen of Morphic is mostly two things, a rectangle filled
 * with a colour and a picture stored as it is; in both, a row's bytes
 * are known without looking at its pixels one by one:
 *
 *   a fill    the first row made a pixel at a time, the others copies
 *             of it (Bytes.blit: the machine's memmove)
 *   a store   each row of the source copied onto the destination's
 *
 *   text      a glyph is mostly paper: the source has one bit a
 *             pixel and its 0 maps to a pixel of zeros, which neither
 *             blending (alpha 0) nor painting draws. Its bytes are
 *             read, a zero one skipped -- eight pixels at once -- and
 *             only the 1s of the others drawn
 *
 * Everything else -- alpha, another map -- goes a pixel at a time.
 * St_bench times them ("colour", "text"). *)
let blit_rows ~(dest : form) ~(source : form option) ~(map : int array option) ~(halftone : form option) ~(rule : int)
    ~(dx : int) ~(dy : int) ~(sx : int) ~(sy : int) ((x0, y0, x1, y1) as area : int * int * int * int) : unit =
  let bpp = dest.depth / 8 in
  let at y = (y * dest.stride) + (x0 * bpp) and len = (x1 - x0) * bpp in
  match (source, map, halftone) with
  | Some f, Some table, None when f.depth = 1 && table.(0) = 0 && (rule = 24 || rule = 25) && bpp > 0 ->
      let ink = table.(1) in
      for y = y0 to y1 - 1 do
        let row = (sy + y - dy) * f.stride and x = ref x0 in
        while !x < x1 do
          let u = sx + !x - dx in
          if u land 7 = 0 && !x + 8 <= x1 && byte f (row + (u lsr 3)) = 0 then x := !x + 8
          else begin
            if (byte f (row + (u lsr 3)) lsr (7 - (u land 7))) land 1 = 1 then
              put dest !x y (combine ~rule ~depth:dest.depth ink (get dest !x y));
            incr x
          end
        done
      done
  | _ when bpp = 0 || rule <> 3 || x1 <= x0 || y1 <= y0 -> blit_pixels ~dest ~source ~map ~halftone ~rule ~dx ~dy ~sx ~sy area
  | None, _, Some { w = 1; h = 1; _ } ->
      blit_pixels ~dest ~source ~map ~halftone ~rule ~dx ~dy ~sx ~sy (x0, y0, x1, y0 + 1);
      for y = y0 + 1 to y1 - 1 do
        Bytes.blit dest.bits (at y0) dest.bits (at y) len
      done
  | Some f, None, None when f.depth = dest.depth ->
      for y = y0 to y1 - 1 do
        Bytes.blit f.bits (((sy + y - dy) * f.stride) + ((sx + x0 - dx) * bpp)) dest.bits (at y) len
      done
  | _ -> blit_pixels ~dest ~source ~map ~halftone ~rule ~dx ~dy ~sx ~sy area

let blit ?(simple = false) ~(dest : form) ~(source : form option) ~(map : int array option) ~(halftone : form option)
    ~(rule : int) ~(dx : int) ~(dy : int) ~(sx : int) ~(sy : int) (area : int * int * int * int) : unit =
  (* a Form copied onto itself: from a copy of it, as St_bitblt *)
  let source =
    match source with Some f when f.bits == dest.bits -> Some { f with bits = Bytes.copy f.bits } | s -> s
  in
  (if simple then blit_pixels else blit_rows) ~dest ~source ~map ~halftone ~rule ~dx ~dy ~sx ~sy area

(*****************************************************************************)
(* The primitive *)
(*****************************************************************************)

let get_form (m : M.t) (o : oop) : form option =
  if M.is_int o || o = M.nil || M.size m o < 3 then None
  else
    let depth =
      if M.size m o < 4 || M.fetch m o 3 = M.nil then Some 1
      else match St_bitblt.int_field m o 3 with Some (1 | 8 | 32) as d -> d | _ -> None
    in
    match (M.body m (M.fetch m o 0), St_bitblt.int_field m o 1, St_bitblt.int_field m o 2, depth) with
    | M.Bytes bits, Some w, Some h, Some depth ->
        let stride = stride ~depth w in
        if w >= 0 && h >= 0 && Bytes.length bits >= stride * h then Some { bits; w; h; stride; depth } else None
    | _ -> None

(* a field that is nil, None; a Form, Some of it *)
let form_or_nil (m : M.t) (o : oop) : form option option =
  if o = M.nil then Some None else Option.map Option.some (get_form m o)

(* the colour map: the fifteenth field when it is bytes (the Blue
 * Book's Pen has its frame there), an entry [bpp] bytes *)
let get_map (m : M.t) (bb : oop) ~(bpp : int) : int array option =
  if M.size m bb < 15 || M.is_int (M.fetch m bb 14) then None
  else
    match M.body m (M.fetch m bb 14) with
    | M.Bytes b ->
        let entry i =
          let v = ref 0 in
          for k = 0 to bpp - 1 do
            v := (!v lsl 8) lor Char.code (Bytes.get b ((i * bpp) + k))
          done;
          !v
        in
        Some (Array.init (Bytes.length b / bpp) entry)
    | _ -> None

let copy_bits (m : M.t) (bb : oop) : bool =
  let field i = St_bitblt.int_field m bb i in
  match (get_form m (M.fetch m bb 0), form_or_nil m (M.fetch m bb 1), form_or_nil m (M.fetch m bb 2), field 3) with
  | Some dest, Some source, Some halftone, Some rule -> (
      let map = get_map m bb ~bpp:(if dest.depth = 32 then 4 else 1) in
      let one_bit = function None -> true | Some (f : form) -> f.depth = 1 in
      if dest.depth = 1 && one_bit source && one_bit halftone && map = None && rule < 16 then St_bitblt.copy_bits m bb
      else
        let rule_ok = (rule >= 0 && rule < 16) || rule = 25 || (rule = 24 && dest.depth = 32) in
        let halftone_ok = match halftone with None -> true | Some f -> f.depth = dest.depth && f.w > 0 && f.h > 0 in
        let source_ok =
          match (source, map) with
          | None, _ -> true
          | Some f, None -> f.depth = dest.depth
          | Some f, Some table -> f.depth <= 8 && Array.length table >= 1 lsl f.depth
        in
        match (field 4, field 5, field 6, field 7) with
        | Some dx, Some dy, Some w, Some h when rule_ok && halftone_ok && source_ok ->
            let or0 i = Option.value (field i) ~default:0 in
            let sx = or0 8 and sy = or0 9 and cx = or0 10 and cy = or0 11 in
            let cw = Option.value (field 12) ~default:dest.w and ch = Option.value (field 13) ~default:dest.h in
            (* the destination's rectangle clipped: to the clipping
             * rectangle, to the Form, and to what the source covers *)
            let x0 = max dx (max cx 0) and y0 = max dy (max cy 0) in
            let x1 = min (dx + w) (min (cx + cw) dest.w) and y1 = min (dy + h) (min (cy + ch) dest.h) in
            let x0, y0, x1, y1 =
              match source with
              | None -> (x0, y0, x1, y1)
              | Some f -> (max x0 (dx - sx), max y0 (dy - sy), min x1 (dx - sx + f.w), min y1 (dy - sy + f.h))
            in
            blit ~dest ~source ~map ~halftone ~rule ~dx ~dy ~sx ~sy (x0, y0, x1, y1);
            St_bitblt.count_change ();
            true
        | _ -> false)
  | _ -> false

(*****************************************************************************)
(* For the host *)
(*****************************************************************************)

(* Color.st's palette: 0 transparent, then 1 + 36 r + 6 g + b, each of
 * r, g and b from 0 to 5 *)
let palette (p : int) : int * int * int * int =
  if p = 0 || p > 216 then (0, 0, 0, 0) else ((p - 1) / 36 * 51, (p - 1) / 6 mod 6 * 51, (p - 1) mod 6 * 51, 255)

let rgba (m : M.t) (o : oop) : (int * int * Bytes.t) option =
  match get_form m o with
  | None -> None
  | Some f ->
      let out = Bytes.create (4 * f.w * f.h) in
      let set i r g b a =
        Bytes.unsafe_set out i (Char.unsafe_chr r);
        Bytes.unsafe_set out (i + 1) (Char.unsafe_chr g);
        Bytes.unsafe_set out (i + 2) (Char.unsafe_chr b);
        Bytes.unsafe_set out (i + 3) (Char.unsafe_chr a)
      in
      for y = 0 to f.h - 1 do
        for x = 0 to f.w - 1 do
          let i = 4 * ((y * f.w) + x) in
          match f.depth with
          | 32 ->
              (* alpha, red, green, blue: the alpha goes last *)
              let j = (y * f.stride) + (4 * x) in
              set i (byte f (j + 1)) (byte f (j + 2)) (byte f (j + 3)) (byte f j)
          | 8 ->
              let r, g, b, a = palette (get f x y) in
              set i r g b a
          | _ -> if get f x y = 1 then set i 0 0 0 255 else set i 255 255 255 255
        done
      done;
      Some (f.w, f.h, out)
