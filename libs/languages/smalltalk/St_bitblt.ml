(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_bitblt.mli *)

module M = St_memory

type oop = M.oop

let count = ref 0
let changes () = !count

(* a Form's bits, width, height, and bytes per row *)
type form = { bits : Bytes.t; w : int; h : int; stride : int }

let int_field (m : M.t) (o : oop) (i : int) : int option =
  let v = M.fetch m o i in
  if M.is_int v then Some (M.int_of v)
  else match M.body m v with M.Float f -> Some (truncate (Float.round f)) | _ -> None

let get_form (m : M.t) (o : oop) : form option =
  if M.is_int o || o = M.nil || M.size m o < 3 then None
  else
    match (M.body m (M.fetch m o 0), int_field m o 1, int_field m o 2) with
    | M.Bytes bits, Some w, Some h ->
        let stride = (w + 15) / 16 * 2 in
        if w >= 0 && h >= 0 && Bytes.length bits >= stride * h then Some { bits; w; h; stride } else None
    | _ -> None

let pixel (f : form) (x : int) (y : int) : int =
  if x < 0 || y < 0 || x >= f.w || y >= f.h then 0
  else (Char.code (Bytes.get f.bits ((y * f.stride) + (x lsr 3))) lsr (7 - (x land 7))) land 1

let set_pixel (f : form) (x : int) (y : int) (v : int) : unit =
  let i = (y * f.stride) + (x lsr 3) in
  let bit = 1 lsl (7 - (x land 7)) in
  let c = Char.code (Bytes.get f.bits i) in
  Bytes.set f.bits i (Char.chr (if v = 1 then c lor bit else c land lnot bit land 255))

let copy_bits (m : M.t) (bb : oop) : bool =
  let field i = int_field m bb i in
  match
    ( get_form m (M.fetch m bb 0),
      (field 3, field 4, field 5, field 6, field 7),
      (field 8, field 9, field 10, field 11, field 12, field 13) )
  with
  | Some dest, (Some rule, Some dx, Some dy, Some w, Some h), (sx, sy, cx, cy, cw, ch) when rule >= 0 && rule < 16 ->
      let source = M.fetch m bb 1 and halftone = M.fetch m bb 2 in
      let source = if source = M.nil then None else get_form m source in
      let halftone = if halftone = M.nil then None else get_form m halftone in
      let sx = Option.value sx ~default:0 and sy = Option.value sy ~default:0 in
      let cx = Option.value cx ~default:0 and cy = Option.value cy ~default:0 in
      let cw = Option.value cw ~default:dest.w and ch = Option.value ch ~default:dest.h in
      (* the destination's rectangle clipped: to the clipping rectangle,
       * then to the Form *)
      let x0 = max dx (max cx 0) and y0 = max dy (max cy 0) in
      let x1 = min (dx + w) (min (cx + cw) dest.w) and y1 = min (dy + h) (min (cy + ch) dest.h) in
      for y = y0 to y1 - 1 do
        for x = x0 to x1 - 1 do
          let s = match source with None -> 1 | Some f -> pixel f (sx + x - dx) (sy + y - dy) in
          let s = match halftone with None -> s | Some f -> s land pixel f (x land 15) (y land 15) in
          let d = pixel dest x y in
          set_pixel dest x y ((rule lsr (3 - ((2 * s) + d))) land 1)
        done
      done;
      incr count;
      true
  | _ -> false

let form (m : M.t) (o : oop) : (int * int * (int -> int -> bool)) option =
  match get_form m o with Some f -> Some (f.w, f.h, fun x y -> pixel f x y = 1) | None -> None
