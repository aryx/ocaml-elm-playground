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

(*****************************************************************************)
(* A pixel at a time *)
(*****************************************************************************)

(* the definition, as St_bitblt.mli states it: each pixel of the
 * rectangle read, combined, written. What [blit] must equal. *)
let blit_pixels ~(dest : form) ~(source : form option) ~(halftone : form option) ~(rule : int)
    ~(dx : int) ~(dy : int) ~(sx : int) ~(sy : int) ((x0, y0, x1, y1) : int * int * int * int) : unit =
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      let s = match source with None -> 1 | Some f -> pixel f (sx + x - dx) (sy + y - dy) in
      let s = match halftone with None -> s | Some f -> s land pixel f (x land 15) (y land 15) in
      let d = pixel dest x y in
      set_pixel dest x y ((rule lsr (3 - ((2 * s) + d))) land 1)
    done
  done

(*****************************************************************************)
(* A byte at a time *)
(*****************************************************************************)

(* claude: the same, eight pixels at once: the rule is a function of
 * two bits, so it is a function of two bytes, bit by bit, done by the
 * machine's and, or and not. What is left is to line the source's bits
 * up with the destination's bytes -- the source starts anywhere, so a
 * destination byte's eight source bits come from two source bytes,
 * shifted -- and to keep the destination's bits outside the rectangle,
 * with a mask at each end of a row:
 *
 *   destination   |........|........|........|     bytes
 *   rectangle          xxxx xxxxxxxx xx
 *   masks          00001111 11111111 11000000
 *   source      |........|........|........|       r bits to the left
 *
 * St_bench times the two ("blit"). *)

(* byte i of row y: 0 outside the Form, the row's padding cleared *)
let byte_at (f : form) (y : int) (i : int) : int =
  if y < 0 || y >= f.h || i < 0 || i * 8 >= f.w then 0
  else
    let c = Char.code (Bytes.get f.bits ((y * f.stride) + i)) in
    if (i + 1) * 8 <= f.w then c else c land (0xFF00 lsr (f.w land 7)) land 255

(* the eight pixels of row y from x on, x anywhere *)
let bits8 (f : form) (y : int) (x : int) : int =
  let i = x asr 3 and r = x land 7 in
  if r = 0 then byte_at f y i else ((byte_at f y i lsl r) lor (byte_at f y (i + 1) lsr (8 - r))) land 255

let blit_bytes ~(dest : form) ~(source : form option) ~(halftone : form option) ~(rule : int)
    ~(dx : int) ~(dy : int) ~(sx : int) ~(sy : int) ((x0, y0, x1, y1) : int * int * int * int) : unit =
  (* the rule's four cases, each all ones or none *)
  let on bit = if rule land bit <> 0 then 255 else 0 in
  let r00 = on 8 and r01 = on 4 and r10 = on 2 and r11 = on 1 in
  (* a fill -- no source, no halftone, a rule that does not look at the
   * destination (0 white, 15 black) -- writes the same byte everywhere
   * but at a row's two ends: the middle of a row at once *)
  let fill = (match (source, halftone) with None, None -> true | _ -> false) && r10 = r11 in
  if x1 > x0 then
    for y = y0 to y1 - 1 do
      let first = x0 lsr 3 and last = (x1 - 1) lsr 3 in
      let middle = fill && last - first >= 2 in
      if middle then Bytes.fill dest.bits ((y * dest.stride) + first + 1) (last - first - 1) (Char.chr r11);
      for i = first to last do
       if not (middle && i > first && i < last) then begin
        let left = i * 8 in
        let mask =
          (if left < x0 then 255 lsr (x0 - left) else 255)
          land if left + 8 > x1 then (0xFF00 lsr (x1 - left)) land 255 else 255
        in
        let s = match source with None -> 255 | Some f -> bits8 f (sy + y - dy) (sx + left - dx) in
        let s = match halftone with None -> s | Some f -> s land byte_at f (y land 15) (i land 1) in
        let at = (y * dest.stride) + i in
        let d = Char.code (Bytes.get dest.bits at) in
        let ns = lnot s and nd = lnot d in
        let r = (r00 land ns land nd) lor (r01 land ns land d) lor (r10 land s land nd) lor (r11 land s land d) in
        Bytes.set dest.bits at (Char.chr ((d land lnot mask) lor (r land mask) land 255))
       end
      done
    done

let blit ?(simple = false) ~(dest : form) ~(source : form option) ~(halftone : form option) ~(rule : int)
    ~(dx : int) ~(dy : int) ~(sx : int) ~(sy : int) (area : int * int * int * int) : unit =
  (* a Form copied onto itself (scrolling): from a copy of it, so that
   * no pixel is read after it was written (Ingalls chose the direction
   * to copy in instead: an exercise) *)
  let source =
    match source with Some f when f.bits == dest.bits -> Some { f with bits = Bytes.copy f.bits } | s -> s
  in
  (if simple then blit_pixels else blit_bytes) ~dest ~source ~halftone ~rule ~dx ~dy ~sx ~sy area

(*****************************************************************************)
(* The primitive *)
(*****************************************************************************)

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
      blit ~dest ~source ~halftone ~rule ~dx ~dy ~sx ~sy (x0, y0, x1, y1);
      incr count;
      true
  | _ -> false

let form (m : M.t) (o : oop) : (int * int * (int -> int -> bool)) option =
  match get_form m o with Some f -> Some (f.w, f.h, fun x y -> pixel f x y = 1) | None -> None
