(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Wire.mli *)

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

type writer = Buffer.t

let to_bytes (write : writer -> unit) : string =
  let b = Buffer.create 64 in
  write b;
  Buffer.contents b

let check (what : string) (lo : int) (hi : int) (n : int) : unit =
  if n < lo || n > hi then invalid_arg (Printf.sprintf "Wire.put_%s: %d not in [%d, %d]" what n lo hi)

let put_u8 (w : writer) (n : int) : unit =
  check "u8" 0 255 n;
  Buffer.add_char w (Char.chr n)

let put_u16 (w : writer) (n : int) : unit =
  check "u16" 0 65535 n;
  Buffer.add_char w (Char.chr (n lsr 8));
  Buffer.add_char w (Char.chr (n land 255))

let max_varint = (1 lsl 28) - 1

(* 7 bits at a time, the most significant first, the high bit set on
 * every byte but the last *)
let put_varint (w : writer) (n : int) : unit =
  check "varint" 0 max_varint n;
  let rec groups n acc = if n < 128 then n :: acc else groups (n lsr 7) ((n land 127) :: acc) in
  let gs = groups n [] in
  let last = List.length gs - 1 in
  List.iteri (fun i g -> Buffer.add_char w (Char.chr (if i < last then g lor 128 else g))) gs

(* 0, -1, 1, -2, 2 -> 0, 1, 2, 3, 4: the sign in the lowest bit *)
let zigzag (n : int) : int = if n >= 0 then 2 * n else (-2 * n) - 1
let unzigzag (z : int) : int = if z land 1 = 0 then z lsr 1 else -((z + 1) lsr 1)

let put_signed (w : writer) (n : int) : unit =
  check "signed" (-(1 lsl 27)) ((1 lsl 27) - 1) n;
  put_varint w (zigzag n)

let put_string (w : writer) (s : string) : unit =
  put_varint w (String.length s);
  Buffer.add_string w s

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

type reader = { bytes : string; mutable pos : int }

exception Refused of string

let fail (r : reader) (why : string) : 'a = raise (Refused (Printf.sprintf "%s, at byte %d" why r.pos))

let byte (r : reader) : int =
  if r.pos >= String.length r.bytes then fail r "bytes missing"
  else
    let b = Char.code r.bytes.[r.pos] in
    r.pos <- r.pos + 1;
    b

let parse (read : reader -> 'a) (bytes : string) : ('a, string) result =
  let r = { bytes; pos = 0 } in
  match read r with
  | v when r.pos = String.length bytes -> Ok v
  | _ -> Error (Printf.sprintf "%d bytes left over, at byte %d" (String.length bytes - r.pos) r.pos)
  | exception Refused why -> Error why

let get_u8 (r : reader) : int = byte r

let get_u16 (r : reader) : int =
  let hi = byte r in
  let lo = byte r in
  (hi lsl 8) lor lo

let get_varint (r : reader) : int =
  let rec go n acc =
    if n = 4 then fail r "a varint longer than 4 bytes"
    else
      let b = byte r in
      (* a first byte of 0x80 adds nothing: a longer form of a shorter
       * varint, refused (one value, one encoding) *)
      if n = 0 && b = 0x80 then fail r "a varint with a useless leading byte"
      else
        let acc = (acc lsl 7) lor (b land 127) in
        if b land 128 <> 0 then go (n + 1) acc else acc
  in
  go 0 0

let get_signed (r : reader) : int = unzigzag (get_varint r)

let get_string (r : reader) : string =
  let n = get_varint r in
  if r.pos + n > String.length r.bytes then fail r (Printf.sprintf "a string of %d bytes, past the end" n)
  else
    let s = String.sub r.bytes r.pos n in
    r.pos <- r.pos + n;
    s
