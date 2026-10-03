(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Gzip.mli *)

(* 4 bytes little-endian; as an Int32, 32 bits in JavaScript too (see
 * Crc32.mli) *)
let int32_at (s : string) (pos : int) : int32 =
  let byte i = Char.code s.[pos + i] in
  Int32.logor (Int32.shift_left (Int32.of_int (byte 3)) 24) (Int32.of_int ((byte 2 lsl 16) lor (byte 1 lsl 8) lor byte 0))

let add_int32 (b : Buffer.t) (n : int32) : unit =
  List.iter (fun shift -> Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical n shift) 0xFFl)))) [ 0; 8; 16; 24 ]

(* the member starting at [pos]: its bytes, and where it ends *)
let member (s : string) (pos : int) : string * int =
  let n = String.length s in
  let byte i = if i < n then Char.code s.[i] else failwith "gzip: the header is cut short" in
  if byte pos <> 0x1F || byte (pos + 1) <> 0x8B then failwith "gzip: not a gzip stream (no 1F 8B)";
  if byte (pos + 2) <> 8 then failwith "gzip: not deflate";
  let flg = byte (pos + 3) in
  if flg land 0xE0 <> 0 then failwith "gzip: reserved flags set";
  (* past MTIME, XFL and OS, then the fields FLG announces *)
  let p = ref (pos + 10) in
  if flg land 4 <> 0 then p := !p + 2 + byte !p + (byte (!p + 1) lsl 8);
  let skip_string () =
    while byte !p <> 0 do incr p done;
    incr p
  in
  if flg land 8 <> 0 then skip_string ();
  if flg land 16 <> 0 then skip_string ();
  if flg land 2 <> 0 then p := !p + 2;
  if !p > n then failwith "gzip: the header is cut short";
  let data, p = Inflate.inflate s ~pos:!p in
  if p + 8 > n then failwith "gzip: no CRC-32 and length at the end";
  if int32_at s p <> Crc32.string data then failwith "gzip: wrong CRC-32, the data is corrupt";
  if int32_at s (p + 4) <> Int32.of_int (String.length data) then failwith "gzip: wrong length";
  (data, p + 8)

let decompress (s : string) : string =
  let b = Buffer.create (String.length s * 4) in
  let rec members pos =
    let data, pos = member s pos in
    Buffer.add_string b data;
    if pos < String.length s then members pos
  in
  members 0;
  Buffer.contents b

let compress (s : string) : string =
  let b = Buffer.create ((String.length s / 4) + 32) in
  (* deflate, no field, no date, XFL 0 (how hard: not said), an unknown
   * system *)
  Buffer.add_string b "\x1F\x8B\x08\x00\x00\x00\x00\x00\x00\xFF";
  Buffer.add_string b (Deflate.deflate s);
  add_int32 b (Crc32.string s);
  add_int32 b (Int32.of_int (String.length s));
  Buffer.contents b
