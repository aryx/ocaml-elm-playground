(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Vga_font.mli *)

let width = 8
let height = 16

(* the 256 glyphs, 16 bytes each, and Unicode to code page 437, read from
 * the file's lines: "41 0041 A 000010386cc6c6fec6c6c6c600000000" *)
let glyphs : Bytes.t = Bytes.make (256 * height) '\000'
let unicode : (int, int) Hashtbl.t = Hashtbl.create 256

let () =
  String.split_on_char '\n' Vga_font_data.txt
  |> List.iter (fun line ->
         if String.length line > 40 then begin
           let c = int_of_string ("0x" ^ String.sub line 0 2) and u = int_of_string ("0x" ^ String.sub line 3 4) in
           (* the rows are the last 32 characters (the character before
            * them can be several bytes of UTF-8) *)
           let hex = String.sub line (String.length line - 32) 32 in
           for y = 0 to height - 1 do
             Bytes.set glyphs ((c * height) + y) (Char.chr (int_of_string ("0x" ^ String.sub hex (2 * y) 2)))
           done;
           if u <> 0 then Hashtbl.replace unicode u c
         end)

let row (c : int) (y : int) : int = Char.code (Bytes.unsafe_get glyphs (((c land 255) * height) + y))
let bit (c : int) (x : int) (y : int) : bool = (row c y lsr (7 - x)) land 1 = 1
let of_unicode (u : int) : int option = Hashtbl.find_opt unicode u
let question = Char.code '?'

let decode (s : string) (i : int) : int * int =
  let b k = Char.code s.[i + k] in
  let n = String.length s - i in
  let c0 = b 0 in
  let cont k = k < n && b k land 0xC0 = 0x80 in
  let cp, len =
    if c0 < 0x80 then (c0, 1)
    else if c0 land 0xE0 = 0xC0 && cont 1 then (((c0 land 0x1F) lsl 6) lor (b 1 land 0x3F), 2)
    else if c0 land 0xF0 = 0xE0 && cont 1 && cont 2 then (((c0 land 0x0F) lsl 12) lor ((b 1 land 0x3F) lsl 6) lor (b 2 land 0x3F), 3)
    else if c0 land 0xF8 = 0xF0 && cont 1 && cont 2 && cont 3 then
      (((c0 land 0x07) lsl 18) lor ((b 1 land 0x3F) lsl 12) lor ((b 2 land 0x3F) lsl 6) lor (b 3 land 0x3F), 4)
    else (-1, 1)
  in
  ((if cp < 0 then question else Option.value ~default:question (of_unicode cp)), len)
