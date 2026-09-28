(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Charset.mli *)

type t = Utf_8 | Windows_1252

(*****************************************************************************)
(* Labels *)
(*****************************************************************************)

(* the WHATWG's labels (Encoding, section 4.2) *)
let utf_8_labels = [ "unicode-1-1-utf-8"; "unicode11utf8"; "unicode20utf8"; "utf-8"; "utf8"; "x-unicode20utf8" ]

let windows_1252_labels =
  [
    "ansi_x3.4-1968"; "ascii"; "cp1252"; "cp819"; "csisolatin1"; "ibm819"; "iso-8859-1"; "iso-ir-100"; "iso8859-1";
    "iso88591"; "iso_8859-1"; "iso_8859-1:1987"; "l1"; "latin1"; "us-ascii"; "windows-1252"; "x-cp1252";
  ]

let of_label (label : string) : t option =
  let label = String.lowercase_ascii (String.trim label) in
  if List.mem label utf_8_labels then Some Utf_8
  else if List.mem label windows_1252_labels then Some Windows_1252
  else None

(*****************************************************************************)
(* Finding "charset=" *)
(*****************************************************************************)

(* the index of [sub] in [s] from [from], if any *)
let rec find (s : string) (sub : string) (from : int) : int option =
  let n = String.length sub in
  if from + n > String.length s then None
  else if String.sub s from n = sub then Some from
  else find s sub (from + 1)

(* the value after "charset=" at [i] in [s] (lowercased already):
 * spaces skipped, then quoted, or up to a space, ';', '>' or '/' *)
let value_after (s : string) (i : int) (stop : int) : string =
  let i = ref (i + String.length "charset=") in
  while !i < stop && s.[!i] = ' ' do incr i done;
  let quote = if !i < stop && (s.[!i] = '"' || s.[!i] = '\'') then Some s.[!i] else None in
  if quote <> None then incr i;
  let start = !i in
  let ends c = match quote with Some q -> c = q | None -> c = ' ' || c = ';' || c = '>' || c = '/' || c = '"' || c = '\'' in
  while !i < stop && not (ends s.[!i]) do incr i done;
  String.sub s start (!i - start)

let of_content_type (content_type : string) : t option =
  let s = String.lowercase_ascii content_type in
  match find s "charset=" 0 with Some i -> of_label (value_after s i (String.length s)) | None -> None

let of_meta (bytes : string) : t option =
  let s = String.lowercase_ascii (String.sub bytes 0 (min 1024 (String.length bytes))) in
  (* each <meta ...> in turn, its first "charset=" inside it *)
  let rec from (i : int) : t option =
    match find s "<meta" i with
    | None -> None
    | Some start -> (
        let stop = match String.index_from_opt s start '>' with Some j -> j | None -> String.length s in
        match find s "charset=" start with
        | Some i when i < stop -> (
            match of_label (value_after s i stop) with Some t -> Some t | None -> from stop)
        | _ -> from stop)
  in
  from 0

(*****************************************************************************)
(* UTF-8 *)
(*****************************************************************************)

(* OCaml's decoder (String.get_utf_8_uchar) is the strict one Unicode
 * asks for: an overlong form, a surrogate, a byte that can't start or
 * continue a sequence are each invalid, with the length of the maximal
 * part to skip -- which is also what the WHATWG replaces by one U+FFFD *)
let is_utf_8 (s : string) : bool =
  let rec ok (i : int) : bool =
    if i >= String.length s then true
    else
      let d = String.get_utf_8_uchar s i in
      Uchar.utf_decode_is_valid d && ok (i + Uchar.utf_decode_length d)
  in
  ok 0

let has_bom (s : string) : bool = String.length s >= 3 && String.sub s 0 3 = "\xEF\xBB\xBF"

let is_ascii (s : string) : bool = String.for_all (fun c -> Char.code c < 0x80) s

let detect ?content_type (bytes : string) : t =
  if has_bom bytes then Utf_8
  else
    match Option.bind content_type of_content_type with
    | Some t -> t
    | None -> (
        match of_meta bytes with
        | Some t -> t
        | None -> if (not (is_ascii bytes)) && is_utf_8 bytes then Utf_8 else Windows_1252)

(*****************************************************************************)
(* Decoding *)
(*****************************************************************************)

(* bytes 80 to 9F, the WHATWG's index windows-1252 *)
let windows_1252_high =
  [|
    0x20AC; 0x0081; 0x201A; 0x0192; 0x201E; 0x2026; 0x2020; 0x2021; 0x02C6; 0x2030; 0x0160; 0x2039; 0x0152; 0x008D;
    0x017D; 0x008F; 0x0090; 0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2013; 0x2014; 0x02DC; 0x2122; 0x0161; 0x203A;
    0x0153; 0x009D; 0x017E; 0x0178;
  |]

let windows_1252 (byte : int) : int = if byte >= 0x80 && byte <= 0x9F then windows_1252_high.(byte - 0x80) else byte

let to_utf_8 (t : t) (bytes : string) : string =
  let b = Buffer.create (String.length bytes) in
  (match t with
  | Windows_1252 -> String.iter (fun c -> Buffer.add_utf_8_uchar b (Uchar.of_int (windows_1252 (Char.code c)))) bytes
  | Utf_8 ->
      let rec go (i : int) =
        if i < String.length bytes then (
          let d = String.get_utf_8_uchar bytes i in
          (* an invalid sequence's d is already U+FFFD *)
          Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar d);
          go (i + Uchar.utf_decode_length d))
      in
      go (if has_bom bytes then 3 else 0));
  Buffer.contents b

let decode ?content_type (bytes : string) : string = to_utf_8 (detect ?content_type bytes) bytes
