(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Brotli_dictionary.mli *)

(*****************************************************************************)
(* The words *)
(*****************************************************************************)

let size = 122_784

(* how many words of each length, 0 to 24, as a number of bits: 2^10
 * words of 4 bytes, 2^5 of 24 *)
let index_bits = [| 0; 0; 0; 0; 10; 10; 11; 11; 10; 10; 10; 10; 10; 9; 9; 8; 7; 7; 8; 7; 7; 6; 6; 5; 5 |]

(* where the words of each length start: after all the shorter ones *)
let offsets : int array =
  let offsets = Array.make 26 0 in
  for length = 4 to 24 do
    offsets.(length + 1) <- offsets.(length) + (length lsl index_bits.(length))
  done;
  offsets

(*****************************************************************************)
(* The transforms *)
(*****************************************************************************)

type change =
  | Identity
  | Ferment_first
  | Ferment_all
  | Omit_first of int
  | Omit_last of int

let transforms : (string * change * string) array =
  let i = Identity and f = Ferment_first and a = Ferment_all in
  let first n = Omit_first n and last n = Omit_last n in
  [|
    ("", i, ""); ("", i, " "); (" ", i, " "); ("", first 1, ""); ("", f, " ");
    ("", i, " the "); (" ", i, ""); ("s ", i, " "); ("", i, " of "); ("", f, "");
    ("", i, " and "); ("", first 2, ""); ("", last 1, ""); (", ", i, " "); ("", i, ", ");
    (" ", f, " "); ("", i, " in "); ("", i, " to "); ("e ", i, " "); ("", i, "\"");
    ("", i, "."); ("", i, "\">"); ("", i, "\n"); ("", last 3, ""); ("", i, "]");
    ("", i, " for "); ("", first 3, ""); ("", last 2, ""); ("", i, " a "); ("", i, " that ");
    (" ", f, ""); ("", i, ". "); (".", i, ""); (" ", i, ", "); ("", first 4, "");
    ("", i, " with "); ("", i, "'"); ("", i, " from "); ("", i, " by "); ("", first 5, "");
    ("", first 6, ""); (" the ", i, ""); ("", last 4, ""); ("", i, ". The "); ("", a, "");
    ("", i, " on "); ("", i, " as "); ("", i, " is "); ("", last 7, ""); ("", last 1, "ing ");
    ("", i, "\n\t"); ("", i, ":"); (" ", i, ". "); ("", i, "ed "); ("", first 9, "");
    ("", first 7, ""); ("", last 6, ""); ("", i, "("); ("", f, ", "); ("", last 8, "");
    ("", i, " at "); ("", i, "ly "); (" the ", i, " of "); ("", last 5, ""); ("", last 9, "");
    (" ", f, ", "); ("", f, "\""); (".", i, "("); ("", a, " "); ("", f, "\">");
    ("", i, "=\""); (" ", i, "."); (".com/", i, ""); (" the ", i, " of the "); ("", f, "'");
    ("", i, ". This "); ("", i, ","); (".", i, " "); ("", f, "("); ("", f, ".");
    ("", i, " not "); (" ", i, "=\""); ("", i, "er "); (" ", a, " "); ("", i, "al ");
    (" ", a, ""); ("", i, "='"); ("", a, "\""); ("", f, ". "); (" ", i, "(");
    ("", i, "ful "); (" ", f, ". "); ("", i, "ive "); ("", i, "less "); ("", a, "'");
    ("", i, "est "); (" ", f, "."); ("", a, "\">"); (" ", i, "='"); ("", f, ",");
    ("", i, "ize "); ("", a, "."); ("\xC2\xA0", i, ""); (" ", i, ","); ("", f, "=\"");
    ("", a, "=\""); ("", i, "ous "); ("", a, ", "); ("", f, "='"); (" ", f, ",");
    (" ", a, "=\""); (" ", a, ", "); ("", a, ","); ("", a, "("); ("", a, ". ");
    (" ", a, "."); ("", a, "='"); (" ", a, ". "); (" ", f, "=\""); (" ", a, "='");
    (" ", f, "='");
  |]

(* the character at [pos] to its capital, the UTF-8 way: a byte under
 * 192 is a character by itself, capitalized if a to z; a byte under
 * 224 starts a character of 2 bytes (Latin, Greek, Cyrillic), over, of
 * 3; their capitals are guessed by flipping a bit or two of the last
 * byte. How many bytes the character has. *)
let ferment (word : Bytes.t) (pos : int) : int =
  let flip i mask = if i < Bytes.length word then Bytes.set word i (Char.chr (Char.code (Bytes.get word i) lxor mask)) in
  let c = Char.code (Bytes.get word pos) in
  if c < 192 then begin
    if c >= Char.code 'a' && c <= Char.code 'z' then flip pos 32;
    1
  end
  else if c < 224 then begin
    flip (pos + 1) 32;
    2
  end
  else begin
    flip (pos + 2) 5;
    3
  end

let transform (id : int) (word : string) : string =
  if id < 0 || id >= Array.length transforms then failwith "Brotli: a transform that doesn't exist";
  let prefix, change, suffix = transforms.(id) in
  let n = String.length word in
  let changed =
    match change with
    | Identity -> word
    | Omit_first k -> if k > n then "" else String.sub word k (n - k)
    | Omit_last k -> if k > n then "" else String.sub word 0 (n - k)
    | Ferment_first | Ferment_all ->
        let b = Bytes.of_string word in
        let pos = ref 0 in
        while !pos < n && (!pos = 0 || change = Ferment_all) do
          pos := !pos + ferment b !pos
        done;
        Bytes.to_string b
  in
  prefix ^ changed ^ suffix

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let word (dictionary : string) ~(length : int) ~(id : int) : string =
  if String.length dictionary <> size then failwith "Brotli: not the dictionary (it has 122,784 bytes)";
  if length < 4 || length > 24 then failwith "Brotli: a dictionary word shorter than 4 bytes or longer than 24";
  let index = id land ((1 lsl index_bits.(length)) - 1) in
  transform (id lsr index_bits.(length)) (String.sub dictionary (offsets.(length) + (index * length)) length)
