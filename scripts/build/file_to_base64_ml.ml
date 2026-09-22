(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A build-time script (run by `ocaml`, from a dune rule: see
 * games/fps/dune), turning any file into an OCaml module holding it as
 * base64, so that a program carries its data around instead of looking
 * for a file at run time -- graphics/font/dune does the same for the
 * Hershey font, with `cat`, which only works for text.
 *
 *   ocaml scripts/build/file_to_base64_ml.ml games/fps/minecraft.png > Foo.ml
 *
 * Its own base64 encoder, not graphics/images/Base64.ml's: this runs
 * before anything is built. *)

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let encode (s : string) : string =
  let out = Buffer.create ((String.length s + 2) / 3 * 4) in
  let n = String.length s in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let rec go i =
    if i < n then begin
      let group = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      let char k = alphabet.[(group lsr (18 - (6 * k))) land 63] in
      Buffer.add_char out (char 0);
      Buffer.add_char out (char 1);
      Buffer.add_char out (if i + 1 < n then char 2 else '=');
      Buffer.add_char out (if i + 2 < n then char 3 else '=');
      go (i + 3)
    end
  in
  go 0;
  Buffer.contents out

let read (path : string) : string =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let () =
  let path = Sys.argv.(1) in
  let base64 = encode (read path) in
  Printf.printf "(* generated from %s by scripts/build/file_to_base64_ml.ml *)\n" path;
  (* cut into lines: a single 7000-character line is no fun in an editor *)
  Printf.printf "let base64 =\n  {|";
  String.iteri (fun i c -> if i > 0 && i mod 76 = 0 then print_char '\n'; print_char c) base64;
  Printf.printf "|}\n"
