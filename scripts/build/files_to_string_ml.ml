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
 * examples/dune), turning files into an OCaml module holding each as a
 * string, so that a program carries its data around:
 *
 *   ocaml scripts/build/files_to_string_ml.ml demo_picture.png demo_picture.gif > Foo.ml
 *
 * gives Foo.demo_picture_png and Foo.demo_picture_gif. The bytes are
 * written as an escaped string literal ("\137PNG..."), which, unlike
 * file_to_base64_ml.ml's base64, needs no decoder at run time: the
 * string is the bytes. *)

let read (path : string) : string =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* "demo_picture.png" -> "demo_picture_png" *)
let name (path : string) : string =
  String.map (fun c -> match c with 'a' .. 'z' | '0' .. '9' | '_' -> c | 'A' .. 'Z' -> Char.lowercase_ascii c | _ -> '_')
    (Filename.basename path)

let () =
  let paths = List.tl (Array.to_list Sys.argv) in
  Printf.printf "(* generated from %s by scripts/build/files_to_string_ml.ml *)\n"
    (String.concat ", " (List.map Filename.basename paths));
  paths
  |> List.iter (fun path ->
         let escaped = String.escaped (read path) in
         (* cut into lines, with OCaml's backslash-newline, which skips
          * the newline and the next line's leading blanks *)
         Printf.printf "let %s =\n  \"" (name path);
         let col = ref 0 in
         (* claude: the characters left of the escape being printed: an
          * escaped backslash is two backslashes, so "just before a
          * backslash" can be the middle of one *)
         let inside = ref 0 in
         String.iteri
           (fun i c ->
             (* never inside an escape: only just before one *)
             if !inside = 0 && !col >= 76 && c = '\\' then begin
               print_string "\\\n   ";
               col := 0
             end;
             if !inside > 0 then decr inside
             else if c = '\\' then
               inside := (match escaped.[i + 1] with '0' .. '9' -> 3 | _ -> 1);
             print_char c;
             incr col)
           escaped;
         print_string "\"\n")
