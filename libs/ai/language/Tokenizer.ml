(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Tokenizer.mli *)

type t = {
  chars : string; (* token i + 1 is chars.[i] *)
  tokens : int array; (* a character's code to its token, -1 for none *)
}

let boundary = 0

let of_text (text : string) : t =
  let used = Array.make 256 false in
  String.iter (fun c -> if c <> '\n' then used.(Char.code c) <- true) text;
  let b = Buffer.create 64 in
  Array.iteri (fun code u -> if u then Buffer.add_char b (Char.chr code)) used;
  let chars = Buffer.contents b in
  let tokens = Array.make 256 (-1) in
  String.iteri (fun i c -> tokens.(Char.code c) <- i + 1) chars;
  { chars; tokens }

let size (t : t) : int = String.length t.chars + 1

let encode (t : t) (word : string) : int list =
  List.init (String.length word) (fun i ->
      match t.tokens.(Char.code word.[i]) with -1 -> raise Not_found | token -> token)

let bounded (t : t) (word : string) : int list = (boundary :: encode t word) @ [ boundary ]
let char (t : t) (token : int) : char = if token = boundary then '.' else t.chars.[token - 1]
let decode (t : t) (tokens : int list) : string = String.of_seq (Seq.map (char t) (List.to_seq tokens))
let words (text : string) : string list = List.filter (fun w -> w <> "") (String.split_on_char '\n' text)
