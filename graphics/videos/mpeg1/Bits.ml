(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Bits.mli *)

type t = { s : string; mutable pos : int (* bits *) }

let of_string (s : string) : t = { s; pos = 0 }

let bit (b : t) (i : int) : int =
  let byte = i lsr 3 in
  if byte >= String.length b.s then 0 else (Char.code b.s.[byte] lsr (7 - (i land 7))) land 1

let peek (b : t) (n : int) : int =
  let v = ref 0 in
  for i = 0 to n - 1 do v := (!v lsl 1) lor bit b (b.pos + i) done;
  !v

let skip (b : t) (n : int) : unit = b.pos <- b.pos + n

let read (b : t) (n : int) : int =
  let v = peek b n in
  skip b n;
  v

let position (b : t) : int = b.pos
let seek (b : t) (p : int) : unit = b.pos <- p
let at_end (b : t) : bool = b.pos >= 8 * String.length b.s

let next_start_code (b : t) : int option =
  let byte = ref ((b.pos + 7) lsr 3) and found = ref None in
  let s = b.s in
  while !found = None && !byte + 3 < String.length s do
    if s.[!byte] = '\000' && s.[!byte + 1] = '\000' && s.[!byte + 2] = '\001' then found := Some (Char.code s.[!byte + 3])
    else incr byte
  done;
  match !found with
  | Some code ->
      b.pos <- 8 * (!byte + 4);
      Some code
  | None ->
      b.pos <- 8 * String.length s;
      None
