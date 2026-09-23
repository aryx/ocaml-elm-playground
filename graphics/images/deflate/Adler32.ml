(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Adler32.mli *)

let base = 65521

let update (adler : int) (s : string) ~(pos : int) ~(len : int) : int =
  let a = ref (adler land 0xFFFF) and b = ref (adler lsr 16) in
  for i = pos to pos + len - 1 do
    a := (!a + Char.code s.[i]) mod base;
    b := (!b + !a) mod base
  done;
  (!b lsl 16) lor !a

let string (s : string) : int = update 1 s ~pos:0 ~len:(String.length s)
