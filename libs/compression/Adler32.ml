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

(* claude: a and b are small ints, below 65521; only b * 65536 + a
 * needs 32 bits, hence an Int32 (see Crc32.mli) *)
let update (adler : int32) (s : string) ~(pos : int) ~(len : int) : int32 =
  let a = ref (Int32.to_int (Int32.logand adler 0xFFFFl)) in
  let b = ref (Int32.to_int (Int32.shift_right_logical adler 16)) in
  for i = pos to pos + len - 1 do
    a := (!a + Char.code (String.unsafe_get s i)) mod base;
    b := (!b + !a) mod base
  done;
  Int32.logor (Int32.shift_left (Int32.of_int !b) 16) (Int32.of_int !a)

let string (s : string) : int32 = update 1l s ~pos:0 ~len:(String.length s)
