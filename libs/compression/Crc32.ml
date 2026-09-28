(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Crc32.mli *)

(* claude: Int32, not int: the register is 32 bits, and an int is 63
 * bits natively but 32 in JavaScript (js_of_ocaml), where 0xEDB88320
 * and 0xFFFFFFFF don't fit; Int32 is the same on both *)

(* the remainder of each byte, dividing bit by bit: shift right, and
 * xor in the polynomial when a 1 falls out *)
let table : int32 array Lazy.t =
  lazy
    (Array.init 256 (fun n ->
         let c = ref (Int32.of_int n) in
         for _ = 1 to 8 do
           let shifted = Int32.shift_right_logical !c 1 in
           c := if Int32.logand !c 1l = 1l then Int32.logxor 0xEDB88320l shifted else shifted
         done;
         !c))

let update (crc : int32) (s : string) ~(pos : int) ~(len : int) : int32 =
  let table = Lazy.force table in
  let c = ref (Int32.lognot crc) in
  for i = pos to pos + len - 1 do
    let index = (Int32.to_int !c lxor Char.code (String.unsafe_get s i)) land 0xFF in
    c := Int32.logxor table.(index) (Int32.shift_right_logical !c 8)
  done;
  Int32.lognot !c

let string (s : string) : int32 = update 0l s ~pos:0 ~len:(String.length s)
