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

(* the remainder of each byte, dividing bit by bit: shift right, and
 * xor in the polynomial when a 1 falls out *)
let table : int array Lazy.t =
  lazy
    (Array.init 256 (fun n ->
         let c = ref n in
         for _ = 1 to 8 do
           c := if !c land 1 = 1 then 0xEDB88320 lxor (!c lsr 1) else !c lsr 1
         done;
         !c))

let update (crc : int) (s : string) ~(pos : int) ~(len : int) : int =
  let table = Lazy.force table in
  let c = ref (crc lxor 0xFFFFFFFF) in
  for i = pos to pos + len - 1 do
    c := table.((!c lxor Char.code s.[i]) land 0xFF) lxor (!c lsr 8)
  done;
  !c lxor 0xFFFFFFFF

let string (s : string) : int = update 0 s ~pos:0 ~len:(String.length s)
