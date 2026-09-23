(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Checksum.mli *)

let offset_basis = 0x811c9dc5l
let prime = 16777619l

let fnv1a (s : string) : int32 =
  String.fold_left (fun hash c -> Int32.mul (Int32.logxor hash (Int32.of_int (Char.code c))) prime) offset_basis s

let of_model (model : 'a) : int32 = fnv1a (Marshal.to_string model [ Marshal.No_sharing ])

let to_hex (h : int32) : string = Printf.sprintf "%08lx" h
