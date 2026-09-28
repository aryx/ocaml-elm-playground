(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Hmac.mli *)

let hmac ~(hash : string -> string) ~(block : int) (key : string) (message : string) : string =
  let key = if String.length key > block then hash key else key in
  let key = key ^ String.make (block - String.length key) '\000' in
  let xored c = String.map (fun k -> Char.chr (Char.code k lxor c)) key in
  hash (xored 0x5c ^ hash (xored 0x36 ^ message))

let sha256 = hmac ~hash:Sha256.digest ~block:64
let sha384 = hmac ~hash:Sha512.digest384 ~block:128
