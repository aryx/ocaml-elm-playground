(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Testutil_crypto.mli *)

let unhex (s : string) : string =
  let s = String.concat "" (String.split_on_char ' ' (String.concat "" (String.split_on_char '\n' s))) in
  String.init (String.length s / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

let hex (s : string) : string = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))
let check_hex (name : string) (expected : string) (actual : string) : unit = Alcotest.(check string) name (hex (unhex expected)) (hex actual)
