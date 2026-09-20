(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/images: Base64 *)

let t = Testo.create

(* the worked examples of Base64.mli and RFC 4648: 3 bytes make 4
 * characters, and a group that is short is padded *)
let test_known () =
  List.iter
    (fun (bytes, base64) ->
      Alcotest.(check string) (Printf.sprintf "%S encodes" bytes) base64 (Base64.encode bytes);
      Alcotest.(check string) (Printf.sprintf "%S decodes back" base64) bytes (Base64.decode base64))
    [ ("", ""); ("Man", "TWFu"); ("Ma", "TWE="); ("M", "TQ=="); ("light work.", "bGlnaHQgd29yay4=") ]

(* any bytes, including the ones no text format would survive *)
let test_bytes () =
  let all = String.init 256 Char.chr in
  Alcotest.(check string) "every byte, there and back" all (Base64.decode (Base64.encode all));
  Alcotest.(check int) "4 characters for every 3 bytes" 344 (String.length (Base64.encode all));
  (* the lines a generated file is cut into don't change what it means *)
  let cut = String.concat "\n" [ String.sub (Base64.encode all) 0 100; String.sub (Base64.encode all) 100 244 ] in
  Alcotest.(check string) "newlines ignored" all (Base64.decode cut)

let tests = Testo.categorize "Base64" [ t "the known examples" test_known; t "any bytes, and newlines" test_bytes ]
