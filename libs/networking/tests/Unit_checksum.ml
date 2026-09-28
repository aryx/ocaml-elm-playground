(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_checksum.mli *)

let hex = Alcotest.string

let tests =
  Testo.categorize "Checksum"
    [
      Testo.create "the worked example: FNV-1a's test vectors" (fun () ->
          Alcotest.check hex "empty" "811c9dc5" (Checksum.to_hex (Checksum.fnv1a ""));
          Alcotest.check hex "a" "e40c292c" (Checksum.to_hex (Checksum.fnv1a "a"));
          Alcotest.check hex "foobar" "bf9cf968" (Checksum.to_hex (Checksum.fnv1a "foobar")));
      Testo.create "equal models, equal checksums, however built" (fun () ->
          let p = (1.5, 2.5) in
          let shared = [ p; p ] and copied = [ (1.5, 2.5); (1.5, 2.5) ] in
          Alcotest.check hex "sharing ignored" (Checksum.to_hex (Checksum.of_model shared)) (Checksum.to_hex (Checksum.of_model copied)));
      Testo.create "a model a bit different" (fun () ->
          let a = Checksum.of_model [ 1.0; 2.0 ] and b = Checksum.of_model [ 1.0; Float.succ 2.0 ] in
          Alcotest.(check bool) "the last bit of a float" true (a <> b));
      Testo.create "a function is not a model" (fun () ->
          Alcotest.check_raises "refused" (Invalid_argument "output_value: functional value") (fun () ->
              ignore (Checksum.of_model (fun x -> x + 1))));
    ]
