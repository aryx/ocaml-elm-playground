(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_sha1.mli *)

let tests =
  Testo.categorize "Sha1"
    [
      Testo.create "the worked examples: FIPS 180's vectors" (fun () ->
          List.iter
            (fun (input, expected) -> Alcotest.(check string) (String.sub input 0 (min 20 (String.length input))) expected (Sha1.hex input))
            [ ("abc", "a9993e364706816aba3e25717850c26c9cd0d89d"); ("", "da39a3ee5e6b4b0d3255bfef95601890afd80709");
              ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "84983e441c3bd26ebaae4aa1f95129e5e54670f1");
              (String.make 1_000_000 'a', "34aa973cd4c4daa4f61eeb2bdbad27316534016f") ]);
      Testo.create "the padding at a block's edge" (fun () ->
          (* 55 bytes fit one block with their padding; 56 need two *)
          Alcotest.(check string) "55" "c1c8bbdc22796e28c0e15163d20899b65621d65a" (Sha1.hex (String.make 55 'a'));
          Alcotest.(check string) "56" "c2db330f6083854c99d4b5bfb6e8f29f201be699" (Sha1.hex (String.make 56 'a'));
          Alcotest.(check string) "64" "0098ba824b5c16427bd7a1122a5a442a25ec644d" (Sha1.hex (String.make 64 'a')));
    ]
