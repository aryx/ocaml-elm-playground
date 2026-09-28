(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_hmac.mli *)

open Testutil_crypto

(* RFC 4231: key, data, HMAC-SHA-256, HMAC-SHA-384 *)
let cases =
  [ ( String.make 20 '\x0b', "Hi There", "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7",
      "afd03944d84895626b0825f4ab46907f15f9dadbe4101ec682aa034c7cebc59cfaea9ea9076ede7f4af152e8b2fa9cb6" );
    ( "Jefe", "what do ya want for nothing?", "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843",
      "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649" );
    ( String.make 131 '\xaa', "Test Using Larger Than Block-Size Key - Hash Key First",
      "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54",
      "4ece084485813e9088d2c63a041bc5b44f9ef1012a2b588f3cd11f05033ac4c60c2ef6ab4030fe8296248df163f44952" );
    ( String.make 131 '\xaa',
      "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.",
      "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2",
      "6617178e941f020d351e2f254e8fd32c602420feb0b8fb9adccebb82461e99c5a678cc31e799176d3860e6110c46523e" ) ]

let tests =
  Testo.categorize "Hmac"
    [
      Testo.create "RFC 4231: SHA-256 and SHA-384" (fun () ->
          List.iteri
            (fun i (key, data, e256, e384) ->
              check_hex (Printf.sprintf "case %d, 256" (i + 1)) e256 (Hmac.sha256 key data);
              check_hex (Printf.sprintf "case %d, 384" (i + 1)) e384 (Hmac.sha384 key data))
            cases);
      Testo.create "RFC 5869's test case 1: HKDF-SHA-256" (fun () ->
          let prk = Hkdf.extract ~hmac:Hmac.sha256 ~salt:(unhex "000102030405060708090a0b0c") (String.make 22 '\x0b') in
          check_hex "PRK" "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5" prk;
          check_hex "OKM" "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"
            (Hkdf.expand ~hmac:Hmac.sha256 prk ~info:(unhex "f0f1f2f3f4f5f6f7f8f9") 42));
    ]
