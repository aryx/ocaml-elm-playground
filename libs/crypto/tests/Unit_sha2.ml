(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_sha2.mli *)

let sha256 =
  [ ("abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    ("", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    (String.make 55 'a', "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318");
    (String.make 56 'a', "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a");
    (String.make 64 'a', "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb");
    (String.make 1_000_000 'a', "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0") ]

let sha512 =
  [ ( "abc",
      "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f",
      "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7" );
    ( "",
      "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e",
      "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b" );
    ( String.make 111 'a',
      "fa9121c7b32b9e01733d034cfc78cbf67f926c7ed83e82200ef86818196921760b4beff48404df811b953828274461673c68d04e297b0eb7b2b4d60fc6b566a2",
      "3c37955051cb5c3026f94d551d5b5e2ac38d572ae4e07172085fed81f8466b8f90dc23a8ffcdea0b8d8e58e8fdacc80a" );
    ( String.make 112 'a',
      "c01d080efd492776a1c43bd23dd99d0a2e626d481e16782e75d54c2503b5dc32bd05f0f1ba33e568b88fd2d970929b719ecbb152f58f130a407c8830604b70ca",
      "187d4e07cb306103c69967bf544d0dfbe9042577599c73c330abc0cb64c61236d5ed565ee19119d8c31779a38f791fcd" );
    ( String.make 128 'a',
      "b73d1929aa615934e61a871596b3f3b33359f42b8175602e89f7e06e5f658a243667807ed300314b95cacdd579f3e33abdfbe351909519a846d465c59582f321",
      "edb12730a366098b3b2beac75a3bef1b0969b15c48e2163c23d96994f8d1bef760c7e27f3c464d3829f56c0d53808b0b" );
    ( String.make 1_000_000 'a',
      "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b",
      "9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985" ) ]

let name s = Printf.sprintf "%d bytes" (String.length s)

let tests =
  Testo.categorize "Sha2"
    [
      Testo.create "SHA-256: FIPS 180's, and the padding's edges" (fun () ->
          List.iter (fun (input, expected) -> Testutil_crypto.check_hex (name input) expected (Sha256.digest input)) sha256);
      Testo.create "SHA-512 and SHA-384" (fun () ->
          List.iter
            (fun (input, e512, e384) ->
              Testutil_crypto.check_hex ("512, " ^ name input) e512 (Sha512.digest input);
              Testutil_crypto.check_hex ("384, " ^ name input) e384 (Sha512.digest384 input))
            sha512);
    ]
