(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_ciphers.mli *)

open Testutil_crypto

let sunscreen = "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."

(* key, nonce, aad, plaintext, sealed (ciphertext and tag): Python's
   cryptography, the last GCM one the GCM paper's test case 3 *)
let chacha_cases = [
  ("a54dca182530bb1d6d132cded6237b2ed91e3f721fcb1971174494d6493c9d5c", "3460be31201e69fedaa0eee8", "", "", "6880755fc21cdbed87004668cb0d9584");
  ("b9997f5c7c2999fdafe593253cd654af4dfad71427a0aeb3fee9232f8af2211f", "9ee491c5b10becb5563bfc1e", "93427ecbc8", "6f", "a40a696c2d1e39ca87891571c9e197c8d7");
  ("fe2955e5cd8e46dc8ed4b7c2764d2a5a4d767706f85d8690024ad6bda3401be9", "c8cbccc935f6cd1f61226ae1", "baf23e3bf9eef5f79f2b4934af87f552", "5338ae1a34004d33ba0d246ac04c81b1", "c0ebc5d558e49620d189d304dd674e1042fde112ee5d3ee77cdc2b455de9eb1d");
  ("0b69b94b0d982e85bb55b672a872637acd7466fcb60e0e8ff18463b0e4b2ba29", "703474f064ac68f700f5b02b", "32", "3dc666f45bdeaa2ccaedcd2b5157410e4dee4af2b34f430a073447de636c0e806c957ba684d6431fb5ead7424d09e15d024c5848f23d1fa6f7361d7f618d15", "f0e8001ffaa7dd8f31b7e236c05e9c7998e9957c26a8eac1a9b81b961d8ba42fb4c017c7da08a456dde814a6ac7f681dda41561115b32d7ff3a2e262b9c6f04fcc08386c887a6d1d974a57135f0ae0");
  ("e70e20e2a6668de7f47e8467e546d53ec8e2a1257bdb256c9b3e4fbb498146ef", "7030cbf9537252dcceadd764", "", "b6a32fbb09adeae109c4a997203975352b878b145c8a42d884cf4cfda72d8e1d5dd92589082d852a7122873ee805add58942167a385286195c679f9c6994e45b", "daf909c864161ce3c838997b4ab27d5261e4b5dc9a84ed5d84702be9eddcb9e301acb80ed15c19fa49a0aa269c5f9531e31f09e9bd1154ace4335a97acf7063f24e36f76456f0d4385bac4a9f6f8b636");
  ("8ab1098012070961f37de436ddfdc99d6e75af6547cfb11b42072482dc531c2b", "c3907c9617eb5e5089e40186", "d5b0c0a13da900a6adcb3d64069481be21c9c727b8db8c188f341a924c7f88dfa1", "baa8a57d119e6fb65d00abc32af38e667f022e872d49cc15c90b999b772b4fc7a6fd4c914a16db4708752b0f1544b835c0e719097dfa8701e9232f21f2812687786976ebfcc327f5931765274ba9829b4406f61ff889326ffa9492edeeee3c669f2bf20894ea27e689c66b6b262e4886b8438f39ba76fef8c90c5101fbe6cf9a48", "d4c9c30a86f3db7aa5d5cf5bb1ae45eff2346951912295bf818e1c95d5a87228317af3ebce360d633c1c8747eeacd06139423754db6efefd3f225aa90361258df4833d0558f80608a4a303c400b18707b45d7bf5c9295cd64ef6fa4153b5f754d9933a2ace415580903c60b7727b57e5c69ecb73bfefbc421293e6f9e6742c59717c97409d3f633ba5ec2e0e14bb85a60e");
]

let gcm_cases = [
  ("61bfdb0ecc682919d2e64692f8194157", "f1d4af90988285cf7a9af7c9", "", "", "6a1cb56cf4d4352b56c2ea2e4115c0cf");
  ("3d5552266afe70e7aae6da47627c2e59", "af2ea37abc84670ad3c4d36b", "8aad1fff8e", "c0", "026ba4b969b2c070864791ad3ab267b1ff");
  ("b8406e2f8a7fc4cce4dd9f0b4110d9f2", "fa0025c8efe57f37724f4d37", "c6857200059aeb8ea17cf3787e0ed29d", "ea2b14004077139b4180df3932249962", "674771fde9c5677aca471628c5984759501d0ce63a3f163333da174fa5b85d30");
  ("1c0b63ffd7298374d9bd74fc11add7b9", "ca6503952269fd669f6376ee", "0fd27ecf14c011ed201f836320adb98bab1686a2", "71879737fd5f72f8d51c4ac91b6d0c48d41a1e5ec9e6a0392854a8615eef109fc1bfa9e2563701288f29b3d73f6ac2b69edd2c19f264bee462a5baf2", "f16021052333183725d63a8603b6eee2bbe23eae0d4732d750ec249f11fda75cc1d1c8d71f8b9f38a3b8ca8d83a215e1a61116c45827a0f6bd836eedcfea67c9165af01ce5d75db9d80e9d84");
  ("8d9801210c7736f3eec580dcfc43fe5d", "049b4d78a7a3ebb92865c851", "", "7ed02111f6a652da3524872b6a31d7ffe4587744d5eb783e96968f89be828565e07e5f7d784e9060a721ca807d7633ed123402f376e5bf1496773d19616326be", "e30ec34a689b1b62ac4345151cd04d4beff86c64399985cbf99d0b41134eae627d3176822a5b6aba86795602913b19f9226361e7c11b806c635e688a131083b6ffc7a6bff588fc4f2591c75b23bd47be");
  ("5be5850336b36f13bcae481668821368", "05a7d1be5e9f276810fdf720", "8151a58ce94982f56a8679a3be12655dce528ea7c056873a18b8e73581c9be87c0", "d033ca4f2e53cb8ad1919dd51a9fb6d4d509ba64c8cf6803de50d83a2ecfbaeb5342071a48cb2dbd574ab29152572237c4fb659a4016f7a11bc62c5271cf64f25d6f15cc50c4b73f4c7e621513a53cc7e99cd79d7fd9c7bce4e05b0b01faee78e4ea5bf2cc362241b7dcbb2ee21414422aa0281bc1450d21386343fb93547121b3", "ef25198f2ba866122bab116de104dbb7a0d95431a9a788b787ce246928132b2a3885861796532b89504bd0e5577073352dd1aec1b672cfc787336d1d980b3a5cb69f9098aed7ee3721b2164ebe6d42ceb6f3407a7c3ba755e195d0cc2f7d81745fad0b7dffbae045afc87728afcc6b68ac1f8f91d155d987ad3fb66a61171f705d96817b98370a5f1be6440787fc6e96fa");
  ("feffe9928665731c6d6a8f9467308308", "cafebabefacedbaddecaf888", "", "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255", "42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091473f59854d5c2af327cd64a62cf35abd2ba6fab4");
]

let tests =
  Testo.categorize "Ciphers"
    [
      Testo.create "ChaCha20: RFC 8439's block (2.3.2) and encryption (2.4.2)" (fun () ->
          let key = String.init 32 Char.chr in
          check_hex "block" "10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e" (Chacha20.block ~key ~nonce:(unhex "000000090000004a00000000") 1);
          check_hex "sunscreen" "6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d" (Chacha20.encrypt ~key ~nonce:(unhex "000000000000004a00000000") ~counter:1 sunscreen));
      Testo.create "Poly1305: RFC 8439 2.5.2" (fun () ->
          check_hex "tag" "a8061dc1305136c6c22b8baf0c0127a9"
            (Poly1305.mac ~key:(unhex "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b") "Cryptographic Forum Research Group"));
      Testo.create "ChaCha20-Poly1305: RFC 8439 2.8.2, and other lengths" (fun () ->
          let key = unhex "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f" and nonce = unhex "070000004041424344454647" in
          let aad = unhex "50515253c0c1c2c3c4c5c6c7" in
          check_hex "sealed" "d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b61161ae10b594f09e26a7e902ecbd0600691" (Chacha20_poly1305.seal ~key ~nonce ~aad sunscreen);
          List.iteri
            (fun i (k, n, a, p, c) ->
              let key = unhex k and nonce = unhex n and aad = unhex a and p = unhex p in
              check_hex (Printf.sprintf "case %d" i) c (Chacha20_poly1305.seal ~key ~nonce ~aad p);
              Alcotest.(check (option string)) "opened" (Some (hex p)) (Option.map hex (Chacha20_poly1305.open_ ~key ~nonce ~aad (unhex c))))
            chacha_cases;
          (* a byte changed: nothing *)
          let sealed = Chacha20_poly1305.seal ~key ~nonce ~aad sunscreen in
          let bad = String.mapi (fun i ch -> if i = 3 then Char.chr (Char.code ch lxor 1) else ch) sealed in
          Alcotest.(check bool) "refused" true (Chacha20_poly1305.open_ ~key ~nonce ~aad bad = None));
      Testo.create "AES: the S-box computed, FIPS 197's C.1 and C.3" (fun () ->
          Alcotest.(check (list int)) "63 7c 77 7b f2 6b 6f c5" [ 0x63; 0x7c; 0x77; 0x7b; 0xf2; 0x6b; 0x6f; 0xc5 ] (Array.to_list (Array.sub Aes.sbox 0 8));
          Alcotest.(check int) "S(53) = ed" 0xed Aes.sbox.(0x53);
          let block = unhex "00112233445566778899aabbccddeeff" in
          check_hex "128" "69c4e0d86a7b0430d8cdb78070b4c55a" (Aes.encrypt_block (Aes.expand (String.init 16 Char.chr)) block);
          check_hex "256" "8ea2b7ca516745bfeafc49904b496089" (Aes.encrypt_block (Aes.expand (String.init 32 Char.chr)) block));
      Testo.create "AES-GCM: the paper's test case 3, and other lengths" (fun () ->
          List.iteri
            (fun i (k, n, a, p, c) ->
              let key = unhex k and nonce = unhex n and aad = unhex a and p = unhex p in
              check_hex (Printf.sprintf "case %d" i) c (Gcm.seal ~key ~nonce ~aad p);
              Alcotest.(check (option string)) "opened" (Some (hex p)) (Option.map hex (Gcm.open_ ~key ~nonce ~aad (unhex c))))
            gcm_cases);
    ]
