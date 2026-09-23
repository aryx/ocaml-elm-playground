(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/images/deflate: the worked examples of Crc32.mli,
 * Adler32.mli, Huffman.mli, Inflate.mli and Zlib.mli (and of
 * notes_images.md, sections 3 and 6) *)

let t = Testo.create

let bytes (l : int list) : string = String.init (List.length l) (fun i -> Char.chr (List.nth l i))

let fails msg f =
  match f () with
  | _ -> Alcotest.failf "%s: no failure" msg
  | exception Failure _ -> ()

let test_checksums () =
  Alcotest.(check int) "CRC-32 (123456789)" 0xCBF43926 (Crc32.string "123456789");
  Alcotest.(check int) "CRC-32, in two parts" 0xCBF43926
    (Crc32.update (Crc32.string "1234") "123456789" ~pos:4 ~len:5);
  Alcotest.(check int) "Adler-32 (hi)" 0x013B00D2 (Adler32.string "hi");
  Alcotest.(check int) "Adler-32 (Wikipedia)" 0x11E60398 (Adler32.string "Wikipedia")

(* A 1, B 2, C 3, D 3 *)
let abcd = [| 1; 2; 3; 3 |]

let test_huffman_codes () =
  Alcotest.(check (array (pair int int))) "A 0, B 10, C 110, D 111"
    [| (0, 1); (2, 2); (6, 3); (7, 3) |] (Huffman.codes abcd)

let test_huffman_decode () =
  (* AAAABBCD = 0 0 0 0 10 10 110 111, 14 bits *)
  let bits = ref [ 0; 0; 0; 0; 1; 0; 1; 0; 1; 1; 0; 1; 1; 1 ] in
  let next_bit () =
    match !bits with
    | b :: rest -> bits := rest; b
    | [] -> Alcotest.fail "read past the end"
  in
  let h = Huffman.of_lengths abcd in
  let decoded = List.init 8 (fun _ -> Huffman.decode next_bit h) in
  Alcotest.(check (list int)) "AAAABBCD" [ 0; 0; 0; 0; 1; 1; 2; 3 ] decoded;
  Alcotest.(check (list int)) "every bit used" [] !bits;
  fails "three codes of length 1" (fun () -> Huffman.of_lengths [| 1; 1; 1 |]);
  (* a single code of length 1 is allowed; its missing twin isn't *)
  let one = Huffman.of_lengths [| 0; 1 |] in
  Alcotest.(check int) "the one code" 1 (Huffman.decode (fun () -> 0) one);
  fails "the missing code" (fun () -> Huffman.decode (fun () -> 1) one)

let abc12 = "abcabcabcabc"

let test_inflate () =
  (* ours, a length of 9 from 3 back, and zlib's, four literals and
   * then (8, 3) *)
  List.iter
    (fun (who, stream) ->
      let out, pos = Inflate.inflate stream ~pos:0 in
      Alcotest.(check string) who abc12 out;
      Alcotest.(check int) (who ^ ": all the bytes read") (String.length stream) pos)
    [ ("the worked example", bytes [ 0x4B; 0x4C; 0x4A; 0x86; 0x23; 0x00 ]);
      ("zlib's", bytes [ 0x4B; 0x4C; 0x4A; 0x4E; 0x84; 0x21; 0x00 ]) ];
  fails "a distance further back than the data" (fun () ->
      (* the worked example with the literals left out: BFINAL 1,
       * fixed, then straight to (9, 3) *)
      Inflate.inflate (bytes [ 0x83; 0x23; 0x00 ]) ~pos:0);
  fails "block type 3" (fun () -> Inflate.inflate (bytes [ 0x07 ]) ~pos:0)

let hi = bytes [ 0x78; 0x01; 0x01; 0x02; 0x00; 0xFD; 0xFF; 0x68; 0x69; 0x01; 0x3B; 0x00; 0xD2 ]

let test_zlib () =
  Alcotest.(check string) "hi, stored" "hi" (Zlib.decompress hi);
  let corrupt = Bytes.of_string hi in
  Bytes.set corrupt 8 'o';
  fails "ho, with hi's Adler-32" (fun () -> Zlib.decompress (Bytes.to_string corrupt));
  let bad_nlen = Bytes.of_string hi in
  Bytes.set bad_nlen 5 '\x00';
  fails "NLEN not LEN's complement" (fun () -> Zlib.decompress (Bytes.to_string bad_nlen));
  fails "header not a multiple of 31" (fun () -> Zlib.decompress ("\x78\x02" ^ String.sub hi 2 11))

let test_deflate () =
  (* Deflate.mli's worked example: Inflate.mli's 6 bytes *)
  Alcotest.(check string) "abcabcabcabc" (bytes [ 0x4B; 0x4C; 0x4A; 0x86; 0x23; 0x00 ]) (Deflate.deflate abc12);
  Random.init 1;
  List.iter
    (fun (what, s) ->
      let z = Zlib.compress s in
      Alcotest.(check string) (what ^ ": compressed and back") s (Zlib.decompress z))
    [ ("nothing", "");
      ("one byte", "a");
      ("a run longer than 258", String.make 100_000 'x');
      ("repeats further than 32 KB", String.concat "" (List.init 20 (fun i -> String.make 5000 (Char.chr (65 + (i mod 3))))));
      ("random bytes", String.init 100_000 (fun _ -> Char.chr (Random.int 256)));
      ("text", String.concat " " (List.init 2000 (fun i -> string_of_int (i * i)))) ]

let tests =
  Testo.categorize "Deflate"
    [
      t "Deflate: abcabcabcabc, and round trips" test_deflate;
      t "CRC-32 and Adler-32" test_checksums;
      t "Huffman: canonical codes from lengths" test_huffman_codes;
      t "Huffman: decoding AAAABBCD" test_huffman_decode;
      t "Inflate: abcabcabcabc, and the overlapping copy" test_inflate;
      t "Zlib: hi, and corruptions caught" test_zlib;
    ]
