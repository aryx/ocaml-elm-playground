(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The worked examples of Fse.mli, Xxhash.mli and Zstd.mli, and frames
 * made by zstd itself (zstd/make_zstd.sh) *)

let t = Testo.create

let fails msg f =
  match f () with
  | _ -> Alcotest.failf "%s: no failure" msg
  | exception Failure _ -> ()

(*****************************************************************************)
(* FSE *)
(*****************************************************************************)

(* 16 states: A 8, B 4, C 3, D "less than one" *)
let abcd () = Fse.of_distribution ~accuracy:4 [| 8; 4; 3; -1 |]

let test_fse_table () =
  let table = abcd () in
  let r = Fse.backward "\xFF\xFF\xFF\x01" ~pos:0 ~len:4 in
  let row state = (Fse.symbol table state, Fse.next table r state) in
  (* every bit read is a 1: the next state is the base plus all ones,
   * the top of the states it may go to *)
  Alcotest.(check (list (pair int int))) "the symbols, and base + all ones"
    [ (0, 1); (0, 3); (1, 3); (2, 15); (0, 5); (1, 7); (2, 3); (0, 7);
      (1, 11); (2, 7); (0, 9); (0, 11); (1, 15); (0, 13); (0, 15) ]
    (List.init 15 row);
  fails "counts that make 15 states of 16" (fun () -> Fse.of_distribution ~accuracy:4 [| 8; 4; 3 |])

let test_fse_decode () =
  let table = abcd () in
  let r = Fse.backward "\xC9\x27" ~pos:0 ~len:2 in
  let s1 = Fse.start table r in
  let s2 = Fse.next table r s1 in
  let s3 = Fse.next table r s2 in
  let s4 = Fse.next table r s3 in
  Alcotest.(check (list int)) "the states" [ 3; 15; 2; 1 ] [ s1; s2; s3; s4 ];
  Alcotest.(check (list int)) "C D B A" [ 2; 3; 1; 0 ] (List.map (Fse.symbol table) [ s1; s2; s3; s4 ]);
  Alcotest.(check int) "every bit read" 0 (Fse.left r);
  Alcotest.(check int) "under the first bit: zeros" 0 (Fse.bits r 3);
  Alcotest.(check int) "and a negative count" (-3) (Fse.left r);
  fails "a last byte without a mark" (fun () -> Fse.backward "\xC9\x00" ~pos:0 ~len:2)

let test_bits () =
  (* C9 27 is the number 27C9 *)
  Alcotest.(check int) "bits 4 to 11" 0x7C (Fse.field "\xC9\x27" ~bit:4 8);
  Alcotest.(check int) "zeros past the end" 0x2 (Fse.field "\xC9\x27" ~bit:12 8);
  Alcotest.(check (list int)) "log2" [ 0; 1; 1; 2; 10 ] (List.map Fse.log2 [ 1; 2; 3; 4; 1024 ])

(*****************************************************************************)
(* XXH64 *)
(*****************************************************************************)

let test_xxh64 () =
  let check what expected s = Alcotest.(check int64) what expected (Xxhash.xxh64 s) in
  check "nothing" 0xEF46DB3751D8E999L "";
  check "abc" 0x44BC2CF5AD770999L "abc";
  (* its low half is the "hi" frame's checksum *)
  check "hi" 0xEA8842E9EA2638FAL "hi"

(*****************************************************************************)
(* Zstd *)
(*****************************************************************************)

let hi = "\x28\xB5\x2F\xFD\x04\x58\x11\x00\x00hi\xFA\x38\x26\xEA"
let hello_text = "hello hello hello hello\n"
let hello = "\x28\xB5\x2F\xFD\x00\x58\x6D\x00\x00\x38hello \n\x01\x00\x99\x4B\x11"

(* `head -c 300000 /dev/zero | zstd`: a compressed block (two literals,
 * the rest of the 128 KB a match from 1 back), then two RLE blocks *)
let zeros =
  "\x28\xB5\x2F\xFD\x04\x58\x54\x00\x00\x10\x00\x00\x01\x00\xFB\xFF\x39\xC0\x02\x02\x00\x10\x00\x03\x9F\x04\x00\x2D\x28\xDE\x26"

let set (s : string) (i : int) (c : char) : string =
  let b = Bytes.of_string s in
  Bytes.set b i c;
  Bytes.to_string b

let test_worked_examples () =
  Alcotest.(check string) "hi, a raw block" "hi" (Zstd.decompress hi);
  Alcotest.(check string) "hello: one sequence, the predefined tables" hello_text (Zstd.decompress hello);
  Alcotest.(check string) "300,000 zeros: a distance of 1, RLE blocks" (String.make 300_000 '\000') (Zstd.decompress zeros)

let test_frames () =
  Alcotest.(check string) "two frames, cat a.zst b.zst" ("hi" ^ hello_text) (Zstd.decompress (hi ^ hello));
  (* 184D2A53, 3 bytes of someone's data *)
  let skippable = "\x53\x2A\x4D\x18\x03\x00\x00\x00abc" in
  Alcotest.(check string) "a skippable frame between them" ("hi" ^ hello_text) (Zstd.decompress (hi ^ skippable ^ hello));
  (* single segment (no window byte), the content's size in a byte *)
  let sized n = "\x28\xB5\x2F\xFD\x20" ^ String.make 1 (Char.chr n) ^ String.sub hello 6 16 in
  Alcotest.(check string) "the size the frame says" hello_text (Zstd.decompress (sized 24));
  fails "another size than the frame says" (fun () -> Zstd.decompress (sized 25))

let test_corruptions () =
  fails "nothing" (fun () -> Zstd.decompress "");
  fails "a gzip stream is not a zstd one" (fun () -> Zstd.decompress "\x1F\x8B\x08\x00");
  fails "ho, with hi's checksum" (fun () -> Zstd.decompress (set hi 10 'o'));
  fails "a reserved bit" (fun () -> Zstd.decompress (set hi 4 '\x0C'));
  fails "a dictionary" (fun () -> Zstd.decompress ("\x28\xB5\x2F\xFD\x01\x58\x07" ^ String.sub hello 6 16));
  fails "block type 3" (fun () -> Zstd.decompress (set hi 6 '\x17'));
  (* the offset's extra bits 111: 15, a distance of 12 after 6
   * literals *)
  fails "a distance further back than the data" (fun () -> Zstd.decompress (set hello 19 '\x9F'));
  (* the mark one bit lower: a bit short *)
  fails "a stream too short for its sequences" (fun () -> Zstd.decompress (set hello 21 '\x09'));
  fails "rubbish after the frame" (fun () -> Zstd.decompress (hi ^ "\x00\x00"));
  for len = 1 to String.length hello - 1 do
    fails (Printf.sprintf "cut after %d bytes" len) (fun () -> Zstd.decompress (String.sub hello 0 len))
  done

let read_file (path : string) : string = In_channel.with_open_bin path In_channel.input_all

let test_zstd_file () =
  let squares = String.concat "" (List.init 5000 (fun i -> string_of_int (i * i) ^ "\n")) in
  let z = read_file "zstd/squares.zst" in
  Alcotest.(check string) "the squares: Huffman literals, FSE tables" squares (Zstd.decompress z);
  (* damaged, it is refused (the checksum, if nothing before) with a
   * Failure, never an array's bounds or a loop *)
  let n = String.length z in
  let refused what s =
    match Zstd.decompress s with
    | out -> if out = squares then Alcotest.failf "%s: decoded all the same" what
    | exception Failure _ -> ()
  in
  (* from the first block's header: the window's byte before it is not
   * used *)
  let i = ref 6 in
  while !i < n do
    refused (Printf.sprintf "byte %d changed" !i) (set z !i (Char.chr (Char.code z.[!i] lxor 0x55)));
    refused (Printf.sprintf "cut after %d bytes" !i) (String.sub z 0 !i);
    i := !i + 97
  done

let tests =
  Testo.categorize "Zstd"
    [
      t "Fse: the 16 states of A 8, B 4, C 3, D" test_fse_table;
      t "Fse: C D B A, backwards" test_fse_decode;
      t "Fse: fields of bits" test_bits;
      t "XXH64" test_xxh64;
      t "Zstd: hi, hello, zeros" test_worked_examples;
      t "Zstd: frames, skippable ones, the size" test_frames;
      t "Zstd: corruptions caught" test_corruptions;
      t "Zstd: zstd's own file, and damaged" test_zstd_file;
    ]
