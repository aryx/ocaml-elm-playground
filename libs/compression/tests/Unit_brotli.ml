(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The worked examples of Brotli.mli and Brotli_dictionary.mli, the
 * check values RFC 7932 gives for its tables, and streams made by
 * Google's encoder (brotli/make_brotli.py) *)

let t = Testo.create

let fails msg f =
  match f () with
  | _ -> Alcotest.failf "%s: no failure" msg
  | exception Failure _ -> ()

let dictionary = Brotli_words.bytes
let decompress (s : string) : string = Brotli.decompress ~dictionary s

(*****************************************************************************)
(* The RFC's tables *)
(*****************************************************************************)

let test_dictionary () =
  Alcotest.(check int) "122,784 bytes" Brotli_dictionary.size (String.length dictionary);
  Alcotest.(check int32) "the CRC-32 of Appendix A" 0x5136CB04l (Crc32.string dictionary);
  let word = Brotli_dictionary.word dictionary in
  Alcotest.(check string) "the first word" "time" (word ~length:4 ~id:0);
  Alcotest.(check string) "world" "world" (word ~length:5 ~id:3);
  Alcotest.(check string) "world." "world." (word ~length:5 ~id:((20 * 1024) + 3));
  Alcotest.(check string) "Hello, " "Hello, " (word ~length:5 ~id:((58 * 1024) + 719));
  fails "a word of 3 bytes" (fun () -> word ~length:3 ~id:0);
  fails "transform 121" (fun () -> word ~length:5 ~id:(121 * 1024));
  fails "another dictionary" (fun () -> Brotli_dictionary.word "time" ~length:4 ~id:0)

let test_transforms () =
  (* as Appendix B says to check them: each transform's prefix, a 0, a
   * byte for its change, its suffix, a 0 *)
  let b = Buffer.create 648 in
  Array.iter
    (fun (prefix, (change : Brotli_dictionary.change), suffix) ->
      let n =
        match change with
        | Identity -> 0
        | Ferment_first -> 1
        | Ferment_all -> 2
        | Omit_first k -> 2 + k
        | Omit_last k -> 11 + k
      in
      Buffer.add_string b (prefix ^ "\000" ^ String.make 1 (Char.chr n) ^ suffix ^ "\000"))
    Brotli_dictionary.transforms;
  Alcotest.(check int) "648 bytes" 648 (Buffer.length b);
  Alcotest.(check int32) "the CRC-32 of Appendix B" 0x3D965F81l (Crc32.string (Buffer.contents b));
  let changed = List.map (fun id -> Brotli_dictionary.transform id "world") [ 0; 1; 5; 9; 12; 44; 58; 72; 54 ] in
  Alcotest.(check (list string)) "world, changed"
    [ "world"; "world "; "world the "; "World"; "worl"; "WORLD"; "World, "; ".com/world"; "" ]
    changed;
  (* é is C3 A9, its capital C3 89: the second byte's bit 32 *)
  Alcotest.(check string) "capitals in UTF-8" "\xC3\x89T\xC3\x89" (Brotli_dictionary.transform 44 "\xC3\xA9t\xC3\xA9")

let test_contexts () =
  (* Lut0, Lut1 and Lut2 of section 7.1, read back through [context]
   * (each is 0 at 0, so the other byte's table shows alone) *)
  let table f = String.init 256 (fun c -> Char.chr (f c)) in
  Alcotest.(check int32) "Lut0" 0x8E91EFB7l (Crc32.string (table (fun c -> Brotli.context ~mode:2 ~p1:c ~p2:0)));
  Alcotest.(check int32) "Lut1" 0xD01A32F4l (Crc32.string (table (fun c -> Brotli.context ~mode:2 ~p1:0 ~p2:c)));
  Alcotest.(check int32) "Lut2" 0x0DD7A0D6l (Crc32.string (table (fun c -> Brotli.context ~mode:3 ~p1:0 ~p2:c)));
  Alcotest.(check (list int)) "the last byte's 6 bits, low and high" [ 0x21; 0x18 ]
    [ Brotli.context ~mode:0 ~p1:0x61 ~p2:0; Brotli.context ~mode:1 ~p1:0x61 ~p2:0 ]

(*****************************************************************************)
(* Streams *)
(*****************************************************************************)

let hi = "\x10\x00\x10hi\x03"
let hello = "\x82\x01\x00\x00\x04\x40\x0C\x52\x2B\x7A\x5A\xE5\x00\x01"

let test_worked_examples () =
  Alcotest.(check string) "nothing, a window of 2^16" "" (decompress "\x06");
  Alcotest.(check string) "nothing, a window of 2^22" "" (decompress "\x3B");
  Alcotest.(check string) "hi, not compressed" "hi" (decompress hi);
  Alcotest.(check string) "two words of the dictionary" "Hello, world." (decompress hello);
  fails "the same without the dictionary" (fun () -> Brotli.decompress hello);
  Alcotest.(check string) "no dictionary needed" "hi" (Brotli.decompress hi)

let test_corruptions () =
  fails "nothing" (fun () -> decompress "");
  fails "bytes after the last meta-block" (fun () -> decompress (hi ^ "\x00"));
  fails "padding bits set" (fun () -> decompress "\x86");
  fails "a window of 2^25 (large window Brotli)" (fun () -> decompress "\x11");
  for len = 1 to String.length hello - 1 do
    fails (Printf.sprintf "cut after %d bytes" len) (fun () -> decompress (String.sub hello 0 len))
  done

let read_file (path : string) : string = In_channel.with_open_bin path In_channel.input_all

(* brotli/make_brotli.py's texts *)
let squares (n : int) : string = String.concat "" (List.init n (fun i -> string_of_int (i * i) ^ "\n"))

let noise (n : int) : string =
  let x = ref 1 in
  String.init n (fun _ ->
      x := ((!x * 75) + 74) mod 65537;
      Char.chr (!x land 255))

let french = String.concat "" (List.init 40 (fun _ -> "Les élèves français étudient à l'école, près de la forêt. "))

let page =
  "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n\
   <title>Information about the government of the United States</title>\n\
   </head>\n<body>\n<p>However, the following information was provided by the university.</p>\n\
   </body>\n</html>\n"

let test_files () =
  Alcotest.(check string) "the squares, at quality 11" (squares 5000) (decompress (read_file "brotli/squares.br"));
  Alcotest.(check string) "squares, noise, French, squares"
    (squares 2000 ^ noise 4000 ^ french ^ squares 2000)
    (decompress (read_file "brotli/mixed.br"));
  let z = read_file "brotli/page.br" in
  Alcotest.(check string) "a web page" page (decompress z);
  fails "the page without the dictionary" (fun () -> Brotli.decompress z)

(* a damaged stream is refused or gives other bytes (Brotli has no
 * checksum), with a Failure: never an array's bounds or a loop *)
let test_damaged () =
  let z = read_file "brotli/squares.br" in
  let survives s =
    match decompress s with
    | _ -> ()
    | exception Failure _ -> ()
  in
  let i = ref 0 in
  while !i < String.length z do
    let b = Bytes.of_string z in
    Bytes.set b !i (Char.chr (Char.code z.[!i] lxor 0x55));
    survives (Bytes.to_string b);
    fails (Printf.sprintf "cut after %d bytes" !i) (fun () -> decompress (String.sub z 0 !i));
    i := !i + 97
  done

let tests =
  Testo.categorize "Brotli"
    [
      t "the dictionary: its CRC-32, words by length and id" test_dictionary;
      t "the 121 transforms: their CRC-32, world changed" test_transforms;
      t "the contexts: the RFC's three tables" test_contexts;
      t "nothing, hi, Hello, world." test_worked_examples;
      t "corruptions caught" test_corruptions;
      t "Google's encoder's files" test_files;
      t "a damaged file" test_damaged;
    ]
