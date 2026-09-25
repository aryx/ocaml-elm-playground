(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_charset.mli *)

let charset : Charset.t Alcotest.testable =
  Alcotest.testable
    (fun fmt t -> Format.pp_print_string fmt (match t with Charset.Utf_8 -> "UTF-8" | Windows_1252 -> "Windows-1252"))
    ( = )

let latin_1 = "caf\xE9"
let utf_8 = "caf\xC3\xA9"

let tests =
  Testo.categorize "Charset"
    [
      Testo.create "cafe, in both encodings, and the mojibake" (fun () ->
          Alcotest.(check string) "Latin-1's E9 is two bytes in UTF-8" utf_8 (Charset.to_utf_8 Windows_1252 latin_1);
          Alcotest.(check string) "UTF-8 kept" utf_8 (Charset.to_utf_8 Utf_8 utf_8);
          Alcotest.(check string) "UTF-8 read as Latin-1: cafÃ©" "caf\xC3\x83\xC2\xA9" (Charset.to_utf_8 Windows_1252 utf_8));
      Testo.create "Word's quotes, labelled Latin-1, are Windows-1252's" (fun () ->
          Alcotest.(check string)
            "93 hi 94" "\xE2\x80\x9Chi\xE2\x80\x9D"
            (Charset.decode ~content_type:"text/html; charset=iso-8859-1" "\x93hi\x94");
          Alcotest.(check int) "80 is the euro" 0x20AC (Charset.windows_1252 0x80);
          Alcotest.(check int) "81 is undefined, itself" 0x81 (Charset.windows_1252 0x81);
          Alcotest.(check int) "E9 is Latin-1's" 0xE9 (Charset.windows_1252 0xE9));
      Testo.create "labels" (fun () ->
          Alcotest.(check (option charset)) "utf8" (Some Utf_8) (Charset.of_label " UTF8 ");
          Alcotest.(check (option charset)) "latin1" (Some Windows_1252) (Charset.of_label "Latin1");
          Alcotest.(check (option charset)) "us-ascii" (Some Windows_1252) (Charset.of_label "us-ascii");
          Alcotest.(check (option charset)) "not ours" None (Charset.of_label "shift_jis"));
      Testo.create "the header" (fun () ->
          Alcotest.(check (option charset))
            "charset=" (Some Windows_1252)
            (Charset.of_content_type "text/html; charset=ISO-8859-1");
          Alcotest.(check (option charset)) "quoted" (Some Utf_8) (Charset.of_content_type "text/html;charset=\"utf-8\"");
          Alcotest.(check (option charset)) "none" None (Charset.of_content_type "text/html"));
      Testo.create "the <meta> prescan" (fun () ->
          Alcotest.(check (option charset)) "HTML5's" (Some Utf_8) (Charset.of_meta "<html><head><meta charset=utf-8>");
          Alcotest.(check (option charset))
            "HTML 4's" (Some Windows_1252)
            (Charset.of_meta
               "<HEAD><META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=iso-8859-1\"></HEAD>");
          Alcotest.(check (option charset))
            "not in a meta" None
            (Charset.of_meta "<p>charset=utf-8</p><meta name=author content=pad>"));
      Testo.create "detect: BOM, header, meta, guess, in that order" (fun () ->
          let meta = "<meta charset=utf-8>" ^ latin_1 in
          Alcotest.(check charset) "the BOM beats the header" Utf_8
            (Charset.detect ~content_type:"text/html; charset=latin1" ("\xEF\xBB\xBF" ^ latin_1));
          Alcotest.(check charset) "the header beats the meta" Windows_1252
            (Charset.detect ~content_type:"text/html; charset=latin1" meta);
          Alcotest.(check charset) "the meta beats the guess" Utf_8 (Charset.detect meta);
          Alcotest.(check charset) "valid UTF-8" Utf_8 (Charset.detect utf_8);
          Alcotest.(check charset) "not UTF-8" Windows_1252 (Charset.detect latin_1);
          Alcotest.(check charset) "all ASCII: the default" Windows_1252 (Charset.detect "cafe"));
      Testo.create "malformed UTF-8" (fun () ->
          Alcotest.(check bool) "valid" true (Charset.is_utf_8 utf_8);
          Alcotest.(check bool) "E9 then a letter" false (Charset.is_utf_8 "caf\xE9s");
          Alcotest.(check bool) "overlong /" false (Charset.is_utf_8 "\xC0\xAF");
          Alcotest.(check bool) "a surrogate" false (Charset.is_utf_8 "\xED\xA0\x80");
          Alcotest.(check string) "each bad sequence a U+FFFD" "a\xEF\xBF\xBDb" (Charset.to_utf_8 Utf_8 "a\xFFb");
          Alcotest.(check string) "the BOM dropped" "a" (Charset.to_utf_8 Utf_8 "\xEF\xBB\xBFa"));
    ]
