(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_entities.mli *)

let tests =
  Testo.categorize "Entities"
    [
      Testo.create "the worked example" (fun () ->
          Alcotest.(check string)
            "Café & crème “brûlée” &foo; AT&T" "Caf\xC3\xA9 & cr\xC3\xA8me \xE2\x80\x9Cbr\xC3\xBBl\xC3\xA9e\xE2\x80\x9D &foo; AT&T"
            (Entities.decode "Caf&eacute; &amp; cr&#232;me &#147;br&ucirc;l&eacute;e&#148; &foo; AT&T"));
      Testo.create "HTML 4's 252 names, and apos" (fun () ->
          Alcotest.(check int) "count" 253 Entities.count;
          Alcotest.(check (option string)) "nbsp, the first of Latin-1" (Some "\xC2\xA0") (Entities.lookup "nbsp");
          Alcotest.(check (option string)) "yuml, the last" (Some "\xC3\xBF") (Entities.lookup "yuml");
          Alcotest.(check (option string)) "Yuml is another" (Some "\xC5\xB8") (Entities.lookup "Yuml");
          Alcotest.(check (option string)) "euro" (Some "\xE2\x82\xAC") (Entities.lookup "euro");
          Alcotest.(check (option string)) "diams, the last symbol" (Some "\xE2\x99\xA6") (Entities.lookup "diams");
          Alcotest.(check (option string)) "names are case-sensitive" None (Entities.lookup "EACUTE"));
      Testo.create "numbers" (fun () ->
          Alcotest.(check string) "decimal" "\xC3\xA9" (Entities.decode "&#233;");
          Alcotest.(check string) "hexadecimal" "\xC3\xA9" (Entities.decode "&#xE9;");
          Alcotest.(check string) "X too" "\xC3\xA9" (Entities.decode "&#XE9;");
          Alcotest.(check string) "the ; forgiven" "\xC3\xA9!" (Entities.decode "&#233!");
          Alcotest.(check string) "beyond the BMP" "\xF0\x9F\x98\x80" (Entities.decode "&#x1F600;"));
      Testo.create "the WHATWG's repairs" (fun () ->
          Alcotest.(check string) "150 is Windows-1252's en dash" "\xE2\x80\x93" (Entities.of_code 150);
          Alcotest.(check string) "0" "\xEF\xBF\xBD" (Entities.of_code 0);
          Alcotest.(check string) "a surrogate" "\xEF\xBF\xBD" (Entities.of_code 0xD800);
          Alcotest.(check string) "above U+10FFFF" "\xEF\xBF\xBD" (Entities.of_code 0x110000));
      Testo.create "what is not a reference is left" (fun () ->
          List.iter
            (fun s -> Alcotest.(check string) s s (Entities.decode s))
            [ "AT&T"; "fish & chips"; "&"; "&;"; "&#;"; "&#x;"; "&foo;"; "&eacute"; "?a=1&copy=2"; "a&&b" ]);
    ]
