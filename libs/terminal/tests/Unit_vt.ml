(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_vt.mli *)

let screen ?(rows = 4) ?(cols = 16) (bytes : string) : Vt.t = Vt.feed (Vt.create ~rows ~cols) bytes

let check_text (msg : string) (expected : string list) (t : Vt.t) : unit = Alcotest.(check (list string)) msg expected (Vt.text t)
let check_cursor (msg : string) (expected : int * int) (t : Vt.t) : unit = Alcotest.(check (pair int int)) msg expected (Vt.cursor t)

let tests =
  Testo.categorize "Vt"
    [
      Testo.create "the worked example: a clear, a red word, row 3 column 5" (fun () ->
          let t = screen "\x1b[2J\x1b[HHello, \x1b[1;31mworld\x1b[0m!\r\n\x1b[3;5Hx" in
          check_text "screen" [ "Hello, world!"; ""; "    x"; "" ] t;
          check_cursor "after the x" (2, 5) t;
          let red = { Vt.plain with fg = Vt.Red; bold = true } in
          Alcotest.(check bool) "w bold red" true ((Vt.cell t 0 7).attrs = red);
          Alcotest.(check bool) "d bold red" true ((Vt.cell t 0 11).attrs = red);
          Alcotest.(check bool) "! plain" true ((Vt.cell t 0 12).attrs = Vt.plain));
      Testo.create "the deferred wrap: 4 columns, abcd CR LF ef, no blank line" (fun () ->
          let t = screen ~cols:4 "abcd\r\nef" in
          check_text "screen" [ "abcd"; "ef"; ""; "" ] t;
          let t = screen ~cols:4 "abcd" in
          check_cursor "on the last column" (0, 3) t;
          check_text "the next character wraps" [ "abcd"; "e"; ""; "" ] (Vt.feed t "e"));
      Testo.create "LF on the last row scrolls, CR LF on each" (fun () ->
          let t = screen ~rows:3 "one\r\ntwo\r\nthree\r\nfour" in
          check_text "one gone" [ "two"; "three"; "four" ] t);
      Testo.create "the scrolling region: a status line that stays" (fun () ->
          (* rows 1-3 scroll, row 4 is a status line, as in vi *)
          let t = screen "\x1b[4;1Hstatus\x1b[1;3r" in
          check_cursor "the region set: cursor home" (0, 0) t;
          let t = Vt.feed t "a\r\nb\r\nc\r\nd" in
          check_text "screen" [ "b"; "c"; "d"; "status" ] t);
      Testo.create "reverse index on the top row scrolls down" (fun () ->
          check_text "screen" [ "new"; "one"; "two"; "" ] (screen "one\r\ntwo\x1b[H\x1bMnew"));
      Testo.create "erasing: J and K, before and after the cursor" (fun () ->
          let full = "aaaa\r\nbbbb\r\ncccc\x1b[2;3H" in
          check_text "K 0" [ "aaaa"; "bb"; "cccc"; "" ] (screen full |> fun t -> Vt.feed t "\x1b[K");
          check_text "K 1" [ "aaaa"; "   b"; "cccc"; "" ] (screen full |> fun t -> Vt.feed t "\x1b[1K");
          check_text "K 2" [ "aaaa"; ""; "cccc"; "" ] (screen full |> fun t -> Vt.feed t "\x1b[2K");
          check_text "J 0" [ "aaaa"; "bb"; ""; "" ] (screen full |> fun t -> Vt.feed t "\x1b[J");
          check_text "J 1" [ ""; "   b"; "cccc"; "" ] (screen full |> fun t -> Vt.feed t "\x1b[1J");
          check_text "J 2" [ ""; ""; ""; "" ] (screen full |> fun t -> Vt.feed t "\x1b[2J"));
      Testo.create "inserting and deleting lines and characters" (fun () ->
          let three = "one\r\ntwo\r\nsix\x1b[2;1H" in
          check_text "L" [ "one"; ""; "two"; "six" ] (Vt.feed (screen three) "\x1b[L");
          check_text "M" [ "one"; "six"; ""; "" ] (Vt.feed (screen three) "\x1b[M");
          check_text "@" [ "abc"; "" ] (screen ~rows:2 "bc\x1b[H\x1b[@a");
          check_text "P" [ "ac"; "" ] (screen ~rows:2 "abc\x1b[1;2H\x1b[P"));
      Testo.create "cursor moves stop at the edges" (fun () ->
          check_cursor "up 9 from the top" (0, 0) (screen "\x1b[9A");
          check_cursor "right 99" (0, 15) (screen "\x1b[99C");
          check_cursor "CUP 0;0 is 1;1" (0, 0) (screen "xy\x1b[0;0H");
          check_cursor "tab" (0, 8) (screen "ab\t");
          check_cursor "BS" (0, 1) (screen "ab\b"));
      Testo.create "UTF-8: one character, one cell, even cut in two" (fun () ->
          let t = screen "\xE2\x94\x80\xC3\xA9x" in
          Alcotest.(check string) "a box line" "\xE2\x94\x80" (Vt.cell t 0 0).glyph;
          Alcotest.(check string) "e acute" "\xC3\xA9" (Vt.cell t 0 1).glyph;
          check_cursor "three cells" (0, 3) t;
          let t = Vt.feed (screen "\xE2\x94") "\x80" in
          Alcotest.(check string) "across two feeds" "\xE2\x94\x80" (Vt.cell t 0 0).glyph;
          Alcotest.(check string) "a stray byte" "\xEF\xBF\xBD" (Vt.cell (screen "\x80") 0 0).glyph);
      Testo.create "sequences not understood leave no trace" (fun () ->
          (* a window title (OSC), the alternate screen, 256 colours, a
             charset, and a sequence cut in two feeds *)
          let t = screen "\x1b]0;my title\x07a\x1b]2;x\x1b\\b\x1b[?1049hc\x1b[38;5;208md\x1b(Be" in
          check_text "only the letters" [ "abcde"; ""; ""; "" ] t;
          Alcotest.(check bool) "38;5;n changed nothing" true ((Vt.cell t 0 3).attrs = Vt.plain);
          check_text "cut in two, X over the a" [ "Xb"; ""; ""; "" ] (Vt.feed (screen "ab\x1b[1") ";1HX"));
      Testo.create "SGR: reverse, background, bright, reset" (fun () ->
          let t = screen "\x1b[7;44ma\x1b[27;49;93mb\x1b[mc" in
          Alcotest.(check bool) "a" true ((Vt.cell t 0 0).attrs = { Vt.plain with reverse = true; bg = Vt.Blue });
          Alcotest.(check bool) "b" true ((Vt.cell t 0 1).attrs = { Vt.plain with fg = Vt.Yellow; bold = true });
          Alcotest.(check bool) "c" true ((Vt.cell t 0 2).attrs = Vt.plain));
      Testo.create "the cursor hidden and shown, BEL counted, save and restore" (fun () ->
          Alcotest.(check bool) "hidden" false (Vt.cursor_visible (screen "\x1b[?25l"));
          Alcotest.(check bool) "shown" true (Vt.cursor_visible (screen "\x1b[?25l\x1b[?25h"));
          Alcotest.(check int) "two bells" 2 (Vt.bells (screen "\x07a\x07"));
          check_cursor "ESC 7, ESC 8" (1, 2) (screen "\r\nab\x1b7\x1b[4;9H\x1b8");
          check_cursor "CSI s, CSI u" (1, 2) (screen "\r\nab\x1b[s\x1b[4;9H\x1b[u"));
      Testo.create "feed leaves the screen it was given unchanged" (fun () ->
          let t = screen "abc" in
          let _ = Vt.feed t "\x1b[2Jxyz" in
          check_text "still abc" [ "abc"; ""; ""; "" ] t);
      Testo.create "keys: Enter, arrows, Control-C" (fun () ->
          Alcotest.(check (option string)) "Enter" (Some "\r") (Vt.key ~ctrl:false "Enter");
          Alcotest.(check (option string)) "up" (Some "\x1b[A") (Vt.key ~ctrl:false "ArrowUp");
          Alcotest.(check (option string)) "Control-C" (Some "\x03") (Vt.key ~ctrl:true "c");
          Alcotest.(check (option string)) "a letter is typed, not a key" None (Vt.key ~ctrl:false "a"));
    ]
