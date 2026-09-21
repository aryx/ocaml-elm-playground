(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/richtext/Page: where every character sits, and the way back
 * from a point to a place in the text -- with every character ten
 * units wide, so that each position is a sum a person can do. *)

let t = Testo.create
let metrics _look _ch = 10.
let page ?(width = 50.) s = Page.layout ~metrics ~width (Rich.of_string s)

let placed p =
  Page.glyphs p
  |> List.map (fun (g : Page.glyph) -> (g.text, g.x, g.baseline))

(* the .mli's worked example *)
let test_the_worked_example () =
  let p = page "ab cd ef" in
  Alcotest.(check (list (triple string (float 1e-9) (float 1e-9))))
    "two lines, the space after cd hanging past the edge"
    [ ("a", 0., 16.); ("b", 10., 16.); (" ", 20., 16.); ("c", 30., 16.); ("d", 40., 16.);
      (" ", 50., 16.); ("e", 0., 38.4); ("f", 10., 38.4) ]
    (placed p);
  (* 16 * 1.4 = 22.4 for a line, and the second baseline 16 below it *)
  Alcotest.(check (float 1e-9)) "two lines of 22.4" 44.8 (Page.height p)

(* the caret before a character, and at the very end *)
let test_caret_at () =
  let p = page "ab cd ef" in
  let x, baseline, _ = Page.caret_at p 3 in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "before c" (30., 16.) (x, baseline);
  let x, baseline, _ = Page.caret_at p 6 in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "before e: the start of line 2" (0., 38.4) (x, baseline);
  let x, baseline, _ = Page.caret_at p 8 in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "after f: the end of the text" (20., 38.4) (x, baseline)

(* the way back: a click to a place in the text *)
let test_offset_at () =
  let p = page "ab cd ef" in
  Alcotest.(check int) "on the left half of c: before it" 3 (Page.offset_at p (32., 10.));
  Alcotest.(check int) "on its right half: after it" 4 (Page.offset_at p (38., 10.));
  Alcotest.(check int) "on line 2, on f" 7 (Page.offset_at p (12., 30.));
  (* past the end of a wrapped line: before the space it was broken
     after, so the caret stays on the line that was clicked *)
  Alcotest.(check int) "past the end of line 1" 5 (Page.offset_at p (200., 10.));
  Alcotest.(check int) "past the end of the text" 8 (Page.offset_at p (200., 30.));
  (* below everything: the last line *)
  Alcotest.(check int) "below the page" 8 (Page.offset_at p (200., 500.))

(* and they agree: a click where the caret is drawn finds the same
   place, for every place in the text *)
let test_the_two_ways_agree () =
  let text = "one two three four five six seven" in
  let p = page ~width:60. text in
  for offset = 0 to String.length text do
    let x, baseline, height = Page.caret_at p offset in
    (* a click just right of where the caret is drawn, in its line *)
    let back = Page.offset_at p (x +. 1., baseline -. (height /. 3.)) in
    (* at the end of a wrapped line the space may be where the click
       lands instead: both are the same place on the screen *)
    if back <> offset && not (back = offset - 1 && text.[offset - 1] = ' ') then
      Alcotest.failf "offset %d is drawn at (%.1f, %.1f) and a click there gives %d" offset x baseline back
  done

let test_newlines_end_lines () =
  let p = page "ab\ncd\n" in
  let lines = List.map (fun (_, _, b) -> b) (placed p) |> List.sort_uniq compare in
  Alcotest.(check int) "two lines of text" 2 (List.length lines);
  (* the caret after the final newline is on a third, empty line *)
  let x, baseline, _ = Page.caret_at p 6 in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "on the empty last line" (0., 60.8) (x, baseline)

(* a bigger look makes a taller line, and puts its baseline lower *)
let test_a_bigger_look_makes_a_taller_line () =
  let r = Rich.of_string "ab" in
  let r = Rich.restyle (fun st -> { st with Style.size = 32. }) (Rich.select ~anchor:1 ~caret:2 r) in
  let p = Page.layout ~metrics ~width:50. r in
  Alcotest.(check (float 1e-9)) "32 * 1.4" 44.8 (Page.height p);
  Alcotest.(check (list (float 1e-9))) "one baseline, at the big letter's em" [ 32.; 32. ]
    (List.map (fun (g : Page.glyph) -> g.baseline) (Page.glyphs p))

let test_a_word_too_long_for_a_line () =
  let p = page "abcdefghij" in
  Alcotest.(check (list (float 1e-9))) "broken where it reaches the edge"
    [ 16.; 16.; 16.; 16.; 16.; 38.4; 38.4; 38.4; 38.4; 38.4 ]
    (List.map (fun (g : Page.glyph) -> g.baseline) (Page.glyphs p))

(* the .mli's example of alignment: "ab cd" in a line 60 wide *)
let test_alignment () =
  let xs ?align width s =
    Page.layout ?align ~metrics ~width (Rich.of_string s)
    |> Page.glyphs
    |> List.map (fun (g : Page.glyph) -> (g.text, g.x))
  in
  let x_of ch l = List.assoc ch l in
  Alcotest.(check (float 1e-9)) "centered: half the slack first" 5. (x_of "a" (xs ~align:Page.Center 60. "ab cd"));
  Alcotest.(check (float 1e-9)) "right: all of it" 10. (x_of "a" (xs ~align:Page.Right 60. "ab cd"));
  Alcotest.(check (float 1e-9)) "justified, but a one-line paragraph is its own last line"
    30. (x_of "c" (xs ~align:Page.Justify 60. "ab cd"));
  (* a paragraph of two lines: the first is stretched, its one space
     between words taking all 10 of the slack; the last is not *)
  let two = xs ~align:Page.Justify 60. "ab cd ef" in
  Alcotest.(check (list (float 1e-9))) "c and d moved right by 10" [ 40.; 50. ] [ x_of "c" two; x_of "d" two ];
  Alcotest.(check (float 1e-9)) "and the last line left alone" 0. (x_of "e" two)

(* the two ways still agree once lines have moved *)
let test_the_two_ways_agree_when_aligned () =
  let text = "one two three four five six seven" in
  List.iter
    (fun align ->
      let p = Page.layout ~align ~metrics ~width:60. (Rich.of_string text) in
      for offset = 0 to String.length text do
        let x, baseline, height = Page.caret_at p offset in
        let back = Page.offset_at p (x +. 1., baseline -. (height /. 3.)) in
        if back <> offset && not (back = offset - 1 && text.[offset - 1] = ' ') then
          Alcotest.failf "offset %d is drawn at (%.1f, %.1f) and a click there gives %d" offset x baseline back
      done)
    [ Page.Center; Page.Right; Page.Justify ]

let tests =
  [
    t "the worked example" test_the_worked_example;
    t "alignment: where a line's slack goes" test_alignment;
    t "the two ways agree, aligned" test_the_two_ways_agree_when_aligned;
    t "where the caret goes" test_caret_at;
    t "a click back to a place in the text" test_offset_at;
    t "the two ways agree, everywhere" test_the_two_ways_agree;
    t "newlines end lines" test_newlines_end_lines;
    t "a bigger look makes a taller line" test_a_bigger_look_makes_a_taller_line;
    t "a word too long for a line" test_a_word_too_long_for_a_line;
  ]
