(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/slides/Outline: the .mli's example, the corners of an
 * outline (tabs, blank lines, points with no title yet), and which
 * slide a caret is on. *)

let t = Testo.create

let shown slides =
  List.map (fun (s : Outline.slide) -> s.title :: List.map (fun (l, p) -> Printf.sprintf "%d:%s" l p) s.points) slides

let example =
  "Why paint programs\n  Pictures are dots\n    and dots are bits\nThe bucket\n  A mask, then the pattern"

let test_the_worked_example () =
  Alcotest.(check (list (list string))) "two slides"
    [ [ "Why paint programs"; "1:Pictures are dots"; "2:and dots are bits" ]; [ "The bucket"; "1:A mask, then the pattern" ] ]
    (shown (Outline.parse example))

let test_corners () =
  Alcotest.(check (list (list string))) "tabs, blank lines, a point before any title"
    [ [ ""; "1:first" ]; [ "Title"; "1:tab"; "2:two tabs"; "1:odd indent" ] ]
    (shown (Outline.parse "  first\n\nTitle\n\ttab\n\t\ttwo tabs\n   odd indent\n\n"));
  Alcotest.(check int) "nothing is no slides" 0 (List.length (Outline.parse "\n \n"))

(* where a word first appears *)
let index_of text word =
  let n = String.length word in
  let rec go i = if String.sub text i n = word then i else go (i + 1) in
  go 0

let test_slide_at () =
  let at s = Outline.slide_at example (index_of example s) in
  Alcotest.(check int) "in the first title" 0 (at "paint");
  Alcotest.(check int) "in a point of the first" 0 (at "dots are");
  Alcotest.(check int) "in the second title" 1 (at "bucket");
  Alcotest.(check int) "at the very end" 1 (Outline.slide_at example (String.length example));
  Alcotest.(check int) "after an untitled first slide" 1 (Outline.slide_at "  a\nB\n  b" 7)

let test_start_of () =
  Alcotest.(check int) "the first slide" 0 (Outline.start_of example 0);
  Alcotest.(check int) "the second" (index_of example "The bucket") (Outline.start_of example 1);
  Alcotest.(check int) "past the last: the end" (String.length example) (Outline.start_of example 2)

let test_round_trip () =
  let slides = Outline.parse example in
  Alcotest.(check string) "written back as it was" example (Outline.to_text slides);
  Alcotest.(check (list (list string))) "and read again" (shown slides) (shown (Outline.parse (Outline.to_text slides)))

let tests =
  [
    t "an outline: the worked example" test_the_worked_example;
    t "tabs, blanks, and a point with no title" test_corners;
    t "which slide the caret is on" test_slide_at;
    t "where a slide starts" test_start_of;
    t "written back, read again" test_round_trip;
  ]
