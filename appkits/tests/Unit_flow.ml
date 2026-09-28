(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/richtext/Flow: a text dealt out into columns, and frames
 * anchored in it following the text -- the .mli's worked example, and
 * a frame pushed along by a line typed above it. Every character 10
 * wide, a line of plain text 22.4 high, the lines made by newlines. *)

let t = Testo.create
let metrics _ _ = 10.
let page s = Page.layout ~metrics ~width:200. (Rich.of_string s)

(* each line by its first character, with its column and top *)
let placed (f : Flow.t) =
  List.map (fun (p : Flow.placed_line) -> ((List.hd p.line.cells).text, p.column, p.top)) f.lines

let where = Alcotest.(list (triple string int (float 1e-9)))

let test_the_worked_example () =
  let f = Flow.flow ~column_height:50. ~anchors:[ (0, 30.) ] (page "a\nb\nc\nd\ne") in
  Alcotest.check where "the lines"
    [ ("a", 0, 0.); ("b", 2, 0.); ("c", 2, 22.4); ("d", 3, 0.); ("e", 3, 22.4) ]
    (placed f);
  Alcotest.(check (list (triple int int (float 1e-9)))) "the frame, at the top of the second column" [ (0, 1, 0.) ]
    (List.map (fun (p : Flow.placed_frame) -> (p.frame, p.in_column, p.at)) f.frames);
  Alcotest.(check int) "four columns" 4 f.columns

let test_text_alone () =
  let f = Flow.flow ~column_height:50. ~anchors:[] (page "a\nb\nc\nd\ne") in
  Alcotest.check where "two to a column" [ ("a", 0, 0.); ("b", 0, 22.4); ("c", 1, 0.); ("d", 1, 22.4); ("e", 2, 0.) ] (placed f)

let test_anchored_at_the_end () =
  let f = Flow.flow ~column_height:100. ~anchors:[ (3, 20.) ] (page "a\nb") in
  Alcotest.(check (list (pair int (float 1e-9)))) "after the last line" [ (0, 44.8) ]
    (List.map (fun (p : Flow.placed_frame) -> (p.in_column, p.at)) f.frames)

(* the frame is in the text's flow: a line typed above it moves it
   down, here into the next column *)
let test_a_line_above_pushes_it () =
  let before = Flow.flow ~column_height:80. ~anchors:[ (2, 30.) ] (page "a\nb\nc") in
  let after = Flow.flow ~column_height:80. ~anchors:[ (4, 30.) ] (page "z\na\nb\nc") in
  let frame (f : Flow.t) = List.map (fun (p : Flow.placed_frame) -> (p.in_column, p.at)) f.frames in
  Alcotest.(check (list (pair int (float 1e-9)))) "under b, in the first column" [ (0, 44.8) ] (frame before);
  Alcotest.(check (list (pair int (float 1e-9)))) "a line above: pushed to the next" [ (1, 0.) ] (frame after)

let tests =
  [
    t "flow: the worked example" test_the_worked_example;
    t "flow: text alone, two lines to a column" test_text_alone;
    t "flow: a frame anchored at the end" test_anchored_at_the_end;
    t "flow: a line typed above pushes a frame along" test_a_line_above_pushes_it;
  ]
