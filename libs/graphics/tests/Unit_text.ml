(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Hershey (the font) and Stroke (thick lines), and Fill.polygons which
 * Stroke relies on *)

let t = Testo.create

let blue = 0x0000ff

let glyph = Alcotest.testable (fun fmt (g : Hershey.glyph) ->
    Format.fprintf fmt "{left=%d; right=%d; %d strokes}" g.left g.right (List.length g.strokes)) ( = )

(* the "A" example in Hershey.mli *)
let test_decode_a () =
  Alcotest.check glyph "A"
    { left = -9; right = 9; strokes = [ [ (0, -12); (-8, 9) ]; [ (0, -12); (8, 9) ]; [ (-5, 2); (5, 2) ] ] }
    (Hershey.decode_glyph "I[RFJ[ RRFZ[ RMTWT")

(* the embedded font decodes to the same "A" *)
let test_font_a () =
  Alcotest.check glyph "the font's A" (Hershey.decode_glyph "I[RFJ[ RRFZ[ RMTWT") (Hershey.glyph 'A')

let test_layout () =
  let strokes, width = Hershey.layout "AA" in
  Alcotest.(check (float 1e-9)) "two A's, 18 units each" 36. width;
  (* the second A's apex is 18 units right of the first's *)
  let apexes = List.filter_map (function (x, -12.) :: _ -> Some x | _ -> None) strokes in
  Alcotest.(check (list (float 1e-9))) "apexes" [ 9.; 9.; 27.; 27. ] apexes

(* Two overlapping squares, both counterclockwise: filled together,
 * half-transparent, their overlap is painted once; filled one after
 * the other, it's painted twice, darker *)
let test_polygons_union () =
  let square x y = [ (x, y); (x +. 6., y); (x +. 6., y +. 6.); (x, y +. 6.) ] in
  let together = Framebuffer.create ~width:10 ~height:10 in
  Fill.polygons together [ square 0. 0.; square 3. 3. ] ~rgb:blue ~alpha:0.5;
  let one_by_one = Framebuffer.create ~width:10 ~height:10 in
  Fill.polygon one_by_one (square 0. 0.) ~rgb:blue ~alpha:0.5;
  Fill.polygon one_by_one (square 3. 3.) ~rgb:blue ~alpha:0.5;
  Alcotest.(check int) "together: overlap painted once" 0x8080ff (Framebuffer.get_rgb together ~x:4 ~y:4);
  Alcotest.(check int) "one by one: painted twice" 0x4040ff (Framebuffer.get_rgb one_by_one ~x:4 ~y:4)

(* A thick "V": its two segments and three disks overlap at the joint,
 * and still no pixel is painted twice *)
let test_stroke_once () =
  let fb = Framebuffer.create ~width:40 ~height:40 in
  Stroke.polylines fb [ [ (5., 5.); (20., 35.); (35., 5.) ] ] ~width:6. ~rgb:blue ~alpha:0.5;
  for y = 0 to 39 do
    for x = 0 to 39 do
      let c = Framebuffer.get_rgb fb ~x ~y in
      if c <> 0xffffff && c <> 0x8080ff then Alcotest.failf "pixel (%d, %d) is 0x%06x" x y c
    done
  done;
  Alcotest.(check int) "the joint is drawn" 0x8080ff (Framebuffer.get_rgb fb ~x:20 ~y:33)

let tests =
  Testo.categorize "Text"
    [
      t "decode the A of Hershey.mli" test_decode_a;
      t "the font's A" test_font_a;
      t "layout" test_layout;
      t "Fill.polygons: union" test_polygons_union;
      t "Stroke: each pixel once" test_stroke_once;
    ]
