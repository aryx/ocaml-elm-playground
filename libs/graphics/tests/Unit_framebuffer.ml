(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let t = Testo.create

(* colors are easier to read in hex in error messages *)
let rgb = Alcotest.testable (fun fmt c -> Format.fprintf fmt "0x%06x" c) ( = )

(* the example in Framebuffer.mli *)
let test_blend () =
  Alcotest.check rgb "half red over white is pink" 0xff8080
    (Framebuffer.blend ~src:0xff0000 ~dst:0xffffff ~alpha:0.5);
  Alcotest.check rgb "opaque hides dst" 0x123456
    (Framebuffer.blend ~src:0x123456 ~dst:0xffffff ~alpha:1.);
  Alcotest.check rgb "invisible keeps dst" 0xffffff
    (Framebuffer.blend ~src:0x123456 ~dst:0xffffff ~alpha:0.)

(* the clipping example in Framebuffer.fill_span *)
let test_fill_span_clipping () =
  let fb = Framebuffer.create ~width:10 ~height:2 in
  Framebuffer.fill_span fb ~y:0 ~x0:(-20) ~x1:3 ~rgb:0x0000ff ~alpha:1.;
  Framebuffer.fill_span fb ~y:1 ~x0:8 ~x1:1200 ~rgb:0x0000ff ~alpha:1.;
  (* off-screen rows are skipped without an error *)
  Framebuffer.fill_span fb ~y:5 ~x0:0 ~x1:10 ~rgb:0x0000ff ~alpha:1.;
  let row y = List.init 10 (fun x -> Framebuffer.get_rgb fb ~x ~y = 0x0000ff) in
  Alcotest.(check (list bool)) "row 0: [-20, 3) -> [0, 3)"
    [ true; true; true; false; false; false; false; false; false; false ] (row 0);
  Alcotest.(check (list bool)) "row 1: [8, 1200) -> [8, 10)"
    [ false; false; false; false; false; false; false; false; true; true ] (row 1)

let test_fill_span_blends () =
  let fb = Framebuffer.create ~width:4 ~height:1 in
  Framebuffer.fill_span fb ~y:0 ~x0:0 ~x1:4 ~rgb:0xff0000 ~alpha:0.5;
  Alcotest.check rgb "white becomes pink" 0xff8080 (Framebuffer.get_rgb fb ~x:2 ~y:0)

let tests =
  Testo.categorize "Framebuffer"
    [
      t "blend" test_blend;
      t "fill_span clipping" test_fill_span_clipping;
      t "fill_span blends" test_fill_span_blends;
    ]
