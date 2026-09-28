(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/core/Pixelate *)

let t = Testo.create

let rows (fb : Framebuffer.t) : int list list =
  List.init fb.height (fun y -> List.init fb.width (fun x -> Framebuffer.get_rgb fb ~x ~y))

(* the .mli's example: at factor 2, [a b] makes [a a b b; a a b b]; the
 * same with the simple version *)
let test_nearest () =
  let small = Framebuffer.create ~width:2 ~height:1 in
  Framebuffer.plot small ~x:0 ~y:0 ~rgb:0xAA ~alpha:1.;
  Framebuffer.plot small ~x:1 ~y:0 ~rgb:0xBB ~alpha:1.;
  let expected = [ [ 0xAA; 0xAA; 0xBB; 0xBB ]; [ 0xAA; 0xAA; 0xBB; 0xBB ] ] in
  let big = Framebuffer.create ~width:4 ~height:2 in
  Pixelate.nearest ~factor:2 small big;
  Alcotest.(check (list (list int))) "blocks" expected (rows big);
  let big = Framebuffer.create ~width:4 ~height:2 in
  Pixelate.nearest_simple ~factor:2 small big;
  Alcotest.(check (list (list int))) "the simple version" expected (rows big)

(* 334 for 1000 at factor 3; the last blocks cut: a 5 x 1 window from a
 * 2 x 1 small framebuffer at factor 3 *)
let test_small_size () =
  Alcotest.(check (list int)) "rounded up" [ 334; 500; 1000 ]
    [ Pixelate.small_size ~factor:3 1000; Pixelate.small_size ~factor:2 1000; Pixelate.small_size ~factor:1 1000 ];
  let small = Framebuffer.create ~width:2 ~height:1 in
  Framebuffer.plot small ~x:1 ~y:0 ~rgb:0xBB ~alpha:1.;
  let big = Framebuffer.create ~width:5 ~height:1 in
  Pixelate.nearest ~factor:3 small big;
  Alcotest.(check (list (list int))) "cut" [ [ 0xFFFFFF; 0xFFFFFF; 0xFFFFFF; 0xBB; 0xBB ] ] (rows big)

let tests = Testo.categorize "Pixelate" [ t "nearest" test_nearest; t "small_size" test_small_size ]
