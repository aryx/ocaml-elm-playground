(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/core/Matting *)

let t = Testo.create

(* a 3x1 framebuffer: pixel 0 left empty, pixel 1 half-transparent
 * red, pixel 2 opaque blue *)
let draw fb =
  Framebuffer.plot fb ~x:1 ~y:0 ~rgb:0xFF0000 ~alpha:0.5;
  Framebuffer.plot fb ~x:2 ~y:0 ~rgb:0x0000FF ~alpha:1.

let pixel (rgba : Matting.rgba) (x : int) : int list = List.init 4 (fun i -> rgba.{(x * 4) + i})

let test_mli_example () =
  let rgba = Matting.premultiplied_rgba ~width:3 ~height:1 draw in
  Alcotest.(check (list int)) "nothing drawn: transparent" [ 0; 0; 0; 0 ] (pixel rgba 0);
  (* the .mli's example *)
  Alcotest.(check (list int)) "half-transparent red" [ 128; 0; 0; 127 ] (pixel rgba 1);
  Alcotest.(check (list int)) "opaque blue" [ 0; 0; 255; 255 ] (pixel rgba 2)

let tests = Testo.categorize "Matting" [ t "the .mli's example" test_mli_example ]
