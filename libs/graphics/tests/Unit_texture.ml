(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Texture *)

let t = Testo.create

(* Texture.mli's example: a black texel, then a white one *)
let image : Texture.image =
  let rgba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout 8 in
  List.iteri (fun i b -> rgba.{i} <- b) [ 0; 0; 0; 255; 255; 255; 255; 255 ];
  { width = 2; height = 1; rgba }

let rgb = Alcotest.(triple int int int)

let test_border () =
  Alcotest.check rgb "nearest at the border: the texel containing it" (255, 255, 255)
    (Texture.sample_nearest image ~u:0.5 ~v:0.5);
  Alcotest.check rgb "bilinear at the border: half way" (128, 128, 128) (Texture.sample_bilinear image ~u:0.5 ~v:0.5)

let test_centers () =
  Alcotest.check rgb "bilinear at a texel's center: that texel" (0, 0, 0) (Texture.sample_bilinear image ~u:0.25 ~v:0.5);
  Alcotest.check rgb "past the edge: the border texel" (255, 255, 255) (Texture.sample_bilinear image ~u:1.5 ~v:0.5);
  Alcotest.check rgb "nearest, clamped" (0, 0, 0) (Texture.sample_nearest image ~u:(-1.) ~v:0.5)

let tests =
  Testo.categorize "Texture" [ t "nearest vs bilinear, the worked example" test_border; t "centers and edges" test_centers ]
