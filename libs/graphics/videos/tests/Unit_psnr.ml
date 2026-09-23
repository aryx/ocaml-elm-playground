(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Psnr: Psnr.mli's worked example *)

let t = Testo.create

let gray (w : int) (v : int) : Rgba_image.t =
  let img = Rgba_image.create ~width:w ~height:w in
  Bigarray.Array1.fill img.rgba v;
  img

let test_worked () =
  let a = gray 8 100 in
  Alcotest.(check (float 1e-9)) "off by 2: MSE 4" 4. (Psnr.mse a (gray 8 102));
  Alcotest.(check (float 0.05)) "42.1 dB" 42.1 (Psnr.psnr a (gray 8 102));
  Alcotest.(check (float 0.05)) "off by 16: 24.0 dB" 24.05 (Psnr.psnr a (gray 8 84));
  Alcotest.(check (float 0.)) "the same: infinitely far from noise" infinity (Psnr.psnr a (gray 8 100))

let tests = Testo.categorize "Psnr" [ t "off by 2, off by 16" test_worked ]
