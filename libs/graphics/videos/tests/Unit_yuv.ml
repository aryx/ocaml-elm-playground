(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Yuv: the worked examples of Yuv.mli, and what 4:2:0 costs, by PSNR *)

let t = Testo.create
let triple = Alcotest.(triple int int int)

(* a picture of [w] x [h] whose pixel (x, y) is [f x y] *)
let picture (w : int) (h : int) (f : int -> int -> int * int * int) : Rgba_image.t =
  let img = Rgba_image.create ~width:w ~height:h in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let r, g, b = f x y and o = 4 * ((y * w) + x) in
      img.rgba.{o} <- r;
      img.rgba.{o + 1} <- g;
      img.rgba.{o + 2} <- b;
      img.rgba.{o + 3} <- 255
    done
  done;
  img

let test_worked () =
  Alcotest.check triple "red, full" (76, 85, 255) (Yuv.of_rgb Full (255, 0, 0));
  Alcotest.check triple "red, studio" (81, 90, 240) (Yuv.of_rgb Studio (255, 0, 0));
  Alcotest.check triple "white, full" (255, 128, 128) (Yuv.of_rgb Full (255, 255, 255));
  Alcotest.check triple "white, studio" (235, 128, 128) (Yuv.of_rgb Studio (255, 255, 255));
  Alcotest.check triple "black, studio: 16, not 0" (16, 128, 128) (Yuv.of_rgb Studio (0, 0, 0));
  (* and back: a gray exactly, a color within the rounding *)
  Alcotest.check triple "gray and back" (100, 100, 100) (Yuv.to_rgb Full (Yuv.of_rgb Full (100, 100, 100)));
  let r, g, b = Yuv.to_rgb Studio (Yuv.of_rgb Studio (255, 0, 0)) in
  if abs (r - 255) > 2 || g > 2 || b > 2 then Alcotest.failf "red and back: (%d, %d, %d)" r g b;
  (* a video's black shown as if full range: not black, the classic bug *)
  Alcotest.check triple "studio black read as full" (16, 16, 16) (Yuv.to_rgb Full (Yuv.of_rgb Studio (0, 0, 0)))

let test_checker () =
  (* red and blue in a 2 x 2 checker: four brightnesses, one purple *)
  let red = (255, 0, 0) and blue = (0, 0, 255) in
  let p = Yuv.of_image Full C420 (picture 2 2 (fun x y -> if (x + y) mod 2 = 0 then red else blue)) in
  Alcotest.(check (list int)) "Y kept" [ 76; 29; 29; 76 ] (List.init 4 (fun i -> Char.code (Bytes.get p.y i)));
  Alcotest.(check (pair int int)) "one Cb, one Cr" (170, 181) (Char.code (Bytes.get p.cb 0), Char.code (Bytes.get p.cr 0));
  (* the sizes: 1.5 bytes a pixel; an odd size rounds up *)
  Alcotest.(check (pair int int)) "320 x 240's color" (160, 120) (Yuv.chroma_size C420 ~width:320 ~height:240);
  Alcotest.(check (pair int int)) "an odd size" (3, 2) (Yuv.chroma_size C420 ~width:5 ~height:3)

let test_cost () =
  (* a smooth picture loses little to 4:2:0; sharp color edges lose much
   * (in color, the brightness kept) *)
  let smooth = picture 32 32 (fun x y -> (x * 8, y * 8, 128)) in
  let sharp = picture 32 32 (fun x y -> if (x + y) mod 2 = 0 then (255, 0, 0) else (0, 0, 255)) in
  let through range chroma img = Yuv.to_image range (Yuv.of_image range chroma img) in
  let db range chroma img = Psnr.psnr img (through range chroma img) in
  if db Full C444 smooth < 45. then Alcotest.failf "4:4:4, full: %.1f dB" (db Full C444 smooth);
  if db Studio C420 smooth < 35. then Alcotest.failf "a smooth picture through 4:2:0: %.1f dB" (db Studio C420 smooth);
  if db Studio C420 sharp > 15. then Alcotest.failf "a red and blue checker through 4:2:0: %.1f dB" (db Studio C420 sharp)

let tests =
  Testo.categorize "Yuv"
    [ t "red, white, black: Yuv.mli's numbers" test_worked; t "4:2:0: a red and blue checker becomes purple" test_checker; t "what 4:2:0 costs, measured" test_cost ]
