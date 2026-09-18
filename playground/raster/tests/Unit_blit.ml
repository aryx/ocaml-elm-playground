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

(* An image from a list of rows of (0xRRGGBB, alpha byte) *)
let image (rows : (int * int) list list) : Blit.image =
  let height = List.length rows and width = List.length (List.hd rows) in
  let rgba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (width * height * 4) in
  List.iteri
    (fun j row ->
      List.iteri
        (fun i (rgb, a) ->
          let o = ((j * width) + i) * 4 in
          rgba.{o} <- (rgb lsr 16) land 0xFF;
          rgba.{o + 1} <- (rgb lsr 8) land 0xFF;
          rgba.{o + 2} <- rgb land 0xFF;
          rgba.{o + 3} <- a)
        row)
    rows;
  { width; height; rgba }

let red = 0xff0000
let blue = 0x0000ff

(* 'R' red, 'B' blue, '.' white, '?' anything else *)
let picture (fb : Framebuffer.t) : string list =
  List.init fb.height (fun y ->
      String.init fb.width (fun x ->
          match Framebuffer.get_rgb fb ~x ~y with
          | 0xff0000 -> 'R'
          | 0x0000ff -> 'B'
          | 0xffffff -> '.'
          | _ -> '?'))

let two_by_one = image [ [ (red, 255); (blue, 255) ] ]

(* the "x2" picture in Blit.mli: inverse mapping, no holes *)
let test_enlarge () =
  let fb = Framebuffer.create ~width:6 ~height:3 in
  Blit.draw fb two_by_one (Affine.scale 2. 2.) ~sample:Blit.sample_nearest ~alpha:1.;
  Alcotest.(check (list string)) "2x" [ "RRBB.."; "RRBB.."; "......" ] (picture fb)

let test_rotate () =
  let fb = Framebuffer.create ~width:3 ~height:3 in
  (* a quarter turn (y down: clockwise on screen), moved back into view:
   * the red pixel on top, the blue one below *)
  let m = Affine.compose (Affine.translate 1. 0.) (Affine.rotate (Float.pi /. 2.)) in
  Blit.draw fb two_by_one m ~sample:Blit.sample_nearest ~alpha:1.;
  Alcotest.(check (list string)) "quarter turn" [ "R.."; "B.."; "..." ] (picture fb)

let test_transparent () =
  let fb = Framebuffer.create ~width:2 ~height:1 in
  Framebuffer.clear fb ~rgb:blue;
  Blit.draw fb (image [ [ (red, 255); (red, 0) ] ]) Affine.identity ~sample:Blit.sample_nearest ~alpha:1.;
  Alcotest.(check (list string)) "alpha 0 shows what's below" [ "RB" ] (picture fb)

(* the example in Blit.mli, on one row: 30% of the way from A's center
 * to B's; and at a pixel's center, exactly that pixel *)
let test_bilinear () =
  let img = image [ [ (0x000000, 255); (0x0000ff, 255) ] ] in
  let c = Blit.sample_bilinear img (0.5 +. 0.3, 0.5) in
  Alcotest.(check int) "70% black + 30% blue" (int_of_float ((0.3 *. 255.) +. 0.5)) c.rgb;
  Alcotest.(check int) "at A's center: A" 0x000000 (Blit.sample_bilinear img (0.5, 0.5)).rgb;
  Alcotest.(check int) "at B's center: B" 0x0000ff (Blit.sample_bilinear img (1.5, 0.5)).rgb

let tests =
  Testo.categorize "Blit"
    [
      t "enlarge: no holes" test_enlarge;
      t "rotate" test_rotate;
      t "transparent pixels" test_transparent;
      t "bilinear" test_bilinear;
    ]
