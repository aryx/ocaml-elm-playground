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
  Blit.draw fb two_by_one (Affine.scale 2. 2.) ~filter:Blit.Nearest ~alpha:1.;
  Alcotest.(check (list string)) "2x" [ "RRBB.."; "RRBB.."; "......" ] (picture fb)

let test_rotate () =
  let fb = Framebuffer.create ~width:3 ~height:3 in
  (* a quarter turn (y down: clockwise on screen), moved back into view:
   * the red pixel on top, the blue one below *)
  let m = Affine.compose (Affine.translate 1. 0.) (Affine.rotate (Float.pi /. 2.)) in
  Blit.draw fb two_by_one m ~filter:Blit.Nearest ~alpha:1.;
  Alcotest.(check (list string)) "quarter turn" [ "R.."; "B.."; "..." ] (picture fb)

let test_transparent () =
  let fb = Framebuffer.create ~width:2 ~height:1 in
  Framebuffer.clear fb ~rgb:blue;
  Blit.draw fb (image [ [ (red, 255); (red, 0) ] ]) Affine.identity ~filter:Blit.Nearest ~alpha:1.;
  Alcotest.(check (list string)) "alpha 0 shows what's below" [ "RB" ] (picture fb)

(* the example in Blit.mli, on one row: 30% of the way from A's center
 * to B's; and at a pixel's center, exactly that pixel *)
let test_bilinear () =
  let img = image [ [ (0x000000, 255); (0x0000ff, 255) ] ] in
  let c = Blit.sample_bilinear img (0.5 +. 0.3, 0.5) in
  Alcotest.(check int) "70% black + 30% blue" (int_of_float ((0.3 *. 255.) +. 0.5)) c.rgb;
  Alcotest.(check int) "at A's center: A" 0x000000 (Blit.sample_bilinear img (0.5, 0.5)).rgb;
  Alcotest.(check int) "at B's center: B" 0x0000ff (Blit.sample_bilinear img (1.5, 0.5)).rgb

(* The optimized Blit.draw (forward differencing, samplers inlined)
 * paints what the simple one does, see Opti *)
let test_opti_same_pixels () =
  (* 5x4 pixels of varied colors and opacities *)
  let img =
    image
      (List.init 4 (fun j ->
           List.init 5 (fun i -> (((i * 50) lsl 16) lor ((j * 60) lsl 8) lor ((i + j) * 20), 255 - (i * 40)))))
  in
  let transforms =
    [
      ("enlarged", Affine.compose (Affine.translate 3.3 2.7) (Affine.scale 7. 9.));
      ( "rotated",
        Affine.compose (Affine.translate 30.2 5.1)
          (Affine.compose (Affine.rotate 0.7) (Affine.scale 6. 6.)) );
    ]
  in
  let draw ~optimized ~filter m =
    let fb = Framebuffer.create ~width:50 ~height:50 in
    Opti.enabled := optimized;
    Fun.protect ~finally:(fun () -> Opti.enabled := true) (fun () -> Blit.draw fb img m ~filter ~alpha:0.9);
    fb
  in
  transforms
  |> List.iter (fun (name, m) ->
         [ ("nearest", Blit.Nearest); ("bilinear", Blit.Bilinear) ]
         |> List.iter (fun (fname, filter) ->
                let simple = draw ~optimized:false ~filter m and fast = draw ~optimized:true ~filter m in
                for y = 0 to 49 do
                  for x = 0 to 49 do
                    let s = Framebuffer.get_rgb simple ~x ~y and f = Framebuffer.get_rgb fast ~x ~y in
                    (* the simple bilinear rounds to integers after each
                     * of its 3 mixes, the optimized one only at the
                     * end: allow a difference of 2 per channel *)
                    let channel k c = (c lsr k) land 0xff in
                    if List.exists (fun k -> abs (channel k s - channel k f) > 2) [ 0; 8; 16 ] then
                      Alcotest.failf "%s, %s, pixel (%d, %d): simple 0x%06x, optimized 0x%06x" name fname x y
                        s f
                  done
                done))

let tests =
  Testo.categorize "Blit"
    [
      t "optimized = simple" test_opti_same_pixels;
      t "enlarge: no holes" test_enlarge;
      t "rotate" test_rotate;
      t "transparent pixels" test_transparent;
      t "bilinear" test_bilinear;
    ]
