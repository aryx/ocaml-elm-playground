(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_imaging.mli *)

let check_int = Alcotest.(check int)

(* an image from rows of grey values *)
let grey (rows : int list list) : Rgba_image.t =
  let h = List.length rows and w = List.length (List.hd rows) in
  let img = Rgba_image.create ~width:w ~height:h in
  List.iteri (fun y row -> List.iteri (fun x v -> List.iter (fun c -> Pixels.set img x y c v) [ 0; 1; 2 ]; Pixels.set img x y 3 255) row) rows;
  img

let red (img : Rgba_image.t) x y = Pixels.get img x y 0

let tests =
  Testo.categorize "Imaging"
    [
      Testo.create "Lut: levels, gamma, posterize, curves, composed" (fun () ->
          let l = Lut.levels ~black:50 ~white:200 ~gamma:1. in
          check_int "50" 0 l.(50);
          check_int "125, halfway" 128 l.(125);
          check_int "200" 255 l.(200);
          check_int "gamma 2 lifts the middle" 180 (Lut.levels ~black:50 ~white:200 ~gamma:2.).(125);
          Alcotest.(check (list int)) "posterize 4" [ 0; 85; 170; 255 ] (List.map (fun v -> (Lut.posterize 4).(v)) [ 10; 100; 150; 250 ]);
          let c = Lut.curves [ (64, 128) ] in
          check_int "through its point" 128 c.(64);
          Alcotest.(check bool) "monotone" true (Array.for_all (fun b -> b) (Array.init 255 (fun v -> c.(v) <= c.(v + 1))));
          Alcotest.(check (array int)) "inverted twice" Lut.identity (Lut.compose Lut.invert Lut.invert));
      Testo.create "Histogram: Histogram.mli's four pixels" (fun () ->
          let img = grey [ [ 0; 255 ]; [ 255; 0 ] ] in
          Pixels.set img 1 1 0 255;
          let h = Histogram.compute img in
          check_int "red at 0" 1 h.red.(0);
          check_int "red at 255" 3 h.red.(255);
          check_int "red's luminance" 1 h.luminance.(76);
          check_int "white" 2 h.luminance.(255));
      Testo.create "Hsl: red is hue 0, turned 120 degrees green" (fun () ->
          let h, s, l = Hsl.of_rgb 255 0 0 in
          Alcotest.(check (list (float 1e-6))) "hsl" [ 0.; 1.; 0.5 ] [ h; s; l ];
          Alcotest.(check (triple int int int)) "green" (0, 255, 0) (Hsl.to_rgb (h +. 120.) s l);
          let img = Hsl.hue_saturation ~hue:120. ~saturation:0. ~lightness:0. (grey [ [ 0 ] ]) in
          check_int "black stays black" 0 (red img 0 0));
      Testo.create "Convolve: Convolve.mli's blur and sharpen" (fun () ->
          let img = grey [ [ 10; 10; 10 ]; [ 10; 100; 10 ]; [ 10; 10; 10 ] ] in
          check_int "blurred" 20 (red (Convolve.apply Convolve.blur img) 1 1);
          check_int "sharpened, clamped" 255 (red (Convolve.apply Convolve.sharpen img) 1 1));
      Testo.create "Gaussian: the kernel of sigma 1" (fun () ->
          let k = Gaussian.kernel 1. in
          check_int "7 weights" 7 (Array.length k);
          Alcotest.(check (float 1e-9)) "sums to 1" 1. (Array.fold_left ( +. ) 0. k);
          Alcotest.(check (float 5e-4)) "the middle" 0.399 k.(3);
          Alcotest.(check (float 1e-12)) "symmetric" k.(1) k.(5));
      Testo.create "Sobel: an edge between black and white" (fun () ->
          let img = grey [ [ 0; 0; 255; 255 ]; [ 0; 0; 255; 255 ]; [ 0; 0; 255; 255 ] ] in
          let e = Sobel.find_edges img in
          check_int "on the edge" 0 (red e 1 1);
          Alcotest.(check (float 1e-9)) "gx = 4 x 255" 1020. (Sobel.magnitude img 2 1 0);
          let flat = Sobel.find_edges (grey [ [ 7; 7; 7 ]; [ 7; 7; 7 ] ]) in
          check_int "nothing" 255 (red flat 1 0));
      Testo.create "Median: a speck removed" (fun () ->
          check_int "the speck" 10 (red (Median.median ~radius:1 (grey [ [ 10; 10; 10 ]; [ 10; 255; 10 ]; [ 10; 10; 10 ] ])) 1 1));
      Testo.create "Add_noise: the same seed, the same grain, within the amount" (fun () ->
          let img = grey [ List.init 50 (fun _ -> 128) ] in
          let a = Add_noise.add ~amount:10 ~seed:7 img and b = Add_noise.add ~amount:10 ~seed:7 img in
          Alcotest.(check bool) "same" true (a.rgba = b.rgba);
          Alcotest.(check bool) "within" true (List.for_all (fun x -> abs (red a x 0 - 128) <= 10) (List.init 50 Fun.id));
          Alcotest.(check bool) "not all the same" true (List.exists (fun x -> red a x 0 <> 128) (List.init 50 Fun.id)));
      Testo.create "Scale: Scale.mli's bilinear ramp, a quarter turn" (fun () ->
          let img = Scale.resize Bilinear ~width:4 ~height:1 (grey [ [ 0; 200 ] ]) in
          Alcotest.(check (list int)) "ramp" [ 0; 50; 150; 200 ] (List.init 4 (fun x -> red img x 0));
          let r = Scale.rotate_90 (grey [ [ 1; 2; 3 ] ]) in
          Alcotest.(check (pair int int)) "turned" (1, 3) (r.width, r.height);
          Alcotest.(check (list int)) "top to bottom" [ 1; 2; 3 ] (List.init 3 (fun y -> red r 0 y)));
      Testo.create "Mask: rectangle, inverse, ellipse's edge, lasso, wand, ants" (fun () ->
          let count (m : Mask.t) = Bytes.fold_left (fun n c -> if c = '\255' then n + 1 else n) 0 m.alpha in
          let r = Mask.rectangle 4 4 (1, 1) (3, 3) in
          check_int "rectangle" 4 (count r);
          check_int "inverse" 12 (count (Mask.invert r));
          Alcotest.(check (option (list int))) "bounds" (Some [ 1; 1; 3; 3 ]) (Option.map (fun (a, b, c, d) -> [ a; b; c; d ]) (Mask.bounds r));
          let e = Mask.ellipse 10 10 (0, 0) (10, 10) in
          check_int "the centre, all in" 255 (Mask.get e 5 5);
          check_int "the corner, out" 0 (Mask.get e 0 0);
          Alcotest.(check bool) "an edge pixel, partly" true (let v = Mask.get e 1 1 in v > 0 && v < 255);
          check_int "a triangle's inside" 255 (Mask.get (Mask.polygon 10 10 [ (0., 0.); (10., 0.); (0., 10.) ]) 2 2);
          let img = grey [ [ 0; 0; 200; 0 ] ] in
          check_int "wand, contiguous" 2 (count (Mask.wand ~tolerance:10 img 0 0));
          check_int "wand, anywhere" 3 (count (Mask.wand ~contiguous:false ~tolerance:10 img 0 0));
          check_int "a pixel's ants: its four sides" 4 (List.length (Mask.edges (Mask.rectangle 3 3 (1, 1) (2, 2)))));
      Testo.create "Composite: a quarter of the way" (fun () ->
          let m = Mask.empty 1 1 in
          Bytes.set m.alpha 0 (Char.chr 64);
          check_int "125" 125 (red (Composite.through m ~before:(grey [ [ 100 ] ]) ~after:(grey [ [ 200 ] ])) 0 0));
      Testo.create "Brush: a hard dab, a stroke's spacing" (fun () ->
          let b = { Brush.radius = 3.; hardness = 1.; opacity = 1. } in
          let img = Brush.paint b (255, 0, 0) (grey (List.init 12 (fun _ -> List.init 12 (fun _ -> 255)))) [ (5.5, 5.5) ] in
          Alcotest.(check (list int)) "the centre, red" [ 255; 0; 0 ] (List.map (Pixels.get img 5 5) [ 0; 1; 2 ]);
          Alcotest.(check (list int)) "5 away, white" [ 255; 255; 255 ] (List.map (Pixels.get img 10 5) [ 0; 1; 2 ]);
          match Brush.spacing b (0., 5.) (20., 5.) with
          | (x0, _) :: (x1, _) :: _ -> Alcotest.(check (float 1e-9)) "1.5 apart" 1.5 (x1 -. x0)
          | _ -> Alcotest.fail "no dabs");
      Testo.create "Gradient: 55% of the way at x 5" (fun () ->
          let img = Gradient.linear (0., 0.) (10., 0.) (0, 0, 0) (255, 255, 255) (grey [ List.init 10 (fun _ -> 0) ]) in
          check_int "140" 140 (red img 5 0));
      Testo.create "the photographs, decoded by our own JPEG reader" (fun () ->
          List.iter
            (fun f ->
              let img = Jpeg.decode (In_channel.with_open_bin ("../../../apps/graphics/photos/" ^ f) In_channel.input_all) in
              Alcotest.(check (pair int int)) f (400, 400) (img.width, img.height))
            [ "blue_marble.jpg"; "aldrin.jpg" ]);
    ]
