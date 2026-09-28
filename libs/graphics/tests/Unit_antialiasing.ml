(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Fill.polygons_aa and Line.wu *)

let t = Testo.create

(* Drawing black on white with antialiasing, a pixel's darkness is its
 * coverage: 0x000000 fully covered, 0xffffff not at all *)
let coverage (fb : Framebuffer.t) ~x ~y = 1. -. (float (Framebuffer.get_rgb fb ~x ~y land 0xff) /. 255.)

let close = Alcotest.float 0.01

(* the example in Fill.mli: 4 sub-rows (centers at y = 0.125, 0.375,
 * 0.625, 0.875) whose spans are [0.5, 2.5), [0.6, 2.6), [0.8, 2.8),
 * [1.0, 3.0). A polygon with exactly those spans: its left side goes
 * through (0.5, 0.125), (0.6, 0.375), ... and its right side is the
 * same, 2 pixels to the right *)
let test_coverage_example () =
  let fb = Framebuffer.create ~width:3 ~height:1 in
  let left = [ (0.5, 0.125); (0.6, 0.375); (0.8, 0.625); (1.0, 0.875) ] in
  let right = List.rev_map (fun (x, y) -> (x +. 2., y)) left in
  let polygon = [ (0.5, 0.0) ] @ left @ [ (1.0, 1.0); (3.0, 1.0) ] @ right @ [ (2.5, 0.0) ] in
  Fill.polygons_aa fb [ polygon ] ~rgb:0 ~alpha:1.;
  Alcotest.check close "pixel 0" 0.275 (coverage fb ~x:0 ~y:0);
  Alcotest.check close "pixel 1" 1.0 (coverage fb ~x:1 ~y:0);
  Alcotest.check close "pixel 2" 0.725 (coverage fb ~x:2 ~y:0)

(* with edges on pixel boundaries, antialiasing changes nothing *)
let test_aligned () =
  let square = [ (1., 1.); (4., 1.); (4., 3.); (1., 3.) ] in
  let aa = Framebuffer.create ~width:6 ~height:4 in
  Fill.polygons_aa aa [ square ] ~rgb:0x0000ff ~alpha:1.;
  let aliased = Framebuffer.create ~width:6 ~height:4 in
  Fill.polygon aliased square ~rgb:0x0000ff ~alpha:1.;
  for y = 0 to 3 do
    for x = 0 to 5 do
      Alcotest.(check int) (Printf.sprintf "(%d, %d)" x y)
        (Framebuffer.get_rgb aliased ~x ~y) (Framebuffer.get_rgb aa ~x ~y)
    done
  done

(* the total coverage is (about) the area: a triangle of area 50 *)
let test_area () =
  let fb = Framebuffer.create ~width:20 ~height:20 in
  Fill.polygons_aa fb [ [ (2.3, 1.7); (12.3, 1.7); (2.3, 11.7) ] ] ~rgb:0 ~alpha:1.;
  let total = ref 0. in
  for y = 0 to 19 do
    for x = 0 to 19 do
      total := !total +. coverage fb ~x ~y
    done
  done;
  Alcotest.check (Alcotest.float 1.) "area 50" 50. !total

(* the example in Line.mli *)
let test_wu () =
  let fb = Framebuffer.create ~width:5 ~height:4 in
  Line.wu fb (0., 1.) (4., 3.) ~rgb:0 ~alpha:1.;
  let row y = List.init 5 (fun x -> Float.round (coverage fb ~x ~y *. 10.) /. 10.) in
  Alcotest.(check (list (float 0.051))) "y = 1" [ 1.0; 0.5; 0.; 0.; 0. ] (row 1);
  Alcotest.(check (list (float 0.051))) "y = 2" [ 0.; 0.5; 1.0; 0.5; 0. ] (row 2);
  Alcotest.(check (list (float 0.051))) "y = 3" [ 0.; 0.; 0.; 0.5; 1.0 ] (row 3)

(* The optimized version (sparse cells) paints what the simple one
 * (coverage array) does, see Opti *)
let test_opti_same_pixels () =
  let shapes =
    [
      [ (50.3, 10.2); (89.7, 49.6); (50.1, 90.4); (10.9, 50.8) ];
      [ (10., 40.); (60.3, 40.); (60.3, 15.7); (95.1, 50.); (60.3, 84.2); (60.3, 60.); (10., 60.) ];
      List.init 5 (fun i ->
          let a = (Float.pi /. 2.) +. (4. *. Float.pi *. float i /. 5.) in
          (50.2 +. (40. *. cos a), 49.7 -. (40. *. sin a)));
      (* bigger than the framebuffer: clipping *)
      [ (-20.5, -10.2); (130.3, 5.1); (60., 140.7) ];
    ]
  in
  let draw ~optimized shape =
    let fb = Framebuffer.create ~width:100 ~height:100 in
    Opti.enabled := optimized;
    Fun.protect ~finally:(fun () -> Opti.enabled := true) (fun () ->
        Fill.polygons_aa fb [ shape ] ~rgb:0x0000ff ~alpha:0.8);
    fb
  in
  shapes
  |> List.iteri (fun i shape ->
         let simple = draw ~optimized:false shape and optimized = draw ~optimized:true shape in
         for y = 0 to 99 do
           for x = 0 to 99 do
             let s = Framebuffer.get_rgb simple ~x ~y and o = Framebuffer.get_rgb optimized ~x ~y in
             (* the two sum the same coverages in a different order:
              * allow a rounding difference of 1 per channel *)
             let channel k c = (c lsr k) land 0xff in
             if List.exists (fun k -> abs (channel k s - channel k o) > 1) [ 0; 8; 16 ] then
               Alcotest.failf "shape %d, pixel (%d, %d): simple 0x%06x, optimized 0x%06x" i x y s o
           done
         done)

let tests =
  Testo.categorize "Antialiasing"
    [
      t "optimized = simple" test_opti_same_pixels;
      t "the coverage example of Fill.mli" test_coverage_example;
      t "pixel-aligned: same as without" test_aligned;
      t "total coverage = area" test_area;
      t "the Wu line of Line.mli" test_wu;
    ]
