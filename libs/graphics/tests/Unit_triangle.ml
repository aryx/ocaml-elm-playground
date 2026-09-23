(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Triangle *)

let t = Testo.create

(* a vertex at pixel (x, y) and depth z *)
let vertex (x, y) z : Project.vertex =
  { vx = x; vy = y; z; u = 0.; v = 0.; inv_z = 1. /. z; u_over_z = 0.; v_over_z = 0.; normal = (0., 0., 1.) }

let fill fb ?zbuffer ~rgb z =
  Triangle.fill fb ~fill_rule:Triangle.Epsilon ~zbuffer ~interpolation:Interpolate.Perspective_correct ~shading:Shading.Flat_color
    ~color:(fun ~u:_ ~v:_ ~brightness:_ -> rgb)
    (vertex (0., 0.) z) (vertex (4., 0.) z) (vertex (0., 4.) z)

(* the framebuffer as rows of '#' (rgb) and '.' (anything else) *)
let picture (fb : Framebuffer.t) rgb =
  List.init fb.height (fun y ->
      String.init fb.width (fun x -> if Framebuffer.get_rgb fb ~x ~y = rgb then '#' else '.'))

(* Triangle.mli's example: (0, 0), (4, 0), (0, 4) covers the 10 pixels
 * with x + y <= 3 *)
let test_coverage () =
  let fb = Framebuffer.create ~width:4 ~height:4 in
  fill fb ~rgb:0xFF0000 1.;
  Alcotest.(check (list string)) "the covered pixels" [ "####"; "###."; "##.."; "#..." ] (picture fb 0xFF0000)

(* with a z-buffer, a farther triangle drawn after doesn't cover a
 * nearer one; without (the painter's algorithm), it does *)
let test_zbuffer () =
  let fb = Framebuffer.create ~width:4 ~height:4 in
  let zbuffer = Zbuffer.create ~width:4 ~height:4 in
  fill fb ~zbuffer ~rgb:0xFF0000 1.;
  fill fb ~zbuffer ~rgb:0x0000FF 2.;
  Alcotest.(check int) "z-buffer: still the nearer, red" 0xFF0000 (Framebuffer.get_rgb fb ~x:1 ~y:1);
  fill fb ~rgb:0x0000FF 2.;
  Alcotest.(check int) "no z-buffer: the last drawn, blue" 0x0000FF (Framebuffer.get_rgb fb ~x:1 ~y:1)

(* how many times each pixel is drawn by [triangles], each drawn alone
 * into its own framebuffer *)
let coverage ~fill_rule ~size (triangles : ((float * float) * (float * float) * (float * float)) list) =
  let counts = Array.make_matrix size size 0 in
  triangles
  |> List.iter (fun (a, b, c) ->
         let fb = Framebuffer.create ~width:size ~height:size in
         Triangle.fill fb ~fill_rule ~zbuffer:None ~interpolation:Interpolate.Linear ~shading:Shading.Flat_color
           ~color:(fun ~u:_ ~v:_ ~brightness:_ -> 0xFF0000)
           (vertex a 1.) (vertex b 1.) (vertex c 1.);
         for y = 0 to size - 1 do
           for x = 0 to size - 1 do
             if Framebuffer.get_rgb fb ~x ~y = 0xFF0000 then counts.(y).(x) <- counts.(y).(x) + 1
           done
         done);
  counts

let count_where p counts = Array.fold_left (fun n row -> Array.fold_left (fun n c -> if p c then n + 1 else n) n row) 0 counts

(* Triangle.mli's rectangle: 2 triangles sharing a diagonal through
 * pixel centers *)
let test_shared_diagonal () =
  let square = [ ((0., 0.), (8., 0.), (8., 8.)); ((0., 0.), (8., 8.), (0., 8.)) ] in
  let epsilon = coverage ~fill_rule:Triangle.Epsilon ~size:8 square in
  Alcotest.(check int) "epsilon: the 8 diagonal pixels drawn twice" 8 (count_where (( = ) 2) epsilon);
  let top_left = coverage ~fill_rule:Triangle.Top_left ~size:8 square in
  Alcotest.(check int) "top-left: none drawn twice" 0 (count_where (fun c -> c > 1) top_left);
  Alcotest.(check int) "top-left: all 64 drawn once" 64 (count_where (( = ) 1) top_left)

(* a fan of 6 triangles with fractional vertices, both windings: with
 * the top-left rule, no pixel twice and no hole inside *)
let test_fan () =
  let center = (10.3, 9.7) in
  let rim = [ (3.2, 4.1); (11.9, 1.3); (18.6, 6.2); (17.1, 16.8); (8.4, 18.9); (2.7, 12.5) ] in
  let rec pairs = function a :: (b :: _ as rest) -> (a, b) :: pairs rest | _ -> [] in
  let triangles =
    pairs (rim @ [ List.hd rim ])
    |> List.mapi (fun i (a, b) -> if i mod 2 = 0 then (center, a, b) else (center, b, a))
  in
  let counts = coverage ~fill_rule:Triangle.Top_left ~size:20 triangles in
  Alcotest.(check int) "no pixel drawn twice" 0 (count_where (fun c -> c > 1) counts);
  (* the pixels whose center is well inside the hexagon: all drawn *)
  let inside (px, py) =
    pairs (rim @ [ List.hd rim ])
    |> List.for_all (fun ((ax, ay), (bx, by)) -> ((bx -. ax) *. (py -. ay)) -. ((by -. ay) *. (px -. ax)) > 0.5)
  in
  let holes = ref 0 and checked = ref 0 in
  for y = 0 to 19 do
    for x = 0 to 19 do
      if inside (float x +. 0.5, float y +. 0.5) then begin
        incr checked;
        if counts.(y).(x) = 0 then incr holes
      end
    done
  done;
  Alcotest.(check bool) "(some pixels checked)" true (!checked > 100);
  Alcotest.(check int) "no hole" 0 !holes

let tests =
  Testo.categorize "Triangle"
    [
      t "the pixels of the worked example" test_coverage;
      t "with and without a z-buffer" test_zbuffer;
      t "a shared edge: epsilon vs the top-left rule" test_shared_diagonal;
      t "the top-left rule on a fan" test_fan;
    ]
