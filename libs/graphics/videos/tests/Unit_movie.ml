(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Movie: the frame showing at a time, and the decoder that only goes
 * forward, counted *)

let t = Testo.create

(* a 1 x 1 picture whose red is [v]: the frame's number, readable back *)
let pixel (v : int) : Rgba_image.t =
  let img = Rgba_image.create ~width:1 ~height:1 in
  img.rgba.{0} <- v;
  img

let red (img : Rgba_image.t) : int = img.rgba.{0}

let test_times () =
  (* a GIF's delays: 0.1, 0.5, 0.2 -- the frames start at 0, 0.1, 0.6 *)
  let m = Movie.of_frames [ (pixel 0, 0.1); (pixel 1, 0.5); (pixel 2, 0.2) ] in
  Alcotest.(check (array (float 1e-9))) "the starts" [| 0.; 0.1; 0.6 |] m.times;
  Alcotest.(check (float 1e-9)) "the duration" 0.8 m.duration;
  List.iter
    (fun (at, i) -> Alcotest.(check int) (Printf.sprintf "at %g s" at) i (red (Movie.frame_at m at)))
    [ (-1., 0); (0., 0); (0.09, 0); (0.1, 1); (0.59, 1); (0.6, 2); (0.79, 2); (5., 2) ]

let test_sequential () =
  (* frame i is the state's count: a delta decoder in miniature *)
  let steps = ref 0 in
  let next s =
    incr steps;
    (s + 1, pixel s)
  in
  let m = Movie.sequential ~width:1 ~height:1 ~times:(Array.init 10 (fun i -> float_of_int i /. 25.)) ~duration:0.4 ~start:(fun () -> 0) ~next in
  let ask i expected_steps =
    steps := 0;
    Alcotest.(check int) (Printf.sprintf "frame %d" i) i (red (m.frame i));
    Alcotest.(check int) (Printf.sprintf "frame %d's cost" i) expected_steps !steps
  in
  ask 0 1;
  ask 1 1;
  ask 3 2 (* 2, then 3 *);
  ask 3 0 (* the same again: nothing *);
  ask 2 0 (* the one before: kept *);
  ask 1 2 (* further back: from the start, 0, 1 *);
  ask 9 8;
  Alcotest.(check int) "at 0.2 s, 25 a second" 5 (Movie.index_at m 0.2)

let tests =
  Testo.categorize "Movie"
    [ t "the frame showing at a time" test_times; t "decoded forward only, the frame before kept, a seek further back from the start" test_sequential ]
