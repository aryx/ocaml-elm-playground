(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Adam: the first step's length, and the valley plain descent crawls
 * along *)

let t = Testo.create

(* the .mli's example: a slope of 300 and one of -0.002 both move
   their weight by the rate, each its own way *)
let test_first_step () =
  let a = Adam.make ~rate:0.1 2 in
  let (a, w) = Adam.step a [| 1.; 1. |] [| 300.; -0.002 |] in
  Alcotest.(check int) "one step taken" 1 (Adam.steps a);
  Alcotest.(check (float 1e-4)) "the steep one, down by the rate" 0.9 w.(0);
  Alcotest.(check (float 1e-4)) "the flat one, up by as much" 1.1 w.(1)

(* Rosenbrock's valley and its slopes, by hand:
   f = (1 - x)^2 + 100 (y - x^2)^2 *)
let valley (w : float array) : float =
  let (x, y) = (w.(0), w.(1)) in
  ((1. -. x) ** 2.) +. (100. *. ((y -. (x *. x)) ** 2.))

let slopes (w : float array) : float array =
  let (x, y) = (w.(0), w.(1)) in
  [| (-2. *. (1. -. x)) -. (400. *. x *. (y -. (x *. x))); 200. *. (y -. (x *. x)) |]

let start = [| -1.2; 1. |]
let steps = 2000

let plain (rate : float) : float =
  let w = ref start in
  for _ = 1 to steps do
    let g = slopes !w in
    w := Array.mapi (fun i x -> x -. (rate *. g.(i))) !w
  done;
  valley !w

let adam (rate : float) : float =
  let a = ref (Adam.make ~rate 2) and w = ref start in
  for _ = 1 to steps do
    let (a', w') = Adam.step !a !w (slopes !w) in
    a := a';
    w := w'
  done;
  valley !w

(* the numbers in Adam.mli come from here *)
let test_valley () =
  let by_plain = plain 0.001 and by_adam = adam 0.02 in
  Printf.eprintf "Rosenbrock from (-1.2, 1), %d steps: plain descent ends at %.2g, Adam at %.2g\n" steps by_plain by_adam;
  Alcotest.(check bool) "both went down" true (by_plain < valley start && by_adam < valley start);
  Alcotest.(check bool) "Adam fifty times nearer the bottom" true (by_adam < by_plain /. 50.);
  (* and why plain descent cannot simply take Adam's rate: it leaves
     the valley altogether *)
  let wild = plain 0.02 in
  Alcotest.(check bool) "plain descent at Adam's rate diverges" true (Float.is_nan wild || wild > valley start)

let tests = [ t "Adam, the first step is the rate" test_first_step; t "Adam, along Rosenbrock's valley" test_valley ]
