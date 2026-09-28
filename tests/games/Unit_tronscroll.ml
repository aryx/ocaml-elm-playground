(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tronscroll.mli *)

open TinyTronscroll

let flags = [ ("seed", "1") ]
let idle : Playground.keyboard = Playground.initial_computer.keyboard

(* player [id] pressing [keys] this tick (and holding them) *)
let player (id : int) (keys : Playground.keyboard) : Multiplayer.player = { id; keyboard = keys; pressed = keys }

(* a round with its riders where the test wants them, no box around *)
let round_with (riders : (int * int * dir * kind option) list) : round =
  let r, _ = new_round flags (List.map (fun _ -> 0) riders) (Playground.initial_seed 1) in
  { r with riders = List.map (fun (x, y, dir, holding) -> { (new_rider 0 (x, y, dir)) with holding }) riders }

(* [n] ticks, the keys of each tick given by [keys tick] *)
let run (n : int) (keys : int -> Playground.keyboard list) (r : round) : round =
  let rec go tick r seed =
    if tick = n then r
    else
      let r, seed = play (List.mapi player (keys tick)) r seed in
      go (tick + 1) r seed
  in
  go 0 r (Playground.initial_seed 7)

let rider (r : round) (i : int) : rider = List.nth r.riders i
let nothing _ = [ idle; idle ]

let head_on () =
  let r = run 2 nothing (round_with [ (100, 100, Right, None); (104, 100, Left, None) ]) in
  Alcotest.(check (list bool)) "both dead" [ false; false ] (List.map (fun rd -> rd.alive) r.riders);
  Alcotest.(check (list int)) "a draw: nobody scores" [ 0; 0 ] (List.map (fun rd -> rd.score) r.riders)

(* right 5 cells, then down, left, up: back into its own trail *)
let loop ~(clear_at : int option) =
  let key tick =
    let k =
      if tick = 5 then { idle with kdown = true }
      else if tick = 6 then { idle with kleft = true }
      else if tick = 7 then { idle with kup = true; kspace = clear_at = Some 7 }
      else idle
    in
    [ k; idle ]
  in
  run 9 key (round_with [ (100, 100, Right, Some Clear); (400, 400, Right, None) ])

let own_trail () =
  Alcotest.(check bool) "into its own trail: dead" false (rider (loop ~clear_at:None) 0).alive;
  let r = loop ~clear_at:(Some 7) in
  Alcotest.(check bool) "cleared first: through it" true (rider r 0).alive;
  Alcotest.(check int) "a new generation" 1 (rider r 0).gen

let swap () =
  let key tick = [ (if tick = 5 then { idle with kspace = true } else idle); idle ] in
  let r = run 6 key (round_with [ (100, 100, Right, Some Swap); (400, 400, Right, None) ]) in
  let rd = rider r 0 in
  (* its trail began at (101, 100); from there it goes left, away from it *)
  Alcotest.(check (triple int int bool)) "at its tail, alive" (100, 100, true) (rd.x, rd.y, rd.alive);
  Alcotest.(check bool) "going left" true (rd.dir = Left);
  let tx, ty, tdir = rd.tail in
  Alcotest.(check (triple int int bool)) "its old head the new tail" (105, 100, true) (tx, ty, tdir = Right)

let freeze_and_speed () =
  let key tick = [ (if tick = 0 then { idle with kspace = true } else idle); idle ] in
  let r = run 10 key (round_with [ (100, 100, Right, Some Freeze); (400, 400, Right, None) ]) in
  (* frozen from the tick after the use, as in the original (motor.c
   * computes freeze[] at the end of a frame): a cell, then still *)
  Alcotest.(check (pair int int)) "the frozen one: a cell, then still" (401, 400) ((rider r 1).x, (rider r 1).y);
  Alcotest.(check int) "the other has" 110 (rider r 0).x;
  let r = run 10 key (round_with [ (100, 100, Right, Some Speed); (400, 400, Right, None) ]) in
  Alcotest.(check int) "twice as far" 120 (rider r 0).x;
  Alcotest.(check int) "the other, once" 410 (rider r 1).x

let tests =
  Testo.categorize "TinyTronscroll"
    [
      Testo.create "two heads in one cell: both dead" head_on;
      Testo.create "its own trail kills, unless cleared" own_trail;
      Testo.create "swap: to its tail, going away from it" swap;
      Testo.create "freeze stops the others, speed doubles" freeze_and_speed;
    ]
