(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Dqn: Q-learning with a network for the table *)

let t = Testo.create

(*****************************************************************************)
(* The cliff, Unit_qlearn's *)
(*****************************************************************************)
(*
     . . . . . . . . . . . .
     . . . . . . . . . . . .
     . . . . . . . . . . . .
     S # # # # # # # # # # G        # the cliff: back to S, -100
*)
let wide = 12
let high = 4
let start = (0, 0)
let goal = (wide - 1, 0)
let cliff ((x, y) : int * int) : bool = y = 0 && x > 0 && x < wide - 1

(* up, down, left, right: where it lands and what that pays *)
let move ((x, y) : int * int) (action : int) : (int * int) * float =
  let (nx, ny) = match action with 0 -> (x, y + 1) | 1 -> (x, y - 1) | 2 -> (x - 1, y) | _ -> (x + 1, y) in
  let (nx, ny) = if nx < 0 || nx >= wide || ny < 0 || ny >= high then (x, y) else (nx, ny) in
  if cliff (nx, ny) then (start, -100.) else ((nx, ny), -1.)

(* a cell as the network reads it: 48 numbers, one of them 1 *)
let seen ((x, y) : int * int) : float array = Array.init (wide * high) (fun i -> if i = (y * wide) + x then 1. else 0.)

(* the way it takes when it only does what it thinks best *)
let greedy (n : Dqn.t) : (int * int) list * float =
  let rec go at steps reward path =
    if at = goal || steps > 100 then (List.rev path, reward)
    else
      let (next, r) = move at (Dqn.best n (seen at)) in
      go next (steps + 1) (reward +. r) (next :: path)
  in
  go start 0 0. [ start ]

(* it finds the table's own way, and the test stops there. It would
   not stay found: taught some more it loses the way and finds it
   again (at 300 episodes of this very run it walks into a wall), the
   table having no such trouble. A network's answer for one cell moves
   when it is taught about another; replay and the frozen target make
   that survivable, not absent (notes_ai_dark_arts.md) *)
let test_cliff () =
  let draws = Lehmer.make 1 in
  let memory = Dqn.memory 5000 in
  let net = ref (Dqn.make ~seed:1 ~rate:0.002 ~shape:(Dqn.Numbers 48) ~inputs:(wide * high) ~actions:4 ()) in
  let target = ref !net in
  let steps = ref 0 and episodes = ref 0 in
  let t0 = Unix.gettimeofday () in
  let right () = match greedy !net with (path, _) -> List.length path - 1 = 13 && List.nth path 13 = goal in
  while (not (right ())) && !episodes < 150 do
    incr episodes;
    let at = ref start and lived = ref 0 in
    while !at <> goal && !lived < 200 do
      (* one time in ten, any action: or it never learns of the others *)
      let action = if Lehmer.float draws 1. < 0.1 then Lehmer.int draws 4 else Dqn.best !net (seen !at) in
      let (next, reward) = move !at action in
      (* the rewards brought down to a size a network likes: -100 for
         the cliff is -1, a step -0.01 *)
      Dqn.remember memory
        { state = seen !at; action; reward = reward /. 100.; next = (if next = goal then None else Some (seen next)) };
      at := next;
      incr lived;
      incr steps;
      if Dqn.remembered memory >= 64 then net := fst (Dqn.step ~discount:1. ~target:!target !net (Dqn.recall draws memory 32));
      (* the frozen copy brought up to date now and then *)
      if !steps mod 200 = 0 then target := !net
    done
  done;
  let (path, reward) = greedy !net in
  Printf.eprintf "Dqn on the cliff: %d episodes, %d steps lived, %.1f s; its way is %d steps for %.0f\n" !episodes !steps
    (Unix.gettimeofday () -. t0) (List.length path - 1) reward;
  Alcotest.(check bool) "it reaches the goal" true (List.nth path (List.length path - 1) = goal);
  (* the table's own answer (Unit_qlearn): up, eleven along the edge,
     down *)
  Alcotest.(check int) "by the shortest way there is" 13 (List.length path - 1);
  Alcotest.(check (float 0.001)) "which costs 13" (-13.) reward

(*****************************************************************************)
(* The pieces *)
(*****************************************************************************)

let test_shapes () =
  let n = Dqn.make ~seed:1 ~inputs:5 ~actions:3 () in
  let state = [| 0.2; -0.4; 1.; 0.; 0.7 |] in
  Alcotest.(check int) "a value per action" 3 (Array.length (Dqn.values n state));
  Alcotest.(check (array (float 1e-12))) "the plain pass and the graph agree" (Dqn.values_by_graph n state) (Dqn.values n state);
  (* a small screen: 12 by 10 pixels, 2 frames, windows of 4 every 2
     then of 2 every 1 *)
  let screen : Dqn.screen = { width = 12; height = 10; frames = 2; first = (4, 2, 3); second = (2, 1, 5); hidden = 8 } in
  let s = Dqn.make ~seed:2 ~shape:(Dqn.Screen screen) ~inputs:240 ~actions:3 () in
  let frames = Array.init 240 (fun i -> float_of_int (i mod 7) /. 7.) in
  Alcotest.(check (array (float 1e-12))) "the screen: the plain pass and the graph agree" (Dqn.values_by_graph s frames) (Dqn.values s frames);
  (* the paper's own sizes: 84 by 84, 4 frames, 3 actions *)
  let paper : Dqn.screen = { width = 84; height = 84; frames = 4; first = (8, 4, 16); second = (4, 2, 32); hidden = 256 } in
  let p = Dqn.make ~seed:3 ~shape:(Dqn.Screen paper) ~inputs:(84 * 84 * 4) ~actions:3 () in
  Alcotest.(check int) "the paper's network, three actions" 676_915 (Dqn.parameters p);
  (* written and read back *)
  (match Result.bind (Weights.of_string (Weights.to_string (Dqn.to_weights s))) Dqn.of_weights with
  | Error why -> Alcotest.fail why
  | Ok back ->
      Alcotest.(check bool) "its shape, from a note" true (back.shape = s.shape);
      Alcotest.(check (array (float 1e-5))) "the same values, from a file" (Dqn.values s frames) (Dqn.values back frames));
  Alcotest.check_raises "pixels and frames that are not the inputs"
    (Invalid_argument "Dqn.make: the screen's pixels and frames are not the inputs") (fun () ->
      ignore (Dqn.make ~seed:1 ~shape:(Dqn.Screen screen) ~inputs:100 ~actions:3 ()))

(* one step lived, repeated: the value of the action taken goes to
   what it paid, and the others are left alone by the loss *)
let test_one_step () =
  let screen : Dqn.screen = { width = 12; height = 10; frames = 2; first = (4, 2, 3); second = (2, 1, 5); hidden = 8 } in
  List.iter
    (fun (name, n, state) ->
      let lived : Dqn.lived = { state; action = 1; reward = 0.5; next = None } in
      let rec learn n k = if k = 0 then n else learn (fst (Dqn.step ~rate:0.01 ~target:n n [| lived; lived |])) (k - 1) in
      let taught = learn n 300 in
      Alcotest.(check (float 0.01)) (name ^ ": the action taken is worth what it paid") 0.5 (Dqn.values taught state).(1);
      Alcotest.(check bool) (name ^ ": the loss is gone") true (Dqn.loss ~target:taught taught [| lived |] < 1e-4))
    [ ("numbers", Dqn.make ~seed:1 ~inputs:5 ~actions:3 (), [| 0.2; -0.4; 1.; 0.; 0.7 |]);
      ("a screen", Dqn.make ~seed:2 ~shape:(Dqn.Screen screen) ~inputs:240 ~actions:3 (), Array.init 240 (fun i -> float_of_int (i mod 7) /. 7.)) ];
  (* with a state after it, the target is asked what that one is
     worth: here a network that says 2 for its best action *)
  let n = Dqn.make ~seed:1 ~inputs:5 ~actions:3 () in
  let state = [| 0.2; -0.4; 1.; 0.; 0.7 |] in
  let after = Array.fold_left Float.max neg_infinity (Dqn.values n state) in
  let lived : Dqn.lived = { state; action = 0; reward = 1.; next = Some state } in
  let off = (Dqn.values n state).(0) -. (1. +. (0.9 *. after)) in
  Alcotest.(check (float 1e-9)) "the reward and the discounted best of what follows" (off *. off) (Dqn.loss ~discount:0.9 ~target:n n [| lived |])

let test_memory () =
  let m = Dqn.memory 3 in
  let lived i : Dqn.lived = { state = [| float_of_int i |]; action = 0; reward = 0.; next = None } in
  Alcotest.(check int) "nothing yet" 0 (Dqn.remembered m);
  List.iter (fun i -> Dqn.remember m (lived i)) [ 1; 2; 3; 4; 5 ];
  Alcotest.(check int) "the last three" 3 (Dqn.remembered m);
  let drawn = Dqn.recall (Lehmer.make 1) m 200 in
  let seen = List.sort_uniq compare (Array.to_list (Array.map (fun (l : Dqn.lived) -> l.state.(0)) drawn)) in
  Alcotest.(check (list (float 0.))) "1 and 2 written over" [ 3.; 4.; 5. ] seen

let tests =
  [
    t "Dqn, its two shapes" test_shapes;
    t "Dqn, one step lived" test_one_step;
    t "Dqn, what it has lived" test_memory;
    t "Dqn, the cliff: the table's own way" test_cliff;
  ]
