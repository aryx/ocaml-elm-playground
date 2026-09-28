(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Qlearn: the cliff world of Sutton and Barto, what exploring is
 * for, and the oldest result in the subject -- a program that learns
 * a game by playing it, until it stops losing *)

let t = Testo.create

(*****************************************************************************)
(* The cliff *)
(*****************************************************************************)

(* 12 by 4. Start at the bottom left, goal at the bottom right, and
 * the eleven cells between them are a cliff: stepping on one costs
 * 100 and puts you back at the start. Every other step costs 1, so
 * the shortest way is the best way.
 *
 *     . . . . . . . . . . . .
 *     . . . . . . . . . . . .
 *     . . . . . . . . . . . .
 *     S C C C C C C C C C C G
 *)
let wide = 12
let high = 4
let start = (0, 0)
let goal = (wide - 1, 0)
let cliff ((x, y) : int * int) : bool = y = 0 && x > 0 && x < wide - 1

type move = Up | Down | Left | Right

let cliff_world : ((int * int), move) Qlearn.world =
  {
    actions = (fun _ -> [ Up; Down; Left; Right ]);
    step =
      (fun (x, y) a ->
        let (nx, ny) =
          match a with Up -> (x, y + 1) | Down -> (x, y - 1) | Left -> (x - 1, y) | Right -> (x + 1, y)
        in
        let (nx, ny) = if nx < 0 || nx >= wide || ny < 0 || ny >= high then (x, y) else (nx, ny) in
        if cliff (nx, ny) then (start, -100.) else ((nx, ny), -1.));
    over = (fun p -> p = goal);
  }

let test_cliff () =
  let q = Qlearn.make ~rate:0.5 ~discount:1. ~explore:0.1 ~seed:1 () in
  let first = Qlearn.episode q cliff_world start in
  for _ = 1 to 500 do
    ignore (Qlearn.episode q cliff_world start)
  done;
  let (reward, path) = Qlearn.greedy_run q cliff_world start in
  Printf.eprintf "cliff: first episode %.0f, after 500 the greedy way is %d steps for %.0f\n" first
    (List.length path - 1) reward;
  Alcotest.(check bool) "it reaches the goal" true (List.nth path (List.length path - 1) = goal);
  (* the shortest way is 13 steps: up, eleven along, down. Learned
     off-policy, it walks the cliff edge rather than the safe row --
     it learns the best policy while behaving randomly enough to fall
     in now and then *)
  Alcotest.(check int) "by the shortest way there is" 13 (List.length path - 1);
  Alcotest.(check bool) "which costs 13" true (Float.abs (reward +. 13.) < 0.001);
  Alcotest.(check bool) "and it has an opinion about most of the grid" true (Qlearn.known q > 100)

(* The same grid, except that walking costs nothing and only the goal
   pays. That one change is what makes exploring necessary, and the
   comparison is the lesson: in the cliff world every step costs 1, so
   a value of 0 -- what an untried action has -- is *better* than
   anything it has tried, and a greedy learner tries everything by
   itself. Optimism in the initial values is a way of exploring
   (Sutton and Barto, 2.6), and it is switched on by accident whenever
   the rewards are negative. Take the costs away and the optimism goes
   with them: every action looks equally fine for ever, the learner
   takes the first one, and never finds the goal at all. *)
let patient_world : ((int * int), move) Qlearn.world =
  { cliff_world with
    step =
      (fun p a ->
        let (next, _) = cliff_world.step p a in
        (next, if next = goal then 1. else 0.)) }

let test_explore () =
  let learns (w : ((int * int), move) Qlearn.world) explore =
    let q = Qlearn.make ~rate:0.5 ~discount:0.95 ~explore ~seed:2 () in
    for _ = 1 to 500 do
      ignore (Qlearn.episode q w start)
    done;
    let (_, path) = Qlearn.greedy_run q w start in
    (List.nth path (List.length path - 1) = goal, List.length path - 1, Qlearn.known q)
  in
  let (found, _, known) = learns patient_world 0. in
  Alcotest.(check bool) "nothing to lose and nothing to gain: it never finds the goal" false found;
  (* and the number that says why: five hundred episodes, and it has
     an opinion about four state-action pairs. It walked into the same
     wall a hundred thousand times. *)
  Alcotest.(check int) "having tried four things in five hundred episodes" 4 known;
  let (found, steps, _) = learns patient_world 0.3 in
  Alcotest.(check bool) "with exploring, it finds it" true found;
  Alcotest.(check bool) "by a way that is nearly the shortest" true (steps <= 17);
  (* where every step costs, a greedy learner explores anyway: an
     untried action is worth 0 and everything it has tried is worth
     less, so it tries everything once. Optimism, by accident. *)
  let (found, steps, _) = learns cliff_world 0. in
  Alcotest.(check bool) "a cost makes zero optimistic, and greedy is enough" true found;
  Alcotest.(check int) "and it still finds the shortest way" 13 steps

(*****************************************************************************)
(* And the old result: learning a game by playing it *)
(*****************************************************************************)

(* tic-tac-toe, the learner as X against a perfect opponent. It cannot
 * win -- nobody can -- so the whole question is whether it learns to
 * stop losing, which is what Samuel's checkers player did in 1959 and
 * what TD-Gammon did properly in 1992. *)

let lines = [ [ 0; 1; 2 ]; [ 3; 4; 5 ]; [ 6; 7; 8 ]; [ 0; 3; 6 ]; [ 1; 4; 7 ]; [ 2; 5; 8 ]; [ 0; 4; 8 ]; [ 2; 4; 6 ] ]
let won (b : string) (who : char) : bool = List.exists (fun l -> List.for_all (fun i -> b.[i] = who) l) lines
let free (b : string) : int list = List.filter (fun i -> b.[i] = '.') (List.init 9 (fun i -> i))
let put (b : string) (i : int) (who : char) : string = String.mapi (fun k c -> if k = i then who else c) b
let over (b : string) : bool = won b 'x' || won b 'o' || free b = []

(* the perfect reply, memoised: minimax over a board this small is
   instant once, and this is played thousands of times *)
let seen : (string, int) Hashtbl.t = Hashtbl.create 1000

let rec perfect (b : string) (who : char) : int =
  match Hashtbl.find_opt seen (b ^ String.make 1 who) with
  | Some i -> i
  | None ->
      let other = if who = 'x' then 'o' else 'x' in
      let score b' = if won b' who then 1 else if won b' other then -1 else if over b' then 0 else -perfect_value b' other in
      let best = List.fold_left (fun best i -> match best with Some (_, s) when s >= score (put b i who) -> best | _ -> Some (i, score (put b i who))) None (free b) in
      let i = match best with Some (i, _) -> i | None -> 0 in
      Hashtbl.replace seen (b ^ String.make 1 who) i;
      i

and perfect_value (b : string) (who : char) : int =
  if over b then if won b 'x' then (if who = 'x' then 1 else -1) else if won b 'o' then (if who = 'o' then 1 else -1) else 0
  else
    let i = perfect b who in
    let b' = put b i who in
    let other = if who = 'x' then 'o' else 'x' in
    if won b' who then 1 else if over b' then 0 else -perfect_value b' other

(* the world: the learner plays x, and the perfect player's reply is
   part of what a move leads to -- an opponent is just weather *)
let tictactoe : (string, int) Qlearn.world =
  {
    actions = free;
    step =
      (fun b i ->
        let b = put b i 'x' in
        if won b 'x' then (b, 1.)
        else if over b then (b, 0.)
        else
          let b = put b (perfect b 'o') 'o' in
          if won b 'o' then (b, -1.) else (b, 0.));
    over;
  }

let empty = "........."

let test_stops_losing () =
  let q = Qlearn.make ~rate:0.3 ~discount:0.95 ~explore:0.25 ~seed:5 () in
  (* how it does before learning anything: it loses *)
  let outcome (q : (string, int) Qlearn.t) : int =
    let (_, path) = Qlearn.greedy_run q tictactoe empty in
    let last = List.nth path (List.length path - 1) in
    if won last 'x' then 1 else if won last 'o' then -1 else 0
  in
  Alcotest.(check int) "at first it loses to the perfect player" (-1) (outcome q);
  for _ = 1 to 4000 do
    ignore (Qlearn.episode q tictactoe empty)
  done;
  Printf.eprintf "tictactoe: %d positions learned, the game now ends %d\n" (Qlearn.known q) (outcome q);
  (* it cannot win -- nobody can -- so a draw is the perfect result *)
  Alcotest.(check int) "after four thousand games it draws" 0 (outcome q);
  Alcotest.(check bool) "having formed an opinion about hundreds of positions" true (Qlearn.known q > 200)

let tests =
  [
    t "Qlearn, the cliff" test_cliff;
    t "Qlearn, what exploring is for" test_explore;
    t "Qlearn, it learns not to lose" test_stops_losing;
  ]
