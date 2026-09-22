(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mcts.mli *)

type 'move result = { best : 'move option; tried : ('move * int * float) list; playouts : int; nodes : int }

(* a node of the tree: the move that led to it, what the playouts
 * through it came to (always counted for MAX: [wins] is MAX's share),
 * the children tried so far, and the moves not tried yet *)
type 'move node = {
  move : 'move option;
  mutable visits : int;
  mutable wins : float; (* MAX's, between 0 and [visits] *)
  mutable children : 'move node list;
  mutable untried : 'move list;
}

let node (move : 'move option) (moves : 'move list) : 'move node =
  { move; visits = 0; wins = 0.; children = []; untried = moves }

(* a finished game, as MAX's share: 1 won, 0 lost, 1/2 drawn *)
let outcome (game : ('state, 'move) Minimax.game) (state : 'state) : float =
  let s = game.score state in
  if s > 0. then 1. else if s < 0. then 0. else 0.5

(* the default playout: uniformly random moves to the end *)
let random_playout (st : Random.State.t) (game : ('state, 'move) Minimax.game) (state : 'state) : 'state =
  let rec go state =
    match game.moves state with
    | [] -> state
    | moves -> go (game.play state (List.nth moves (Random.State.int st (List.length moves))))
  in
  go state

(* UCB1, from the point of view of whoever is to play at [parent]: what
 * has been working for *them*, plus what has barely been tried *)
let ucb (exploration : float) (maximizing : bool) (parent_visits : int) (c : 'move node) : float =
  if c.visits = 0 then Float.infinity
  else
    let share = c.wins /. float_of_int c.visits in
    let mine = if maximizing then share else 1. -. share in
    mine +. (exploration *. sqrt (log (float_of_int parent_visits) /. float_of_int c.visits))

type ('state, 'move) thinking = {
  game : ('state, 'move) Minimax.game;
  root_state : 'state;
  root : 'move node;
  exploration : float;
  playout : Random.State.t -> ('state, 'move) Minimax.game -> 'state -> 'state;
  st : Random.State.t;
  mutable played : int;
  mutable nodes : int;
}

let start ?(exploration = sqrt 2.) ?(seed = 0) ?(playout = random_playout) (game : ('state, 'move) Minimax.game)
    (state : 'state) : ('state, 'move) thinking =
  {
    game;
    root_state = state;
    root = node None (game.moves state);
    exploration;
    playout;
    st = Random.State.make [| seed |];
    played = 0;
    nodes = 1;
  }

(* one iteration: select, expand, simulate, backup. The path taken is
 * returned so the result can be added to every node on it *)
let rec descend (t : ('state, 'move) thinking) (n : 'move node) (state : 'state) (path : 'move node list) :
    'move node list * 'state =
  if n.untried <> [] then (
    (* expand: one of the moves never tried here *)
    let i = Random.State.int t.st (List.length n.untried) in
    let move = List.nth n.untried i in
    n.untried <- List.filteri (fun j _ -> j <> i) n.untried;
    let child_state = t.game.play state move in
    let child = node (Some move) (t.game.moves child_state) in
    n.children <- child :: n.children;
    t.nodes <- t.nodes + 1;
    (child :: path, child_state))
  else
    match n.children with
    (* the game is over here: nothing to expand, nothing to choose *)
    | [] -> (path, state)
    | children ->
        let maximizing = t.game.max_to_play state in
        let best =
          List.fold_left
            (fun best c -> if ucb t.exploration maximizing n.visits c > ucb t.exploration maximizing n.visits best then c else best)
            (List.hd children) children
        in
        descend t best (t.game.play state (Option.get best.move)) (best :: path)

let iterate (t : ('state, 'move) thinking) : unit =
  let (path, state) = descend t t.root t.root_state [ t.root ] in
  let ended = t.playout t.st t.game state in
  let share = outcome t.game ended in
  List.iter
    (fun n ->
      n.visits <- n.visits + 1;
      n.wins <- n.wins +. share)
    path;
  t.played <- t.played + 1

let plan (t : ('state, 'move) thinking) : 'move result =
  let tried =
    List.map
      (fun (c : 'move node) ->
        (Option.get c.move, c.visits, if c.visits = 0 then 0. else c.wins /. float_of_int c.visits))
      t.root.children
  in
  (* MCTS's answer is the most visited child, not the best scoring one:
   * a high score on three playouts means nothing, and the selection
   * rule has already spent its visits on what it believes *)
  let best =
    List.fold_left (fun best (m, visits, _) -> match best with Some (_, v) when v >= visits -> best | _ -> Some (m, visits)) None tried
  in
  { best = Option.map fst best; tried; playouts = t.played; nodes = t.nodes }

let think ~(playouts : int) (t : ('state, 'move) thinking) : ('state, 'move) thinking =
  for _ = 1 to playouts do
    iterate t
  done;
  t

let search ?exploration ?seed ?playout (game : ('state, 'move) Minimax.game) ~(playouts : int) (state : 'state) :
    'move result =
  plan (think ~playouts (start ?exploration ?seed ?playout game state))
