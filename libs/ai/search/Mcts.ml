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
  prior : float; (* what a policy thought of this move, 1 with none *)
  (* what the policy thinks of the moves from here, asked once, when
   * the node is made: empty with no policy *)
  priors : ('move * float) list;
}

let node ?(prior = 1.) ?(priors = []) (move : 'move option) (moves : 'move list) : 'move node =
  { move; visits = 0; wins = 0.; children = []; untried = moves; prior; priors }

(* a finished game, as MAX's share: 1 won, 0 lost, 1/2 drawn *)
let outcome (game : ('state, 'move) Minimax.game) (state : 'state) : float =
  let s = game.score state in
  if s > 0. then 1. else if s < 0. then 0. else 0.5

(* the default playout: uniformly random moves to the end *)
let random_playout (st : Lehmer.state) (game : ('state, 'move) Minimax.game) (state : 'state) : 'state =
  let rec go state =
    match game.moves state with
    | [] -> state
    | moves -> go (game.play state (List.nth moves (Lehmer.int st (List.length moves))))
  in
  go state

(* UCB1, from the point of view of whoever is to play at [parent]: what
 * has been working for *them*, plus what has barely been tried.
 *
 * With a policy ([policy = true]) this is PUCT instead: the second
 * term is weighted by what the policy thought of the move, and an
 * unvisited child is no longer infinitely attractive but as
 * attractive as the policy says *)
let ucb ~(policy : bool) (exploration : float) (maximizing : bool) (parent_visits : int) (c : 'move node) : float =
  if policy then
    let share = if c.visits = 0 then 0.5 else c.wins /. float_of_int c.visits in
    let mine = if maximizing then share else 1. -. share in
    mine
    +. (exploration *. c.prior *. sqrt (float_of_int parent_visits) /. (1. +. float_of_int c.visits))
  else if c.visits = 0 then Float.infinity
  else
    let share = c.wins /. float_of_int c.visits in
    let mine = if maximizing then share else 1. -. share in
    mine +. (exploration *. sqrt (log (float_of_int parent_visits) /. float_of_int c.visits))

type ('state, 'move) thinking = {
  game : ('state, 'move) Minimax.game;
  root_state : 'state;
  root : 'move node;
  exploration : float;
  playout : Lehmer.state -> ('state, 'move) Minimax.game -> 'state -> 'state;
  (* AlphaGo's two: what the moves are worth before trying them, and
   * what a position is worth without playing it out *)
  prior : ('state -> ('move * float) list) option;
  evaluate : ('state -> float) option;
  st : Lehmer.state;
  mutable played : int;
  mutable nodes : int;
}

let start ?exploration ?(seed = 0) ?(playout = random_playout) ?prior ?evaluate
    (game : ('state, 'move) Minimax.game) (state : 'state) : ('state, 'move) thinking =
  {
    game;
    root_state = state;
    (* PUCT wants a larger constant than UCB1: its exploring term
     * falls off as 1/(1+N) rather than sqrt(log N / N) *)
    exploration = (match exploration with Some e -> e | None -> if prior = None then sqrt 2. else 1.5);
    root = node ?priors:(Option.map (fun policy -> policy state) prior) None (game.moves state);
    playout;
    prior;
    evaluate;
    st = Lehmer.make seed;
    played = 0;
    nodes = 1;
  }

(* expand: [move], never tried at [n], becomes its new child *)
let expand (t : ('state, 'move) thinking) (n : 'move node) (state : 'state) (path : 'move node list) (move : 'move)
    (prior : float) : 'move node list * 'state =
  n.untried <- List.filter (fun m -> m <> move) n.untried;
  let child_state = t.game.play state move in
  let moves = t.game.moves child_state in
  (* the policy is asked about the new position now, once, and its
   * answer kept in the node: asking at every visit was one more
   * evaluation of the network for each level of each playout *)
  let priors = match t.prior with Some policy when moves <> [] -> policy child_state | _ -> [] in
  let child = node ~prior ~priors (Some move) moves in
  n.children <- child :: n.children;
  t.nodes <- t.nodes + 1;
  (child :: path, child_state)

(* one iteration: select, expand, simulate, backup. The path taken is
 * returned so the result can be added to every node on it *)
let rec descend (t : ('state, 'move) thinking) (n : 'move node) (state : 'state) (path : 'move node list) :
    'move node list * 'state =
  let maximizing = t.game.max_to_play state in
  let best_child (score : 'move node -> float) : 'move node option =
    List.fold_left
      (fun best c -> match best with Some b when score b >= score c -> best | _ -> Some c)
      None n.children
  in
  match t.prior with
  | None -> (
      if n.untried <> [] then
        (* every move is tried once before any is tried twice: one of
         * those never tried here, at random *)
        expand t n state path (List.nth n.untried (Lehmer.int t.st (List.length n.untried))) 1.
      else
        match best_child (ucb ~policy:false t.exploration maximizing n.visits) with
        (* the game is over here: nothing to expand, nothing to choose *)
        | None -> (path, state)
        | Some best -> descend t best (t.game.play state (Option.get best.move)) (best :: path))
  | Some _ -> (
      (* with a policy the moves never tried compete with the children
       * by the same rule, each with no visit yet and the share of a
       * game not played, a half. So a move the policy thinks little of
       * waits until the others have disappointed, and the playouts go
       * deep along what the policy likes instead of wide over
       * everything once: the first version tried every move of a
       * position before choosing among them, and fifty playouts saw
       * two moves ahead. *)
      let score = ucb ~policy:true t.exploration maximizing (max 1 n.visits) in
      let untried =
        if n.untried = [] then None
        else
          let prior m = match List.assoc_opt m n.priors with Some p -> p | None -> 0.01 in
          let m =
            List.fold_left (fun best m -> if prior m > prior best then m else best) (List.hd n.untried) n.untried
          in
          Some (m, prior m, score (node ~prior:(prior m) (Some m) []))
      in
      match (untried, best_child score) with
      | (None, None) -> (path, state)
      | (Some (m, p, fresh), child) when (match child with None -> true | Some c -> fresh > score c) ->
          expand t n state path m p
      | (_, Some best) -> descend t best (t.game.play state (Option.get best.move)) (best :: path)
      | (Some (m, p, _), None) -> expand t n state path m p)

let iterate (t : ('state, 'move) thinking) : unit =
  let (path, state) = descend t t.root t.root_state [ t.root ] in
  let share =
    match t.evaluate with
    (* a value head replaces the playout: one opinion instead of one
     * random game -- except at a position that is already over, where
     * the rules know better than any network *)
    | Some value when t.game.moves state <> [] -> Float.max 0. (Float.min 1. (value state))
    | _ -> outcome t.game (t.playout t.st t.game state)
  in
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
   * rule has already spent its visits on what it believes.
   *
   * Ties go to the better share, from the point of view of whoever is
   * to play. Early on every child has been visited once and the
   * visits decide nothing at all -- without this the answer would be
   * whichever move happened to be first in the list, which is how a
   * search with a *perfect* evaluation was seen to give a losing move
   * at twelve playouts (Unit_mcts). *)
  let maximizing = t.game.max_to_play t.root_state in
  let mine share = if maximizing then share else 1. -. share in
  let best =
    List.fold_left
      (fun best (m, visits, share) ->
        match best with
        | Some (_, v, s) when v > visits || (v = visits && mine s >= mine share) -> best
        | _ -> Some (m, visits, share))
      None tried
  in
  let best = Option.map (fun (m, v, _) -> (m, v)) best in
  { best = Option.map fst best; tried; playouts = t.played; nodes = t.nodes }

let think ~(playouts : int) (t : ('state, 'move) thinking) : ('state, 'move) thinking =
  for _ = 1 to playouts do
    iterate t
  done;
  t

let search ?exploration ?seed ?playout ?prior ?evaluate (game : ('state, 'move) Minimax.game) ~(playouts : int)
    (state : 'state) : 'move result =
  plan (think ~playouts (start ?exploration ?seed ?playout ?prior ?evaluate game state))
