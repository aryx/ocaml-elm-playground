(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Deepening.mli *)

type 'move plan = { value : float; best : 'move option; depth : int; nodes : int }

(* the budget ran out: the depth being searched is abandoned *)
exception Out_of_nodes

type ('state, 'move) settings = {
  game : ('state, 'move) Minimax.game;
  order : ('state -> 'move list -> 'move list) option;
  key : ('state -> int64) option;
  table : 'move Zobrist.table option;
}

(* the moves to try, best first: the table's move if it has one, then
 * the game's own hint, then the rest *)
let ordered (s : ('state, 'move) settings) (state : 'state) (first : 'move option) : 'move list =
  let moves = s.game.moves state in
  let moves = match s.order with Some order -> order state moves | None -> moves in
  match first with Some m when List.mem m moves -> m :: List.filter (fun x -> x <> m) moves | _ -> moves

let remembered (s : ('state, 'move) settings) (state : 'state) : 'move Zobrist.entry option =
  match (s.key, s.table) with Some key, Some table -> Zobrist.find table (key state) | _ -> None

let keep (s : ('state, 'move) settings) (state : 'state) (e : 'move Zobrist.entry) : unit =
  match (s.key, s.table) with Some key, Some table -> Zobrist.remember table (key state) e | _ -> ()

(* alpha-beta with the three tricks; [nodes] counts what it visits and
 * [budget] stops it *)
let rec search_at (s : ('state, 'move) settings) ~(depth : int) ~(alpha : float) ~(beta : float) ~(nodes : int ref)
    ~(budget : int option) (state : 'state) : float * 'move option =
  incr nodes;
  (match budget with Some b when !nodes > b -> raise Out_of_nodes | _ -> ());
  let known = remembered s state in
  let from_table =
    match known with
    | Some e when e.depth >= depth -> (
        match e.bound with
        | Exact -> Some (e.value, e.best)
        | Lower when e.value >= beta -> Some (e.value, e.best)
        | Upper when e.value <= alpha -> Some (e.value, e.best)
        | _ -> None)
    | _ -> None
  in
  match from_table with
  | Some answer -> answer
  | None -> (
      let moves = ordered s state (match known with Some e -> e.best | None -> None) in
      match moves with
      | [] -> (s.game.score state, None)
      | _ when depth <= 0 -> (s.game.score state, None)
      | _ ->
          let maximizing = s.game.max_to_play state in
          let best = ref None and value = ref (if maximizing then Float.neg_infinity else Float.infinity) in
          (* the window this node was entered with: what the value below
           * is a bound against *)
          let from_alpha = alpha and from_beta = beta in
          let alpha = ref alpha and beta = ref beta in
          (try
             moves
             |> List.iter (fun move ->
                    let (child, _) =
                      search_at s ~depth:(depth - 1) ~alpha:!alpha ~beta:!beta ~nodes ~budget (s.game.play state move)
                    in
                    if maximizing then (
                      if child > !value || !best = None then (
                        value := child;
                        best := Some move);
                      alpha := Float.max !alpha !value)
                    else (
                      if child < !value || !best = None then (
                        value := child;
                        best := Some move);
                      beta := Float.min !beta !value);
                    (* the cut: this move is already too good for the
                     * other side to allow *)
                    if !alpha >= !beta then raise Exit)
           with Exit -> ());
          (* it stopped early: all it knows is that the value is at most
           * what the window allowed (a fail low), or at least it (a
           * fail high) *)
          let bound : Zobrist.bound =
            if !value <= from_alpha then Upper else if !value >= from_beta then Lower else Exact
          in
          keep s state { value = !value; depth; bound; best = !best };
          (!value, !best))

(* one depth, from the root, the previous depth's best move first *)
let one_depth (s : ('state, 'move) settings) ~(depth : int) ~(nodes : int ref) ~(budget : int option)
    (previous : 'move option) (state : 'state) : float * 'move option =
  let first =
    match previous with Some _ -> previous | None -> ( match remembered s state with Some e -> e.best | None -> None)
  in
  incr nodes;
  let maximizing = s.game.max_to_play state in
  let best = ref None and value = ref (if maximizing then Float.neg_infinity else Float.infinity) in
  let alpha = ref Float.neg_infinity and beta = ref Float.infinity in
  ordered s state first
  |> List.iter (fun move ->
         let (child, _) = search_at s ~depth:(depth - 1) ~alpha:!alpha ~beta:!beta ~nodes ~budget (s.game.play state move) in
         if maximizing then (
           if child > !value || !best = None then (
             value := child;
             best := Some move);
           alpha := Float.max !alpha !value)
         else (
           if child < !value || !best = None then (
             value := child;
             best := Some move);
           beta := Float.min !beta !value));
  keep s state { value = !value; depth; bound = Exact; best = !best };
  (!value, !best)

let settings game order key table = { game; order; key; table }

let search ?budget ?order ?key ?table (game : ('state, 'move) Minimax.game) ~(depth : int) (state : 'state) : 'move plan =
  let s = settings game order key table in
  let nodes = ref 0 in
  let plan = ref { value = game.score state; best = None; depth = 0; nodes = 0 } in
  (try
     for d = 1 to depth do
       let (value, best) = one_depth s ~depth:d ~nodes ~budget !plan.best state in
       plan := { value; best; depth = d; nodes = !nodes }
     done
   with Out_of_nodes -> ());
  { !plan with nodes = !nodes }

(* a search a game can leave and come back to: what it needs to go on,
 * and the best answer so far *)
type ('state, 'move) thinking = {
  settings : ('state, 'move) settings;
  position : 'state;
  max_depth : int;
  so_far : 'move plan;
  (* what the next depth may spend: [think]'s [nodes], doubled every
   * time a depth does not fit in it, or no deep search would ever
   * finish in a frame's worth *)
  allowance : int;
}

let start ?order ?key ?table (game : ('state, 'move) Minimax.game) ~(depth : int) (state : 'state) :
    ('state, 'move) thinking =
  {
    settings = settings game order key table;
    position = state;
    max_depth = depth;
    so_far = { value = game.score state; best = None; depth = 0; nodes = 0 };
    allowance = 0;
  }

let think ~(nodes : int) (t : ('state, 'move) thinking) : ('state, 'move) thinking =
  if t.so_far.depth >= t.max_depth then t
  else
    let budget = max nodes t.allowance in
    let counted = ref 0 in
    let (so_far, allowance) =
      try
        let (value, best) =
          one_depth t.settings ~depth:(t.so_far.depth + 1) ~nodes:counted ~budget:(Some budget) t.so_far.best t.position
        in
        ({ t.so_far with value; best; depth = t.so_far.depth + 1 }, nodes)
      with Out_of_nodes -> (t.so_far, 2 * budget)
    in
    { t with so_far = { so_far with nodes = t.so_far.nodes + !counted }; allowance }

let plan (t : ('state, 'move) thinking) : 'move plan = t.so_far
let done_ (t : ('state, 'move) thinking) : bool = t.so_far.depth >= t.max_depth
