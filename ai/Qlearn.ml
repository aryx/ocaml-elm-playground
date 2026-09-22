(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Qlearn.mli *)

type ('state, 'action) t = {
  table : ('state * 'action, float) Hashtbl.t;
  rate : float;
  discount : float;
  explore : float;
  st : Random.State.t;
}

let make ?(rate = 0.2) ?(discount = 0.9) ?(explore = 0.1) ?(seed = 0) () : ('state, 'action) t =
  { table = Hashtbl.create 997; rate; discount; explore; st = Random.State.make [| seed |] }

let value (q : ('state, 'action) t) (s : 'state) (a : 'action) : float =
  match Hashtbl.find_opt q.table (s, a) with Some v -> v | None -> 0.

let values (q : ('state, 'action) t) (s : 'state) (actions : 'action list) : ('action * float) list =
  List.map (fun a -> (a, value q s a)) actions

(* the best it knows of, ties going to the first: with an empty table
 * that is the first action, which is why a fresh learner walks in a
 * straight line until something pays *)
let best (q : ('state, 'action) t) (s : 'state) (actions : 'action list) : 'action option =
  List.fold_left
    (fun best a -> match best with Some b when value q s b >= value q s a -> Some b | _ -> Some a)
    None actions

let choose (q : ('state, 'action) t) (s : 'state) (actions : 'action list) : 'action option =
  match actions with
  | [] -> None
  | _ ->
      if Random.State.float q.st 1. < q.explore then Some (List.nth actions (Random.State.int q.st (List.length actions)))
      else best q s actions

let learn (q : ('state, 'action) t) ~(state : 'state) ~(action : 'action) ~(reward : float) ~(next : 'state)
    ~(next_actions : 'action list) : unit =
  (* the best it could do next -- not what it will do, which is what
   * makes this off-policy *)
  let future =
    match next_actions with
    | [] -> 0. (* the end: there is no future to discount *)
    | _ -> List.fold_left (fun m a -> Float.max m (value q next a)) Float.neg_infinity next_actions
  in
  let was = value q state action in
  let target = reward +. (q.discount *. future) in
  Hashtbl.replace q.table (state, action) (was +. (q.rate *. (target -. was)))

type ('state, 'action) world = {
  actions : 'state -> 'action list;
  step : 'state -> 'action -> 'state * float;
  over : 'state -> bool;
}

let episode ?(limit = 1000) (q : ('state, 'action) t) (w : ('state, 'action) world) (start : 'state) : float =
  let rec go state got left =
    if left = 0 || w.over state then got
    else
      match choose q state (w.actions state) with
      | None -> got
      | Some action ->
          let (next, reward) = w.step state action in
          let next_actions = if w.over next then [] else w.actions next in
          learn q ~state ~action ~reward ~next ~next_actions;
          go next (got +. reward) (left - 1)
  in
  go start 0. limit

let greedy_run ?(limit = 1000) (q : ('state, 'action) t) (w : ('state, 'action) world) (start : 'state) :
    float * 'state list =
  let rec go state got path left =
    if left = 0 || w.over state then (got, List.rev (state :: path))
    else
      match best q state (w.actions state) with
      | None -> (got, List.rev (state :: path))
      | Some action ->
          let (next, reward) = w.step state action in
          go next (got +. reward) (state :: path) (left - 1)
  in
  go start 0. [] limit

let known (q : ('state, 'action) t) : int = Hashtbl.length q.table
