(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ai.mli *)

let default_speed = 200.
let default_force = 400.

(* the body as Steering sees it: a place, a velocity and two limits *)
let vehicle ~speed ~force (b : Physics.body) : Steering.vehicle =
  { position = (b.x, b.y); velocity = (b.vx, b.vy); max_speed = speed; max_force = force }

(* a steering force is an acceleration; [Physics.push] takes a force,
 * which it divides by the mass *)
let pushed ((fx, fy) : Steering.vec) (b : Physics.body) : Physics.body = Physics.push (fx *. b.mass) (fy *. b.mass) b

(* the body steered towards the velocity [desired] wants for it *)
let steered ?(speed = default_speed) ?(force = default_force) (desired : Steering.vehicle -> Steering.vec) (b : Physics.body) :
    Physics.body =
  let v = vehicle ~speed ~force b in
  pushed (Steering.steer v (desired v)) b

let seek ?speed ?force x y b = steered ?speed ?force (Steering.seek (x, y)) b
let flee ?speed ?force x y b = steered ?speed ?force (Steering.flee (x, y)) b
let arrive ?speed ?force ?slowing x y b = steered ?speed ?force (Steering.arrive ?slowing (x, y)) b

(* the target as a vehicle: only its place and velocity matter *)
let target_of (t : Physics.body) = vehicle ~speed:default_speed ~force:default_force t

let chase ?speed ?force target b = steered ?speed ?force (Steering.pursue (target_of target)) b
let escaping ?speed ?force target b = steered ?speed ?force (Steering.evade (target_of target)) b

(* smooth noise from the time: two slow sines, at speeds whose ratio
 * isn't a simple fraction, so the pattern takes long to repeat *)
let wandering ?speed ?force time b =
  let angle = (1.2 *. Float.sin (time *. 0.7)) +. (0.8 *. Float.sin ((time *. 1.9) +. 1.)) in
  steered ?speed ?force (Steering.wander ~angle) b

let avoiding ?speed ?force rocks b = steered ?speed ?force (Steering.avoid (List.map (fun (x, y, r) -> ((x, y), r)) rocks)) b

let flocking ?(speed = default_speed) ?(force = default_force) ?(radius = 100.) ?separation ?alignment ?cohesion
    (others : Physics.body list) (b : Physics.body) : Physics.body =
  (* the same vehicle for [b] in [others] and on its own, so that Flock
   * can leave it out of its own neighbours (by physical equality) *)
  let pairs = List.map (fun o -> (o, vehicle ~speed ~force o)) others in
  let v = match List.assq_opt b pairs with Some v -> v | None -> vehicle ~speed ~force b in
  pushed (Flock.flock ?separation ?alignment ?cohesion ~radius (List.map snd pairs) v) b

let following ?speed ?force ?(width = 20.) path b = steered ?speed ?force (Steering.follow ~width path) b

let facing (b : Physics.body) : Physics.body =
  if b.vx = 0. && b.vy = 0. then b else Physics.pointing (Float.atan2 b.vy b.vx *. 180. /. Float.pi) b

(*****************************************************************************)
(* Ways through a map *)
(*****************************************************************************)

let straight = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
let corners = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ]

(* the grid as Pathfind sees it: what a tile's neighbours are and
 * what each costs. A corner costs what it really is, sqrt 2, or
 * diagonal ways come out cheaper than they are *)
let problem_of ~(cost : int * int -> float) ~(diagonal : bool) (goal : int * int) : (int * int) Pathfind.problem =
  let steps = if diagonal then List.map (fun d -> (d, 1.)) straight @ List.map (fun d -> (d, sqrt 2.)) corners
              else List.map (fun d -> (d, 1.)) straight in
  {
    neighbors =
      (fun (x, y) ->
        List.filter_map
          (fun ((dx, dy), far) ->
            let c = (x + dx, y + dy) in
            let k = cost c in
            if Float.is_finite k then Some (c, k *. far) else None)
          steps);
    goal = (fun c -> c = goal);
    (* never more than the truth, or A* stops being A*: one per step
     * (the cheapest a tile can be), and the diagonal's shortcut taken
     * into account when there are diagonals *)
    estimate =
      (fun (x, y) ->
        let (gx, gy) = goal in
        let dx = float_of_int (abs (x - gx)) and dy = float_of_int (abs (y - gy)) in
        if diagonal then Float.max dx dy +. ((sqrt 2. -. 1.) *. Float.min dx dy) else dx +. dy);
  }

let way_over ~(cost : int * int -> float) ?(diagonal = false) (from : int * int) (to_ : int * int) : (int * int) list =
  if not (Float.is_finite (cost to_)) then []
  else match (Pathfind.astar (problem_of ~cost ~diagonal to_) from).path with
    | _ :: rest -> rest (* the tile it is standing on is not a step *)
    | [] -> []

let way ~(walkable : int * int -> bool) ?diagonal (from : int * int) (to_ : int * int) : (int * int) list =
  way_over ~cost:(fun c -> if walkable c then 1. else Float.infinity) ?diagonal from to_

(* the cost to the goal from every tile it can be reached from. One
 * search, from the goal outward: the steps cost the same both ways,
 * so a field built from the goal is every tile's way to it *)
type flow = { to_go : (int * int, float) Hashtbl.t; diagonal : bool }

let flow ~(walkable : int * int -> bool) ?(diagonal = false) (goal : int * int) : flow =
  let cost c = if walkable c then 1. else Float.infinity in
  let field = Pathfind.field (problem_of ~cost ~diagonal goal) goal in
  let to_go = Hashtbl.create (List.length field) in
  List.iter (fun (c, k) -> Hashtbl.replace to_go c k) field;
  { to_go; diagonal }

let steps_to_go (f : flow) (tile : int * int) : float option = Hashtbl.find_opt f.to_go tile

(* downhill: of the neighbours on the field, the one closest to the
 * goal, and None on the goal itself (nothing is closer) *)
let next_step (f : flow) ((x, y) : int * int) : (int * int) option =
  match Hashtbl.find_opt f.to_go (x, y) with
  | None -> None
  | Some here ->
      let steps = if f.diagonal then straight @ corners else straight in
      List.fold_left
        (fun best (dx, dy) ->
          let c = (x + dx, y + dy) in
          match Hashtbl.find_opt f.to_go c with
          | Some k when k < here -> (
              match best with Some (_, kb) when kb <= k -> best | _ -> Some (c, k))
          | _ -> best)
        None steps
      |> Option.map fst

(*****************************************************************************)
(* An opponent *)
(*****************************************************************************)

type ('state, 'move) rules = {
  moves : 'state -> 'move list;
  play : 'state -> 'move -> 'state;
  score : 'state -> float;
  my_turn : 'state -> bool;
}

(* the two ways of thinking, and how much of it fits in a frame *)
type how = Ahead of int | Playouts of int

type ('state, 'move) opponent = {
  game : ('state, 'move) Minimax.game;
  how : how;
  hint : ('state -> 'move list -> 'move list) option;
  chunk : int option;
}

(* Minimax's MAX is the machine: [score] is already its view, and
 * [my_turn] is where it gets to choose *)
let game_of (r : ('state, 'move) rules) : ('state, 'move) Minimax.game =
  { moves = r.moves; play = r.play; score = r.score; max_to_play = r.my_turn }

let thinking_ahead (depth : int) (r : ('state, 'move) rules) : ('state, 'move) opponent =
  { game = game_of r; how = Ahead depth; hint = None; chunk = None }

let playing_out (playouts : int) (r : ('state, 'move) rules) : ('state, 'move) opponent =
  { game = game_of r; how = Playouts playouts; hint = None; chunk = None }

let hinting order (o : ('state, 'move) opponent) : ('state, 'move) opponent = { o with hint = Some order }
let a_frame_of (n : int) (o : ('state, 'move) opponent) : ('state, 'move) opponent = { o with chunk = Some n }

let nodes_a_frame = 20_000
let playouts_a_frame = 12

let best_move (o : ('state, 'move) opponent) (state : 'state) : 'move option =
  match o.how with
  | Ahead depth -> (Deepening.search ?order:o.hint o.game ~depth state).best
  | Playouts playouts -> (Mcts.search o.game ~playouts state).best

let thoughts (o : ('state, 'move) opponent) (state : 'state) : ('move * float) list =
  match o.how with
  (* one search per move, each with a window of its own. Not
   * [alphabeta]'s [children]: once a move beats the others, the rest
   * are cut and report the bound that cut them, which is a fine thing
   * to search with and a wrong thing to draw *)
  | Ahead depth ->
      List.map (fun m -> (m, (Minimax.alphabeta o.game ~depth:(depth - 1) (o.game.play state m)).value)) (o.game.moves state)
  | Playouts playouts -> List.map (fun (m, _, share) -> (m, share)) (Mcts.search o.game ~playouts state).tried

type ('state, 'move) pondering =
  | Deep of ('state, 'move) Deepening.thinking * int
  | Rollouts of ('state, 'move) Mcts.thinking * int * int (* a frame's worth, and how many are left *)

let pondering (o : ('state, 'move) opponent) (state : 'state) : ('state, 'move) pondering =
  match o.how with
  | Ahead depth -> Deep (Deepening.start ?order:o.hint o.game ~depth state, Option.value o.chunk ~default:nodes_a_frame)
  | Playouts playouts ->
      Rollouts (Mcts.start o.game state, Option.value o.chunk ~default:playouts_a_frame, playouts)

let ponder (p : ('state, 'move) pondering) : ('state, 'move) pondering =
  match p with
  | Deep (t, chunk) -> Deep (Deepening.think ~nodes:chunk t, chunk)
  | Rollouts (t, chunk, left) ->
      let n = min chunk left in
      Rollouts (Mcts.think ~playouts:n t, chunk, left - n)

let settled (p : ('state, 'move) pondering) : bool =
  match p with Deep (t, _) -> Deepening.done_ t | Rollouts (_, _, left) -> left <= 0

let answer (p : ('state, 'move) pondering) : 'move option =
  match p with Deep (t, _) -> (Deepening.plan t).best | Rollouts (t, _, _) -> (Mcts.plan t).best

let so_far (p : ('state, 'move) pondering) : ('move * float) list =
  match p with
  (* a search ahead has one line it is sure of, not an opinion of each
   * move: the depth that finished, and what it thinks it is worth *)
  | Deep (t, _) -> (
      let plan = Deepening.plan t in
      match plan.best with Some m -> [ (m, plan.value) ] | None -> [])
  | Rollouts (t, _, _) -> List.map (fun (m, _, share) -> (m, share)) (Mcts.plan t).tried

(*****************************************************************************)
(* What a character is doing *)
(*****************************************************************************)

type ('mode, 'context) change = ('mode, 'context) Fsm.rule
type 'mode mind = 'mode Fsm.run

let on ?(why = "") (from : 'mode) (test : 'context -> bool) (target : 'mode) : ('mode, 'context) change =
  { from; label = why; guard = (fun context _since -> test context); target }

let after ?(why = "") (frames : int) (from : 'mode) (target : 'mode) : ('mode, 'context) change =
  { from; label = why; guard = Fsm.after frames; target }

let mind (m : 'mode) : 'mode mind = Fsm.start m
let doing (m : 'mode mind) : 'mode = m.state
let doing_for (m : 'mode mind) : int = m.since
let deciding (changes : ('mode, 'context) change list) (context : 'context) (m : 'mode mind) : 'mode mind =
  Fsm.step changes context m
let changed (m : 'mode mind) : string option = m.fired
let modes (changes : ('mode, 'context) change list) : 'mode list = Fsm.states changes
let links (changes : ('mode, 'context) change list) : ('mode * string * 'mode) list =
  List.map (fun (c : ('mode, 'context) change) -> (c.from, c.label, c.target)) changes

(*****************************************************************************)
(* A bot *)
(*****************************************************************************)

type ('world, 'senses, 'intent) bot = ('world, 'senses, 'intent) Bot.t
type ('senses, 'intent) playing = ('senses, 'intent) Bot.running

let bot ~(senses : 'senses option -> 'world -> 'senses) (decide : 'senses -> 'intent) : ('world, 'senses, 'intent) bot =
  Bot.make ~sense:senses ~decide ()

let reacting_in (frames : int) (b : ('world, 'senses, 'intent) bot) : ('world, 'senses, 'intent) bot =
  { b with delay = frames }

let deciding_every (frames : int) (b : ('world, 'senses, 'intent) bot) : ('world, 'senses, 'intent) bot =
  { b with rate = max 1 frames }

(* one number for both handicaps: at 0 it sees a fifth of a second late
 * and changes its mind ten times a second, at 1 it is a machine *)
let beginner_delay = 12
let beginner_rate = 6

let skill (k : float) (b : ('world, 'senses, 'intent) bot) : ('world, 'senses, 'intent) bot =
  let k = Float.min 1. (Float.max 0. k) in
  { b with
    delay = int_of_float (Float.round ((1. -. k) *. float_of_int beginner_delay));
    rate = 1 + int_of_float (Float.round ((1. -. k) *. float_of_int (beginner_rate - 1))) }

let playing (intent : 'intent) : ('senses, 'intent) playing = Bot.start intent
let thinks = Bot.step
let noticed = Bot.last_senses
let aim_error ~spread ?(settle = 30.) ~seen_for ~seed () = Bot.aim_error ~spread ~settle ~seen_for ~seed ()
