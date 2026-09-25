(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Lightcycles.mli *)

(*****************************************************************************)
(* The arena *)
(*****************************************************************************)

let size = 90 (* cells, each side *)
let cell = 10. (* pixels *)

type layout = string * string list

let layouts : layout list =
  [ ("THE GRID", List.init 10 (fun _ -> ".........."));
    (* four pillars round the middle, four in the corners *)
    ( "PILLARS",
      [ ".........."; ".#......#."; ".........."; "...#..#..."; "..........";
        ".........."; "...#..#..."; ".........."; ".#......#."; ".........." ] );
    (* a block in the middle, corners closing round it *)
    ( "RINGS",
      [ ".........."; ".##....##."; ".#......#."; ".........."; "....##....";
        "....##...."; ".........."; ".#......#."; ".##....##."; ".........." ] ) ]

(* a cell is a wall if it is on the border, or in a block of the layout
 * (the layout's character for its square of 9 x 9 cells) *)
let arena_of ((_, rows) : layout) : Tilemap.t =
  let block r c = (List.nth rows (r / 9)).[c / 9] = '#' in
  Tilemap.of_strings cell
    (List.init size (fun r ->
         String.init size (fun c -> if r = 0 || r = size - 1 || c = 0 || c = size - 1 || block r c then '#' else ' ')))

type dir = Up | Down | Left | Right

let delta (d : dir) : int * int = match d with Up -> (0, -1) | Down -> (0, 1) | Left -> (-1, 0) | Right -> (1, 0)
let opposite (d : dir) : dir = match d with Up -> Down | Down -> Up | Left -> Right | Right -> Left

type cycle = {
  col : int;
  row : int;
  dir : dir;
  wanted : dir; (* the last arrow pressed: a turn happens at the next step *)
  mark : char; (* its trail's character, '1' to '4' *)
  corners : (int * int) list; (* where it turned, the last first, and where it started *)
  alive : bool;
  energy : int;
  boosting : bool;
}

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type round = {
  arena : Tilemap.t;
  cycles : cycle list;
  over : int list option; (* the round's points, and... *)
  pause : int; (* ...the frames to wait before the next round *)
  frames : int;
}

type brain = Room of int | Search of int
type settings = { riders : int; humans : int; brain : brain; arenas : layout list }
type game = { round : round; scores : int list; settings : settings; round_no : int }

type scene = Title | Playing of game | Winner of game
type model = scene Scene2d.t

(* a cycle moves one cell every [step] frames *)
let step = 2
let rounds_to_win = 3
let boost_max = 240

(* where the riders start: the first two facing each other across the
 * middle, the other two above and below it *)
let starts = [ (size / 4, size / 2, Right); (3 * size / 4, size / 2, Left); (size / 2, size / 4, Down); (size / 2, 3 * size / 4, Up) ]

let new_round (settings : settings) (round_no : int) : round =
  let cycles =
    List.mapi
      (fun i (col, row, dir) ->
        { col; row; dir; wanted = dir; mark = Char.chr (Char.code '1' + i); corners = [ (col, row) ]; alive = true;
          energy = boost_max; boosting = false })
      (List.filteri (fun i _ -> i < settings.riders) starts)
  in
  let layout = List.nth settings.arenas ((round_no - 1) mod List.length settings.arenas) in
  let arena = List.fold_left (fun a c -> Tilemap.set a c.col c.row c.mark) (arena_of layout) cycles in
  { arena; cycles; over = None; pause = 0; frames = 0 }

let new_game (settings : settings) : game =
  { round = new_round settings 1; scores = List.init settings.riders (fun _ -> 0); settings; round_no = 1 }

let classic (humans : int) : settings = { riders = 2; humans; brain = Room 600; arenas = [ List.hd layouts ] }
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The computer: the flood fill *)
(*****************************************************************************)

let free (arena : Tilemap.t) ((c, r) : int * int) : bool = Tilemap.get arena c r = Some ' '

(* How much room from (c, r): the free cells reachable from it, counted
 * by a flood fill (a breadth-first search), up to [limit] (enough to
 * tell a dead end from open space, and cheaper) *)
let room (arena : Tilemap.t) (start : int * int) (limit : int) : int =
  let seen = Hashtbl.create 256 in
  let queue = Queue.create () in
  if free arena start then begin Hashtbl.replace seen start (); Queue.push start queue end;
  while (not (Queue.is_empty queue)) && Hashtbl.length seen < limit do
    let c, r = Queue.pop queue in
    List.iter
      (fun d ->
        let dc, dr = delta d in
        let n = (c + dc, r + dr) in
        if free arena n && not (Hashtbl.mem seen n) then begin Hashtbl.replace seen n (); Queue.push n queue end)
      [ Up; Down; Left; Right ]
  done;
  Hashtbl.length seen

(* the three ways a cycle can go, straight on first *)
let ways (d : dir) : dir list = d :: List.filter (fun d' -> d' <> d && d' <> opposite d) [ Up; Down; Left; Right ]

(* the way with the most room, straight on when it's as good; room
 * counted up to [limit] cells: the less, the later a trap is noticed *)
let computer_turn ?(limit = 600) (arena : Tilemap.t) (me : cycle) : dir =
  let score d =
    let dc, dr = delta d in
    room arena (me.col + dc, me.row + dr) limit
  in
  List.fold_left (fun best d -> if score d > score best then d else best) me.dir (ways me.dir)

(*****************************************************************************)
(* The computer: a search *)
(*****************************************************************************)

(* claude: The search sees the arena as bytes, a cell per byte (1: taken),
 * copied from the Tilemap once per decision: a search visits thousands
 * of positions, and each asks about hundreds of cells. The cells taken
 * during the search are marked in it while a position is scored, then
 * unmarked. The breadth-first searches below share their arrays,
 * allocated once: [seen] holds, per cell, the number of the search that
 * last reached it, so nothing needs clearing between two. *)
let cells = size * size
let index ((c, r) : int * int) : int = (r * size) + c
let seen = Array.make cells 0
let whose = Array.make cells 0
let dist = Array.make cells 0
let queue = Array.make cells 0
let search_no = ref 0

let neighbors (k : int) : int list = [ k - size; k + size; k - 1; k + 1 ]

(* the free cells reachable from [start] (its own cell taken), up to
 * [limit] *)
let room_in (grid : Bytes.t) (start : int) (limit : int) : int =
  incr search_no;
  let n = !search_no in
  seen.(start) <- n;
  queue.(0) <- start;
  let head = ref 0 and tail = ref 1 and count = ref 0 in
  while !head < !tail && !count < limit do
    let k = queue.(!head) in
    incr head;
    List.iter
      (fun k' ->
        if Bytes.get grid k' = '\000' && seen.(k') <> n then begin
          seen.(k') <- n; queue.(!tail) <- k'; incr tail; incr count
        end)
      (neighbors k)
  done;
  !count

(* The Voronoi partition of the arena between two heads: the cells each
 * reaches strictly first, grown from both at once (a cell both reach
 * at the same step is nobody's). It is what the best bots of the 2010
 * Google AI Challenge scored a position by: room is not how much space
 * there is around you but how much of it is yours -- the cells you'd
 * get to before him. Looking [radius] cells away is enough to see who
 * is cutting whom off. *)
let voronoi_in (grid : Bytes.t) (me : int) (foe : int) (radius : int) : int =
  incr search_no;
  let n = !search_no in
  let score = ref 0 in
  let visit k owner d =
    seen.(k) <- n; whose.(k) <- owner; dist.(k) <- d
  in
  visit me 1 0; visit foe 2 0;
  queue.(0) <- me; queue.(1) <- foe;
  let head = ref 0 and tail = ref 2 in
  while !head < !tail do
    let k = queue.(!head) in
    incr head;
    let owner = whose.(k) and d = dist.(k) + 1 in
    if owner <> 3 && d <= radius then
      List.iter
        (fun k' ->
          if Bytes.get grid k' = '\000' then
            if seen.(k') <> n then begin
              visit k' owner d; queue.(!tail) <- k'; incr tail;
              score := !score + (if owner = 1 then 1 else -1)
            end
            else if dist.(k') = d && whose.(k') <> owner && whose.(k') <> 3 then begin
              (* reached by both at the same step: nobody's *)
              score := !score - (if whose.(k') = 1 then 1 else -1);
              whose.(k') <- 3
            end)
        (neighbors k)
  done;
  !score

let voronoi (free : int * int -> bool) (me : int * int) (foe : int * int) : int =
  let grid = Bytes.init cells (fun k -> if free (k mod size, k / size) then '\000' else '\001') in
  Bytes.set grid (index me) '\001';
  Bytes.set grid (index foe) '\001';
  voronoi_in grid (index me) (index foe) 24

(* a position's worth to the searching rider: its territory (the
 * Voronoi partition, 30 cells around), and its room -- the free cells
 * it can still reach, up to 150, minus the other's. The territory alone
 * is short-sighted where the two are walled off from each other and
 * neither has territory the other could take: then what counts is the
 * room each has left, which is all the flood fill of [computer_turn]
 * looks at. Measured, before the room was counted: the search lost to
 * the flood fill. *)
let worth (grid : Bytes.t) (me : int) (foe : int) : float =
  float_of_int (voronoi_in grid me foe 30 + room_in grid me 150 - room_in grid foe 150)

(* a position of the search: the two heads and their ways, the cells
 * taken since it began, and whose move it is. The moves are
 * simultaneous in the game; here the searching rider moves first and
 * its move is only [pending] until the other's is known, when both are
 * made and a crash of either -- or both into the same cell -- ends it *)
type ending = Riding | Me_out | Foe_out | Both_out

type position = {
  me : int * int;
  mdir : dir;
  foe : int * int;
  fdir : dir;
  taken : (int * int) list;
  pending : (int * int) option;
  ending : ending;
}

let search_turn ~(depth : int) (arena : Tilemap.t) (cycles : cycle list) (i : int) : dir =
  let me = List.nth cycles i in
  let others = List.filter (fun c -> c.alive && c.mark <> me.mark) cycles in
  match List.sort (fun a b -> compare (abs (a.col - me.col) + abs (a.row - me.row)) (abs (b.col - me.col) + abs (b.row - me.row))) others with
  | [] -> computer_turn arena me
  | foe :: _ ->
      let grid = Bytes.init cells (fun k -> if free arena (k mod size, k / size) then '\000' else '\001') in
      let free_in (p : position) (cr : int * int) = Bytes.get grid (index cr) = '\000' && not (List.mem cr p.taken) in
      (* scored with the cells taken since the search began marked *)
      let scored (taken : (int * int) list) (m : int * int) (f : int * int) =
        List.iter (fun cr -> Bytes.set grid (index cr) '\001') taken;
        let v = worth grid (index m) (index f) in
        List.iter (fun cr -> Bytes.set grid (index cr) '\000') taken;
        v
      in
      let go (c, r) d = let dc, dr = delta d in (c + dc, r + dr) in
      let game : (position, dir) Minimax.game =
        { moves = (fun p -> if p.ending <> Riding then [] else ways (if p.pending = None then p.mdir else p.fdir));
          play =
            (fun p d ->
              match p.pending with
              | None -> { p with pending = Some (go p.me d); mdir = d }
              | Some m ->
                  let f = go p.foe d in
                  let me_out = (not (free_in p m)) || m = f and foe_out = (not (free_in p f)) || m = f in
                  let ending = match (me_out, foe_out) with true, true -> Both_out | true, false -> Me_out | false, true -> Foe_out | _ -> Riding in
                  { me = m; mdir = p.mdir; foe = f; fdir = d; taken = m :: f :: p.taken; pending = None; ending });
          score =
            (fun p ->
              match p.ending with
              | Both_out -> -1000.
              | Me_out -> -100000.
              | Foe_out -> 100000.
              | Riding -> (
                  match p.pending with
                  | Some m when not (free_in p m) -> -100000.
                  | Some m -> scored (m :: p.taken) m p.foe
                  | None -> scored p.taken p.me p.foe));
          max_to_play = (fun p -> p.pending = None) }
      in
      let start = { me = (me.col, me.row); mdir = me.dir; foe = (foe.col, foe.row); fdir = foe.dir; taken = []; pending = None; ending = Riding } in
      (match (Minimax.alphabeta game ~depth start).best with Some d -> d | None -> me.dir)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let player1_wants (k : keyboard) (current : dir) : dir =
  if k.kup then Up else if k.kdown then Down else if k.kleft then Left else if k.kright then Right else current

let player2_wants (k : keyboard) (current : dir) : dir =
  if k.kw then Up else if k.ks then Down else if k.ka then Left else if k.kd then Right else current

(* the boost's key: space for the arrows' rider, e for w/a/s/d's *)
let boost_key (i : int) (k : keyboard) : bool = if i = 0 then k.kspace else Set_.mem "e" k.keys

(* a turn, unless it's straight back into its own trail; a turn is a
 * corner of the trail *)
let turn (c : cycle) : cycle =
  if c.wanted = opposite c.dir || c.wanted = c.dir then c
  else { c with dir = c.wanted; corners = (c.col, c.row) :: c.corners }

let advance (c : cycle) : cycle =
  let dc, dr = delta c.dir in
  { c with col = c.col + dc; row = c.row + dr }

(* One step of the cycles that move this frame: they move at once; one
 * entering a cell that isn't free crashes, and two entering the same
 * cell both crash, head-on. A crashed cycle stops where it was, its
 * trail left on the arena. When one is left riding (or none), the
 * round is over, the arena as it was at the crash. *)
let step_round (r : round) (moving : cycle -> bool) : round =
  let moved = List.map (fun c -> if c.alive && moving c then Some (advance (turn c)) else None) r.cycles in
  let cells = List.filter_map (Option.map (fun c -> (c.col, c.row))) moved in
  let crashed (m : cycle option) =
    match m with
    | Some c -> (not (free r.arena (c.col, c.row))) || List.length (List.filter (( = ) (c.col, c.row)) cells) > 1
    | None -> false
  in
  let crashes = List.map crashed moved in
  if not (List.mem true crashes) then
    let cycles = List.map2 (fun c m -> Option.value m ~default:c) r.cycles moved in
    let arena = List.fold_left (fun a m -> match m with Some c -> Tilemap.set a c.col c.row c.mark | None -> a) r.arena moved in
    { r with arena; cycles }
  else
    let alive = List.map2 (fun c crash -> c.alive && not crash) r.cycles crashes in
    let riding = List.length (List.filter Fun.id alive) in
    if riding <= 1 then
      { r with cycles = List.map2 (fun c a -> { c with alive = a }) r.cycles alive; over = Some (List.map (fun a -> if a then 1 else 0) alive); pause = 90 }
    else
      let cycles = List.map2 (fun c (m, crash) -> if crash then { c with alive = false } else Option.value m ~default:c) r.cycles (List.combine moved crashes) in
      let arena =
        List.fold_left2 (fun a m crash -> match m with Some c when not crash -> Tilemap.set a c.col c.row c.mark | _ -> a) r.arena moved crashes
      in
      { r with arena; cycles }

let update_game (k : keyboard) (g : game) : game =
  let r = g.round in
  let r = { r with frames = r.frames + 1 } in
  match r.over with
  | Some _ when r.pause > 0 -> { g with round = { r with pause = r.pause - 1 } }
  | Some _ -> { g with round = new_round g.settings (g.round_no + 1); round_no = g.round_no + 1 }
  | None ->
      let cycles =
        List.mapi
          (fun i c ->
            if not c.alive then c
            else if i < g.settings.humans then
              let wanted = if i = 0 then player1_wants k c.wanted else player2_wants k c.wanted in
              let boosting = boost_key i k && c.energy >= 4 in
              { c with wanted; boosting; energy = (if boosting then c.energy - 4 else min boost_max (c.energy + 1)) }
            else
              let wanted =
                if r.frames mod step <> 0 then c.wanted
                else match g.settings.brain with Room limit -> computer_turn ~limit r.arena c | Search depth -> search_turn ~depth r.arena r.cycles i
              in
              { c with wanted })
          r.cycles
      in
      let r = { r with cycles } in
      let r = if r.frames mod step = 0 || List.exists (fun c -> c.boosting) cycles then step_round r (fun c -> c.boosting || r.frames mod step = 0) else r in
      (match r.over with
      | Some points -> { g with round = r; scores = List.map2 ( + ) g.scores points }
      | None -> { g with round = r })

let winner (g : game) : int option =
  if g.round.pause <> 0 then None
  else
    let best = List.fold_left max 0 g.scores in
    if best < rounds_to_win then None
    else List.find_map (fun (i, s) -> if s = best then Some i else None) (List.mapi (fun i s -> (i, s)) g.scores)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let key name = Scene2d.pressed (fun k -> Set_.mem name k.keys) s in
  match s.scene with
  | Title ->
      if key "1" then Scene2d.go (Playing (new_game (classic 1))) s
      else if key "2" then Scene2d.go (Playing (new_game (classic 2))) s
      else s
  | Playing g ->
      let g = update_game computer.keyboard g in
      if winner g <> None then Scene2d.go (Winner g) s else { s with scene = Playing g }
  | Winner _ -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go Title s else s
