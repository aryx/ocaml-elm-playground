(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Bomberman (Hudson Soft, 1983; the NES version, 1985):
 * a grid of pillars and soft blocks, bombs exploding in a cross. Arrows
 * to move, space to drop a bomb. Two games, chosen on the title screen:
 *
 *  - STAGE, the NES game: blow up the balloons, find the exit hidden
 *    under a block, and walk into it; another block hides a power-up, a
 *    longer fire. Don't stand in the fire, and don't touch the balloons.
 *  - BATTLE, the mode that made the series famous (it was already in
 *    the PC Engine's Bomberman, 1990): you against three computer
 *    bombers in an arena, the last one standing wins the round, the
 *    first to three rounds the game. Burnt blocks sometimes leave a
 *    power-up: one more bomb at once, a longer fire, more speed.
 *
 * The second maze game, and the maze kit's second user (gamekits/maze/):
 * the bombers move like Pac-Man (Grid_move: along the corridors, the
 * turn asked for early remembered), the balloons wander like Pac-Man's
 * blue ghosts (Chase.at_random: at each tile, a random way, but never
 * back). What's new here is the explosion ([blast]): the fire spreads
 * from the bomb in the four directions, tile by tile, up to its range,
 * stopped by a pillar, and by the first soft block, which it destroys;
 * a bomb it reaches explodes too, at once: chain reactions, the game's
 * best moments, in a loop until no bomb is left to catch fire.
 *
 *            #                 the fire of a bomb (o) of range 2:
 *         +  F  #              stopped by the pillar above, destroying
 *      .  F  F  o  F  F  .     the block on the left of its path, and
 *            F                 reaching the bomb on the right, which
 *            F                 explodes in turn
 *
 * The stages and the arenas are ours, written as text, 15x13. No
 * randomness: the balloons' random ways come from Pac-Man's own
 * generator (Chase.next_random), and so does what a block hides in
 * battle, so a game can be replayed.
 *
 * The battle's computer bombers are written on the ai/ layer: Bot (a
 * reaction delay and a rate of decisions, which are the level chosen on
 * the title screen), Fsm (escape, hunt, dig) and Pathfind (the way to
 * safety or to a target, breadth-first over the tiles). Not Sense: the
 * whole arena is in view, for them as for you, and hiding is not part
 * of this game. Their real problem is the genre's own, and it is all in
 * [danger]: where the fire *will* be -- every bomb's cross, chains
 * included -- so as never to walk into it, to get out of it in time,
 * and never to drop a bomb without a way out ([safe_drop]).
 *
 * Exercises: the power-ups the series added (the kick, the remote
 * detonator, the bomb pass), the other enemies (faster, walking through
 * blocks), a timer, a second human player on w/a/s/d, and the battle
 * over a network with plan_networking_teaching.md (Saturn Bomberman,
 * 1996: ten players).
 *)
open Playground

(*****************************************************************************)
(* The stage *)
(*****************************************************************************)

(* '#' a pillar or a wall, '+' a soft block, and two blocks hiding
 * something: 'E' the exit, 'F' a fire power-up (once revealed: 'e',
 * 'f'); 'P' where the bomber starts, 'B' the balloons *)
let stage_rows =
  [ "###############";
    "#P  ++ + ++   #";
    "# #+# #+#+#+# #";
    "#  ++   +  + +#";
    "#+# # #+# #+#+#";
    "#+ +  +++ + + #";
    "# #+#+# # #+#+#";
    "#   + +  E+   #";
    "#+# #+# #+# # #";
    "# ++ + +F +B+ #";
    "# # # #+#+# #+#";
    "#   +  B +  +B#";
    "###############" ]

let t = 60 (* a tile, in pixels *)
let stage = Tilemap.of_strings (float_of_int t) stage_rows
let grid : Grid_move.grid = { tile = t; cols = Tilemap.cols stage; rows = Tilemap.rows stage }
let bounds = Tilemap.bounds stage

let start = match Tilemap.find stage 'P' with p :: _ -> p | [] -> (1, 1)

(* the blocks, whatever they hide, and the floor, whatever lies on it
 * (the exit, and the power-ups: 'f' fire, 'b' a bomb more, 's' speed) *)
let is_block (c : char option) : bool = c = Some '+' || c = Some 'E' || c = Some 'F'
let is_floor (c : char option) : bool = c = Some ' ' || c = Some 'e' || c = Some 'f' || c = Some 'b' || c = Some 's'

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a bomb: where, how long before it goes off, whose it is (it counts
 * against its owner's bombs at once), and how far its fire will go *)
type bomb = { col : int; row : int; timer : int; owner : int; reach : int }

type game = {
  map : Tilemap.t; (* the blocks left, and what they revealed *)
  bomber : Grid_move.mover;
  bombs : bomb list;
  fire : ((int * int) * int) list; (* burning tiles, and for how long *)
  balloons : Grid_move.mover list;
  range : int; (* how far the fire goes *)
  lives : int;
  score : int;
  dying : int; (* > 0: caught, for that long *)
  rng : int;
  frames : int;
}

(* the stage, with the bomber and the balloons taken out of the map *)
let new_game () : game =
  let map =
    List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') stage (Tilemap.find stage 'P' @ Tilemap.find stage 'B')
  in
  { map; bomber = Grid_move.mover_at grid start; bombs = []; fire = [];
    balloons = List.map (Grid_move.mover_at grid) (Tilemap.find stage 'B');
    range = 1; lives = 3; score = 0; dying = 0; rng = 1; frames = 0 }

(*****************************************************************************)
(* Bombs and fire *)
(*****************************************************************************)

let bomb_on (bombs : bomb list) (cr : int * int) : bool = List.exists (fun b -> (b.col, b.row) = cr) bombs
let bomb_at (g : game) (cr : int * int) : bool = bomb_on g.bombs cr

(* the fire of a bomb of [reach] at (col, row): its tile, and in each
 * direction the tiles up to [reach], stopping before a pillar, and at
 * the first block (included: it burns) *)
let fire_from (map : Tilemap.t) (reach : int) (col, row) : (int * int) list =
  let rec spread (dc, dr) n acc =
    if n > reach then acc
    else
      let cr = (col + (dc * n), row + (dr * n)) in
      match Tilemap.get map (fst cr) (snd cr) with
      | c when is_block c -> cr :: acc
      | c when is_floor c -> spread (dc, dr) (n + 1) (cr :: acc)
      | _ -> acc
  in
  (col, row) :: List.concat_map (fun d -> spread d 1 []) [ (0, -1); (0, 1); (-1, 0); (1, 0) ]

let fire_of (g : game) (cr : int * int) : (int * int) list = fire_from g.map g.range cr

(* Explosions: every bomb whose timer is out explodes; its fire sets off
 * the bombs it reaches, which explode in the same frame, and so on
 * (chain reactions); the blocks in the fire [burn]. The map after, the
 * tiles on fire, the bombs left, and the blocks that burned. *)
let rec blast ~(burn : Tilemap.t -> int * int -> Tilemap.t) (map : Tilemap.t) (bombs : bomb list) :
    Tilemap.t * (int * int) list * bomb list * (int * int) list =
  match List.partition (fun b -> b.timer <= 0) bombs with
  | [], _ -> (map, [], bombs, [])
  | exploding, rest ->
      let tiles = List.concat_map (fun b -> fire_from map b.reach (b.col, b.row)) exploding in
      let rest = List.map (fun b -> if List.mem (b.col, b.row) tiles then { b with timer = 0 } else b) rest in
      let burnt = List.filter (fun (c, r) -> is_block (Tilemap.get map c r)) tiles in
      let map', tiles', bombs', burnt' = blast ~burn (List.fold_left burn map tiles) rest in
      (map', tiles @ tiles', bombs', burnt @ burnt')

(* the stage's blocks: burnt away, or revealing what they hid *)
let burn_stage (m : Tilemap.t) (c, r) : Tilemap.t =
  match Tilemap.get m c r with
  | Some '+' -> Tilemap.set m c r ' '
  | Some 'E' -> Tilemap.set m c r 'e'
  | Some 'F' -> Tilemap.set m c r 'f'
  | _ -> m

let explode (g : game) : game =
  let map, tiles, bombs, burnt = blast ~burn:burn_stage g.map g.bombs in
  { g with bombs; map; fire = List.map (fun cr -> (cr, 30)) tiles @ g.fire; score = g.score + (10 * List.length burnt) }

let burning (g : game) (cr : int * int) : bool = List.exists (fun (f, _) -> f = cr) g.fire

(*****************************************************************************)
(* Walking *)
(*****************************************************************************)

(* one step of a bomber wanting [dir] (Stop: none), at [speed] pixels a
 * frame, on the floor and not onto a bomb -- except the one just
 * dropped under him, to walk away from it. With no direction wanted
 * it stops at the next center (unlike Pac-Man, who runs on) *)
let walk (map : Tilemap.t) (bombs : bomb list) (dir : Grid_move.dir) (speed : int) (m : Grid_move.mover) : Grid_move.mover =
  let here = Grid_move.tile_of grid m in
  let open_ cr = (is_floor (Tilemap.get map (fst cr) (snd cr)) && not (bomb_on bombs cr)) || cr = here in
  if dir <> Stop then Grid_move.move_player grid ~open_ speed { m with wanted = dir }
  else if Grid_move.at_center grid m then { m with dir = Stop; wanted = Stop }
  else Grid_move.slide grid ~choose:(fun m -> { m with dir = Stop }) speed { m with wanted = Stop }

(* the arrow held (Stop: none) *)
let arrow (k : keyboard) : Grid_move.dir =
  if k.kup then Up else if k.kdown then Down else if k.kleft then Left else if k.kright then Right else Stop

(*****************************************************************************)
(* The stage's update *)
(*****************************************************************************)

let balloon_open (g : game) (cr : int * int) : bool =
  is_floor (Tilemap.get g.map (fst cr) (snd cr)) && not (bomb_at g cr)

let update_game (s : 'scene Scene2d.t) (computer : computer) (g : game) : game =
  let g = { g with frames = g.frames + 1; rng = Chase.next_random g.rng } in
  if g.dying > 0 then
    if g.dying > 1 then { g with dying = g.dying - 1 }
    else
      (* back to the start, the blocks as they are *)
      { g with dying = 0; lives = g.lives - 1; bomber = Grid_move.mover_at grid start; bombs = []; fire = [] }
  else
    let b = walk g.map g.bombs (arrow computer.keyboard) 3 g.bomber in
    let here = Grid_move.tile_of grid b in
    let g = { g with bomber = b } in
    (* a bomb where the bomber stands, two at most at once (the
     * original starts with one, and a power-up adds more): with two,
     * chain reactions *)
    let g =
      if Scene2d.pressed (fun k -> k.kspace) s && List.length g.bombs < 2 && not (bomb_at g here) then
        { g with bombs = { col = fst here; row = snd here; timer = 150; owner = 0; reach = g.range } :: g.bombs }
      else g
    in
    (* the power-up *)
    let g =
      if Tilemap.get g.map (fst here) (snd here) = Some 'f' then
        { g with range = g.range + 1; map = Tilemap.set g.map (fst here) (snd here) ' ' }
      else g
    in
    let g = { g with bombs = List.map (fun b -> { b with timer = b.timer - 1 }) g.bombs } |> explode in
    let g = { g with fire = List.filter_map (fun (cr, n) -> if n > 1 then Some (cr, n - 1) else None) g.fire } in
    let balloons =
      List.mapi (fun i m -> Grid_move.slide grid ~choose:(Chase.at_random grid ~open_:(balloon_open g) (g.rng + (i * 7))) 2 m) g.balloons
    in
    (* the fire gets the balloons in it *)
    let dead, balloons = List.partition (fun m -> burning g (Grid_move.tile_of grid m)) balloons in
    let g = { g with balloons; score = g.score + (100 * List.length dead) } in
    let caught = burning g here || List.exists (fun m -> Grid_move.tile_of grid m = here) g.balloons in
    if caught then { g with dying = 90 } else g

(*****************************************************************************)
(* The battle's arenas *)
(*****************************************************************************)

(* claude: The battle's arenas, as text too: '#' a pillar or a wall, '+'
 * a soft block (what it hides decided when it burns: [hidden]), '1' to
 * '4' where the bombers start, each in a corner with room to drop a
 * first bomb and step round a corner out of its fire. The rounds go
 * from one to the next. *)
let arenas : (string * Tilemap.t) array =
  Array.map
    (fun (name, rows) ->
      if List.exists (fun r -> String.length r <> 15) rows || List.length rows <> 13 then failwith ("TinyBomberman: " ^ name);
      (name, Tilemap.of_strings (float_of_int t) rows))
    [| ( "CLASSIC",
         [ "###############";
           "#1 +++++++++ 2#";
           "# #+#+#+#+#+# #";
           "#++++ +++ ++++#";
           "#+#+#+# #+#+#+#";
           "#+++ +++++ +++#";
           "#+#+# #+# #+#+#";
           "#+++ +++++ +++#";
           "#+#+#+# #+#+#+#";
           "#++++ +++ ++++#";
           "# #+#+#+#+#+# #";
           "#3 +++++++++ 4#";
           "###############" ] );
       (* a walled room in the middle, open north and south *)
       ( "THE RING",
         [ "###############";
           "#1   +++++   2#";
           "# ## +++++ ## #";
           "# #+ + + + +# #";
           "#  + ## ## +  #";
           "#++ +#   #+ ++#";
           "#++++#   #++++#";
           "#++ +#   #+ ++#";
           "#  + ## ## +  #";
           "# #+ + + + +# #";
           "# ## +++++ ## #";
           "#3   +++++   4#";
           "###############" ] );
       (* two open lanes crossing in the middle: long lines of fire *)
       ( "THE CROSS",
         [ "###############";
           "#1 ++++ ++++ 2#";
           "# #+#+# #+#+# #";
           "#++++++ ++++++#";
           "#+#+#+# #+#+#+#";
           "#++++++ ++++++#";
           "#             #";
           "#++++++ ++++++#";
           "#+#+#+# #+#+#+#";
           "#++++++ ++++++#";
           "# #+#+# #+#+# #";
           "#3 ++++ ++++ 4#";
           "###############" ] ) |]

(* What a burnt block leaves: most nothing, some a power-up -- decided
 * by where it is and the round, with the stage's generator, so the
 * same round hides the same things *)
let hidden (round : int) ((c, r) : int * int) : char =
  match Chase.next_random (Chase.next_random ((c * 31) + (r * 17) + (round * 101))) mod 12 with
  | 0 | 1 -> 'b'
  | 2 | 3 -> 'f'
  | 4 -> 's'
  | _ -> ' '

(*****************************************************************************)
(* The battle's model *)
(*****************************************************************************)

type bomber = {
  idx : int; (* 0 you, 1 to 3 the computer *)
  m : Grid_move.mover;
  alive : bool; (* false once burnt, [dying] down to 0 *)
  dying : int;
  most : int; (* bombs at once *)
  reach : int; (* its fire's *)
  speed : int; (* pixels a frame *)
  wins : int;
}

(* where the computer wants to be: a tile, or anywhere out of the fire
 * to come; the way there is found by its feet ([feet]) *)
type goal = Tile of (int * int) | Safety | Here

(* what a bomber asks for: a direction and a bomb -- and, the
 * computer, where it wants to be *)
type order = { dir : Grid_move.dir; drop : bool; goal : goal }

type level = Easy | Normal | Hard

(* what a computer bomber is doing: getting out of the fire to come,
 * going for someone, or digging through the blocks (for the power-ups
 * they hide, and to open a way) *)
type mood = Escape | Hunt | Dig

(* what it sees: all of it, as you do (the arena is in view), but seen
 * [delay] frames ago (Bot.mli) *)
type senses = {
  me : int;
  at : int * int;
  map : Tilemap.t;
  bombs : bomb list;
  fire : (int * int) list;
  danger : (int * int) list; (* where the fire is, or will be *)
  foes : (int * int) list; (* the others standing *)
  bombs_left : int;
  reach : int;
  mind : mood Fsm.run;
}

type battle = {
  arena_no : int;
  map : Tilemap.t;
  bombers : bomber list;
  bombs : bomb list;
  fire : ((int * int) * int) list;
  clock : int;
  ended : int option; (* frames since the round was decided *)
  round_no : int;
  level : level;
  minds : (senses, order) Bot.running array;
}

let rounds_to_win = 3
let round_time = 60 * 150 (* frames: past it, a draw *)

let standing (b : bomber) : bool = b.alive && b.dying = 0

let arena_map (n : int) : Tilemap.t =
  let map = snd arenas.(n) in
  List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') map (List.concat_map (fun ch -> Tilemap.find map ch) [ '1'; '2'; '3'; '4' ])

let start_of (n : int) (idx : int) : int * int =
  match Tilemap.find (snd arenas.(n)) (Char.chr (Char.code '1' + idx)) with cr :: _ -> cr | [] -> (1, 1)

let still : order = { dir = Stop; drop = false; goal = Here }

let new_round (level : level) (round_no : int) (wins : int list) : battle =
  let arena_no = (round_no - 1) mod Array.length arenas in
  { arena_no; map = arena_map arena_no;
    bombers =
      List.mapi
        (fun idx wins -> { idx; m = Grid_move.mover_at grid (start_of arena_no idx); alive = true; dying = 0; most = 1; reach = 2; speed = 3; wins })
        wins;
    bombs = []; fire = []; clock = 0; ended = None; round_no; level; minds = Array.init 4 (fun _ -> Bot.start still) }

let new_battle (level : level) : battle = new_round level 1 [ 0; 0; 0; 0 ]

(*****************************************************************************)
(* The computer's bombers, on ai/ *)
(*****************************************************************************)

(* Where the fire is, or will be: the tiles burning now, and the cross
 * of every bomb on the ground as if they all went off now -- chains
 * included, since a bomb in another's cross goes with it, whatever its
 * own timer says. Blocks stop the fire here; they'll burn, but what
 * lies behind them is safe until they have. *)
let danger (map : Tilemap.t) (bombs : bomb list) (fire : (int * int) list) : (int * int) list =
  let _, tiles, _, _ = blast ~burn:(fun m _ -> m) map (List.map (fun b -> { b with timer = 0 }) bombs) in
  fire @ tiles

let walkable (map : Tilemap.t) (bombs : bomb list) (fire : (int * int) list) (cr : int * int) : bool =
  is_floor (Tilemap.get map (fst cr) (snd cr)) && (not (bomb_on bombs cr)) && not (List.mem cr fire)

(* The way from [from] to the nearest tile where [goal] holds, through
 * the tiles [ok] allows (the first one excepted: the bomber is on it,
 * bomb or not), breadth-first (Pathfind.mli): every step costs the
 * same, so the first found is a shortest one. [] if there is none. *)
let way ~(ok : int * int -> bool) ~(goal : int * int -> bool) (from : int * int) : (int * int) list =
  let problem : (int * int) Pathfind.problem =
    { neighbors = (fun (c, r) -> List.filter_map (fun n -> if ok n then Some (n, 1.) else None) [ (c, r - 1); (c, r + 1); (c - 1, r); (c + 1, r) ]);
      goal; estimate = (fun _ -> 0.) }
  in
  (Pathfind.breadth_first problem from).path

(* Can a bomb be dropped here and walked away from? With it, the fire
 * to come is its cross too; there must be a way, through tiles not
 * burning, to a tile out of it all, a few steps away -- a bomb goes off
 * in 150 frames, and a bomber walks a tile in 20 at the start. It is
 * the rule that keeps the computer from blowing itself up, and the one
 * a beginner forgets. *)
let safe_drop (map : Tilemap.t) (bombs : bomb list) (fire : (int * int) list) ~(reach : int) (at : int * int) : bool =
  let bombs' = { col = fst at; row = snd at; timer = 150; owner = -1; reach } :: bombs in
  let danger' = danger map bombs' fire in
  match way ~ok:(walkable map bombs' fire) ~goal:(fun cr -> not (List.mem cr danger')) at with
  | [] -> false
  | path -> List.length path <= 7

let in_line (s : senses) (foe : int * int) : bool = List.mem foe (fire_from s.map s.reach s.at)
let reachable (s : senses) (goal : int * int -> bool) : (int * int) list = way ~ok:(walkable s.map s.bombs s.fire) ~goal s.at
let next_to_block (s : senses) ((c, r) : int * int) : bool =
  List.exists (fun (c', r') -> is_block (Tilemap.get s.map c' r')) [ (c, r - 1); (c, r + 1); (c - 1, r); (c + 1, r) ]

(* three moods, and the rules between them: the fire to come first,
 * always; out of it -- and for eight frames more, or a bomber steps
 * back into a cross it has just left -- hunting if someone can be
 * reached, digging if not *)
let moods : (mood, senses) Fsm.machine =
  let in_danger s = List.mem s.at s.danger in
  let can_reach s = reachable s (fun cr -> List.mem cr s.foes) <> [] in
  [
    { from = Hunt; label = "fire coming"; guard = (fun s _ -> in_danger s); target = Escape };
    { from = Dig; label = "fire coming"; guard = (fun s _ -> in_danger s); target = Escape };
    { from = Escape; label = "safe"; guard = (fun s since -> (not (in_danger s)) && since >= 8 && can_reach s); target = Hunt };
    { from = Escape; label = "safe"; guard = (fun s since -> (not (in_danger s)) && since >= 8 && not (can_reach s)); target = Dig };
    { from = Dig; label = "a way through"; guard = (fun s _ -> can_reach s); target = Hunt };
    { from = Hunt; label = "walled off"; guard = (fun s _ -> not (can_reach s)); target = Dig };
  ]

let senses_of (was : senses option) ((b, idx) : battle * int) : senses =
  let me = List.find (fun (x : bomber) -> x.idx = idx) b.bombers in
  let fire = List.map fst b.fire in
  let s =
    { me = idx; at = Grid_move.tile_of grid me.m; map = b.map; bombs = b.bombs; fire;
      danger = danger b.map b.bombs fire;
      foes = List.filter_map (fun (x : bomber) -> if x.idx <> idx && standing x then Some (Grid_move.tile_of grid x.m) else None) b.bombers;
      bombs_left = me.most - List.length (List.filter (fun (x : bomb) -> x.owner = idx) b.bombs);
      reach = me.reach;
      mind = (match was with Some s -> s.mind | None -> Fsm.start Dig) }
  in
  { s with mind = Fsm.step moods s s.mind }

(* The tactics, from the senses: a tile to go to, and whether to drop a
 * bomb where it stands. Its feet ([feet]) find the way there, and check
 * the bomb again, on the arena as it is now. *)
let decide (s : senses) : order =
  let target path = match List.rev path with goal :: _ -> Tile goal | [] -> Here in
  let can_drop = s.bombs_left > 0 && (not (bomb_on s.bombs s.at)) && safe_drop s.map s.bombs s.fire ~reach:s.reach s.at in
  match s.mind.state with
  | Escape -> { still with goal = Safety }
  | Hunt ->
      (* someone in its line of fire: the bomb, and the escape will
       * follow; else closer, to the nearest one it can reach *)
      if List.exists (in_line s) s.foes && can_drop then { still with drop = true }
      else { still with goal = target (reachable s (fun cr -> List.mem cr s.foes)) }
  | Dig -> (
      (* a power-up in reach first; else a bomb against a block; else
       * to the nearest tile next to one *)
      match reachable s (fun (c, r) -> List.mem (Tilemap.get s.map c r) [ Some 'b'; Some 'f'; Some 's' ]) with
      | _ :: _ as path when List.length path <= 8 -> { still with goal = target path }
      | _ ->
          if next_to_block s s.at && can_drop then { still with drop = true }
          else { still with goal = target (reachable s (next_to_block s)) })

(* Its feet (Bot.mli's reflex): on the arena as it is *now*, not as it
 * was seen [delay] frames ago -- the way to the goal found from where
 * it stands, never into the fire to come unless it stands in it
 * already (then the way out may cross it), and the bomb dropped only
 * if it still can be walked away from. Without them a late decision
 * walks it into a cross that was not there when it decided, and the
 * way to a goal, corrected from where it stood a moment ago,
 * oscillates (TinyBoomerangFu.ml met both). A decision is changed only
 * at a tile's center, where the maze kit turns.
 *
 * And two things it knows at once, without waiting for its senses:
 * where its own bombs are -- it dropped them -- and that a bomb is
 * dropped to be run from. So a drop is followed by the run to safety
 * straight away, and standing in its own bombs' fire it runs, whatever
 * the late decision says. Measured, before: most computer bombers died
 * by their own bomb, starting to run when their senses, 12 frames
 * later, told them it was there -- 7 tiles away from safety is 140
 * frames, and a fuse 150. Someone else's bomb it still sees late: that
 * is what the level is. *)
let feet ((b, idx) : battle * int) (o : order) : order =
  let me = List.find (fun (x : bomber) -> x.idx = idx) b.bombers in
  let fire = List.map fst b.fire in
  let at = Grid_move.tile_of grid me.m in
  let danger_now = danger b.map b.bombs fire in
  let mine = List.length (List.filter (fun (x : bomb) -> x.owner = idx) b.bombs) in
  let own = danger b.map (List.filter (fun (x : bomb) -> x.owner = idx) b.bombs) [] in
  (* a decision is repeated until the next one (Bot.mli): the bomb it
   * decided on, dropped, is not dropped again on the way out, where it
   * would shut the way *)
  let drop = o.drop && mine < me.most && (not (List.mem at own)) && safe_drop b.map b.bombs fire ~reach:me.reach at in
  let risky = List.mem at danger_now in
  let goal = if drop || List.mem at own then Safety else o.goal in
  let ok cr = walkable b.map b.bombs fire cr && (risky || not (List.mem cr danger_now)) in
  let arrive = match goal with Tile g -> fun cr -> cr = g | Safety -> fun cr -> not (List.mem cr danger_now) | Here -> fun _ -> true in
  let dir =
    if arrive at && goal <> Safety || (goal = Safety && not risky) then if Grid_move.at_center grid me.m then Grid_move.Stop else me.m.dir
    else if not (Grid_move.at_center grid me.m) then me.m.dir
    else
      match way ~ok ~goal:arrive at with
      | _ :: (c, r) :: _ -> if c > fst at then Grid_move.Right else if c < fst at then Left else if r > snd at then Down else Up
      | _ -> Stop
  in
  { dir; drop; goal }

(* the level: how late it sees, and how often it changes its mind --
 * Bot.mli's honest knobs, and nothing else *)
let mind : (battle * int, senses, order) Bot.t = Bot.make ~delay:12 ~rate:6 ~reflex:feet ~sense:senses_of ~decide ()

let mind_of (l : level) : (battle * int, senses, order) Bot.t =
  match l with Easy -> { mind with delay = 24; rate = 10 } | Normal -> mind | Hard -> { mind with delay = 5; rate = 3 }

(*****************************************************************************)
(* The battle's update *)
(*****************************************************************************)

(* a burnt block in battle: gone, leaving what it hid; a power-up in the
 * fire burns too *)
let burn_battle (round : int) (m : Tilemap.t) (c, r) : Tilemap.t =
  match Tilemap.get m c r with
  | Some '+' -> Tilemap.set m c r (hidden round (c, r))
  | Some ('b' | 'f' | 's') -> Tilemap.set m c r ' '
  | _ -> m

let step_battle (s : 'scene Scene2d.t) (k : keyboard) (b : battle) : battle =
  let minds = Array.copy b.minds in
  let orders =
    List.map
      (fun (x : bomber) ->
        if not (standing x) then still
        else if x.idx = 0 then { still with dir = arrow k; drop = Scene2d.pressed (fun k -> k.kspace) s }
        else
          let o, running = Bot.step (mind_of b.level) (b, x.idx) minds.(x.idx) in
          minds.(x.idx) <- running;
          o)
      b.bombers
  in
  (* the moves, then the bombs dropped *)
  let bombers = List.map2 (fun o (x : bomber) -> if standing x then { x with m = walk b.map b.bombs o.dir x.speed x.m } else x) orders b.bombers in
  let bombs =
    List.fold_left2
      (fun bombs o (x : bomber) ->
        let here = Grid_move.tile_of grid x.m in
        let mine = List.length (List.filter (fun (y : bomb) -> y.owner = x.idx) bombs) in
        if standing x && o.drop && mine < x.most && not (bomb_on bombs here) then
          { col = fst here; row = snd here; timer = 150; owner = x.idx; reach = x.reach } :: bombs
        else bombs)
      b.bombs orders bombers
  in
  (* the power-ups picked up *)
  let map, bombers =
    List.fold_left
      (fun (map, acc) (x : bomber) ->
        let c, r = Grid_move.tile_of grid x.m in
        let x, taken =
          if not (standing x) then (x, false)
          else
            match Tilemap.get map c r with
            | Some 'b' -> ({ x with most = x.most + 1 }, true)
            | Some 'f' -> ({ x with reach = x.reach + 1 }, true)
            | Some 's' -> ({ x with speed = min 5 (x.speed + 1) }, true)
            | _ -> (x, false)
        in
        ((if taken then Tilemap.set map c r ' ' else map), acc @ [ x ]))
      (b.map, []) bombers
  in
  (* the bombs' fuses, the explosions, the fire dying down *)
  let bombs = List.map (fun (x : bomb) -> { x with timer = x.timer - 1 }) bombs in
  let map, tiles, bombs, _ = blast ~burn:(burn_battle b.round_no) map bombs in
  let fire = List.map (fun cr -> (cr, 30)) tiles @ List.filter_map (fun (cr, n) -> if n > 1 then Some (cr, n - 1) else None) b.fire in
  (* the fire gets whoever stands in it *)
  let bombers =
    List.map
      (fun (x : bomber) ->
        if standing x && List.mem_assoc (Grid_move.tile_of grid x.m) fire then { x with dying = 60 }
        else if x.dying > 1 then { x with dying = x.dying - 1 }
        else if x.dying = 1 then { x with dying = 0; alive = false }
        else x)
      bombers
  in
  let clock = b.clock + 1 in
  let ended =
    match b.ended with
    | Some n -> Some (n + 1)
    | None -> if List.length (List.filter (fun (x : bomber) -> x.alive) bombers) <= 1 || clock > round_time then Some 0 else None
  in
  { b with map; bombers; bombs; fire; clock; ended; minds }

(* the round's point, to the one left standing (nobody, on a draw) *)
let score_round (b : battle) : battle =
  match List.filter (fun (x : bomber) -> x.alive) b.bombers with
  | [ w ] -> { b with bombers = List.map (fun (x : bomber) -> if x.idx = w.idx then { x with wins = x.wins + 1 } else x) b.bombers }
  | _ -> b

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type mode = Stage | Battle

type scene =
  | Title of mode * level
  | Playing of game
  | Game_over of int
  | Cleared of int
  | Fighting of battle
  | Champion of battle

type model = scene Scene2d.t

let initial_model : model = Scene2d.start (Title (Stage, Normal))

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  let key f = Scene2d.pressed f s in
  match s.scene with
  | Title (mode, level) ->
      (* up and down: the game; left and right: the battle's level *)
      let mode = if key (fun k -> k.kup) then Stage else if key (fun k -> k.kdown) then Battle else mode in
      let levels = [| Easy; Normal; Hard |] in
      let i = match level with Easy -> 0 | Normal -> 1 | Hard -> 2 in
      let i = if key (fun k -> k.kleft) then max 0 (i - 1) else if key (fun k -> k.kright) then min 2 (i + 1) else i in
      let level = levels.(i) in
      if space then Scene2d.go (match mode with Stage -> Playing (new_game ()) | Battle -> Fighting (new_battle level)) s
      else { s with scene = Title (mode, level) }
  | Playing g ->
      let g = update_game s computer g in
      let col, row = Grid_move.tile_of grid g.bomber in
      if g.lives = 0 then Scene2d.go (Game_over g.score) s
      else if g.balloons = [] && Tilemap.get g.map col row = Some 'e' && g.dying = 0 then Scene2d.go (Cleared g.score) s
      else { s with scene = Playing g }
  | Fighting b -> (
      let b = step_battle s computer.keyboard b in
      match b.ended with
      | Some n when n > 120 ->
          let b = score_round b in
          if List.exists (fun (x : bomber) -> x.wins >= rounds_to_win) b.bombers then Scene2d.go (Champion b) s
          else { s with scene = Fighting (new_round b.level (b.round_no + 1) (List.map (fun (x : bomber) -> x.wins) b.bombers)) }
      | _ -> { s with scene = Fighting b })
  | Game_over _ | Cleared _ -> if space then Scene2d.go (Title (Stage, Normal)) s else s
  | Champion b -> if space then Scene2d.go (Title (Battle, b.level)) s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let block_art =
  [ "############"; "#.....#....."; "#.....#....."; "############"; "...#.....#.."; "...#.....#..";
    "############"; "#.....#....."; "#.....#....."; "############"; "...#.....#.."; "...#.....#.." ]

let block = Sprite.pixels 5. [ ('#', rgb 120 70 40); ('.', rgb 190 120 70) ] block_art

(* a power-up: a colored tile and its sign *)
let power_up (c : color) (sign : shape) : shape = group [ square c 44.; sign ]

let tile (c : char) : shape =
  match c with
  | '#' -> group [ square (rgb 110 110 120) 60.; square (rgb 160 160 170) 48. ]
  | '+' | 'E' | 'F' -> block
  | 'e' -> group [ square (rgb 60 60 60) 50.; rectangle (rgb 20 20 20) 30. 44. |> move_y (-3.) ]
  | 'f' -> power_up (rgb 250 120 30) (triangle yellow 16. |> rotate 90.)
  | 'b' -> power_up (rgb 70 110 230) (group [ circle black 13.; rectangle (rgb 250 200 40) 3. 8. |> move 6. 13. ])
  | 's' -> power_up (rgb 60 190 90) (group [ triangle white 10. |> rotate (-90.) |> move_x (-6.); triangle white 10. |> rotate (-90.) |> move_x 6. ])
  | _ -> group []

(* the bomber: a helmet and suit of [suit], pink face; two frames when
 * walking *)
let bomber_frames_of (suit : color) =
  let top = [ "...WWWW..."; "..WWWWWW.."; "..WPPPPW.."; "..WPKPKW.."; "...PPPP..."; ".BBWWWWBB." ] in
  List.map
    (Sprite.pixels 5. [ ('W', suit); ('P', rgb 250 180 160); ('K', black); ('B', rgb 60 90 220) ])
    [ top @ [ ".B.WWWW.B."; "...BBBB..."; "..PP..PP.." ]; top @ [ ".B.WWWW.B."; "...BBBB..."; "...PP.PP.." ] ]

let bomber_frames = bomber_frames_of white

(* the battle's four: you in white, the computer in black, red, green *)
let suits = [| white; rgb 50 50 60; rgb 220 60 60; rgb 70 180 90 |]
let suit_names = [| "YOU"; "BLACK"; "RED"; "GREEN" |]

(* the color of each one's name: its suit's, black's lightened to be
 * read on the dark background *)
let name_color (i : int) : color = if i = 1 then rgb 150 150 165 else suits.(i)
let battle_frames = Array.map bomber_frames_of suits

let balloon =
  Sprite.pixels 5. [ ('O', rgb 250 140 60); ('W', white); ('K', black) ]
    [ "...OOOO..."; ".OOOOOOOO."; "OOWWOOWWOO"; "OOWKOOWKOO"; "OOOOOOOOOO"; ".OOOOOOOO."; "..OO..OO.."; ".O..OO..O." ]

let at (m : Grid_move.mover) (shape : shape) : shape =
  let x, y = Grid_move.to_world grid bounds m in
  shape |> move x y

let cell ((col, row) : int * int) (shape : shape) : shape =
  let x, y = Tilemap.center stage col row in
  shape |> move x y

let text color size str = words color str |> scale size

let bomb_shape (b : bomb) : shape =
  let pulse = 1. +. (0.08 *. sin (float_of_int b.timer /. 4.)) in
  cell (b.col, b.row) (group [ circle black 22.; rectangle (rgb 250 200 40) 4. 12. |> move 10. 22. ] |> scale pulse)

let fire_shape ((cr, _) : (int * int) * int) : shape = cell cr (group [ square (rgb 250 110 20) 60.; square (rgb 255 220 60) 34. ])

let ground = rectangle (rgb 60 140 60) (bounds.right -. bounds.left) (bounds.top -. bounds.bottom)

let view_game (g : game) : shape list =
  let bomber =
    if g.dying > 0 then (if g.frames / 6 mod 2 = 0 then List.hd bomber_frames else group []) |> rotate (float_of_int g.dying *. 8.)
    else Sprite.cycle (if g.bomber.dir = Stop then 0 else g.frames / 8) bomber_frames
  in
  [ ground; Tilemap.view tile g.map ]
  @ List.map bomb_shape g.bombs
  @ List.map fire_shape g.fire
  @ List.map (fun m -> at m balloon) g.balloons
  @ [ at g.bomber bomber;
      text white 3. (Printf.sprintf "LIVES %d   SCORE %d   FIRE %d" g.lives g.score g.range) |> move_y (bounds.top +. 40.) ]

let view_battle (s : model) (b : battle) : shape list =
  let frames = b.clock in
  let bomber_shape (x : bomber) =
    let fr = battle_frames.(x.idx) in
    if not x.alive then group []
    else if x.dying > 0 then (if frames / 6 mod 2 = 0 then List.hd fr else group []) |> rotate (float_of_int x.dying *. 10.) |> at x.m
    else at x.m (Sprite.cycle (if x.m.dir = Stop then 0 else frames / 8) fr)
  in
  let score =
    List.mapi
      (fun i (x : bomber) ->
        text (name_color x.idx) 2.4 (Printf.sprintf "%s %d" suit_names.(x.idx) x.wins)
        |> move (bounds.left +. 90. +. (float_of_int i *. 230.)) (bounds.top +. 40.))
      b.bombers
  in
  let banner =
    match b.ended with
    | None -> []
    | Some _ -> (
        match List.filter (fun (x : bomber) -> x.alive) b.bombers with
        | [ w ] -> [ text (if w.idx = 0 then yellow else name_color w.idx) 5. (if w.idx = 0 then "YOU WIN THE ROUND" else suit_names.(w.idx) ^ " WINS THE ROUND") ]
        | _ -> Scene2d.blink 0.6 s [ text white 5. "DRAW" ])
  in
  [ ground; Tilemap.view tile b.map ]
  @ List.map bomb_shape b.bombs
  @ List.map fire_shape b.fire
  @ List.map bomber_shape b.bombers
  @ score
  @ [ text gray 2. (Printf.sprintf "ROUND %d -- %s" b.round_no (fst arenas.(b.arena_no))) |> move_y (bounds.bottom -. 30.) ]
  @ banner

let level_name = function Easy -> "EASY" | Normal -> "NORMAL" | Hard -> "HARD"

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 30 30 40) screen.width screen.height
  ::
  (match s.scene with
  | Title (mode, level) ->
      let chosen m = if m = mode then yellow else gray in
      [ text white 7. "TINY BOMBERMAN" |> move_y 250.;
        List.hd bomber_frames |> scale 2. |> move (-60.) 100.; balloon |> scale 2. |> move 60. 100.;
        text (chosen Stage) 3. "STAGE" |> move_y (-20.);
        text (chosen Battle) 3. "BATTLE" |> move_y (-70.) ]
      @ (if mode = Battle then
           List.mapi
             (fun i l -> text (if l = level then yellow else gray) (if l = level then 2.6 else 2.2) (level_name l) |> move ((float_of_int i -. 1.) *. 170.) (-120.))
             [ Easy; Normal; Hard ]
         else [])
      @ [ text gray 2.2 "up / down: the game   left / right: the computer's level" |> move_y (-170.);
          text gray 2.2 "arrows: move   space: bomb" |> move_y (-205.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-260.) ]
  | Playing g -> view_game g
  | Fighting b -> view_battle s b
  | Champion b ->
      let w = List.fold_left (fun best (x : bomber) -> if x.wins > best.wins then x else best) (List.hd b.bombers) b.bombers in
      view_battle s b
      @ [ text (if w.idx = 0 then yellow else name_color w.idx) 7. (if w.idx = 0 then "YOU WIN!" else suit_names.(w.idx) ^ " WINS!") |> move_y 60. ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-60.) ]
  | Game_over score ->
      [ text red 7. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-100.) ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-200.) ]
  | Cleared score ->
      [ text yellow 7. "STAGE CLEAR!"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-100.) ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-200.) ])

let app = game view update initial_model

let main = Playground_platform.run_app app
