(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Boomerang Fu (Cranky Watermelon, 2020): four foods
 * in one arena, one boomerang each, one hit kills. You are the avocado;
 * the other three are the computer. Arrows move; hold space and they
 * only turn you, to aim, and let it go to throw; x dashes, z jumps.
 *
 * Everything in the game follows from one rule: your only weapon leaves
 * your hand.
 *
 *   holding it          thrown                    caught again
 *   +-----------+       +------------------+      +-----------+
 *   | armed,    | space | unarmed, for as  | the  | armed,    |
 *   | but only  | ----> | long as it takes | arc  | but only  |
 *   | at arm's  |       | to come back:    | ---> | at arm's  |
 *   | length    |       | only the dash    |      | length    |
 *   +-----------+       +------------------+      +-----------+
 *
 * So a throw is not "firing", it is spending: for the second and a half
 * the boomerang is out you have nothing to fight with, and the dash is
 * all your defence. Hold it instead and the dash becomes a slash, which
 * kills -- but only at arm's length. The whole game is that trade, and
 * the reason the arena is small enough that someone unarmed can always
 * be reached.
 *
 * Three rules do it, and they are all in [step_rangs] and [cuts]:
 *   - the way out, the boomerang cuts *everyone*, its own thrower
 *     included. That single line is where the comedy of the original
 *     comes from: bounce one off the fence and it can come back through
 *     you;
 *   - the way back it is harmless to its owner, who catches it;
 *   - and it comes back to where the owner *is*, not to where the throw
 *     started (the return leg steers, every frame, at a moving target).
 *     Which is why you can throw and run, and why no two flights are
 *     alike.
 *
 * A real boomerang returns by gyroscopic precession, the lift on the
 * leading blade turning the spin axis. None of that is here: the way
 * out, the velocity is simply turned two degrees a frame and slowed by
 * a drag, and when it has slowed enough it homes. It is the arc that
 * has to read as a boomerang, not the aerodynamics -- the same choice
 * as everywhere else in this repository (TinySlingshot.ml's
 * parabola, gamekits/racing's bicycle car).
 *
 * The arenas are the original's: islands floating over nothing, with
 * holes through them, water, a terrace to fight from, and, on the
 * second one, a river crossed by two bridges. They are written as text
 * ([garden], [river]), one character a cell, and the rounds alternate
 * between the two (the flag map=river starts on the second). Three
 * heights are all the levels there are: a half-unit step you walk up,
 * a one-unit terrace you jump up onto (or climb by its steps), and
 * anything lower you simply walk off. The jump is also what carries
 * you over a hole or the narrow part of the river -- jump and dash
 * together over the wide part -- and, timed, over a boomerang: it flies
 * at head height, and the cut is tested in three dimensions.
 *
 * The camera is the other thing the genre decides for you. A party game
 * on one screen cannot follow anybody, so it frames *everybody*
 * ([framing]): high and nearly isometric, it zooms in as the fight
 * closes and out as the players scatter, and at the end of a round on
 * the one left standing -- Boomerang Fu's camera, and in 2D
 * TinyXpilot.ml's [frame]. Its lineage is the fixed one-screen
 * arena, from Bomberman (1983) to Samurai Gunn and TowerFall (both
 * 2013); TinyBomberman.ml is this game's 2D ancestor in this
 * repository, and the same shape: four players, one screen, one hit.
 *
 * Being high costs one thing, and it is paid with one quad per flying
 * object: at this angle you cannot tell a boomerang at head height, or
 * a food in the middle of a jump, from one on the grass, so everything
 * that leaves the ground drags a [shadow] under it, exactly as
 * TinyMario64.ml's Mario does for the same reason.
 *
 * The computer plays with the [intent] a human sends -- a direction, a
 * throw, a dash -- and nothing else: no extra speed, no knowing where a
 * boomerang will be. Its three players differ only in four numbers
 * ([wits]): how close each likes to be, how straight a shot it wants,
 * how early it dodges, and how obliquely it comes at you. It is written
 * on the ai/ layer and only on it (TinyPacman.ml and TinySoldat.ml are
 * where a hand-written computer and one on ai/ are compared): Sense
 * decides what it sees -- an enemy only when no stone or cliff is
 * between them, remembered for a second and a half after that -- Bot
 * makes it act on what it saw six frames ago and change its mind twenty
 * times a second rather than sixty, Fsm holds dodge / hunt / keep away
 * and the rules between them, hysteresis included, as a guard rather
 * than as an if, and when the way to its man is not straight (the
 * river, the terrace's cliff) Pathfind's A* finds one over the arena's
 * cells ([route]). It never jumps.
 *
 * What stays out of it is Steering: its verbs are forces on
 * Physics bodies, and these characters have no velocity to steer --
 * they move at a fixed speed and their dash is committed, on purpose.
 * (Steering.direction is the form for characters like these; the
 * one thing it would replace here, [clear_way], is not a steering
 * behaviour but an obstacle check, so it stays.) Uses Camera3d (its
 * [follow] smooths the zoom), Scene2d, and from ai/ Pathfind, Sense, Bot
 * and Fsm; no gamekit, and no physics engine: a jump is two lines of
 * gravity.
 *
 * Exercises: the original's power-ups (fire, ice, and above all the
 * teleport, which drops you where your boomerang is -- the one that
 * best fits the core rule: your weapon left you, so follow it), a
 * second human player on w/a/s/d, letting anybody catch anybody's
 * boomerang, a computer that jumps (a jump is an edge of the path two
 * cells long, over a hole), arenas that move, bridges that break.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The arenas *)
(*****************************************************************************)

(* claude: An arena is a grid of 2-unit cells, written as text, one
 * character a cell:
 *
 *   .  grass        /  a step (half a unit up)   :  the terrace (one unit)
 *   #  a stone      ~  water                      _  a hole, onto the void
 *   =  a bridge     1 2 3 4  where each food starts, on the grass
 *
 * and everything off the grid is the void too: you can walk off the
 * edge of the world, as in the original. *)
type cell = Ground of number (* its height *) | Wall | Water | Hole | Bridge

type arena = { name : string; cols : int; rows : int; cells : cell array array; starts : (number * number) list }

let tile = 2.
let wall_height = 1.7
let water_level = -0.3
let step_up = 0.55 (* the most one walks up without jumping: a step, not the terrace *)

(* the middle of cell (c, r), the grid centered on the origin *)
let center_of ~(cols : int) ~(rows : int) ((c, r) : int * int) : number * number =
  (((float_of_int c +. 0.5) -. (float_of_int cols /. 2.)) *. tile, ((float_of_int r +. 0.5) -. (float_of_int rows /. 2.)) *. tile)

let parse (name : string) (lines : string list) : arena =
  let rows = List.length lines and cols = String.length (List.hd lines) in
  if List.exists (fun l -> String.length l <> cols) lines then failwith ("TinyBoomerangFu: a ragged row in " ^ name);
  let cell_of = function
    | '.' | '1' .. '4' -> Ground 0.
    | '/' -> Ground 0.5
    | ':' -> Ground 1.
    | '#' -> Wall
    | '~' -> Water
    | '=' -> Bridge
    | _ -> Hole
  in
  let at = List.concat (List.mapi (fun r l -> List.init cols (fun c -> (l.[c], (c, r)))) lines) in
  let start d = match List.assoc_opt d at with Some cr -> center_of ~cols ~rows cr | None -> failwith ("TinyBoomerangFu: no start in " ^ name) in
  { name; cols; rows; cells = Array.of_list (List.map (fun l -> Array.init cols (fun c -> cell_of l.[c])) lines);
    starts = List.map start [ '1'; '2'; '3'; '4' ] }

(* the old arena grown into a garden: fenced, but open in the middle of
 * each side, where you can walk off the world; four holes in the grass;
 * and a terrace in the middle, with a hole through it, reached by its
 * steps from the north and the south and by a jump from the east and
 * the west *)
let garden =
  parse "THE GARDEN"
    [ "#####....#####";
      "#.1........2.#";
      "#..._....._..#";
      "#.#..////..#.#";
      "....::::::....";
      "_...::__::..._";
      "_...::__::..._";
      "....::::::....";
      "#.#..////..#.#";
      "#..._....._..#";
      "#.3........4.#";
      "#####....#####" ]

(* a river between two banks: two bridges, a narrow place in the middle
 * you can jump (the wide one takes a jump and a dash), two terraces on
 * each bank, holes along both edges and no fence at all *)
let river =
  parse "THE RIVER"
    [ "__.....~~...__";
      "_.1....~~..2._";
      "_..#...~~.#.._";
      "::/....==../::";
      "::.....~~...::";
      "_......~....._";
      "_..__..~..__._";
      "::....~~....::";
      "::/...==.../::";
      "_..#..~~..#.._";
      "_.3...~~...4._";
      "__....~~....__" ]

let arenas = [| garden; river |]

let cell_of (a : arena) (x : number) (z : number) : int * int =
  ( int_of_float (Float.floor ((x /. tile) +. (float_of_int a.cols /. 2.))),
    int_of_float (Float.floor ((z /. tile) +. (float_of_int a.rows /. 2.))) )

let cell (a : arena) ((c, r) : int * int) : cell =
  if c < 0 || r < 0 || c >= a.cols || r >= a.rows then Hole else a.cells.(r).(c)

let cell_at (a : arena) (x : number) (z : number) : cell = cell a (cell_of a x z)

(* what one can stand on there, and how high *)
let floor_of : cell -> number option = function Ground h -> Some h | Bridge -> Some 0. | Wall | Water | Hole -> None
let floor_at (a : arena) (x : number) (z : number) : number option = floor_of (cell_at a x z)

(* how high what is there reaches: what a boomerang bounces off, and a
 * look is stopped by *)
let top_of : cell -> number = function
  | Ground h -> h
  | Bridge -> 0.
  | Wall -> wall_height
  | Water -> water_level
  | Hole -> Float.neg_infinity

let top_at (a : arena) (x : number) (z : number) : number = top_of (cell_at a x z)

(* a stone, or ground more than a step above feet at [y], under any
 * corner of a body of [r]: what stops a player. Holes and water stop
 * nobody -- you fall in *)
let blocked (a : arena) ~(y : number) (r : number) (x : number) (z : number) : bool =
  List.exists
    (fun (dx, dz) -> match cell_at a (x +. dx) (z +. dz) with Wall -> true | c -> top_of c > y +. step_up)
    [ (-.r, -.r); (r, -.r); (-.r, r); (r, r) ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type food = Avocado | Strawberry | Lemon | Blueberry

let foods = [ Avocado; Strawberry; Lemon; Blueberry ]
let food_color = function
  | Avocado -> rgb 130 190 70
  | Strawberry -> rgb 235 60 80
  | Lemon -> rgb 245 210 60
  | Blueberry -> rgb 120 115 225

let food_name = function Avocado -> "AVOCADO" | Strawberry -> "STRAWBERRY" | Lemon -> "LEMON" | Blueberry -> "BLUEBERRY"

(* what is inside one, which is only ever seen on a cut face *)
let flesh_color = function
  | Avocado -> rgb 222 228 150
  | Strawberry -> rgb 250 195 195
  | Lemon -> rgb 250 240 170
  | Blueberry -> rgb 195 180 235

(* the two halves of someone who has been cut, flying apart *)
type half = { hx : number; hy : number; hz : number; hvx : number; hvy : number; hvz : number; hspin : number; htop : bool }

type state = Alive | Cut of half list | Falling of int (* frames down a hole, or into the water *)

type player = {
  idx : int;
  kind : food;
  px : number;
  py : number; (* its feet: the ground's height, or higher in a jump *)
  pz : number;
  vy : number; (* 0 on the ground *)
  heading : number; (* where it faces, degrees: 0 towards -z, 90 towards +x *)
  holds : bool; (* its boomerang in its hand *)
  dash : int; (* frames of dash left *)
  cool : int; (* frames before the next dash *)
  think : int; (* the computer's: frames before it throws again *)
  aim : int; (* frames its feet have been planted, aiming *)
  way : number * number; (* the direction it walked last frame (see [clear_way]) *)
  state : state;
  wins : int;
}

(* a boomerang in the air: out (dangerous to everyone), or coming back
 * (harmless to its owner, who catches it) *)
type leg = Out | Back

type rang = {
  rx : number;
  ry : number; (* its height: a head's, above the ground it flies over *)
  rz : number;
  rvx : number;
  rvz : number;
  owner : int;
  leg : leg;
  bounced : bool; (* off the fence or a pillar: see [cuts] *)
  away : bool; (* it has been a body's length or three from its thrower *)
  age : int;
}

(* What one player asks for this frame: where to go, whether to keep
 * its feet planted while it turns that way to aim, and the buttons. A
 * human fills it from the keyboard, the computer from [decide];
 * nothing downstream knows which. The computer may also name a place
 * to walk to, [toward], whose way its feet find ([footing]). *)
type intent = { go : (number * number) option; plant : bool; throw : bool; dash_now : bool; jump : bool; toward : (number * number) option }

(* what the computer is doing: three states and the rules between them
 * are [modes], below *)
type mode = Dodge | Hunt | Away

(* and what it may know. The mode lives in here
 * because Bot hands the last senses to the next sensing (Bot.mli):
 * a bot's memory -- where it last saw someone, and what it was doing
 * about it -- is part of what it senses, not something the game keeps
 * for it *)
type senses = {
  arena_no : int; (* the ground under its feet, which it may know *)
  at : number * number;
  facing : number;
  armed : bool; (* its boomerang in its hand *)
  cool : int;
  think : int;
  seed : int; (* which cook: its wits, and its own wobble *)
  last_way : number * number;
  (* one target per other cook, by its number, and the one it thinks
   * about ([Sense.focus]) *)
  watched : (int * (number * number) Sense.target) list;
  enemy : (number * number) Sense.target;
  (* a boomerang on its way at it: how far along its line, how far off
   * it, and the line's direction *)
  incoming : (number * number * number * number) option;
  mind : mode Fsm.run;
}

type game = {
  arena_no : int; (* in [arenas] *)
  players : player list;
  rangs : rang list;
  ended : int option; (* frames since the round was decided *)
  clock : int; (* frames this round has lasted *)
  round_no : int;
  (* claude: one per cook: the senses it has seen but not yet acted
   * on, and the intent it is repeating (Bot.mli) *)
  minds : (senses, intent) Bot.running array;
  cam : camera; (* where [framing] has got to: it glides, see [step_game] *)
}

type scene = Title | Playing of game | Winner of game
type model = scene Scene2d.t

let rounds_to_win = 3
let time_up = 60 * 45 (* frames a round may last *)

let alive (p : player) : bool = match p.state with Alive -> true | _ -> false
let d2 (ax : number) (az : number) (bx : number) (bz : number) : number =
  let dx = ax -. bx and dz = az -. bz in
  (dx *. dx) +. (dz *. dz)

(*****************************************************************************)
(* The camera *)
(*****************************************************************************)

(* claude: Everybody standing, in as close a shot as holds them all.
 * [k] is the whole of it, the camera's distance as a fraction of the
 * one that shows the entire arena: the players' spread plus a margin,
 * between a close-up and the full view. The depth counts a little more
 * than the width (x1.4), the screen being wider than it is tall. And
 * the closer the shot, the more it centers on the players rather than
 * on the arena: all the way out, the camera does not move at all.
 *
 *      scattered: k = 1.2            together: k = 0.6
 *   +-----------------------+     +-----------------------+
 *   |  A . . . . . . . . S  |     |                       |
 *   |  . . . :::::: . . .   |     |      A   S            |
 *   |  . . . :::::: . . .   |     |        L              |
 *   |  L . . . . . . . . B  |     |                   B   |
 *   +-----------------------+     +-----------------------+
 *
 * The last one standing gets the close-up: the round's end zooms onto
 * the winner, as in the original. *)
let zoom_in = 0.5
let zoom_out = 1.2

let framing (players : player list) : camera =
  let ps = match List.filter alive players with [] -> players | ps -> ps in
  let lo f = List.fold_left (fun m p -> Float.min m (f p)) Float.infinity ps in
  let hi f = List.fold_left (fun m p -> Float.max m (f p)) Float.neg_infinity ps in
  let x (p : player) = p.px and z (p : player) = p.pz in
  let spread = Float.max (hi x -. lo x) (1.4 *. (hi z -. lo z)) in
  let k = Float.max zoom_in (Float.min zoom_out ((spread +. 9.) /. 24.)) in
  let pull = (zoom_out -. k) /. (zoom_out -. zoom_in) in
  let cx = (lo x +. hi x) /. 2. *. pull and cz = (lo z +. hi z) /. 2. *. pull in
  Camera3d.from_far ~fov:34. ~offset:(0., 27. *. k, 29. *. k) (cx, 0., cz +. 1.)

(*****************************************************************************)
(* A new game *)
(*****************************************************************************)

(* a player at its start, armed, facing the middle *)
let place (a : arena) (p : player) : player =
  let x, z = List.nth a.starts p.idx in
  let heading = atan2 (-.x) z *. 180. /. Float.pi in
  { p with px = x; py = 0.; pz = z; vy = 0.; heading; holds = true; dash = 0; cool = 0; think = 45; aim = 0; way = Camera3d.forward heading; state = Alive }

let new_game ?(arena_no = 0) () : game =
  let players =
    List.mapi
      (fun i kind ->
        place arenas.(arena_no)
          { idx = i; kind; px = 0.; py = 0.; pz = 0.; vy = 0.; heading = 0.; holds = true; dash = 0; cool = 0; think = 45; aim = 0; way = (0., 1.);
            state = Alive; wins = 0 })
      foods
  in
  { arena_no; players; rangs = []; ended = None; clock = 0; round_no = 1;
    minds = Array.init 4 (fun _ -> Bot.start { go = None; plant = false; throw = false; dash_now = false; jump = false; toward = None });
    cam = framing players }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Intents *)
(*****************************************************************************)

let idle : intent = { go = None; plant = false; throw = false; dash_now = false; jump = false; toward = None }

(* Space held, the feet stay planted and the arrows only turn you: you
 * aim; let it go, and it is thrown -- the original's way, and the only
 * way to aim across the water or a hole, where walking the way you
 * aim would take you in. It is thrown on the release only after a
 * frame of aiming, so that the space that started the game throws
 * nothing. *)
let keys_intent (s : model) (k : keyboard) (p : player) : intent =
  let dx = (if k.kright then 1. else 0.) -. if k.kleft then 1. else 0. in
  let dz = (if k.kdown then 1. else 0.) -. if k.kup then 1. else 0. in
  { go = (if dx = 0. && dz = 0. then None else Some (dx, dz));
    plant = k.kspace;
    throw = s.before.kspace && (not k.kspace) && p.aim > 0;
    toward = None;
    dash_now = Scene2d.pressed (fun k -> Set_.mem "x" k.keys) s;
    jump = Scene2d.pressed (fun k -> Set_.mem "z" k.keys) s }

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

let radius = 0.55 (* a food's *)
let speed = 0.155
let dash_speed = 0.42
let dash_frames = 9
let dash_cool = 45
let slash_dist = 1.5 (* the dash of someone still holding their boomerang *)

(* a jump: 1.5 units up (the terrace is 1) and 24 frames in the air,
 * which walking carries 3.7 units -- a hole of one cell, the narrow
 * part of the river; with a dash in the air, the wide part *)
let gravity = 0.022
let jump_speed = 0.26
let rang_height = 0.78 (* a boomerang flies this high above the ground *)

let norm (dx, dz) =
  let n = Float.hypot dx dz in
  if n < 0.0001 then (0., 0.) else (dx /. n, dz /. n)

let heading_of (dx, dz) : number = atan2 dx (-.dz) *. 180. /. Float.pi

(* the stones and the cliffs stop a body of [r] with its feet at [y];
 * one axis at a time, so you slide along a stone instead of sticking
 * to it *)
let slide (a : arena) (y : number) (r : number) (x : number) (z : number) (dx : number) (dz : number) : number * number =
  let x' = if blocked a ~y r (x +. dx) z then x else x +. dx in
  let z' = if blocked a ~y r x' (z +. dz) then z else z +. dz in
  (x', z')

(* One player's frame: the dash is committed (its direction is locked
 * for its 9 frames, like every move in TinyTombRaider.ml), and
 * a throw leaves the hand empty. *)
let step_player (a : arena) (it : intent) (p : player) : player * rang option =
  let starting = p.dash = 0 && it.dash_now && p.cool = 0 in
  let dash = if starting then dash_frames else max 0 (p.dash - 1) in
  let cool = if starting then dash_cool else max 0 (p.cool - 1) in
  (* you face where you walk -- which is also how you aim; or, feet
   * planted, you only turn *)
  let planted = it.plant && p.holds && dash = 0 in
  let heading = match it.go with Some d when p.dash = 0 -> heading_of (norm d) | _ -> p.heading in
  let dx, dz =
    if dash > 0 then
      let fx, fz = Camera3d.forward heading in
      (fx *. dash_speed, fz *. dash_speed)
    else if planted then (0., 0.)
    else match it.go with None -> (0., 0.) | Some d -> let ux, uz = norm d in (ux *. speed, uz *. speed)
  in
  let px, pz = slide a p.py radius p.px p.pz dx dz in
  let way = match it.go with Some d -> norm d | None -> p.way in
  (* claude: up and down: gravity every frame, a jump only from the
   * ground, and the ground catches whoever comes down onto it -- or
   * walks up a step onto it, which is the same test (a step is less
   * than [step_up] above the feet). Walk off the terrace and nothing
   * holds you: you are in the air, and come down on the grass *)
  let standing = p.vy = 0. && floor_at a p.px p.pz = Some p.py in
  let vy = if standing && it.jump then jump_speed else p.vy -. gravity in
  let py, vy =
    match floor_at a px pz with
    | Some h when p.py +. vy <= h && p.py >= h -. step_up -> (h, 0.)
    | _ -> (p.py +. vy, vy)
  in
  let aim = if planted then p.aim + 1 else 0 in
  let p = { p with px; py; pz; vy; heading; dash; cool; way; aim; think = max 0 (p.think - 1) } in
  (* over a hole or the water, and below its rim: nothing holds you up *)
  if floor_at a px pz = None && py < -0.25 then ({ p with state = Falling 0 }, None)
  else if it.throw && p.holds then
    (* it leaves the hand, unless the hand is in a stone: thrown with
     * your back to one it would start inside it, and bounce at once *)
    let fx, fz = Camera3d.forward heading in
    let ry = py +. rang_height in
    let hx = px +. (fx *. 0.5) and hz = pz +. (fz *. 0.5) in
    let rx, rz = if top_at a hx hz > ry +. 0.3 then (px, pz) else (hx, hz) in
    ( { p with holds = false; aim = 0 },
      Some { rx; ry; rz; rvx = fx *. 0.58; rvz = fz *. 0.58; owner = p.idx; leg = Out; bounced = false; away = false; age = 0 } )
  else (p, None)

(*****************************************************************************)
(* The boomerang *)
(*****************************************************************************)

let curve = 2.2 (* degrees the flight turns each frame, going out *)
let drag = 0.975
let turn_back = 0.2 (* the speed at which the way out becomes the way back *)
let back_speed = 0.62
let steer = 0.18 (* how fast the way back aims at the owner, who moves *)
let catch_dist = 0.9
let cut_dist = 0.95

let turn_vec (deg : number) (vx, vz) =
  let a = deg *. Float.pi /. 180. in
  let c = cos a and s = sin a in
  ((c *. vx) -. (s *. vz), (s *. vx) +. (c *. vz))

(* Going out: a curve, a drag, and a bounce off the fence and the
 * stones (which is how a throw can end up in the thrower). It keeps to
 * a head's height above the ground it flies over, rising over a step
 * or the terrace's edge and sinking off it: a throw from the grass
 * reaches someone on the terrace -- bounce it off the cliff instead,
 * and the terrace is a fortress nobody below can touch -- and a throw
 * from the terrace flies over the stones, which is the high ground's
 * advantage. Coming back: it steers at its owner and sails over
 * everything -- a wall must never be able to keep your weapon from
 * you. *)
let step_rang (a : arena) (players : player list) (r : rang) : rang =
  let owner = List.find (fun p -> p.idx = r.owner) players in
  match r.leg with
  | Out ->
      let vx, vz = turn_vec curve (r.rvx, r.rvz) in
      let vx = vx *. drag and vz = vz *. drag in
      (* bounce: whichever axis the obstacle is on, that component flips *)
      let off x z = top_at a x z > r.ry +. 0.3 in
      let bx = off (r.rx +. vx) r.rz and bz = off r.rx (r.rz +. vz) in
      let vx = if bx then -.vx else vx and vz = if bz then -.vz else vz in
      let leg = if Float.hypot vx vz < turn_back || r.age > 70 then Back else Out in
      let rx = r.rx +. vx and rz = r.rz +. vz in
      let ry = match floor_at a rx rz with Some h -> Float.max (h +. 0.3) (r.ry +. ((h +. rang_height -. r.ry) *. 0.1)) | None -> r.ry in
      { r with rx; ry; rz; rvx = vx; rvz = vz; leg; bounced = r.bounced || bx || bz;
        away = r.away || d2 rx rz owner.px owner.pz > 9.; age = r.age + 1 }
  | Back ->
      let ux, uz = norm (owner.px -. r.rx, owner.pz -. r.rz) in
      let vx = r.rvx +. ((ux *. back_speed) -. r.rvx) *. steer in
      let vz = r.rvz +. ((uz *. back_speed) -. r.rvz) *. steer in
      let ry = r.ry +. ((owner.py +. rang_height -. r.ry) *. steer) in
      { r with rx = r.rx +. vx; ry; rz = r.rz +. vz; rvx = vx; rvz = vz; age = r.age + 1 }

(* when a throw turns on the one who threw it: it has to have left the
 * hand cleanly (three units away) and to have come off something --
 * the fence, or a pillar. Anything else would punish a throw for the
 * way the arena is shaped rather than for a bad idea. *)
let own_risk (r : rang) : bool = r.leg = Out && r.bounced && r.away

(* the ones still in the air, and the owners who just caught theirs *)
let step_rangs (a : arena) (players : player list) (rangs : rang list) : rang list * int list =
  let home (r : rang) =
    let o = List.find (fun p -> p.idx = r.owner) players in
    r.leg = Back && d2 r.rx r.rz o.px o.pz < catch_dist *. catch_dist
  in
  let caught, flying = List.partition home (List.map (step_rang a players) rangs) in
  (flying, List.map (fun r -> r.owner) caught)

(*****************************************************************************)
(* Being cut *)
(*****************************************************************************)

(* sliced along (dx, dz): the two halves leave sideways, and the top one
 * a little higher -- the only violence a game about fruit allows *)
let slice (p : player) (dx, dz) : player =
  let sx, sz = norm (-.dz, dx) in
  let h top sign =
    { hx = p.px; hy = p.py +. (if top then 0.85 else 0.4); hz = p.pz; hvx = sx *. 0.13 *. sign; hvy = (if top then 0.2 else 0.12);
      hvz = sz *. 0.13 *. sign; hspin = 0.; htop = top }
  in
  { p with state = Cut [ h true 1.; h false (-1.) ]; holds = false }

(* a half comes down on whatever ground is under it and rolls to a
 * stop -- or off the edge, and on down into the void *)
let step_half (a : arena) (h : half) : half =
  let hvy = h.hvy -. gravity in
  let hy = h.hy +. hvy in
  match floor_at a h.hx h.hz with
  | Some f when hy < f +. 0.22 && h.hy >= f -. 0.3 ->
      { h with hy = f +. 0.22; hvy = 0.; hvx = h.hvx *. 0.94; hvz = h.hvz *. 0.94; hx = h.hx +. h.hvx;
        hz = h.hz +. h.hvz; hspin = h.hspin +. (h.hvx *. 40.) }
  | _ -> { h with hx = h.hx +. h.hvx; hy; hz = h.hz +. h.hvz; hvy; hspin = h.hspin +. 11. }

let step_dead (a : arena) (p : player) : player =
  match p.state with
  | Cut halves -> { p with state = Cut (List.map (step_half a) halves) }
  | Falling n -> { p with state = Falling (n + 1) }
  | Alive -> p

(* a boomerang at the height of [p]'s body: a jump can clear one *)
let at_body (r : rang) (p : player) : bool = Float.abs (r.ry -. (p.py +. 0.7)) < 0.85

(* Who the boomerangs and the slashes killed this frame, and along which
 * direction they were cut. The way out cuts everyone the boomerang
 * touches; the way back only cuts the others, since its owner catches
 * it. And a throw becomes dangerous to the one who threw it the moment
 * it caroms off something -- which is both the fair rule (it must leave
 * your hand cleanly) and the funny one (the fence gives it back to
 * you, edge first). *)
let cuts (players : player list) (rangs : rang list) : (int * (number * number)) list =
  let by_rang =
    List.concat_map
      (fun r ->
        List.filter_map
          (fun p ->
            if alive p && (r.owner <> p.idx || own_risk r) && at_body r p
               && d2 r.rx r.rz p.px p.pz < (cut_dist +. radius) *. (cut_dist +. radius) then
              Some (p.idx, norm (r.rvx, r.rvz))
            else None)
          players)
      rangs
  in
  (* a dash with the boomerang still in hand is a slash *)
  let by_slash =
    List.concat_map
      (fun a ->
        if alive a && a.dash > 0 && a.holds then
          List.filter_map
            (fun b ->
              if alive b && b.idx <> a.idx && Float.abs (a.py -. b.py) < 0.8
                 && d2 a.px a.pz b.px b.pz < (slash_dist +. radius) *. (slash_dist +. radius) then
                Some (b.idx, Camera3d.forward a.heading)
              else None)
            players
        else [])
      players
  in
  by_rang @ by_slash

(*****************************************************************************)
(* The computer's three players *)
(*****************************************************************************)

(* The three differ only here: how close it likes to fight, how straight
 * it wants the shot before throwing, how early it starts dodging, and
 * how obliquely it comes at you ([slant], so that the three of them
 * don't all arrive along the same line).
 * They also *hesitate*: [think] frames of holding the boomerang before
 * it may be thrown again (45 at the start of a round, 30 after a
 * catch). Without it three opponents who all throw the frame they have
 * a line cut the fourth player down in a second and a half, and the
 * game is not playable -- the single most important number in this
 * file, and there is nothing clever about it. *)
type wits = { range : number; aim : number; nerve : number; slant : number }

let wits_of (idx : int) : wits =
  match idx with
  | 1 -> { range = 6.5; aim = 14.; nerve = 4.5; slant = 22. } (* the strawberry rushes *)
  | 2 -> { range = 10.5; aim = 7.; nerve = 5.5; slant = -30. } (* the lemon throws from far *)
  | _ -> { range = 8.; aim = 10.; nerve = 7.5; slant = 35. } (* the blueberry keeps out of the way *)

(* the angle from [p]'s heading to a direction, in degrees, in -180..180 *)
let angle_to (from : number) (dx, dz) : number =
  let a = heading_of (dx, dz) -. from in
  let a = Float.rem (a +. 540.) 360. -. 180. in
  a

(* a hole or the water within [m] of (x, z), in either direction: the
 * margin the computer keeps from an edge (walking along one, a step
 * that ends outside it can still clip its corner). 0.6 and not more:
 * a bridge is two units wide *)
let near_gap (a : arena) (m : number) (x : number) (z : number) : bool =
  List.exists
    (fun (dx, dz) -> match cell_at a (x +. dx) (z +. dz) with Hole | Water -> true | _ -> false)
    [ (-.m, -.m); (m, -.m); (-.m, m); (m, m) ]

(* Can one walk straight from here to there? Looked at every half unit:
 * ground under the feet all the way, never a gap too near nor a stone,
 * and never more than a step up from the last look -- the terrace's
 * cliff is a wall from below, and nothing from above. The fence counts
 * too -- it stops nobody dead, it just takes the part of your step that
 * was into it, which leaves a computer that ignores it grinding
 * sideways along the wall for a hundred frames. *)
let line_ok (a : arena) ((px, pz) : number * number) ((tx, tz) : number * number) : bool =
  let n = max 1 (int_of_float (Float.ceil (Float.hypot (tx -. px) (tz -. pz) /. 0.5))) in
  let rec ok i prev =
    i > n
    ||
    let t = float_of_int i /. float_of_int n in
    let x = px +. ((tx -. px) *. t) and z = pz +. ((tz -. pz) *. t) in
    match floor_at a x z with
    | Some h -> h <= prev +. step_up && (not (near_gap a 0.6 x z)) && (not (blocked a ~y:prev radius x z)) && ok (i + 1) h
    | None -> false
  in
  ok 1 (Option.value (floor_at a px pz) ~default:0.)

(* is the ground clear that far along a direction? The computer looks
 * where it is going: two units for a step, four for a dash, which
 * covers the whole 9 frames of one. (A human gets no such check: you
 * may dash into a hole, and you will.) *)
let way_ok_from (a : arena) ((px, pz) : number * number) (ahead : number) (ux, uz) : bool =
  line_ok a (px, pz) (px +. (ux *. ahead), pz +. (uz *. ahead))

let way_ok (a : arena) (p : player) (ahead : number) (dir : number * number) : bool = way_ok_from a (p.px, p.pz) ahead dir

(* A direction that doesn't walk into a hole, a stone or the fence: the
 * wanted one if it is clear; else the one it walked last frame, while
 * that stays clear; else the wanted step with one of its components
 * dropped, which slides along whatever is in the way.
 *
 * The middle line is the whole trick, and it took a wedged computer to
 * find it. Pick the first clear way out of a list every frame and a
 * walker caught between a pit and a pillar picks "south" at z = -4.92,
 * which takes it to -4.77, where south is no longer clear and the list
 * gives "north", which takes it back to -4.92: a two-frame loop it
 * never leaves -- half a minute of a round, measured, spent shivering
 * a centimetre.
 * Keeping last frame's way until that way itself is blocked breaks the
 * loop, and costs one field. (Steering behaviours call this hysteresis;
 * it is the same reason a thermostat has two temperatures.) *)
let clear_way_from (a : arena) ((px, pz) : number * number) ~(last : number * number) ?(ahead = 2.) (dx, dz) : number * number =
  let ok = way_ok_from a (px, pz) ahead in
  let d = norm (dx, dz) in
  if ok d then d
  else if ok last then last
  else
    let ux, uz = d in
    let sign v = if v >= 0. then 1. else -1. in
    let sideways =
      (if Float.abs ux > Float.abs uz then [ (sign ux, 0.); (0., sign uz) ] else [ (0., sign uz); (sign ux, 0.) ])
      @ [ (-.sign ux, 0.); (0., -.sign uz) ]
    in
    match List.find_opt ok sideways with Some c -> c | None -> d

let clear_way (a : arena) (p : player) ?ahead (dir : number * number) : number * number = clear_way_from a (p.px, p.pz) ~last:p.way ?ahead dir

(* a stone, or a cliff above the line between two pairs of eyes, and
 * nothing else: the holes and the water are holes, not walls, and you
 * can see across both *)
let in_sight (a : arena) ((ax, az) : number * number) ((bx, bz) : number * number) : bool =
  let eye x z = Option.value (floor_at a x z) ~default:0. +. 0.9 in
  let ya = eye ax az and yb = eye bx bz in
  let steps = 12 in
  not
    (List.exists
       (fun i ->
         let t = float_of_int i /. float_of_int steps in
         top_at a (ax +. ((bx -. ax) *. t)) (az +. ((bz -. az) *. t)) > ya +. ((yb -. ya) *. t))
       (List.init (steps + 1) (fun i -> i)))

(* claude: The way to someone, and whether it is a straight one. When it
 * is not -- he is across the river, up on the terrace, behind a hole --
 * A* over the cells (Pathfind.mli): from cell to cell sideways, onto
 * ground at most a step higher, and the way is the middle of the next
 * cell of the path. [clear_way] only ever slides along an obstacle; it
 * cannot know that the bridge is ten units to the north.
 * Walked with [clear_way] looking only as far as that middle ([approach],
 * below): look its usual two units and a waypoint beside a stone reads
 * as blocked from one step and clear from the next, and the walker
 * shivers between the two, as in [clear_way]'s story.
 *
 *    . . . ~ ~ . . .       S the strawberry, A you: the straight line
 *    . . . = = . . .       is through the water; the path is up the
 *    . S . ~ ~ . A .       bank, over the bridge and down the other
 *    . . . ~ ~ . . .       side, and S walks at the next cell of it
 *)
let route (a : arena) ((px, pz) : number * number) ((tx, tz) : number * number) : (number * number) * bool =
  let direct = (tx -. px, tz -. pz) in
  if line_ok a (px, pz) (tx, tz) then (direct, true)
  else
    let goal = cell_of a tx tz in
    let problem : (int * int) Pathfind.problem =
      { neighbors =
          (fun (c, r) ->
            let here = Option.value (floor_of (cell a (c, r))) ~default:0. in
            List.filter_map
              (fun (dc, dr) ->
                let n = (c + dc, r + dr) in
                match floor_of (cell a n) with Some h when h <= here +. step_up -> Some (n, 1.) | _ -> None)
              [ (1, 0); (-1, 0); (0, 1); (0, -1) ]);
        goal = (fun n -> n = goal);
        estimate = Pathfind.manhattan goal }
    in
    let center = center_of ~cols:a.cols ~rows:a.rows in
    match (Pathfind.astar problem (cell_of a px pz)).path with
    | here :: next :: _ ->
        (* first onto the lane between the two middles, then along it:
         * come into a cell off its middle and head straight for the
         * next one, and a turn clips the stone on the inside of it *)
        let hx, hz = center here and nx, nz = center next in
        let lane = if nx <> hx then (0., hz -. pz) else (hx -. px, 0.) in
        if Float.hypot (fst lane) (snd lane) > 0.3 then (lane, false) else ((nx -. px, nz -. pz), false)
    | _ -> (direct, false)

(* Closing in from afar: obliquely by [slant] degrees when the way is
 * straight and the slanted one is clear too (a slant into the
 * terrace's cliff, with the hysteresis of [clear_way], sends a walker
 * wandering along it for good), and along the [route] when it is not
 * -- looking only as far as its waypoint, never less than a step *)
let approach (a : arena) (at : number * number) (target : number * number) (slant : number) : (number * number) * number option =
  match route a at target with
  | way, false -> (way, Some (Float.max 0.5 (Float.min 2. (Float.hypot (fst way) (snd way)))))
  | direct, true ->
      let slanted = turn_vec slant direct in
      ((if way_ok_from a at 2. (norm slanted) then slanted else direct), None)

(*****************************************************************************)
(* The computer's senses and tactics, on ai/ *)
(*****************************************************************************)
(* claude: The three cooks are written on the ai/ layer, which decides
 * what a bot is allowed to know and how quickly it may act on it:
 *
 *   Sense       its enemy is seen only when no stone (or cliff) is
 *               between them, and remembered for a second and a half
 *               after that
 *   Bot         it acts on what it saw 6 frames ago and changes its
 *               mind 20 times a second, not 60
 *   Fsm         dodge / hunt / keep away as three states and the rules
 *               between them, with the hysteresis written into a
 *               transition's guard instead of into an if
 *   Pathfind    the way to someone when it is not straight ([route])
 *
 * The delay is what you feel: it steps out of a boomerang's line a
 * tenth of a second after it becomes dangerous, which is about when
 * you would. *)

(* each of the others, seen or remembered, with how far it is; the
 * dead are nobody to look for *)
let look (g : game) (p : player) (was : (int * (number * number) Sense.target) list) :
    (int * number * (number * number) Sense.target) list =
  List.filter_map
    (fun q ->
      if q.idx = p.idx then None
      else
        let t = Option.value (List.assoc_opt q.idx was) ~default:Sense.unknown in
        let distance = sqrt (d2 p.px p.pz q.px q.pz) in
        let t =
          if alive q then
            Sense.update ~sight:40. ~distance ~clear:(in_sight arenas.(g.arena_no) (p.px, p.pz) (q.px, q.pz)) ~position:(q.px, q.pz) t
            |> Sense.forget ~after:90
          else Sense.unknown
        in
        Some (q.idx, distance, t))
    g.players

(* is a boomerang coming at me? the nearest whose line it is standing
 * in: how far along its line, and how far off it -- the same two
 * numbers a player reads off the screen *)
let threat_to (g : game) (p : player) (w : wits) : (number * number * number * number) option =
  List.fold_left
    (fun best r ->
      let ux, uz = norm (r.rvx, r.rvz) in
      let relx = p.px -. r.rx and relz = p.pz -. r.rz in
      let along = (relx *. ux) +. (relz *. uz) in
      let across = (relx *. uz) -. (relz *. ux) in
      if along > 0. && along < w.nerve && Float.abs across < 1.9 && (r.owner <> p.idx || own_risk r) && at_body r p then
        match best with Some (a, _, _, _) when a < along -> best | _ -> Some (along, across, ux, uz)
      else best)
    None g.rangs

(* three states, and the rules between them. The one that matters is
 * the third: a bot that leaves Dodge the instant the line is clear
 * steps back into it, which is the flip-flop [clear_way]'s comment
 * describes. Its guard keeps it stepping aside for another eight
 * frames ([Fsm.after] is that test on its own; here it is part of a
 * larger one, since where it goes next depends on whether it is
 * armed), and the machine says so where an if would hide it *)
let modes : (mode, senses) Fsm.machine =
  [
    { from = Hunt; label = "a boomerang"; guard = (fun s _ -> s.incoming <> None); target = Dodge };
    { from = Away; label = "a boomerang"; guard = (fun s _ -> s.incoming <> None); target = Dodge };
    { from = Dodge; label = "it passed"; guard = (fun s since -> s.incoming = None && since >= 8 && s.armed); target = Hunt };
    { from = Dodge; label = "it passed"; guard = (fun s since -> s.incoming = None && since >= 8 && not s.armed); target = Away };
    { from = Hunt; label = "thrown"; guard = (fun s _ -> not s.armed); target = Away };
    { from = Away; label = "caught"; guard = (fun s _ -> s.armed); target = Hunt };
  ]

let senses_of (was : senses option) ((g, idx) : game * int) : senses =
  let p = List.find (fun q -> q.idx = idx) g.players in
  let w = wits_of idx in
  let seen = look g p (match was with Some s -> s.watched | None -> []) in
  let s =
    {
      arena_no = g.arena_no;
      at = (p.px, p.pz);
      facing = p.heading;
      armed = p.holds;
      cool = p.cool;
      think = p.think;
      seed = idx;
      last_way = p.way;
      watched = List.map (fun (i, _, t) -> (i, t)) seen;
      enemy = Option.value (Sense.focus (List.map (fun (_, d, t) -> (d, t)) seen)) ~default:Sense.unknown;
      incoming = threat_to g p w;
      mind = (match was with Some s -> s.mind | None -> Fsm.start Hunt);
    }
  in
  { s with mind = Fsm.step modes s s.mind }

(* The tactics, from the senses alone: when the enemy is behind a
 * stone it walks to where it last saw him, and can be wrong; when it
 * has forgotten everybody it goes looking, to whichever start is the
 * farthest from where it stands -- from there another one is, so it
 * sweeps the arena back and forth instead of arriving somewhere and
 * waiting there, where the terrace may hide everyone for good *)
let decide (s : senses) : intent =
  let a = arenas.(s.arena_no) in
  let w = wits_of s.seed in
  let go dir = clear_way_from a s.at ~last:s.last_way dir in
  match s.enemy.position with
  | None ->
      let px, pz = s.at in
      let there = List.fold_left (fun (bx, bz) (x, z) -> if d2 px pz x z > d2 px pz bx bz then (x, z) else (bx, bz)) s.at a.starts in
      { idle with go = Some (fst (approach a s.at there 0.)); toward = Some there }
  | Some (tx, tz) -> (
      let px, pz = s.at in
      (* standing right on him, the way to him is the way it faces: a
       * zero direction has no heading, and a slash along a made-up one
       * dashes off the edge of the world *)
      let to_target = if d2 px pz tx tz < 0.0001 then Camera3d.forward s.facing else (tx -. px, tz -. pz) in
      let dist = Float.hypot (fst to_target) (snd to_target) in
      match s.mind.state with
      | Dodge -> (
          match s.incoming with
          | Some (along, across, ux, uz) ->
              (* out of its line, the way it is already leaning, and a
               * dash if it is nearly here *)
              let side = if across >= 0. then 1. else -1. in
              let away = go (uz *. side, -.ux *. side) in
              { idle with go = Some away; dash_now = along < 2.8 && s.cool = 0 && way_ok_from a s.at 4.2 away }
          (* the eight frames after it passed: keep going that way *)
          | None -> { idle with go = Some (go s.last_way) })
      (* unarmed: keep away until the boomerang is back *)
      | Away -> { idle with go = Some (go (-.fst to_target, -.snd to_target)) }
      | Hunt ->
          (* it aims by facing where it walks, exactly as you do, and
             its aim wobbles while the enemy is freshly seen
             (Bot.aim_error): a shot taken the moment someone
             appears is a worse shot *)
          let wobble = Bot.aim_error ~spread:6. ~settle:20. ~seen_for:s.enemy.seen_for ~seed:s.seed () in
          let aimed = Float.abs (angle_to s.facing to_target +. wobble) < w.aim in
          let ready = s.think = 0 && aimed && dist < w.range && s.enemy.visible in
          if dist < 2.4 then
            (* face to face: the slash if the dash is there, and a
             * point-blank throw if it is not. Without that second half
             * two of them can end up nose to nose for ever, one waiting
             * on a cooldown the other is not going to let run out *)
            let at_him = go to_target in
            let slash = s.cool = 0 && way_ok_from a s.at 4.2 at_him in
            { idle with go = Some at_him; throw = (not slash) && ready; dash_now = slash }
          else if s.think = 0 && dist < w.range && s.enemy.visible && not (way_ok_from a s.at 2. (norm to_target)) then
            (* in range and in sight, but the ground between is not
             * walkable -- across the river, from the terrace's edge --
             * so it plants its feet and turns to aim, as you do *)
            { idle with go = Some to_target; plant = true; throw = aimed }
          else if dist > w.range || not s.enemy.visible then
            (* far off, or out of sight: it closes in ([approach]),
             * obliquely so that the three of them don't all arrive
             * along the same line, or along the [route] when the way
             * is not straight *)
            let want, ahead = approach a s.at (tx, tz) w.slant in
            { idle with go = Some (clear_way_from a s.at ~last:s.last_way ?ahead want); toward = Some (tx, tz) }
          else
            (* in range: still hesitating, it circles its man; ready, it
             * walks *straight* at him, because facing where you walk is
             * what aiming is here, for the computer exactly as for you.
             * (Keep the slant inside the range and two of them orbit
             * each other for ever, each 22 degrees off a shot it can
             * never take. That deadlock is why this case exists.) *)
            let want = if s.think > 0 then (-.snd to_target, fst to_target) else to_target in
            { idle with go = Some (go want); throw = ready })

(* Its reflex (Bot.mli): whatever the bot decided, six frames
 * late, its feet look at the ground where they are *now*, one unit
 * ahead -- about what six frames of walking cover -- and a dash where
 * it is going; and a place to walk to ([toward]) they find the way to
 * from where they are now. The delay is fair for aiming and dodging,
 * and absurd for your own feet: without the first line a bot that
 * decided to walk along the river walks on into it, and without the
 * second one that steps onto the lane of its [route] from where it
 * was six frames ago overshoots it, turns back, overshoots it again,
 * and never gets anywhere -- a control loop with a delay in it,
 * which is a textbook way to build an oscillator. *)
let footing ((g, idx) : game * int) (it : intent) : intent =
  let a = arenas.(g.arena_no) and p = List.find (fun q -> q.idx = idx) g.players in
  match (it.go, it.toward) with
  | None, _ -> it
  (* feet planted to aim, which only a hand holding the boomerang can
   * do: the aim decided before a throw is repeated after it, and
   * without its boomerang the same intent walks *)
  | Some _, _ when it.plant && p.holds -> it
  | Some _, Some t ->
      let way, ahead = approach a (p.px, p.pz) t (wits_of p.idx).slant in
      { it with go = Some (clear_way a p ?ahead way) }
  | Some d, None ->
      let d = clear_way a p ~ahead:1. d in
      { it with go = Some d; dash_now = it.dash_now && way_ok a p 4.2 d }

(* a person's reaction is about a tenth of a second, and no hand
 * changes its mind sixty times a second (Bot.mli) *)
let mind : (game * int, senses, intent) Bot.t = Bot.make ~delay:6 ~rate:3 ~reflex:footing ~sense:senses_of ~decide ()

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let step_game (s : model) (k : keyboard) (g : game) : game =
  let a = arenas.(g.arena_no) in
  (* one intent per player, from the keyboard or from Bot, which sees
     less and answers later *)
  let minds = Array.copy g.minds in
  let intents =
    List.map
      (fun p ->
        if not (alive p) then idle
        else if p.idx = 0 then keys_intent s k p
        else
          let it, running = Bot.step mind (g, p.idx) minds.(p.idx) in
          minds.(p.idx) <- running;
          it)
      g.players
  in
  let stepped = List.map2 (fun it p -> if alive p then step_player a it p else (step_dead a p, None)) intents g.players in
  let players = List.map fst stepped in
  let thrown = List.filter_map snd stepped in
  let rangs, caught = step_rangs a players (g.rangs @ thrown) in
  let players = List.map (fun p -> if List.mem p.idx caught then { p with holds = true; think = 30 } else p) players in
  let cut = cuts players rangs in
  let players = List.map (fun p -> match List.assoc_opt p.idx cut with Some d -> slice p d | None -> p) players in
  (* the dead take their boomerang with them *)
  let rangs = List.filter (fun r -> alive (List.find (fun p -> p.idx = r.owner) players)) rangs in
  (* the round is over when one is left -- or when the clock runs out,
   * which is the arcade's answer to two players who will not close
   * (and a promise that a round always ends) *)
  let standing = List.filter alive players in
  let clock = g.clock + 1 in
  let ended =
    match g.ended with
    | Some n -> Some (n + 1)
    | None -> if List.length standing <= 1 || clock > time_up then Some 0 else None
  in
  (* the camera glides to its framing rather than jumping to it: 5% of
   * the way a frame, 90% in 45 frames, slow enough that a dash does not
   * shake the picture *)
  { g with players; rangs; ended; clock; minds; cam = Camera3d.follow 0.05 (framing players) g.cam }

(* the round's point, to the one left standing (nobody, if the clock ran
 * out on two of them) -- counted where the round ends, so that the last
 * one can be looked at with its score already on it *)
let score (g : game) : game =
  let winner = match List.filter alive g.players with [ p ] -> Some p.idx | _ -> None in
  { g with players = List.map (fun p -> if Some p.idx = winner then { p with wins = p.wins + 1 } else p) g.players }

(* the next round, on the other arena: everyone at their start, the
 * scores kept, and the camera gliding out from the last winner *)
let next_round (g : game) : game =
  let arena_no = (g.arena_no + 1) mod Array.length arenas in
  { g with arena_no; players = List.map (place arenas.(arena_no)) g.players; rangs = []; ended = None; clock = 0;
    round_no = g.round_no + 1; minds = Array.init 4 (fun _ -> Bot.start idle) }

let first_arena (flags : (string * string) list) : int = if List.assoc_opt "map" flags = Some "river" then 1 else 0

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  match s.scene with
  | Title ->
      if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Playing (new_game ~arena_no:(first_arena computer.flags) ())) s
      else s
  | Playing g -> (
      let g = step_game s computer.keyboard g in
      match g.ended with
      | Some n when n > 100 ->
          let g = score g in
          if List.exists (fun p -> p.wins >= rounds_to_win) g.players then Scene2d.go (Winner g) s
          else { s with scene = Playing (next_round g) }
      | _ -> { s with scene = Playing g })
  | Winner _ -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go Title s else s

(*****************************************************************************)
(* View: the arena *)
(*****************************************************************************)

let grass1 = rgb 116 176 88
let grass2 = rgb 104 164 80
let terrace1 = rgb 150 198 104
let terrace2 = rgb 138 188 96
let step_color = rgb 128 184 92
let dirt = rgb 124 92 62
let water1 = rgb 72 140 208
let water2 = rgb 64 130 198
let wood = rgb 158 112 68
let dark_wood = rgb 110 76 46
let stone = rgb 165 160 150
let void = rgb 32 46 74
let bottom = -1.6 (* the islands' underside *)

(* a quad lying flat, facing up (the winding of Playground3d.plane) *)
let quad_up (c : color) (y : number) (x1 : number) (z1 : number) (x2 : number) (z2 : number) : shape3d =
  polygon3d c [ (x1, y, z1); (x1, y, z2); (x2, y, z2); (x2, y, z1) ]

(* the upright face of the cell (x1, z1)-(x2, z2) on the side of its
 * neighbour (dc, dr), from [yb] up to [yt], wound to face that
 * neighbour (north is -z) *)
let face (x1, z1, x2, z2) ((dc, dr) : int * int) (yb : number) (yt : number) : (number * number * number) list =
  match (dc, dr) with
  | 0, -1 -> [ (x1, yb, z1); (x1, yt, z1); (x2, yt, z1); (x2, yb, z1) ]
  | 0, _ -> [ (x2, yb, z2); (x2, yt, z2); (x1, yt, z2); (x1, yb, z2) ]
  | -1, _ -> [ (x1, yb, z2); (x1, yt, z2); (x1, yt, z1); (x1, yb, z1) ]
  | _ -> [ (x2, yb, z1); (x2, yt, z1); (x2, yt, z2); (x2, yb, z2) ]

(* claude: Cell by cell: its top, and an upright face towards each
 * neighbour lower than it, down to that neighbour's top -- the cliff of
 * the terrace, the bank of the river, and at an edge the island's
 * earth all the way down to its underside; a stone is a box from the
 * underside up. Nothing else: the faces between two cells of the same
 * height would never be seen. A bridge is planks with the water
 * showing between them, and a rail each side. *)
let draw_arena (a : arena) : shape3d =
  let level (c, r) = match cell a (c, r) with Hole -> bottom | cl -> top_of cl in
  let sides color (c, r) rect ~low top =
    List.filter_map
      (fun d ->
        let nb = Float.max low (level (c + fst d, r + snd d)) in
        if nb < top then Some (polygon3d color (face rect d nb top)) else None)
      [ (0, -1); (0, 1); (-1, 0); (1, 0) ]
  in
  let draw c r =
    let x1, z1 = center_of ~cols:a.cols ~rows:a.rows (c, r) in
    let x1 = x1 -. (tile /. 2.) and z1 = z1 -. (tile /. 2.) in
    let x2 = x1 +. tile and z2 = z1 +. tile in
    let rect = (x1, z1, x2, z2) in
    let even = (c + r) mod 2 = 0 in
    match a.cells.(r).(c) with
    | Hole -> []
    | Wall -> [ box stone tile (wall_height -. bottom) tile |> move3d (x1 +. 1.) ((wall_height +. bottom) /. 2.) (z1 +. 1.) ]
    | Ground h ->
        let top = if h >= 1. then if even then terrace1 else terrace2 else if h > 0. then step_color else if even then grass1 else grass2 in
        quad_up top h x1 z1 x2 z2 :: sides dirt (c, r) rect ~low:bottom h
    | Water -> quad_up (if even then water1 else water2) water_level x1 z1 x2 z2 :: sides dirt (c, r) rect ~low:bottom water_level
    | Bridge ->
        (* it runs towards whichever of its sides has ground or bridge *)
        let solid d = match cell a (c + fst d, r + snd d) with Ground _ | Bridge -> true | _ -> false in
        let along_x = solid (-1, 0) || solid (1, 0) in
        let planks =
          List.init 4 (fun i ->
              let u = float_of_int i *. 0.5 in
              if along_x then quad_up wood 0. (x1 +. u +. 0.04) (z1 +. 0.1) (x1 +. u +. 0.46) (z2 -. 0.1)
              else quad_up wood 0. (x1 +. 0.1) (z1 +. u +. 0.04) (x2 -. 0.1) (z1 +. u +. 0.46))
        in
        let rails =
          if along_x then List.map (fun z -> box dark_wood tile 0.1 0.1 |> move3d (x1 +. 1.) 0.45 z) [ z1 +. 0.08; z2 -. 0.08 ]
          else List.map (fun x -> box dark_wood 0.1 0.1 tile |> move3d x 0.45 (z1 +. 1.)) [ x1 +. 0.08; x2 -. 0.08 ]
        in
        let posts =
          List.map (fun (x, z) -> box dark_wood 0.14 0.5 0.14 |> move3d x 0.25 z)
            (if along_x then [ (x1 +. 0.08, z1 +. 0.08); (x1 +. 0.08, z2 -. 0.08) ] else [ (x1 +. 0.08, z1 +. 0.08); (x2 -. 0.08, z1 +. 0.08) ])
        in
        planks @ rails @ posts @ sides dark_wood (c, r) rect ~low:(-0.25) 0.
  in
  cached3d (List.concat (List.init a.rows (fun r -> List.concat (List.init a.cols (fun c -> draw c r)))))

let drawn : shape3d array = Array.map draw_arena arenas

(* one quad on the ground: what tells you, from up here, where something
 * flying actually is -- and how high a jump has got *)
let shadow (a : arena) (x : number) (z : number) (r : number) : shape3d list =
  let at c y = [ quad_up c (y +. 0.03) (x -. r) (z -. r) (x +. r) (z +. r) ] in
  match cell_at a x z with
  | Ground h -> at (rgb 74 118 60) h
  | Bridge -> at dark_wood 0.
  | Water -> at (rgb 40 90 150) water_level
  | Wall | Hole -> []

(*****************************************************************************)
(* View: the foods and their boomerangs *)
(*****************************************************************************)

let eyes (dead : bool) : shape3d list =
  let cross dx =
    let bar a = box (rgb 30 30 40) 0.22 0.05 0.05 |> rotate3d 0. 0. a |> move3d dx 0.78 (-0.58) in
    [ bar 40.; bar (-40.) ]
  in
  let open_eye dx = [ box white 0.19 0.21 0.06 |> move3d dx 0.78 (-0.56); box (rgb 30 30 40) 0.09 0.11 0.05 |> move3d dx 0.78 (-0.61) ] in
  List.concat_map (fun dx -> if dead then cross dx else open_eye dx) [ -0.22; 0.22 ]

(* what grows on top of each food *)
let topping (kind : food) : shape3d list =
  match kind with
  | Avocado -> [ box (rgb 96 64 36) 0.13 0.3 0.13 |> move_y3d 1.35 ]
  | Strawberry ->
      List.map (fun a -> box (rgb 70 150 60) 0.5 0.06 0.18 |> move_x3d 0.2 |> rotate3d 0. a 0. |> move_y3d 1.25) [ 0.; 120.; 240. ]
  | Lemon -> [ box (rgb 205 170 40) 0.11 0.22 0.11 |> move_y3d 1.3 ]
  | Blueberry -> List.map (fun a -> box (rgb 90 85 180) 0.12 0.2 0.12 |> move_x3d 0.28 |> rotate3d 0. a 0. |> move_y3d 1.22) [ 0.; 90.; 180.; 270. ]

let food_shape (kind : food) (heading : number) : shape3d =
  group3d ((sphere (food_color kind) 0.62 |> move_y3d 0.68) :: (topping kind @ eyes false)) |> rotate3d 0. (-.heading) 0.

(* Half a ball: a dome, and the disc of flesh the boomerang left, which
 * is the whole reason you can tell a cut body from a standing one at
 * this distance -- two green balls side by side just look like two
 * avocados, a pale disc reads as a cut across the room. Not
 * Playground3d.sphere, which has no half; the disc is drawn twice, once
 * each way round, so that it is there whichever way the half has
 * tumbled to. *)
let half_ball (skin : color) (flesh : color) (r : number) : shape3d =
  let lat = 4 and lon = 10 in
  let pt la lo =
    let th = Float.pi /. 2. *. float_of_int la /. float_of_int lat in
    let ph = 2. *. Float.pi *. float_of_int lo /. float_of_int lon in
    (r *. sin th *. cos ph, r *. cos th, r *. sin th *. sin ph)
  in
  let dome =
    List.concat
      (List.init lat (fun la -> List.init lon (fun lo -> polygon3d skin [ pt la lo; pt la (lo + 1); pt (la + 1) (lo + 1); pt (la + 1) lo ])))
  in
  let rim = List.init lon (fun lo -> let x, _, z = pt lat lo in (x, 0., z)) in
  group3d ((polygon3d flesh rim :: polygon3d flesh (List.rev rim) :: dome))

(* one of the two, tumbling; the top one keeps the face, crossed out *)
let half_shape (kind : food) (h : half) : shape3d =
  let ball = half_ball (food_color kind) (flesh_color kind) 0.5 in
  let ball = if h.htop then ball else ball |> rotate3d 180. 0. 0. in
  group3d (if h.htop then ball :: eyes true else [ ball ])
  |> rotate3d h.hspin 0. (h.hspin *. 0.6)
  |> move3d h.hx h.hy h.hz

(* two arms meeting at the elbow, spinning flat around its own middle *)
let rang_shape (c : color) (spin_deg : number) : shape3d =
  let arm a = box c 0.8 0.13 0.22 |> move_x3d 0.34 |> rotate3d 0. a 0. in
  group3d [ arm 58.; arm (-58.) ] |> rotate3d 0. spin_deg 0.

let rang_y (r : rang) : number = r.ry +. (0.08 *. sin (float_of_int r.age *. 0.25))

(* the rings a food leaves on the water, widening while it sinks *)
let splash (x : number) (z : number) (n : int) : shape3d list =
  let r = 0.5 +. (0.06 *. float_of_int n) in
  List.init 10 (fun i ->
      let fx, fz = Camera3d.forward (float_of_int i *. 36.) in
      box (rgb 225 240 250) 0.22 0.06 0.22 |> move3d (x +. (fx *. r)) (water_level +. 0.05) (z +. (fz *. r)))

let player_shapes (a : arena) (p : player) : shape3d list =
  match p.state with
  | Alive ->
      (* feet planted, aiming: dots on the ground the way it will fly *)
      let aiming =
        if p.aim = 0 then []
        else
          let fx, fz = Camera3d.forward p.heading in
          List.map (fun d -> box (food_color p.kind) 0.16 0.05 0.16 |> move3d (p.px +. (fx *. d)) (p.py +. 0.05) (p.pz +. (fz *. d))) [ 1.; 1.6; 2.2; 2.8 ]
      in
      shadow a p.px p.pz 0.5 @ aiming @ [ food_shape p.kind p.heading |> move3d p.px p.py p.pz ]
  | Cut halves -> List.concat_map (fun h -> if h.hy < -8. then [] else shadow a h.hx h.hz 0.35 @ [ half_shape p.kind h ]) halves
  | Falling n -> (
      match cell_at a p.px p.pz with
      | Water ->
          let y = p.py -. (0.04 *. float_of_int n) in
          (if n < 30 then splash p.px p.pz n else []) @ if y < -1.6 then [] else [ food_shape p.kind p.heading |> move3d p.px y p.pz ]
      | _ ->
          let y = p.py -. (0.25 *. float_of_int n) in
          if y < -8. then [] else [ food_shape p.kind p.heading |> move3d p.px y p.pz ])

let rang_shapes (a : arena) (players : player list) (r : rang) : shape3d list =
  let owner = List.find (fun p -> p.idx = r.owner) players in
  shadow a r.rx r.rz 0.3
  @ [ rang_shape (rgb 225 200 130) (float_of_int r.age *. 26.) |> move3d r.rx (rang_y r) r.rz;
      (* a spark of the owner's color in the middle: whose it is *)
      box (food_color owner.kind) 0.2 0.16 0.2 |> move3d r.rx (rang_y r) r.rz ]

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text color size str = words color str |> scale size

let scoreboard (screen : screen) (g : game) : shape list =
  List.mapi
    (fun i p ->
      let name = if p.idx = 0 then "YOU" else food_name p.kind in
      text (food_color p.kind) 2.2 (Printf.sprintf "%s %d" name p.wins)
      |> move (screen.left +. 120. +. (float_of_int i *. 230.)) (screen.top -. 35.))
    g.players

let banner (s : model) (g : game) : shape list =
  match g.ended with
  | None -> []
  | Some _ -> (
      match List.filter alive g.players with
      | [ p ] -> [ text (food_color p.kind) 5. (if p.idx = 0 then "YOU WIN THE ROUND" else food_name p.kind ^ " WINS THE ROUND") |> move_y 200. ]
      | [] -> Scene2d.blink 0.6 s [ text white 5. "EVERYONE AT ONCE" |> move_y 200. ]
      | _ -> Scene2d.blink 0.6 s [ text white 5. "TIME" |> move_y 200. ])

let view_game (g : game) : shape3d list =
  let a = arenas.(g.arena_no) in
  [ Camera3d.floor ~color:void ~ground:(-12.) g.cam; drawn.(g.arena_no) ]
  @ List.concat_map (player_shapes a) g.players
  @ List.concat_map (rang_shapes a g.players) g.rangs

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      (* far enough back that the words have somewhere to sit *)
      let cam = Camera3d.orbit ~fov:36. ~distance:33. ~height:23. ~look:7. (spin 24. computer.time) (0., 0., 0.) in
      let n = first_arena computer.flags in
      (* on the terrace of the garden, around the hole; on the river's
       * two bridges *)
      let spots = if n = 0 then [ (-3., -3.); (3., -3.); (3., 3.); (-3., 3.) ] else [ (1., -5.); (3., -5.); (1., 5.); (-1., 5.) ] in
      let ring =
        List.map2
          (fun kind (x, z) ->
            food_shape kind (heading_of (-.x, -.z)) |> move3d x (Option.value (floor_at arenas.(n) x z) ~default:0.) z)
          foods spots
      in
      ( cam,
        [ Camera3d.floor ~color:void ~ground:(-12.) cam; drawn.(n) ] @ ring
        @ [ rang_shape (rgb 225 200 130) (spin 1.2 computer.time) |> move3d 0. (1.9 +. (0.2 *. wave 0. 1. 2. computer.time)) 0. ]
        @ List.map hud
            ([ text (rgb 130 190 70) 6. "TINY BOOMERANG FU" |> move_y 300.;
               text white 2.4 "arrows: move   space: hold to aim, let go to throw   x: dash   z: jump" |> move_y 235.;
               text gray 2.1 "thrown, it cuts anyone -- you too. In hand, the dash is a slash." |> move_y 195.;
               text gray 2.1 (Printf.sprintf "first to %d rounds" rounds_to_win) |> move_y 160. ]
            @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 110. ]) )
  | Playing g ->
      ( g.cam,
        view_game g
        @ List.map hud
            (scoreboard screen g @ banner s g
            @ [ text gray 2. (Printf.sprintf "ROUND %d -- %s" g.round_no arenas.(g.arena_no).name) |> move_y (screen.bottom +. 30.) ]) )
  | Winner g ->
      let champion = List.fold_left (fun best p -> if p.wins > best.wins then p else best) (List.hd g.players) g.players in
      ( g.cam,
        view_game g
        @ List.map hud
            (scoreboard screen g
            @ [ text (food_color champion.kind) 6. (Printf.sprintf "%s WINS!" (if champion.idx = 0 then "YOU" else food_name champion.kind)) |> move_y 80. ]
            @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-40.) ]) )

let app = game3d view update initial_model

(* the foods are spheres, so they are worth shading smoothly; nothing
 * here needs the back faces (no sky: the arena floats over a void).
 * The flag map=river comes from the command line, or the
 * page's URL *)
let main = Playground3d_platform.run_app3d ~flags:(Playground_platform.flags ()) app
