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
 * the other three are the computer. Arrows move, space throws, x dashes.
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
 * The camera is the other thing the genre decides for you. A party game
 * on one screen cannot follow anybody, so the camera here is fixed,
 * high and nearly isometric, and never moves -- which is *why* these
 * arenas are one screen big, from Bomberman (1983) to Samurai Gunn and
 * TowerFall (both 2013) to this one. TinyBomberman.ml is this
 * game's 2D ancestor in this repository, and the same shape: four
 * players, one screen, one hit.
 *
 * Being fixed and high costs one thing, and it is paid with one quad
 * per flying object: at this angle you cannot tell a boomerang at head
 * height from one lying on the grass, so everything that leaves the
 * ground drags a [shadow] under it, exactly as TinyMario64.ml's
 * Mario does for the same reason.
 *
 * The computer plays with the [intent] a human sends -- a direction, a
 * throw, a dash -- and nothing else: no extra speed, no knowing where a
 * boomerang will be. Its three players differ only in four numbers
 * ([wits]): how close each likes to be, how straight a shot it wants,
 * how early it dodges, and how obliquely it comes at you.
 *
 * Its [brain] stays in this file rather than in ai/. What ai/ has today
 * is Minimax (a turn to choose) and Pathfind (a grid to cross), and
 * this is neither: an open floor, 60 frames a second, and one decision
 * -- step where? The pieces it would want, Steering and Fsm (dodge,
 * hunt, keep away *is* a state machine), are the unwritten half of
 * plan_ai_teaching.md, and the one shape that plan sketches for
 * steering is forces on Physics bodies, which these characters do not
 * have: they move at a fixed speed and their dash is committed, on
 * purpose. So: written out here, the way TinyPacman.ml's ghosts
 * and gamekits/lightcycles' computer are, and noted in that plan as a
 * waiting user of both modules.
 *
 * Exercises: the original's power-ups (fire, ice, and above all the
 * teleport, which drops you where your boomerang is -- the one that
 * best fits the core rule: your weapon left you, so follow it), a
 * second human player on w/a/s/d, letting anybody catch anybody's
 * boomerang, arenas that move.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The arena *)
(*****************************************************************************)

(* the arena is 24 x 24, centered on the origin, drawn as 2-unit tiles;
 * the fence is the outermost two of them, so the ground you can stand
 * on reaches [inner] *)
let half = 12.
let inner = 10.
let tile = 2.

(* a box on the ground, by its center and its half-extents: a pillar, or
 * a pit. Players are stopped by the pillars, boomerangs bounce off
 * them, and the pits stop nobody -- you fall in. *)
type rect = { cx : number; cz : number; hw : number; hd : number }

let pillars = [ { cx = -6.; cz = -6.; hw = 1.2; hd = 1.2 }; { cx = 6.; cz = -6.; hw = 1.2; hd = 1.2 };
                { cx = -6.; cz = 6.; hw = 1.2; hd = 1.2 }; { cx = 6.; cz = 6.; hw = 1.2; hd = 1.2 } ]

let pits = [ { cx = -8.; cz = 0.; hw = 2.; hd = 2. }; { cx = 8.; cz = 0.; hw = 2.; hd = 2. } ]

let in_rect (r : rect) ?(grow = 0.) (x : number) (z : number) : bool =
  Float.abs (x -. r.cx) < r.hw +. grow && Float.abs (z -. r.cz) < r.hd +. grow

let in_pit (x : number) (z : number) : bool = List.exists (fun p -> in_rect p x z) pits
let hits_pillar ?(grow = 0.) (x : number) (z : number) : bool = List.exists (fun p -> in_rect p ~grow x z) pillars

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

type state = Alive | Cut of half list | Falling of int (* frames down the pit *)

type player = {
  idx : int;
  kind : food;
  px : number;
  pz : number;
  heading : number; (* where it faces, degrees: 0 towards -z, 90 towards +x *)
  holds : bool; (* its boomerang in its hand *)
  dash : int; (* frames of dash left *)
  cool : int; (* frames before the next dash *)
  think : int; (* the computer's: frames before it throws again *)
  way : number * number; (* the direction it walked last frame (see [clear_way]) *)
  state : state;
  wins : int;
}

(* a boomerang in the air: out (dangerous to everyone), or coming back
 * (harmless to its owner, who catches it) *)
type leg = Out | Back

type rang = {
  rx : number;
  rz : number;
  rvx : number;
  rvz : number;
  owner : int;
  leg : leg;
  bounced : bool; (* off the fence or a pillar: see [cuts] *)
  away : bool; (* it has been a body's length or three from its thrower *)
  age : int;
}

type game = {
  players : player list;
  rangs : rang list;
  ended : int option; (* frames since the round was decided *)
  clock : int; (* frames this round has lasted *)
  round_no : int;
}

type scene = Title | Playing of game | Winner of game
type model = scene Scene2d.t

let starts = [ (-8.5, -8.5); (8.5, -8.5); (-8.5, 8.5); (8.5, 8.5) ]
let rounds_to_win = 3
let time_up = 60 * 45 (* frames a round may last *)

(* a player at its corner, armed, facing the middle *)
let place (p : player) : player =
  let x, z = List.nth starts p.idx in
  let heading = atan2 (-.x) z *. 180. /. Float.pi in
  { p with px = x; pz = z; heading; holds = true; dash = 0; cool = 0; think = 45; way = Camera3d.forward heading; state = Alive }

let new_game () : game =
  { players =
      List.mapi
        (fun i kind ->
          place { idx = i; kind; px = 0.; pz = 0.; heading = 0.; holds = true; dash = 0; cool = 0; think = 45; way = (0., 1.); state = Alive; wins = 0 })
        foods;
    rangs = []; ended = None; clock = 0; round_no = 1 }

let initial_model : model = Scene2d.start Title

let alive (p : player) : bool = match p.state with Alive -> true | _ -> false
let d2 (ax : number) (az : number) (bx : number) (bz : number) : number =
  let dx = ax -. bx and dz = az -. bz in
  (dx *. dx) +. (dz *. dz)

(*****************************************************************************)
(* Intents *)
(*****************************************************************************)

(* What one player asks for this frame: where to go, and the two
 * buttons. A human fills it from the keyboard, the computer from
 * [brain]; nothing downstream knows which. *)
type intent = { go : (number * number) option; throw : bool; dash_now : bool }

let idle : intent = { go = None; throw = false; dash_now = false }

let keys_intent (s : model) (k : keyboard) : intent =
  let dx = (if k.kright then 1. else 0.) -. if k.kleft then 1. else 0. in
  let dz = (if k.kdown then 1. else 0.) -. if k.kup then 1. else 0. in
  { go = (if dx = 0. && dz = 0. then None else Some (dx, dz));
    throw = Scene2d.pressed (fun k -> k.kspace) s;
    dash_now = Scene2d.pressed (fun k -> Set_.mem "x" k.keys) s }

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

let radius = 0.55 (* a food's *)
let speed = 0.155
let dash_speed = 0.42
let dash_frames = 9
let dash_cool = 45
let slash_dist = 1.5 (* the dash of someone still holding their boomerang *)

let norm (dx, dz) =
  let n = Float.hypot dx dz in
  if n < 0.0001 then (0., 0.) else (dx /. n, dz /. n)

let heading_of (dx, dz) : number = atan2 dx (-.dz) *. 180. /. Float.pi

(* the walls and the pillars stop a body of [r]; one axis at a time, so
 * you slide along a pillar instead of sticking to it *)
let slide (r : number) (x : number) (z : number) (dx : number) (dz : number) : number * number =
  let clamp v = Float.max (-.(inner -. r)) (Float.min (inner -. r) v) in
  let x' = clamp (x +. dx) in
  let x' = if hits_pillar ~grow:r x' z then x else x' in
  let z' = clamp (z +. dz) in
  let z' = if hits_pillar ~grow:r x' z' then z else z' in
  (x', z')

(* One player's frame: the dash is committed (its direction is locked
 * for its 9 frames, like every move in TinyTombRaider.ml), and
 * a throw leaves the hand empty. *)
let step_player (it : intent) (p : player) : player * rang option =
  let starting = p.dash = 0 && it.dash_now && p.cool = 0 in
  let dash = if starting then dash_frames else max 0 (p.dash - 1) in
  let cool = if starting then dash_cool else max 0 (p.cool - 1) in
  (* you face where you walk -- which is also how you aim *)
  let heading = match it.go with Some d when p.dash = 0 -> heading_of (norm d) | _ -> p.heading in
  let dx, dz =
    if dash > 0 then
      let fx, fz = Camera3d.forward heading in
      (fx *. dash_speed, fz *. dash_speed)
    else match it.go with None -> (0., 0.) | Some d -> let ux, uz = norm d in (ux *. speed, uz *. speed)
  in
  let px, pz = slide radius p.px p.pz dx dz in
  let way = match it.go with Some d -> norm d | None -> p.way in
  let p = { p with px; pz; heading; dash; cool; way; think = max 0 (p.think - 1) } in
  (* over a pit: nothing holds you up *)
  if in_pit px pz then ({ p with state = Falling 0 }, None)
  else if it.throw && p.holds then
    (* it leaves the hand, but never outside the arena: thrown with your
     * back to the fence it would start beyond it and bounce at once *)
    let fx, fz = Camera3d.forward heading in
    let edge v = Float.max (-.(inner -. 0.5)) (Float.min (inner -. 0.5) v) in
    ( { p with holds = false },
      Some { rx = edge (px +. (fx *. 0.5)); rz = edge (pz +. (fz *. 0.5)); rvx = fx *. 0.58; rvz = fz *. 0.58;
             owner = p.idx; leg = Out; bounced = false; away = false; age = 0 } )
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
 * pillars (which is how a throw can end up in the thrower). Coming
 * back: it steers at its owner and sails over everything -- a wall must
 * never be able to keep your weapon from you. *)
let step_rang (players : player list) (r : rang) : rang =
  let owner = List.find (fun p -> p.idx = r.owner) players in
  match r.leg with
  | Out ->
      let vx, vz = turn_vec curve (r.rvx, r.rvz) in
      let vx = vx *. drag and vz = vz *. drag in
      (* bounce: whichever axis the obstacle is on, that component flips *)
      let off x z = Float.abs x > inner -. 0.3 || Float.abs z > inner -. 0.3 || hits_pillar ~grow:0.3 x z in
      let bx = off (r.rx +. vx) r.rz and bz = off r.rx (r.rz +. vz) in
      let vx = if bx then -.vx else vx and vz = if bz then -.vz else vz in
      let leg = if Float.hypot vx vz < turn_back || r.age > 70 then Back else Out in
      let rx = r.rx +. vx and rz = r.rz +. vz in
      { r with rx; rz; rvx = vx; rvz = vz; leg; bounced = r.bounced || bx || bz;
        away = r.away || d2 rx rz owner.px owner.pz > 9.; age = r.age + 1 }
  | Back ->
      let ux, uz = norm (owner.px -. r.rx, owner.pz -. r.rz) in
      let vx = r.rvx +. ((ux *. back_speed) -. r.rvx) *. steer in
      let vz = r.rvz +. ((uz *. back_speed) -. r.rvz) *. steer in
      { r with rx = r.rx +. vx; rz = r.rz +. vz; rvx = vx; rvz = vz; age = r.age + 1 }

(* when a throw turns on the one who threw it: it has to have left the
 * hand cleanly (three units away) and to have come off something --
 * the fence, or a pillar. Anything else would punish a throw for the
 * way the arena is shaped rather than for a bad idea. *)
let own_risk (r : rang) : bool = r.leg = Out && r.bounced && r.away

(* the ones still in the air, and the owners who just caught theirs *)
let step_rangs (players : player list) (rangs : rang list) : rang list * int list =
  let home (r : rang) =
    let o = List.find (fun p -> p.idx = r.owner) players in
    r.leg = Back && d2 r.rx r.rz o.px o.pz < catch_dist *. catch_dist
  in
  let caught, flying = List.partition home (List.map (step_rang players) rangs) in
  (flying, List.map (fun r -> r.owner) caught)

(*****************************************************************************)
(* Being cut *)
(*****************************************************************************)

(* sliced along (dx, dz): the two halves leave sideways, and the top one
 * a little higher -- the only violence a game about fruit allows *)
let slice (p : player) (dx, dz) : player =
  let sx, sz = norm (-.dz, dx) in
  let h top sign =
    { hx = p.px; hy = (if top then 0.85 else 0.4); hz = p.pz; hvx = sx *. 0.13 *. sign; hvy = (if top then 0.2 else 0.12);
      hvz = sz *. 0.13 *. sign; hspin = 0.; htop = top }
  in
  { p with state = Cut [ h true 1.; h false (-1.) ]; holds = false }

(* the halves stay in the arena: one flying over the fence and on into
 * the void is funny once and confusing after that *)
let in_arena (v : number) : number = Float.max (-.(inner -. 0.4)) (Float.min (inner -. 0.4) v)

let step_half (h : half) : half =
  let hvy = h.hvy -. 0.022 in
  let hy = h.hy +. hvy in
  if hy < 0.22 then
    { h with hy = 0.22; hvy = 0.; hvx = h.hvx *. 0.94; hvz = h.hvz *. 0.94; hx = in_arena (h.hx +. h.hvx);
      hz = in_arena (h.hz +. h.hvz); hspin = h.hspin +. (h.hvx *. 40.) }
  else { h with hx = in_arena (h.hx +. h.hvx); hy; hz = in_arena (h.hz +. h.hvz); hvy; hspin = h.hspin +. 11. }

let step_dead (p : player) : player =
  match p.state with
  | Cut halves -> { p with state = Cut (List.map step_half halves) }
  | Falling n -> { p with state = Falling (n + 1) }
  | Alive -> p

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
            if alive p && (r.owner <> p.idx || own_risk r) && d2 r.rx r.rz p.px p.pz < (cut_dist +. radius) *. (cut_dist +. radius) then
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
            (fun b -> if alive b && b.idx <> a.idx && d2 a.px a.pz b.px b.pz < (slash_dist +. radius) *. (slash_dist +. radius) then Some (b.idx, Camera3d.forward a.heading) else None)
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

(* is the ground clear that far along a direction? The computer looks
 * where it is going: two units for a step, four for a dash, which
 * covers the whole 9 frames of one. (A human gets no such check: you
 * may dash into a pit, and you will.) *)
let way_ok (p : player) (ahead : number) (ux, uz) : bool =
  let at t =
    let x = p.px +. (ux *. t) and z = p.pz +. (uz *. t) in
    (* the pits with a margin: walking along the very edge of one, a
     * step that ends outside it can still clip its corner. The fence
     * counts too -- it stops nobody dead, it just takes the part of
     * your step that was into it, which leaves a computer that ignores
     * it grinding sideways along the wall for a hundred frames *)
    Float.abs x < inner -. radius
    && Float.abs z < inner -. radius
    && (not (List.exists (fun r -> in_rect r ~grow:0.9 x z) pits))
    && not (hits_pillar ~grow:radius x z)
  in
  List.for_all (fun f -> at (ahead *. f)) [ 0.25; 0.5; 0.75; 1. ]

(* A direction that doesn't walk into a pit, a pillar or the fence: the
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
let clear_way (p : player) (dx, dz) : number * number =
  let ok = way_ok p 2. in
  let d = norm (dx, dz) in
  if ok d then d
  else if ok p.way then p.way
  else
    let ux, uz = d in
    let sign v = if v >= 0. then 1. else -1. in
    let sideways =
      (if Float.abs ux > Float.abs uz then [ (sign ux, 0.); (0., sign uz) ] else [ (0., sign uz); (sign ux, 0.) ])
      @ [ (-.sign ux, 0.); (0., -.sign uz) ]
    in
    match List.find_opt ok sideways with Some c -> c | None -> d

let brain (g : game) (p : player) : intent =
  let w = wits_of p.idx in
  let enemies = List.filter (fun q -> alive q && q.idx <> p.idx) g.players in
  match enemies with
  | [] -> idle
  | _ ->
      let target = List.fold_left (fun best q -> if d2 p.px p.pz q.px q.pz < d2 p.px p.pz best.px best.pz then q else best) (List.hd enemies) enemies in
      let to_target = (target.px -. p.px, target.pz -. p.pz) in
      let dist = Float.hypot (fst to_target) (snd to_target) in
      (* is a boomerang coming at me? how far along its line, and how far
       * off it: the same two numbers a player reads off the screen *)
      let threat =
        List.fold_left
          (fun best r ->
            let ux, uz = norm (r.rvx, r.rvz) in
            let relx = p.px -. r.rx and relz = p.pz -. r.rz in
            let along = (relx *. ux) +. (relz *. uz) in
            let across = (relx *. uz) -. (relz *. ux) in
            if along > 0. && along < w.nerve && Float.abs across < 1.9 && (r.owner <> p.idx || own_risk r) then
              match best with Some (a, _, _, _) when a < along -> best | _ -> Some (along, across, ux, uz)
            else best)
          None g.rangs
      in
      (match threat with
      | Some (along, across, ux, uz) ->
          (* out of its line, the way it is already leaning, and a dash
           * if it is nearly here *)
          let side = if across >= 0. then 1. else -1. in
          let away = clear_way p (uz *. side, -.ux *. side) in
          { go = Some away; throw = false; dash_now = along < 2.8 && p.cool = 0 && way_ok p 4.2 away }
      | None ->
          if p.holds then
            let aimed = Float.abs (angle_to p.heading to_target) < w.aim in
            let ready = p.think = 0 && aimed && dist < w.range in
            if dist < 2.4 then
              (* face to face: the slash if the dash is there, and a
               * point-blank throw if it is not. Without that second
               * half two of them can end up nose to nose for ever, one
               * waiting on a cooldown the other is not going to let
               * run out *)
              let at_him = clear_way p to_target in
              let slash = p.cool = 0 && way_ok p 4.2 at_him in
              { go = Some at_him; throw = (not slash) && ready; dash_now = slash }
            else
              (* Three ways of walking, and which one it is decides
               * whether it can shoot at all: far off it closes in
               * obliquely ([slant]), so the three of them don't all
               * arrive along the same line; in range but still
               * hesitating it circles its man; and when it is ready it
               * walks *straight* at him, because facing where you walk
               * is what aiming is here, for the computer exactly as for
               * you. (Keep the slant inside the range and two of them
               * orbit each other for ever, each 22 degrees off a shot
               * it can never take. That deadlock is why the third case
               * exists.) *)
              let want =
                if dist > w.range then turn_vec w.slant to_target
                else if p.think > 0 then (-.snd to_target, fst to_target)
                else to_target
              in
              { go = Some (clear_way p want); throw = ready; dash_now = false }
          else
            (* unarmed: keep away until the boomerang is back *)
            { go = Some (clear_way p (-.fst to_target, -.snd to_target)); throw = false; dash_now = false })

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let step_game (s : model) (k : keyboard) (g : game) : game =
  (* one intent per player, from the keyboard or from [brain] *)
  let intents = List.map (fun p -> if not (alive p) then idle else if p.idx = 0 then keys_intent s k else brain g p) g.players in
  let stepped = List.map2 (fun it p -> if alive p then step_player it p else (step_dead p, None)) intents g.players in
  let players = List.map fst stepped in
  let thrown = List.filter_map snd stepped in
  let rangs, caught = step_rangs players (g.rangs @ thrown) in
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
  { g with players; rangs; ended; clock }

(* the round's point, to the one left standing (nobody, if the clock ran
 * out on two of them) -- counted where the round ends, so that the last
 * one can be looked at with its score already on it *)
let score (g : game) : game =
  let winner = match List.filter alive g.players with [ p ] -> Some p.idx | _ -> None in
  { g with players = List.map (fun p -> if Some p.idx = winner then { p with wins = p.wins + 1 } else p) g.players }

(* the next round: everyone back in their corner, the scores kept *)
let next_round (g : game) : game =
  { players = List.map place g.players; rangs = []; ended = None; clock = 0; round_no = g.round_no + 1 }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  match s.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Playing (new_game ())) s else s
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
let wood = rgb 158 112 68
let stone = rgb 165 160 150
let void = rgb 32 46 74

(* a quad lying flat, facing up (the winding of Playground3d.plane) *)
let quad_up (c : color) (y : number) (x1 : number) (z1 : number) (x2 : number) (z2 : number) : shape3d =
  polygon3d c [ (x1, y, z1); (x1, y, z2); (x2, y, z2); (x2, y, z1) ]

(* the grass, tile by tile with the pits left out; a dark box sunk under
 * each pit so you look down into a hole and not through the world; the
 * fence; the pillars *)
let arena : shape3d =
  let n = int_of_float (2. *. half /. tile) in
  let tiles =
    List.concat
      (List.init n (fun i ->
           List.filter_map
             (fun j ->
               let x1 = -.half +. (float_of_int i *. tile) and z1 = -.half +. (float_of_int j *. tile) in
               let x2 = x1 +. tile and z2 = z1 +. tile in
               if in_pit ((x1 +. x2) /. 2.) ((z1 +. z2) /. 2.) then None
               else Some (quad_up (if (i + j) mod 2 = 0 then grass1 else grass2) 0. x1 z1 x2 z2))
             (List.init n Fun.id)))
  in
  let pit_boxes = List.map (fun p -> box (rgb 46 44 52) ((p.hw *. 2.) +. 2.) 4. ((p.hd *. 2.) +. 2.) |> move3d p.cx (-3.4) p.cz) pits in
  let fence =
    [ box wood 24. 1.2 2. |> move3d 0. 0.6 (-.half +. 1.); box wood 24. 1.2 2. |> move3d 0. 0.6 (half -. 1.);
      box wood 2. 1.2 24. |> move3d (-.half +. 1.) 0.6 0.; box wood 2. 1.2 24. |> move3d (half -. 1.) 0.6 0. ]
  in
  let blocks = List.map (fun p -> box stone (p.hw *. 2.) 1.7 (p.hd *. 2.) |> move3d p.cx 0.85 p.cz) pillars in
  cached3d (tiles @ pit_boxes @ fence @ blocks)

(* one quad on the grass: what tells you, from up here, where something
 * flying actually is *)
let shadow (x : number) (z : number) (r : number) : shape3d list =
  if in_pit x z || Float.abs x > inner || Float.abs z > inner then []
  else [ quad_up (rgb 74 118 60) 0.03 (x -. r) (z -. r) (x +. r) (z +. r) ]

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

let rang_y (r : rang) : number = 0.78 +. (0.08 *. sin (float_of_int r.age *. 0.25))

let player_shapes (p : player) : shape3d list =
  match p.state with
  | Alive -> shadow p.px p.pz 0.5 @ [ food_shape p.kind p.heading |> move3d p.px 0. p.pz ]
  | Cut halves -> List.concat_map (fun h -> shadow h.hx h.hz 0.35 @ [ half_shape p.kind h ]) halves
  | Falling n ->
      let y = -0.25 *. float_of_int n in
      if y < -6. then [] else [ food_shape p.kind p.heading |> move3d p.px y p.pz ]

let rang_shapes (players : player list) (r : rang) : shape3d list =
  let owner = List.find (fun p -> p.idx = r.owner) players in
  shadow r.rx r.rz 0.3
  @ [ rang_shape (rgb 225 200 130) (float_of_int r.age *. 26.) |> move3d r.rx (rang_y r) r.rz;
      (* a spark of the owner's color in the middle: whose it is *)
      box (food_color owner.kind) 0.2 0.16 0.2 |> move3d r.rx (rang_y r) r.rz ]

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text color size str = words color str |> scale size

(* fixed, high, and nearly isometric: the party game's one screen *)
let arena_camera : camera = Camera3d.from_far ~fov:34. ~offset:(0., 27., 29.) (0., 0., 1.)

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
  [ Camera3d.floor ~color:void ~ground:(-12.) arena_camera; arena ]
  @ List.concat_map player_shapes g.players
  @ List.concat_map (rang_shapes g.players) g.rangs

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      (* far enough back that the words have somewhere to sit *)
      let cam = Camera3d.orbit ~fov:36. ~distance:33. ~height:23. ~look:7. (spin 24. computer.time) (0., 0., 0.) in
      let ring =
        List.mapi
          (fun i kind ->
            let a = float_of_int i *. 90. in
            let x, z = Camera3d.forward a in
            food_shape kind (a +. 180.) |> move3d (x *. 4.5) 0. (z *. 4.5))
          foods
      in
      ( cam,
        [ Camera3d.floor ~color:void ~ground:(-12.) cam; arena ] @ ring
        @ [ rang_shape (rgb 225 200 130) (spin 1.2 computer.time) |> move3d 0. (1.4 +. (0.2 *. wave 0. 1. 2. computer.time)) 0. ]
        @ List.map hud
            ([ text (rgb 130 190 70) 6. "TINY BOOMERANG FU" |> move_y 300.;
               text white 2.4 "arrows: move    space: throw    x: dash" |> move_y 235.;
               text gray 2.1 "thrown, it cuts anyone -- you too. In hand, the dash is a slash." |> move_y 195.;
               text gray 2.1 (Printf.sprintf "first to %d rounds" rounds_to_win) |> move_y 160. ]
            @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 110. ]) )
  | Playing g ->
      ( arena_camera,
        view_game g
        @ List.map hud
            (scoreboard screen g @ banner s g
            @ [ text gray 2. (Printf.sprintf "ROUND %d" g.round_no) |> move_y (screen.bottom +. 30.) ]) )
  | Winner g ->
      let champion = List.fold_left (fun best p -> if p.wins > best.wins then p else best) (List.hd g.players) g.players in
      ( arena_camera,
        view_game g
        @ List.map hud
            (scoreboard screen g
            @ [ text (food_color champion.kind) 6. (Printf.sprintf "%s WINS!" (if champion.idx = 0 then "YOU" else food_name champion.kind)) |> move_y 80. ]
            @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-40.) ]) )

let app = game3d view update initial_model

(* the foods are spheres, so they are worth shading smoothly; nothing
 * here needs the back faces (no sky: the arena floats over a void) *)
let main = Playground3d_platform.run_app3d app
