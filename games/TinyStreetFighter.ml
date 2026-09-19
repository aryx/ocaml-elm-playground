(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Street Fighter II (Capcom, 1991, Akira Nishitani and
 * Akira Yasuda): two fighters, one screen, best of three rounds. You are
 * white: a/d to walk (away from the other: block), w to jump, s to
 * crouch (and block low), f to punch, g to kick; down, down-forward,
 * forward and punch (a "quarter circle") throws a fireball. The computer
 * is red, or a second player: the arrows, k to punch, l to kick (2 on
 * the title).
 *
 * Street Fighter II made fighting games a genre, and the arcade a
 * tournament: players learned each move's frames, and found that some
 * moves, landing, left them time to hit again before the other could
 * block -- combos, an accident the designers kept, and built the next
 * games on. Karate Champ (Technōs, 1984) and the first Street Fighter
 * (1987) came before; Mortal Kombat (Midway, 1992) answered. (Names and
 * dates from memory, to check.)
 *
 * What's new here, with the brawler kit (kits/brawler/):
 *
 *  - Moves in frames ([moves], Frame_data): a jab is 3 frames of
 *    startup, 2 active, 6 of recovery; hit, the other is stunned 12:
 *    +5, time for another jab. The fireball's startup is 12: throw it
 *    too close, and you're hit first.
 *
 *  - Boxes (Hitbox): a move hits when its hitbox, while active, meets
 *    the other's hurtbox -- a crouching fighter's is lower, and a jab
 *    goes over it. The flag hitboxes draws them.
 *
 *  - Blocking by holding back ([blocking]): standing blocks high and mid
 *    (the jump kick is an overhead: stand to block it), crouching blocks
 *    low and mid (the low kick must be blocked crouching): a guessing
 *    game, the heart of the genre.
 *
 *  - Special moves from an input history ([quarter_circle]): the last
 *    frames' directions, as the fighter faces, searched for down,
 *    down-forward, forward in that order -- the motion of the stick,
 *    recognized.
 *
 *  - Hitstop ([hitstop]): at each hit, the whole game stops for a few
 *    frames. A punch is felt more than it's seen; the pause says it
 *    connected. (Every fighting game does it; so do action games since.)
 *
 * What it uses: the brawler kit (Hitbox, Frame_data, Stickman: the
 * fighters are stick figures, their moves key poses), Scene2d. Not
 * Physics: a jump is the same arc every time, as in the arcade, and the
 * fighters push each other apart by a rule ([separate]), not by forces.
 *
 * Exercises: throws (close, forward and punch: unblockable), the dragon
 * punch (forward, down, down-forward: invincible as it rises), combos
 * counted on the screen, more fighters (each a table of moves and poses),
 * rollback netcode for two players on two computers (GGPO, see
 * plan_networking_teaching.md).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* Moves *)
(*****************************************************************************)

type attack = Jab | Kick | Low_kick | Jump_kick | Fireball

(* what a block needs: standing (a jump kick), crouching (a low kick),
 * or either *)
type height = High | Low | Mid

let move_of (a : attack) : Frame_data.move =
  match a with
  | Jab -> { startup = 3; active = 2; recovery = 6; damage = 5; hitstun = 12; blockstun = 8; hitbox = { x = 75.; y = 170.; w = 50.; h = 30. } }
  | Kick -> { startup = 6; active = 3; recovery = 14; damage = 9; hitstun = 16; blockstun = 10; hitbox = { x = 95.; y = 120.; w = 70.; h = 40. } }
  | Low_kick -> { startup = 5; active = 3; recovery = 12; damage = 7; hitstun = 14; blockstun = 9; hitbox = { x = 95.; y = 20.; w = 80.; h = 30. } }
  | Jump_kick -> { startup = 4; active = 14; recovery = 2; damage = 9; hitstun = 16; blockstun = 10; hitbox = { x = 65.; y = 40.; w = 70.; h = 50. } }
  | Fireball -> { startup = 12; active = 1; recovery = 22; damage = 10; hitstun = 18; blockstun = 12; hitbox = { x = 0.; y = 0.; w = 0.; h = 0. } }

let height_of (a : attack) : height = match a with Low_kick -> Low | Jump_kick -> High | _ -> Mid

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state =
  | Idle
  | Walk
  | Crouch
  | Jump of number (* sideways speed *)
  | Attacking of attack * int (* frames since it started *)
  | Hit of int (* stunned, frames left *)
  | Blocking of int (* blockstun, frames left *)
  | Down (* knocked out *)

type dir = { back : bool; forward : bool; up : bool; down : bool }

(* a fighter's keys this frame, as it faces: forward is towards the other *)
type input = { dir : dir; punch : bool; kick : bool }

type fighter = {
  x : number;
  y : number; (* height above the floor, jumping *)
  vy : number;
  facing : number; (* 1. right, -1. left *)
  hp : int;
  state : state;
  history : (int * dir) list; (* the directions of the last frames, most recent first *)
  wins : int;
  hit_done : bool; (* this attack has hit: once only *)
}

type fireball = { fx : number; fy : number; dir_x : number; owner : int }

type game = {
  p1 : fighter;
  p2 : fighter;
  fireballs : fireball list;
  sparks : (number * number * int) list;
  hitstop : int; (* frames the game stands still *)
  timer : int; (* frames left in the round *)
  round : int;
  over : int; (* frames since the round ended, 0 while it goes on *)
  two_players : bool;
  plan : input list; (* the computer's next inputs, a fireball's motion *)
  seed : int;
  frames : int;
}

type scene = Title | Fight of game | Winner of game
type model = scene Scene2d.t

let floor_y = -300.
let tall = 220.
let edge = 440.

let new_fighter (x : number) (facing : number) (wins : int) : fighter =
  { x; y = 0.; vy = 0.; facing; hp = 100; state = Idle; history = []; wins; hit_done = false }

let new_round (g : game) : game =
  { g with p1 = new_fighter (-200.) 1. g.p1.wins; p2 = new_fighter 200. (-1.) g.p2.wins; fireballs = []; sparks = []; hitstop = 0; timer = 60 *.. 60; round = g.round +.. 1; over = 0 }

let new_game (two_players : bool) : game =
  new_round { p1 = new_fighter 0. 1. 0; p2 = new_fighter 0. (-1.) 0; fireballs = []; sparks = []; hitstop = 0; timer = 0; round = 0; over = 0; two_players; plan = []; seed = 7; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Inputs *)
(*****************************************************************************)

let read (facing : number) (left : bool) (right : bool) (up : bool) (down : bool) (punch : bool) (kick : bool) : input =
  let forward = if facing > 0. then right else left and back = if facing > 0. then left else right in
  { dir = { back; forward; up; down }; punch; kick }

(* [quarter_circle history]: down, then down-forward, then forward, in
 * that order, in the last 20 frames (most recent first in [history]).
 * E.g. 10 frames ago down, 7 down-forward, 3 forward: yes; forward then
 * down: no. *)
let quarter_circle (now : int) (history : (int * dir) list) : bool =
  let recent = List.rev (List.filter (fun (f, _) -> now -.. f <= 20) history) in
  let is_down d = d.down && not d.forward and is_df d = d.down && d.forward and is_fwd d = d.forward && not d.down in
  let rec find stage l =
    match (stage, l) with
    | 3, _ -> true
    | _, [] -> false
    | 0, (_, d) :: rest -> find (if is_down d then 1 else 0) rest
    | 1, (_, d) :: rest -> find (if is_df d then 2 else 1) rest
    | _, (_, d) :: rest -> find (if is_fwd d then 3 else 2) rest
  in
  find 0 recent

(*****************************************************************************)
(* A fighter's frame *)
(*****************************************************************************)

let can_act (f : fighter) : bool = match f.state with Idle | Walk | Crouch -> true | _ -> false

(* holding back, able to act: blocking, standing or crouching *)
let blocking (f : fighter) (i : input) : bool = can_act f && i.dir.back

let gravity = 0.9

(* [step_fighter now i f]: what the fighter does, by its state: walking,
 * crouching, jumping, attacking (the fireball one of its moves, found
 * in the history of the stick), or its stun running out *)
let step_fighter (now : int) (i : input) (f : fighter) : fighter =
  let f = { f with history = List.filter (fun (t, _) -> now -.. t <= 30) ((now, i.dir) :: f.history) } in
  match f.state with
  | Idle | Walk | Crouch ->
      if i.punch && quarter_circle now f.history then { f with state = Attacking (Fireball, 1); hit_done = false }
      else if i.dir.up then { f with state = Jump ((if i.dir.forward then 4. else if i.dir.back then -4. else 0.) * f.facing); vy = 16. }
      else if i.punch then { f with state = Attacking (Jab, 1); hit_done = false }
      else if i.kick then { f with state = Attacking ((if i.dir.down then Low_kick else Kick), 1); hit_done = false }
      else if i.dir.down then { f with state = Crouch }
      else if i.dir.forward then { f with x = f.x + (4. * f.facing); state = Walk }
      else if i.dir.back then { f with x = f.x - (3. * f.facing); state = Walk }
      else { f with state = Idle }
  | Jump vx ->
      let vy = f.vy - gravity in
      let y = f.y + vy in
      if y <= 0. then { f with y = 0.; vy = 0.; state = Idle }
      else if i.kick || i.punch then { f with x = f.x + vx; y; vy; state = Attacking (Jump_kick, 1); hit_done = false }
      else { f with x = f.x + vx; y; vy; state = Jump vx }
  | Attacking (Jump_kick, n) ->
      let vy = f.vy - gravity in
      let y = f.y + vy in
      if y <= 0. then { f with y = 0.; vy = 0.; state = Idle } else { f with x = f.x + (3. * f.facing); y; vy; state = Attacking (Jump_kick, n +.. 1) }
  | Attacking (a, n) -> if n >= Frame_data.length (move_of a) then { f with state = (if i.dir.down then Crouch else Idle) } else { f with state = Attacking (a, n +.. 1) }
  | Hit n -> if n <= 1 then { f with state = Idle } else { f with state = Hit (n -.. 1) }
  | Blocking n -> if n <= 1 then { f with state = Idle } else { f with state = Blocking (n -.. 1) }
  | Down -> f

let crouching (f : fighter) (i : input) : bool = f.state = Crouch || (can_act f && i.dir.down)

let hurtbox (f : fighter) (i : input) : Hitbox.box =
  let b : Hitbox.box = if crouching f i || (match f.state with Attacking (Low_kick, _) -> true | _ -> false) then { x = 0.; y = 70.; w = 80.; h = 140. } else { x = 0.; y = 110.; w = 70.; h = 220. } in
  Hitbox.place f.facing (f.x, f.y) b

(* the fighters don't walk through each other, nor off the screen *)
let separate (a : fighter) (b : fighter) : fighter * fighter =
  let a = { a with x = clamp (-.edge) edge a.x } and b = { b with x = clamp (-.edge) edge b.x } in
  let gap = 70. - Float.abs (a.x - b.x) in
  if gap <= 0. || (a.y > 60. || b.y > 60.) then (a, b)
  else
    let s = if a.x < b.x then -1. else 1. in
    ({ a with x = clamp (-.edge) edge (a.x + (s * gap / 2.)) }, { b with x = clamp (-.edge) edge (b.x - (s * gap / 2.)) })

(* [strike attacker ai defender di]: the attacker's move, active, its
 * hitbox on the other's hurtbox: blocked (the right way), or a hit *)
let strike (a : fighter) (d : fighter) (di : input) : fighter * fighter * (number * number) option =
  match a.state with
  | Attacking (att, n) when att <> Fireball && (not a.hit_done) && Frame_data.phase (move_of att) n = Frame_data.Active ->
      let m = move_of att in
      let hb = Hitbox.place a.facing (a.x, a.y) m.hitbox in
      if not (Hitbox.overlap hb (hurtbox d di)) then (a, d, None)
      else
        let blocked = blocking d di && (match height_of att with High -> not (crouching d di) | Low -> crouching d di | Mid -> true) in
        let a = { a with hit_done = true } in
        if blocked then (a, { d with state = Blocking m.blockstun; x = d.x + (a.facing * 12.) }, None)
        else (a, { d with hp = max 0 (d.hp -.. m.damage); state = Hit m.hitstun; x = d.x + (a.facing * 18.); y = 0.; vy = 0. }, Some (hb.x, hb.y))
  | _ -> (a, d, None)

(*****************************************************************************)
(* The computer *)
(*****************************************************************************)

(* the other's keys, chosen: jumping a fireball coming, blocking an
 * attack starting (one time in two), throwing fireballs from afar --
 * planned as a player's: down, down-forward, forward and punch, one a
 * frame, recognized by the same [quarter_circle] --, walking in, and at
 * close range a jab, a kick or a low kick *)
let computer_input (g : game) (me : fighter) (other : fighter) : input * input list * int =
  let seed = ((g.seed *.. 1103515245) +.. 12345) land 0x7fffffff in
  let r = seed /.. 65536 mod 100 in
  let none = { back = false; forward = false; up = false; down = false } in
  let just d = { dir = d; punch = false; kick = false } in
  let dist = Float.abs (me.x - other.x) in
  let fireball_coming = List.exists (fun fb -> fb.owner <> 2 && Float.abs (fb.fx - me.x) < 260. && (me.x - fb.fx) * fb.dir_x > 0.) g.fireballs in
  let other_starting = match other.state with Attacking (a, n) -> Frame_data.phase (move_of a) n = Frame_data.Startup | _ -> false in
  match g.plan with
  | next :: rest -> (next, rest, seed)
  | [] ->
      if not (can_act me) then (just none, [], seed)
      else if fireball_coming then (just { none with up = r < 60; back = r >= 60 }, [], seed)
      else if other_starting && dist < 200. && r < 50 then (just { none with back = true; down = (match other.state with Attacking (Low_kick, _) -> true | _ -> false) }, [], seed)
      else if dist > 380. && r < 3 && not (List.exists (fun fb -> fb.owner = 2) g.fireballs) then
        (just { none with down = true }, [ just { none with down = true; forward = true }; { dir = { none with forward = true }; punch = true; kick = false } ], seed)
      else if dist > 150. then (just { none with forward = r < 70 }, [], seed)
      else if r < 6 then ({ dir = none; punch = true; kick = false }, [], seed)
      else if r < 10 then ({ dir = none; punch = false; kick = true }, [], seed)
      else if r < 13 then ({ dir = { none with down = true }; punch = false; kick = true }, [], seed)
      else (just none, [], seed)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let hitstop = 6

let update_fight (computer : computer) (scenes : model) (g : game) : game =
  let g = { g with frames = g.frames +.. 1; sparks = List.filter_map (fun (x, y, n) -> if n < 12 then Some (x, y, n +.. 1) else None) g.sparks } in
  if g.hitstop > 0 then { g with hitstop = g.hitstop -.. 1 }
  else if g.over > 0 then { g with over = g.over +.. 1 }
  else
    let k = computer.keyboard in
    let pressed key = Scene2d.pressed (fun kb -> Set_.mem key kb.keys) scenes in
    let i1 = read g.p1.facing k.ka k.kd k.kw k.ks (pressed "f") (pressed "g") in
    let i2, plan, seed =
      if g.two_players then (read g.p2.facing k.kleft k.kright k.kup k.kdown (pressed "k") (pressed "l"), [], g.seed)
      else computer_input g g.p2 g.p1
    in
    let g = { g with plan; seed } in
    let p1 = step_fighter g.frames i1 g.p1 and p2 = step_fighter g.frames i2 g.p2 in
    (* a fireball's 12th frame: it's thrown *)
    let thrown (f : fighter) owner = match f.state with Attacking (Fireball, 12) -> [ { fx = f.x + (60. * f.facing); fy = 150.; dir_x = f.facing; owner } ] | _ -> [] in
    let fireballs = List.map (fun fb -> { fb with fx = fb.fx + (8. * fb.dir_x) }) g.fireballs @ thrown p1 1 @ thrown p2 2 in
    (* the hits, both ways, at the same frame: trades happen *)
    let p1', p2', s1 = strike p1 p2 i2 in
    let p2'', p1'', s2 = strike p2' p1' i1 in
    let p1, p2 = (p1'', p2'') in
    (* the fireballs hitting, blocked (a little damage: chip), or leaving *)
    let hits fb (f : fighter) (i : input) = Hitbox.overlap { x = fb.fx; y = fb.fy; w = 40.; h = 30. } (hurtbox f i) in
    let fb_hit (i : input) owner (fbs, (f : fighter), spark) fb =
      if fb.owner = owner || not (hits fb f i) then (fb :: fbs, f, spark)
      else if blocking f i && not (crouching f i) then (fbs, { f with hp = max 0 (f.hp -.. 2); state = Blocking 12 }, spark)
      else (fbs, { f with hp = max 0 (f.hp -.. 10); state = Hit 18; y = 0.; vy = 0. }, Some (fb.fx, fb.fy))
    in
    let fireballs, p2, s3 = List.fold_left (fb_hit i2 2) ([], p2, None) fireballs in
    let fireballs, p1, s4 = List.fold_left (fb_hit i1 1) ([], p1, None) fireballs in
    let fireballs = List.filter (fun fb -> Float.abs fb.fx < 560.) fireballs in
    (* facing each other, apart *)
    let face (f : fighter) (o : fighter) = if can_act f && f.y = 0. then { f with facing = (if o.x > f.x then 1. else -1.) } else f in
    let p1, p2 = separate (face p1 p2) (face p2 p1) in
    let sparks = List.filter_map (fun s -> Option.map (fun (x, y) -> (x, floor_y + y, 0)) s) [ s1; s2; s3; s4 ] in
    if sparks <> [] then Audio.play Audio.hit;
    let g = { g with p1; p2; fireballs; sparks = sparks @ g.sparks; hitstop = (if sparks <> [] then hitstop else 0); timer = g.timer -.. 1 } in
    (* the round's end: a knockout, or the time out *)
    if g.p1.hp = 0 || g.p2.hp = 0 || g.timer <= 0 then begin
      Audio.play Audio.explosion;
      let p1_wins = g.p1.hp > g.p2.hp and p2_wins = g.p2.hp > g.p1.hp in
      { g with over = 1; p1 = { g.p1 with wins = g.p1.wins +.. (if p1_wins then 1 else 0); state = (if g.p1.hp = 0 then Down else g.p1.state) };
        p2 = { g.p2 with wins = g.p2.wins +.. (if p2_wins then 1 else 0); state = (if g.p2.hp = 0 then Down else g.p2.state) } }
    end
    else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let key c = Scene2d.pressed (fun k -> Set_.mem c k.keys) s in
  match s.scene with
  | Title ->
      if Scene2d.pressed (fun k -> k.kspace) s || key "1" then Scene2d.go (Fight (new_game false)) s
      else if key "2" then Scene2d.go (Fight (new_game true)) s
      else s
  | Fight g ->
      let g = update_fight computer s g in
      if g.over > 150 then if g.p1.wins >= 2 || g.p2.wins >= 2 then Scene2d.go (Winner g) s else Scene2d.go (Fight (new_round g)) s
      else { s with scene = Fight g }
  | Winner _ -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the poses: a guard, walking, crouching, jumping; each attack's key
 * frames; hit, blocking, down *)
let guard : Stickman.pose = { lean = 5.; front_arm = (40., 150.); back_arm = (25., 150.); front_leg = (22., 5.); back_leg = (-22., -5.) }
let walk2 : Stickman.pose = { guard with front_leg = (5., -10.); back_leg = (-5., 10.) }
let crouch : Stickman.pose = { lean = 20.; front_arm = (50., 160.); back_arm = (30., 160.); front_leg = (85., -10.); back_leg = (-50., 20.) }
let jump : Stickman.pose = { lean = 10.; front_arm = (60., 160.); back_arm = (30., 160.); front_leg = (80., -20.); back_leg = (20., -60.) }
let jab : Stickman.pose = { guard with front_arm = (90., 90.); lean = 12. }
let kick : Stickman.pose = { guard with lean = -20.; front_leg = (95., 95.); back_leg = (-5., 0.) }
let low_kick : Stickman.pose = { crouch with front_leg = (85., 88.); lean = 10. }
let jump_kick : Stickman.pose = { jump with front_leg = (75., 95.) }
let fireball : Stickman.pose = { guard with lean = 15.; front_arm = (85., 85.); back_arm = (80., 80.) }
let hurt : Stickman.pose = { guard with lean = -25.; front_arm = (-30., -50.); back_arm = (-40., -60.) }
let block : Stickman.pose = { guard with lean = -8.; front_arm = (70., 175.); back_arm = (60., 170.) }
let crouch_block : Stickman.pose = { crouch with front_arm = (70., 175.); back_arm = (60., 170.) }

(* an attack's poses: from the guard to the move by the end of its
 * startup, held while active, back to the guard during recovery *)
let attack_pose (a : attack) (n : int) (base : Stickman.pose) : Stickman.pose =
  let m = move_of a in
  let target = match a with Jab -> jab | Kick -> kick | Low_kick -> low_kick | Jump_kick -> jump_kick | Fireball -> fireball in
  Stickman.at [ (0, base); (m.startup, target); (m.startup +.. m.active, target); (Frame_data.length m, base) ] n

let pose_of (f : fighter) (frames : int) : Stickman.pose =
  match f.state with
  | Idle -> guard
  | Walk -> if frames /.. 8 mod 2 = 0 then guard else walk2
  | Crouch -> crouch
  | Jump _ -> jump
  | Attacking (a, n) -> attack_pose a n (match a with Low_kick -> crouch | Jump_kick -> jump | _ -> guard)
  | Hit _ -> hurt
  | Blocking _ -> block
  | Down -> hurt

let view_fighter (color : color) (back : color) (frames : int) (f : fighter) : shape =
  let figure = Stickman.draw color back tall f.facing (pose_of f frames) in
  let figure = if f.state = Down then figure |> rotate (90. * f.facing) |> move (-.f.facing * 20.) (-.60.) else figure in
  figure |> move f.x (floor_y + f.y)

let health (x : number) (f : fighter) (name : string) (align : number) : shape list =
  let w = 380. * float_of_int f.hp / 100. in
  [ rectangle (rgb 120 20 20) 380. 26. |> move x 440.; rectangle (rgb 240 200 40) w 26. |> move (x - (align * (380. - w) / 2.)) 440.;
    text white 2.3 name |> move (x - (align * 150.)) 400. ]
  @ List.init f.wins (fun i -> circle (rgb 240 200 40) 8. |> move (x - (align * (170. - (float_of_int i * 22.)))) 400.)

(* a hit: a star of light, growing and fading *)
let star_spark (n : int) : shape =
  let r = 12. + (float_of_int n * 3.) in
  group (List.init 6 (fun i -> rectangle (if n mod 4 < 2 then yellow else white) r 4. |> rotate (float_of_int i * 30.))) |> fade (1. - (float_of_int n / 12.))

let view_fight (computer : computer) (g : game) : shape list =
  let hitboxes = List.mem_assoc "hitboxes" computer.flags in
  let boxes (f : fighter) =
    let none = { dir = { back = false; forward = false; up = false; down = false }; punch = false; kick = false } in
    Hitbox.draw green (hurtbox f none)
    :: (match f.state with Attacking (a, n) when Frame_data.phase (move_of a) n = Frame_data.Active -> [ Hitbox.draw red (Hitbox.place f.facing (f.x, f.y) (move_of a).hitbox) |> move_y floor_y ] | _ -> [])
  in
  [ rectangle (rgb 250 150 80) 1000. 500. |> move_y 250.; rectangle (rgb 250 190 120) 1000. 550. |> move_y (-25.); circle (rgb 255 230 150) 90. |> move 250. 150.;
    rectangle (rgb 120 80 60) 1000. 200. |> move_y (-400.); rectangle (rgb 90 60 45) 1000. 4. |> move_y floor_y ]
  @ [ view_fighter (rgb 240 240 240) (rgb 170 170 170) g.frames g.p1; view_fighter (rgb 230 50 40) (rgb 150 30 25) g.frames g.p2 ]
  @ List.map (fun fb -> group [ circle (rgb 90 170 255) 22.; circle white 10. ] |> move fb.fx (floor_y + fb.fy)) g.fireballs
  @ List.map (fun (x, y, n) -> star_spark n |> move x y) g.sparks
  @ (if hitboxes then List.concat_map (fun f -> List.map (move_y floor_y) (boxes f)) [ g.p1; g.p2 ] else [])
  @ health (-280.) g.p1 "WHITE" 1. @ health 280. g.p2 (if g.two_players then "RED" else "COMPUTER") (-1.)
  @ [ text white 4. (string_of_int (max 0 (g.timer /.. 60))) |> move_y 430. ]
  @ (if g.frames < 90 && g.over = 0 && g.timer > (60 *.. 60) -.. 90 then [ text white 5. (if g.timer > (60 *.. 60) -.. 50 then Printf.sprintf "ROUND %d" g.round else "FIGHT!") ] else [])
  @ if g.over > 0 then [ text (rgb 240 200 40) 7. (if g.p1.hp = 0 || g.p2.hp = 0 then "K.O." else "TIME") ] else []


let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_fight computer (new_game false)
      @ [ rectangle black 820. 300. |> fade 0.85 |> move_y 40.; text (rgb 240 200 40) 7. "TINY STREET FIGHTER" |> move_y 140.;
          text white 2.2 "a/d walk (back: block)  w jump  s crouch  f punch  g kick" |> move_y 70.;
          text white 2.2 "down, down-forward, forward + punch: a fireball" |> move_y 35.;
          text white 2.2 "2 players: the arrows, k punch, l kick" |> move_y 0. ]
      @ Scene2d.blink 1. s [ text yellow 3. "SPACE OR 1: VS COMPUTER   2: TWO PLAYERS" |> move_y (-60.) ]
  | Fight g -> view_fight computer g
  | Winner g ->
      view_fight computer g @ [ text (rgb 240 200 40) 6. (if g.p1.wins >= 2 then "WHITE WINS" else if g.two_players then "RED WINS" else "YOU LOSE") |> move_y 100. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 20. ])

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
