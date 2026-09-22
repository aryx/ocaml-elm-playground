(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Virtua Fighter (Yu Suzuki, Sega AM2, 1993): two
 * fighters of flat-shaded boxes on a raised ring, best of three rounds,
 * and you can win a round by knocking the other *off* it. Left/right to
 * walk (away from the other: block), down to crouch, f to punch, g to
 * kick, down and g for a low kick. The computer fights back.
 *
 * Virtua Fighter was the first 3D fighting game, and what it changed
 * was not the rules -- Street Fighter II's frames and boxes are here
 * unaltered, out of the same kit -- but what a character *is*. Capcom
 * drew Ryu a few hundred times. Sega could not draw a polygon figure a
 * few hundred times, so a fighter became a skeleton: a torso, a head,
 * four limbs of two parts each, and a pose is the angles of its
 * joints. A move is three or four key poses, and the frames between
 * are interpolated. Every 3D character since is animated this way.
 *
 * So this game is games/TinyStreetFighter with its figure replaced:
 *
 *     games/TinyStreetFighter      games3d/TinyVirtuaFighter
 *     Stickman: lines, in 2D       Skeleton: boxes, in 3D
 *             \                        /
 *          Hitbox, Frame_data: the same fight
 *
 * What the third dimension brings, and what this file is about:
 *
 *  - Hierarchical transforms (gamekits/brawler/3d/Skeleton.mli): a forearm
 *    is drawn in its upper arm's frame, so a shoulder's angle moves the
 *    hand without the hand knowing. In 2D each line could be drawn
 *    where it lay; here a limb must be built, bent, and only then
 *    turned by its parent.
 *
 *  - Keyframes ([poses]): a jab is three poses -- guard, arm out,
 *    guard again -- at frames that match the move's own startup, active
 *    and recovery. The animation and the rules read the same numbers,
 *    which is why the fist is out exactly while the move can hit.
 *
 *  - The ring ([ring_half]), Virtua Fighter's own rule: the floor ends.
 *    A hit pushes, a blocked hit pushes less, and a fighter pushed past
 *    the edge loses the round however much health is left -- a second
 *    way to win that a wall-bounded game does not have, and the reason
 *    VF players fight for position rather than for damage.
 *
 *  - A camera that frames two things ([camera_for]): it watches the
 *    point between the fighters and backs away as they separate, which
 *    is the 3D counterpart of a 2D fighting game's zooming camera.
 *    Nothing else in this repository has had to keep two subjects in
 *    view at once.
 *
 * Uses: the brawler kit's Frame_data and Hitbox (unchanged, shared with
 * games/TinyStreetFighter and games/TinyFinalFight) and its 3D half's
 * Skeleton, Scene2d, Camera3d. Not Physics3d (a jump is the same arc
 * every time, as in the arcade, and a push is a rule, not a force), not
 * a sidestep: Virtua Fighter 1 fought on a line too, and the ring is
 * what made it three-dimensional.
 *
 * Exercises: the sidestep VF2 added (a third axis to the fight, and
 * hitboxes that need a width); throws (a move that wins when it lands
 * and loses when it does not); a replay camera that circles the last
 * hit; a shadow under each fighter (games3d/TinyMario64's) so the ring
 * edge reads even better.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The ring *)
(*****************************************************************************)

(* the fight is on the x axis, the camera to -z of it; a fighter is
 * about 1.8 tall, so the ring is some six of them across *)
let fighter_height = 1.8
let ring_half = 5.5
let ring_top = 0.
let ring_drop = 1.6

let ring : shape3d =
  let floor = 2. *. ring_half in
  cached3d
    [ box (rgb 186 154 106) floor 0.35 floor |> move_y3d (ring_top -. 0.175);
      box (rgb 150 120 80) (floor +. 0.7) 0.5 (floor +. 0.7) |> move_y3d (ring_top -. 0.6);
      box (rgb 120 96 64) (floor +. 1.2) 0.6 (floor +. 1.2) |> move_y3d (ring_top -. 1.15);
      (* a rim, so that the edge is visible from the fighting camera *)
      box (rgb 210 70 60) floor 0.12 0.3 |> move3d 0. (ring_top +. 0.06) (-.ring_half);
      box (rgb 210 70 60) floor 0.12 0.3 |> move3d 0. (ring_top +. 0.06) ring_half;
      box (rgb 210 70 60) 0.3 0.12 floor |> move3d (-.ring_half) (ring_top +. 0.06) 0.;
      box (rgb 210 70 60) 0.3 0.12 floor |> move3d ring_half (ring_top +. 0.06) 0. ]

(*****************************************************************************)
(* Moves: the same frames as games/TinyStreetFighter's *)
(*****************************************************************************)

type attack = Punch | Kick | Low_kick

(* The boxes are in the fighter's own units -- along the way it faces,
 * and up from its feet -- so they are the 2D kit's boxes, unchanged: a
 * fight on a line needs no third dimension to know what hit what. *)
let move_of (a : attack) : Frame_data.move =
  match a with
  | Punch ->
      { startup = 4; active = 3; recovery = 8; damage = 6; hitstun = 14; blockstun = 9;
        hitbox = { x = 0.75; y = 1.45; w = 0.55; h = 0.3 } }
  | Kick ->
      { startup = 7; active = 4; recovery = 16; damage = 11; hitstun = 18; blockstun = 11;
        hitbox = { x = 0.95; y = 1.05; w = 0.75; h = 0.4 } }
  | Low_kick ->
      { startup = 6; active = 3; recovery = 13; damage = 8; hitstun = 15; blockstun = 10;
        hitbox = { x = 0.9; y = 0.3; w = 0.8; h = 0.3 } }

let low (a : attack) : bool = a = Low_kick

(* how far a hit pushes the one who takes it, and how far a blocked one
 * does: this is what walks a fighter towards the edge *)
let push_of (a : attack) : number = match a with Punch -> 0.28 | Kick -> 0.55 | Low_kick -> 0.35

(*****************************************************************************)
(* Poses *)
(*****************************************************************************)

(* The arms are *tucked*, elbows down and forearms across the chest,
 * not half held out: a guard that already reaches halfway makes a
 * punch look like a twitch, and the keyframe test says so in numbers. *)
let guard : Skeleton.pose =
  { lean = 6.;
    turn = -18.;
    front_arm = Skeleton.limb ~yaw:14. ~bend:128. 22.;
    back_arm = Skeleton.limb ~yaw:(-12.) ~bend:132. 16.;
    front_leg = Skeleton.limb ~bend:14. 12.;
    back_leg = Skeleton.limb ~bend:16. (-14.) }

let crouch : Skeleton.pose =
  { guard with
    lean = 24.;
    front_leg = Skeleton.limb ~bend:70. 38.;
    back_leg = Skeleton.limb ~bend:74. (-34.) }

let punch_out : Skeleton.pose =
  { guard with lean = 10.; turn = 8.; front_arm = Skeleton.limb ~yaw:0. ~bend:4. 92. }

(* leaning back over a planted, bent back leg: without the lean a
 * raised leg reads as a long step rather than a kick *)
let kick_out : Skeleton.pose =
  { guard with
    lean = -22.;
    front_leg = Skeleton.limb ~bend:6. 98.;
    back_leg = Skeleton.limb ~bend:30. (-16.) }

let low_kick_out : Skeleton.pose =
  { guard with lean = 12.; front_leg = Skeleton.limb ~bend:10. 52.; back_leg = Skeleton.limb ~bend:28. (-10.) }

let hurt : Skeleton.pose =
  { guard with
    lean = -22.;
    turn = 12.;
    front_arm = Skeleton.limb ~yaw:26. ~bend:30. 26.;
    back_arm = Skeleton.limb ~yaw:(-24.) ~bend:24. 18. }

let flat : Skeleton.pose =
  { Skeleton.stand with
    lean = -86.;
    front_arm = Skeleton.limb ~yaw:30. 30.;
    back_arm = Skeleton.limb ~yaw:(-30.) 26.;
    front_leg = Skeleton.limb ~bend:20. 8.;
    back_leg = Skeleton.limb ~bend:18. (-6.) }

(* A move's keyframes are its own frame data: the fist is out exactly
 * while the move is active, because both read [startup] and [active]
 * from the same record. Write the animation by eye instead and a
 * fighter looks as though it hits before it does. *)
let poses_of (a : attack) (frame : int) : Skeleton.pose =
  let m = move_of a in
  let out = match a with Punch -> punch_out | Kick -> kick_out | Low_kick -> low_kick_out in
  Skeleton.at
    [ (0, guard); (m.startup, out); (m.startup + m.active, out); (Frame_data.length m, guard) ]
    frame

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state =
  | Idle
  | Crouch
  | Attacking of attack * int (* the move, and which of its frames *)
  | Stunned of int (* hitstun or blockstun left *)
  | Down of int (* knocked flat, frames left *)

type dir = { back : bool; forward : bool; down : bool }
type input = { dir : dir; punch : bool; kick : bool }

type fighter = {
  x : number; (* along the ring, its feet at (x, 0) *)
  facing : number; (* 1: towards +x, -1: towards -x *)
  health : int;
  state : state;
  blocked : bool; (* the last hit was blocked: for the sound of it *)
}

type round = {
  a : fighter; (* the player *)
  b : fighter; (* the computer *)
  frames : int;
  hitstop : int; (* the whole game held still, a few frames at each hit *)
  over : int; (* > 0 once the round is decided, frames before the next *)
  won_a : int;
  won_b : int;
  ring_out : bool;
}

type scene = Title | Fighting of round | Over of round * bool (* the player won *)
type model = scene Scene2d.t

let full_health = 100
let rounds_to_win = 2

let new_fighter (x : number) (facing : number) : fighter =
  { x; facing; health = full_health; state = Idle; blocked = false }

let new_round (won_a : int) (won_b : int) : round =
  { a = new_fighter (-2.2) 1.; b = new_fighter 2.2 (-1.); frames = 0; hitstop = 0; over = 0; won_a; won_b;
    ring_out = false }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let no_input : input = { dir = { back = false; forward = false; down = false }; punch = false; kick = false }

let busy (f : fighter) : bool =
  match f.state with Attacking _ | Stunned _ | Down _ -> true | Idle | Crouch -> false

let crouching (f : fighter) (i : input) : bool =
  match f.state with Crouch -> true | Idle -> i.dir.down | _ -> false

(* Blocking is holding back, as it has been since Street Fighter II:
 * standing blocks high and mid, crouching blocks low and mid, and
 * guessing which is coming is the whole game. *)
let blocking (f : fighter) (i : input) (a : attack) : bool =
  (not (busy f)) && i.dir.back && if low a then crouching f i else true

let hurtbox (f : fighter) (i : input) : Hitbox.box =
  let b : Hitbox.box =
    if crouching f i || (match f.state with Down _ -> true | _ -> false) then
      { x = 0.; y = 0.55; w = 0.7; h = 1.1 }
    else { x = 0.; y = 0.9; w = 0.6; h = 1.8 }
  in
  Hitbox.place f.facing (f.x, 0.) b

(* one frame of a fighter's own state, before anyone is hit *)
let step_fighter (i : input) (f : fighter) : fighter =
  match f.state with
  | Down n -> { f with state = (if n <= 1 then Idle else Down (n - 1)) }
  | Stunned n -> { f with state = (if n <= 1 then Idle else Stunned (n - 1)) }
  | Attacking (a, n) ->
      let m = move_of a in
      if n >= Frame_data.length m then { f with state = Idle } else { f with state = Attacking (a, n + 1) }
  | Idle | Crouch ->
      let walk = 0.055 in
      let f = { f with state = (if i.dir.down then Crouch else Idle) } in
      if i.dir.down then f
      else if i.punch then { f with state = Attacking (Punch, 1) }
      else if i.kick then { f with state = Attacking ((if i.dir.down then Low_kick else Kick), 1) }
      else if i.dir.forward then { f with x = f.x +. (f.facing *. walk) }
      else if i.dir.back then { f with x = f.x -. (f.facing *. walk) }
      else f

(* the attacker's active frame meeting the other's hurtbox: damage,
 * stun, and the push that walks the loser towards the edge *)
let strike (attacker : fighter) (defender : fighter) (di : input) : fighter * fighter * bool =
  match attacker.state with
  | Attacking (a, n) when Frame_data.phase (move_of a) n = Frame_data.Active ->
      let m = move_of a in
      let hit = Hitbox.place attacker.facing (attacker.x, 0.) m.hitbox in
      if not (Hitbox.overlap hit (hurtbox defender di)) then (attacker, defender, false)
      else
        let guarded = blocking defender di a in
        let push = push_of a *. if guarded then 0.55 else 1. in
        let defender =
          { defender with
            health = (if guarded then defender.health - (m.damage / 4) else defender.health - m.damage);
            state = Stunned (if guarded then m.blockstun else m.hitstun);
            x = defender.x +. (attacker.facing *. push);
            blocked = guarded }
        in
        (* the move is spent: it cannot hit twice *)
        ({ attacker with state = Attacking (a, m.startup + m.active) }, defender, true)
  | _ -> (attacker, defender, false)

(* fighters cannot walk through each other *)
let separate (a : fighter) (b : fighter) : fighter * fighter =
  let gap = 0.75 in
  let d = b.x -. a.x in
  if Float.abs d >= gap then (a, b)
  else
    let push = (gap -. Float.abs d) /. 2. *. if d >= 0. then 1. else -1. in
    ({ a with x = a.x -. push }, { b with x = b.x +. push })

let face (a : fighter) (b : fighter) : fighter * fighter =
  let facing_a = if b.x >= a.x then 1. else -1. in
  (* a fighter turns round only when it is free to *)
  ( (if busy a then a else { a with facing = facing_a }),
    if busy b then b else { b with facing = -.facing_a } )

let off_ring (f : fighter) : bool = Float.abs f.x > ring_half

(* The computer: walk into range, then punch, kick or sweep; hold back
 * while the other is swinging, which is what makes it look as though
 * it is reading the fight. Three rules, like games3d/TinyBoomerangFu's. *)
let computer (self : fighter) (other : fighter) (frames : int) : input =
  let gap = Float.abs (other.x -. self.x) in
  let seed = (frames / 17) + int_of_float (Float.abs self.x *. 3.) in
  let other_swinging = match other.state with Attacking _ -> true | _ -> false in
  if busy self then no_input
  else if other_swinging && gap < 1.5 then
    { no_input with dir = { back = true; forward = false; down = seed mod 3 = 0 } }
  else if gap > 1.35 then { no_input with dir = { back = false; forward = true; down = false } }
  else
    match seed mod 4 with
    | 0 -> { no_input with punch = true }
    | 1 -> { no_input with kick = true }
    | 2 -> { no_input with kick = true; dir = { back = false; forward = false; down = true } }
    | _ -> { no_input with dir = { back = true; forward = false; down = false } }

let axis_input (keys : keyboard) (f : fighter) : input =
  let left = keys.kleft and right = keys.kright in
  let towards = if f.facing > 0. then right else left in
  let away = if f.facing > 0. then left else right in
  { dir = { back = away; forward = towards; down = keys.kdown };
    punch = Set_.mem "f" keys.keys;
    kick = Set_.mem "g" keys.keys }

let step_round (keys : keyboard) (r : round) : round =
  if r.hitstop > 0 then { r with hitstop = r.hitstop - 1 }
  else if r.over > 0 then { r with over = r.over - 1; frames = r.frames + 1 }
  else
    let ia = axis_input keys r.a in
    let ib = computer r.b r.a r.frames in
    let a = step_fighter ia r.a and b = step_fighter ib r.b in
    (* each may be hitting the other on the same frame, as in the
     * arcade: a trade *)
    let a, b, hit1 = strike a b ib in
    let b, a, hit2 = strike b a ia in
    let a, b = separate a b in
    let a, b = face a b in
    let out_a = off_ring a and out_b = off_ring b in
    let down (f : fighter) = { f with state = Down 70 } in
    let a = if out_a then down a else a and b = if out_b then down b else b in
    let decided = out_a || out_b || a.health <= 0 || b.health <= 0 in
    let lost_a = out_a || a.health <= 0 in
    { a; b;
      frames = r.frames + 1;
      hitstop = (if hit1 || hit2 then 6 else 0);
      over = (if decided && r.over = 0 then 120 else r.over);
      ring_out = (r.ring_out || out_a || out_b);
      won_a = (if decided && not lost_a then r.won_a + 1 else r.won_a);
      won_b = (if decided && lost_a then r.won_b + 1 else r.won_b) }

let update (computer_ : computer) (m : model) : model =
  let m = Scene2d.update computer_ m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  match m.scene with
  | Title -> if space then Scene2d.go (Fighting (new_round 0 0)) m else m
  | Fighting r ->
      let r = step_round computer_.keyboard r in
      if r.over = 1 then
        if r.won_a >= rounds_to_win || r.won_b >= rounds_to_win then
          Scene2d.go (Over (r, r.won_a >= rounds_to_win)) m
        else Scene2d.go (Fighting (new_round r.won_a r.won_b)) m
      else { m with scene = Fighting r }
  | Over (r, won) -> if space then Scene2d.go Title m else { m with scene = Over (r, won) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let pose_of (f : fighter) (i : input) : Skeleton.pose =
  match f.state with
  | Down _ -> flat
  | Stunned _ -> hurt
  | Attacking (a, n) -> poses_of a n
  | Crouch -> crouch
  | Idle -> if i.dir.down then crouch else guard

let draw_fighter (body : color) (back : color) (f : fighter) (i : input) : shape3d =
  let heading = if f.facing > 0. then 90. else -90. in
  Skeleton.draw ~body ~back ~skin:(rgb 240 200 165) fighter_height heading (pose_of f i)
  |> move3d f.x (if off_ring f then ring_top -. ring_drop else ring_top) 0.

(* A camera for two: it looks at the point between the fighters, from
 * the side, and backs off as they separate -- close enough to see a
 * jab when they are on top of each other, far enough to keep both on
 * screen across the ring. The 2D games do the same thing by zooming. *)
let camera_for (r : round) : camera =
  let middle = (r.a.x +. r.b.x) /. 2. in
  let gap = Float.abs (r.b.x -. r.a.x) in
  let back = 4.6 +. (gap *. 0.75) in
  (* [far] is not a detail: Camera3d.sky's plane is 2400 across, so a
   * camera left at the default 1000 has its corners cut off and the
   * sky simply is not drawn *)
  camera ~eye:(middle *. 0.55, 2.1 +. (gap *. 0.09), back) ~target:(middle *. 0.55, 1.05, 0.) ~far:2400. ()

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

(* a health bar per fighter, the player's filling from the left *)
let bars (screen : screen) (r : round) : shape list =
  let bar (f : fighter) (side : number) (color : color) =
    let width = 380. in
    let left = side *. (screen.right -. 40. -. (width /. 2.)) in
    let fraction = Float.max 0. (float_of_int f.health /. float_of_int full_health) in
    [ rectangle (rgb 40 40 45) (width +. 8.) 34. |> move left (screen.top -. 50.);
      (* it empties towards the middle of the screen, the way a fighting
       * game's does: the bar's outer end stays put *)
      rectangle color (width *. fraction) 26.
      |> move (left +. (side *. (width *. (1. -. fraction) /. 2.))) (screen.top -. 50.) ]
  in
  let won (n : int) (side : number) =
    List.init n (fun k ->
        circle yellow 9.
        |> move (side *. (screen.right -. 60. -. (float_of_int k *. 26.))) (screen.top -. 90.))
  in
  bar r.a (-1.) (rgb 240 210 60) @ bar r.b 1. (rgb 240 210 60) @ won r.won_a (-1.) @ won r.won_b 1.

let view (computer_ : computer) (m : model) : camera * shape3d list =
  let screen = computer_.screen in
  let r = match m.scene with Title -> new_round 0 0 | Fighting r | Over (r, _) -> r in
  let cam =
    match m.scene with
    | Title -> Camera3d.orbit ~distance:9. ~height:3.4 ~look:1.2 (spin 16. computer_.time) (0., 1., 0.)
    | _ -> camera_for r
  in
  let world =
    [ Camera3d.floor ~color:(rgb 64 88 72) ~ground:(ring_top -. ring_drop -. 0.4) cam ]
    @ Camera3d.sky ~sky:(rgb 132 150 190) ~horizon:(rgb 70 92 76) ~ground:(ring_top -. ring_drop -. 0.4) cam
    @ [ ring;
        draw_fighter (rgb 235 235 240) (rgb 170 170 180) r.a no_input;
        draw_fighter (rgb 215 70 60) (rgb 150 50 45) r.b no_input ]
  in
  let hud_shapes =
    match m.scene with
    | Title ->
        [ text (rgb 240 210 60) 6. "TINY VIRTUA FIGHTER" |> move_y 320.;
          rectangle black 900. 190. |> move_y (-250.) |> fade 0.55;
          text white 2.5 "left/right: walk (away blocks)   down: crouch" |> move_y (-210.);
          text white 2.5 "f: punch   g: kick   down + g: low kick" |> move_y (-255.);
          text white 2.5 "best of three -- or knock them off the ring" |> move_y (-300.) ]
        @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-380.) ]
    | Fighting _ ->
        bars screen r
        @ (if r.over > 0 then
             [ text yellow 8. (if r.ring_out then "RING OUT!" else "K.O.") |> move_y 150. ]
           else [])
        @ if r.frames < 60 then [ text yellow 8. "FIGHT!" |> move_y 220. ] else []
    | Over (_, won) ->
        bars screen r
        @ [ rectangle black 700. 150. |> move_y 180. |> fade 0.6;
            text yellow 8. (if won then "YOU WIN" else "YOU LOSE") |> move_y 200. ]
        @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y 100. ]
  in
  (cam, world @ List.map hud hud_shapes)

let app = game3d view update initial_model

(* flat shading, Virtua Racing's and Virtua Fighter's look, on the same
 * Model 1 board; the back faces drawn too, for the sky *)
let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = Flat; backface_culling = false }
    app
