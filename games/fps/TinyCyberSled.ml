(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Cyber Sled (Namco, 1993): two hover-tanks, "sleds",
 * in a closed arena, each shooting until the other's armor is gone.
 * Here you fight a sled driven by the computer, or, with players=2, a
 * friend on the same keyboard.
 *
 * Cyber Sled was drawn like Virtua Racing, the year before it, and for
 * the same reason: its board (Namco's System 21) drew flat-shaded
 * polygons, one color per face and no textures. That is exactly the
 * look of playground3d with [shading = Flat], so the arena, the pillars
 * and the sleds are a few boxes and polygons each.
 *
 * The game is its controls. Each player had two joysticks, one per
 * tread, as in Battlezone (TinyBattlezone), with a third direction added:
 *
 *        both forward: ahead       left forward, right back: turn right
 *        both back: reverse        left back, right forward: turn left
 *        both sideways: strafe     one stick alone: a wide curve
 *
 * The left stick is w/s (and a/d sideways), the right stick is the up
 * and down arrows (and left/right sideways). Space fires the cannon, and
 * v switches the view from the cockpit to behind the sled. The mix does
 * not reduce to "a direction and a speed": [treads] turns the two sticks
 * into a speed, a turn and a slide, and the rest of the game is ordinary.
 * Strafing is what makes a duel interesting: circling the other sled
 * sideways, keeping your gun on it while its shells miss behind you.
 *
 * The computer's sled plays that way too ([cpu]): it turns to face you,
 * keeps its distance, slides left or right in turns, and fires when its
 * gun is on you. Its shells are slow enough to dodge by strafing.
 *
 * The game above is the whole game, and the flags add to it, each in a
 * section of its own that the rest calls through a hook or two, so that
 * it can be read, or removed, as a unit:
 *   - missiles=on: homing missiles, three a round, on a second button
 *     (enter), turning after their target -- the original's other weapon;
 *   - ramps=on: two ramps, and a sled leaving the ground at their top
 *     (a height and a vertical speed, and gravity), over the shells;
 *   - players=2: two players on one keyboard, the screen split in two,
 *     as the linked cabinets were played (the keys are on the title);
 *   - juice=off: without the juice, which is on by default -- sparks at
 *     each hit, the camera shaken, the loser blown to pieces, smoke
 *     after the missiles and from the wreck (Juice3d).
 * E.g. dune exec games/fps/TinyCyberSled.exe -- missiles=on ramps=on players=2
 *
 * What it uses: Playground3d (box, polygon3d, cached3d for the arena,
 * hud, split3d for the split screen) and Camera3d (cockpit, behind,
 * orbit, sky), Juice3d for the juice, Scene2d for the title and the end.
 * No kit, and no physics engine: a sled is a point with a heading,
 * pushed back from the walls and pillars one axis at a time, and with
 * the ramps a height that falls.
 *
 * Exercise: the two players on two computers. Multiplayer.mli runs a
 * game over the network, but only a 2D one (its view gives shapes); a
 * Multiplayer.game3d would be the same machinery around a Playground3d
 * app, and this game's update, deterministic already (no Random, no
 * clock), is all it would need from the game.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The arena *)
(*****************************************************************************)

(* the arena is a square, from -half to +half on x and z *)
let half = 60.

(* the pillars: where (their center, on the ground), how wide and deep,
 * how high, and their color. Sleds and shells don't go through them. *)
type pillar = { px : number; pz : number; w : number; d : number; h : number; color : color }

let pillars =
  [ { px = 0.; pz = 0.; w = 8.; d = 8.; h = 6.; color = rgb 230 200 60 };
    { px = -30.; pz = -25.; w = 6.; d = 14.; h = 4.; color = rgb 60 170 220 };
    { px = 30.; pz = 25.; w = 6.; d = 14.; h = 4.; color = rgb 60 170 220 };
    { px = 28.; pz = -30.; w = 12.; d = 5.; h = 3.; color = rgb 220 110 180 };
    { px = -28.; pz = 30.; w = 12.; d = 5.; h = 3.; color = rgb 220 110 180 } ]

(* inside a wall or a pillar, grown by [r] *)
let solid ?(r = 0.) (x : number) (z : number) : bool =
  Float.abs x > half -. r
  || Float.abs z > half -. r
  || List.exists (fun p -> Float.abs (x -. p.px) < (p.w /. 2.) +. r && Float.abs (z -. p.pz) < (p.d /. 2.) +. r) pillars

(*****************************************************************************)
(* The duel *)
(*****************************************************************************)

type sled = {
  x : number;
  z : number;
  heading : number; (* degrees: 0 towards -z, 90 towards +x *)
  armor : int;
  reload : int; (* frames before the cannon fires again *)
  hit : int; (* > 0: just hit, drawn flashing for that long *)
  (* ramps=on: the height above the ground, and how fast it rises; 0
   * without the ramps *)
  y : number;
  vy : number;
  missiles : int; (* missiles=on: the missiles left *)
}

(* a shell: where, its heading, the frames left, whose, and how much
 * armor it takes. [sy]: its height, a sled's gun's (ramps=on: a sled
 * in the air fires from there, and flies over the shells); [homing]:
 * a missile (missiles=on) *)
type shell = { sx : number; sy : number; sz : number; sh : number; life : int; from_p1 : bool; damage : int; homing : bool }

(* p1 is you, p2 the computer's sled, or with players=2 the second
 * player's *)
type game = { p1 : sled; p2 : sled; shells : shell list; behind : bool; frames : int }

type scene = Title | Playing of game | Over of game

type model = {
  scenes : scene Scene2d.t;
  fx : Juice3d.t; (* the juice's, see its section *)
}

let sled_at (x : number) (z : number) (heading : number) : sled =
  { x; z; heading; armor = 100; reload = 0; hit = 0; y = 0.; vy = 0.; missiles = 3 }

let new_game () : game = { p1 = sled_at 20. 45. 0.; p2 = sled_at (-20.) (-45.) 180.; shells = []; behind = false; frames = 0 }

let initial_model : model = { scenes = Scene2d.start Title; fx = Juice3d.none ~seed:1 }

(* the flags, read at each frame *)
type variants = { missiles : bool; ramps : bool; players : int }

let variants (computer : computer) : variants =
  let on name = List.assoc_opt name computer.flags = Some "on" in
  { missiles = on "missiles"; ramps = on "ramps"; players = (if List.assoc_opt "players" computer.flags = Some "2" then 2 else 1) }

let distance (x1 : number) (z1 : number) (x2 : number) (z2 : number) : number =
  Float.sqrt (((x1 -. x2) ** 2.) +. ((z1 -. z2) ** 2.))

(* the heading from (x1, z1) to (x2, z2), and an angle in -180..180 *)
let bearing x1 z1 x2 z2 = atan2 (x2 -. x1) (-.(z2 -. z1)) *. 180. /. Float.pi
let normalize (a : number) : number = Float.rem (Float.rem (a +. 180.) 360. +. 360.) 360. -. 180.

(*****************************************************************************)
(* The controls: two sticks *)
(*****************************************************************************)

(* a player's keys: each stick's four directions, and the two buttons *)
type pad = {
  left_up : string;
  left_down : string;
  left_left : string;
  left_right : string;
  right_up : string;
  right_down : string;
  right_left : string;
  right_right : string;
  fire : string;
  missile : string;
}

let one_player : pad =
  { left_up = "w"; left_down = "s"; left_left = "a"; left_right = "d"; right_up = "ArrowUp"; right_down = "ArrowDown";
    right_left = "ArrowLeft"; right_right = "ArrowRight"; fire = "space"; missile = "Enter" }

(* The two sticks, each -1, 0 or 1 forward and sideways, into what the
 * sled does. The treads' mean is the speed ahead, their difference the
 * turn (the left one faster turns right), and the sticks pushed
 * sideways together slide the sled across. *)
let treads ~(left : number) ~(right : number) ~(left_x : number) ~(right_x : number) : number * number * number =
  let speed = 0.25 *. (left +. right) /. 2. in
  let turn = 2.5 *. (left -. right) /. 2. in
  let slide = 0.2 *. (left_x +. right_x) /. 2. in
  (speed, turn, slide)

let sticks (keys : string Set_.t) (pad : pad) : number * number * number =
  let stick plus minus = (if Set_.mem plus keys then 1. else 0.) -. if Set_.mem minus keys then 1. else 0. in
  treads ~left:(stick pad.left_up pad.left_down) ~right:(stick pad.right_up pad.right_down)
    ~left_x:(stick pad.left_right pad.left_left) ~right_x:(stick pad.right_right pad.right_left)

(*****************************************************************************)
(* The ramps (ramps=on: none) *)
(*****************************************************************************)

(* Two wedges, each rising towards one end, and a height for the sled:
 * on a ramp, the sled is at the ramp's height, and rises as fast as it
 * climbs; off its high end, the ground drops away, and the sled keeps
 * that speed upwards, gravity taking it back down. A jump is nothing
 * more: the climb's speed, kept.
 *
 *              +                     sled in the air
 *            / |          . - ' ' - .
 *          /   |        '            ' .
 *   -->  /     |                         '   landed
 *   ----+------+-------------------------------
 *    low end   high end (a wall, from the other side)
 *)

(* x0..x1 across, z0..z1 along, [h] high at its north end (-z) or its
 * south end *)
type ramp = { x0 : number; x1 : number; z0 : number; z1 : number; h : number; north : bool }

let ramps =
  [ { x0 = -46.; x1 = -38.; z0 = -6.; z1 = 10.; h = 4.; north = true };
    { x0 = 38.; x1 = 46.; z0 = -10.; z1 = 6.; h = 4.; north = false } ]

let gravity = 0.006

(* the ground's height at (x, z) *)
let ground (v : variants) (x : number) (z : number) : number =
  if not v.ramps then 0.
  else
    List.fold_left
      (fun g r ->
        if x < r.x0 || x > r.x1 || z < r.z0 || z > r.z1 then g
        else
          let along = if r.north then (r.z1 -. z) /. (r.z1 -. r.z0) else (z -. r.z0) /. (r.z1 -. r.z0) in
          Float.max g (r.h *. along))
      0. ramps

(* a step up of more than a little is a wall: a ramp's high end, and
 * its sides away from the low end *)
let passable (v : variants) (s : sled) (x : number) (z : number) : bool = ground v x z -. s.y < 0.6

(* the sled's height, after it moved: on the ground, or flying *)
let lift (v : variants) (s : sled) : sled =
  if not v.ramps then s
  else
    let g = ground v s.x s.z in
    if s.y +. s.vy > g +. 0.01 then { s with y = s.y +. s.vy; vy = s.vy -. gravity } else { s with y = g; vy = g -. s.y }

(* a ramp: its slope, its two sides, and its high end *)
let ramp_shape (r : ramp) : shape3d =
  let color = rgb 200 120 60 and side = rgb 150 80 40 in
  let hi, lo = if r.north then (r.z0, r.z1) else (r.z1, r.z0) in
  group3d
    [ polygon3d color [ (r.x0, 0., lo); (r.x1, 0., lo); (r.x1, r.h, hi); (r.x0, r.h, hi) ];
      polygon3d side [ (r.x0, 0., lo); (r.x0, r.h, hi); (r.x0, 0., hi) ];
      polygon3d side [ (r.x1, 0., lo); (r.x1, r.h, hi); (r.x1, 0., hi) ];
      polygon3d side [ (r.x0, 0., hi); (r.x1, 0., hi); (r.x1, r.h, hi); (r.x0, r.h, hi) ] ]

let ramp_shapes : shape3d = cached3d (List.map ramp_shape ramps)
let ramps_view (v : variants) : shape3d list = if v.ramps then [ ramp_shapes ] else []

(*****************************************************************************)
(* The missiles (missiles=on: none) *)
(*****************************************************************************)

(* A missile is a shell that turns: each frame, towards its target, by
 * at most a few degrees -- a pursuit curve. Slower than a shell, and
 * turning slower than a sled can slide, it can be outrun sideways, but
 * not by driving straight away. *)

let missile_speed = 0.8
let missile_turn = 2.5

(* the missile after its target: its heading, and its height *)
let steer (target : sled) (sh : shell) : shell =
  let turn = normalize (bearing sh.sx sh.sz target.x target.z -. sh.sh) in
  let sy = sh.sy +. Float.max (-0.05) (Float.min 0.05 (target.y +. 1. -. sh.sy)) in
  { sh with sh = sh.sh +. Float.max (-.missile_turn) (Float.min missile_turn turn); sy }

let launch (s : sled) (from_p1 : bool) : shell =
  let fx, fz = Camera3d.forward s.heading in
  { sx = s.x +. (3. *. fx); sy = s.y +. 1.4; sz = s.z +. (3. *. fz); sh = s.heading; life = 240; from_p1; damage = 15;
    homing = true }

(* a player's missile, on its button's press *)
let player_missile (v : variants) (pressed : bool) (from_p1 : bool) (s : sled) (shells : shell list) : sled * shell list =
  if v.missiles && pressed && s.missiles > 0 then ({ s with missiles = s.missiles - 1 }, launch s from_p1 :: shells)
  else (s, shells)

(* the computer's: when you are far, roughly ahead, every four seconds *)
let cpu_missile (v : variants) (frames : int) (d : number) (turn : number) (s : sled) (shells : shell list) :
    sled * shell list =
  if v.missiles && s.missiles > 0 && d > 40. && Float.abs turn < 20. && frames mod 240 = 0 then
    ({ s with missiles = s.missiles - 1 }, launch s false :: shells)
  else (s, shells)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the sled turned, then moved [speed] ahead and [slide] to its right,
 * each axis kept only if it's free: a sled against a wall slides along
 * it *)
let drive (v : variants) (s : sled) (other : sled) ((speed, turn, slide) : number * number * number) : sled =
  let heading = s.heading +. turn in
  let fx, fz = Camera3d.forward heading in
  (* the right of a heading is its forward turned by 90 degrees *)
  let rx, rz = (-.fz, fx) in
  let x = s.x +. (speed *. fx) +. (slide *. rx) and z = s.z +. (speed *. fz) +. (slide *. rz) in
  let free x z = (not (solid ~r:1.5 x z)) && distance x z other.x other.z > 3.5 && passable v s x z in
  let x = if free x s.z then x else s.x in
  let z = if free x z then z else s.z in
  lift v { s with heading; x; z }

(* a shell from the sled's gun, ahead of its nose *)
let fire (s : sled) (from_p1 : bool) (damage : int) : shell =
  let fx, fz = Camera3d.forward s.heading in
  { sx = s.x +. (3. *. fx); sy = s.y +. 1.; sz = s.z +. (3. *. fz); sh = s.heading; life = 90; from_p1; damage;
    homing = false }

let move_shell (g : game) (sh : shell) : shell option =
  let sh = if sh.homing then steer (if sh.from_p1 then g.p2 else g.p1) sh else sh in
  let speed = if sh.homing then missile_speed else 1.3 in
  let fx, fz = Camera3d.forward sh.sh in
  let sh = { sh with sx = sh.sx +. (speed *. fx); sz = sh.sz +. (speed *. fz); life = sh.life - 1 } in
  if sh.life = 0 || solid sh.sx sh.sz then None else Some sh

let cool (s : sled) : sled = { s with reload = max 0 (s.reload - 1); hit = max 0 (s.hit - 1) }

(* The computer's sled: it turns to face you, closes in when far, backs
 * off when near, and slides one way then the other, a few seconds each,
 * so that it is a moving target *)
let cpu (v : variants) (g : game) : game =
  let c = g.p2 and me = g.p1 in
  let turn = normalize (bearing c.x c.z me.x me.z -. c.heading) in
  let d = distance c.x c.z me.x me.z in
  let speed = if d > 35. then 0.18 else if d < 18. then -0.12 else 0. in
  let slide = if g.frames / 150 mod 2 = 0 then 0.12 else -0.12 in
  let c = drive v c me (speed, Float.max (-1.8) (Float.min 1.8 turn), slide) in
  let c, shells = cpu_missile v g.frames d turn c g.shells in
  if Float.abs turn < 4. && c.reload = 0 && d < 70. then { g with p2 = { c with reload = 45 }; shells = fire c false 8 :: shells }
  else { g with p2 = c; shells }

(* a player's sled: its sticks, its cannon (held down, it keeps
 * firing), its missile *)
let player (v : variants) (computer : computer) (s : scene Scene2d.t) (pad : pad) (from_p1 : bool) (me : sled)
    (other : sled) (shells : shell list) : sled * shell list =
  let keys = computer.keyboard.keys in
  let me = drive v me other (sticks keys pad) in
  let me, shells =
    if Set_.mem pad.fire keys && me.reload = 0 then ({ me with reload = 10 }, fire me from_p1 4 :: shells) else (me, shells)
  in
  player_missile v (Scene2d.pressed (fun k -> Set_.mem pad.missile k.keys) s) from_p1 me shells

(* a shell in a sled, near enough and at its height: its armor down,
 * and the shell gone *)
let hits (sh : shell) (s : sled) : bool = distance sh.sx sh.sz s.x s.z < 2. && Float.abs (sh.sy -. (s.y +. 1.)) < 1.5

let damage (shells : shell list) (from_p1 : bool) (s : sled) : sled =
  let taken = List.fold_left (fun n sh -> if sh.from_p1 = from_p1 && hits sh s then n + sh.damage else n) 0 shells in
  if taken = 0 then s else { s with armor = s.armor - taken; hit = 8 }

(* the keys of the two players, with players=2 (see its section) *)
let two_players : pad * pad =
  ( { left_up = "w"; left_down = "s"; left_left = "a"; left_right = "d"; right_up = "t"; right_down = "g";
      right_left = "f"; right_right = "h"; fire = "q"; missile = "e" },
    { left_up = "i"; left_down = "k"; left_left = "j"; left_right = "l"; right_up = "ArrowUp"; right_down = "ArrowDown";
      right_left = "ArrowLeft"; right_right = "ArrowRight"; fire = "Enter"; missile = "Shift" } )

let update_game (computer : computer) (s : scene Scene2d.t) (g : game) : game =
  let v = variants computer in
  let behind = if Scene2d.pressed (fun k -> Set_.mem "v" k.keys) s then not g.behind else g.behind in
  let g = { p1 = cool g.p1; p2 = cool g.p2; behind; frames = g.frames + 1; shells = List.filter_map (move_shell g) g.shells } in
  let g =
    if v.players = 1 then
      let p1, shells = player v computer s one_player true g.p1 g.p2 g.shells in
      cpu v { g with p1; shells }
    else
      let pad1, pad2 = two_players in
      let p1, shells = player v computer s pad1 true g.p1 g.p2 g.shells in
      let p2, shells = player v computer s pad2 false g.p2 p1 shells in
      { g with p1; p2; shells }
  in
  let p1 = damage g.shells false g.p1 and p2 = damage g.shells true g.p2 in
  let shells = List.filter (fun sh -> not (hits sh (if sh.from_p1 then g.p2 else g.p1))) g.shells in
  { g with p1; p2; shells }

let update_rules (computer : computer) (s : scene Scene2d.t) : scene Scene2d.t =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if g.p1.armor <= 0 || g.p2.armor <= 0 then Scene2d.go (Over g) s else { s with scene = Playing g }
  | Over _ -> if (space && s.elapsed > 1.) || s.elapsed > 10. then Scene2d.go Title s else s

(*****************************************************************************)
(* The juice (juice=off: none of it) *)
(*****************************************************************************)

(* Everything the juice does is here, and the rules above don't know
 * about it: [update] runs them, then [juiced] looks at what they just
 * did -- a sled's armor went down, a missile appeared, a sled died --
 * and starts the effects (Juice3d.mli). The view draws them through
 * [Juice3d.camera] and [Juice3d.view]. *)

let blue = rgb 60 110 230
let red_sled = rgb 220 60 50

let juiced (before : scene) (after : scene Scene2d.t) (fx : Juice3d.t) : Juice3d.t =
  let at (s : sled) = (s.x, s.y +. 1., s.z) in
  match (before, after.scene) with
  | Playing g, Playing g' ->
      (* a hit: sparks where it hit, the camera knocked (harder when it
       * is yours, with one screen) *)
      let hurt (s : sled) (s' : sled) trauma fx =
        if s'.armor < s.armor then fx |> Juice3d.burst ~at:(at s') Juice3d.sparks |> Juice3d.shake trauma else fx
      in
      let fx = fx |> hurt g.p1 g'.p1 0.45 |> hurt g.p2 g'.p2 0.2 in
      (* a missile's smoke, puffed every few frames *)
      List.fold_left
        (fun fx sh -> if sh.homing && g'.frames mod 4 = 0 then Juice3d.burst ~at:(sh.sx, sh.sy, sh.sz) Juice3d.smoke fx else fx)
        fx g'.shells
  | Playing _, Over g ->
      (* the loser in pieces: debris, a white flash, the big shake *)
      let loser, color = if g.p1.armor <= 0 then (g.p1, blue) else (g.p2, red_sled) in
      fx |> Juice3d.burst ~at:(at loser) (Juice3d.debris color) |> Juice3d.burst ~at:(at loser) Juice3d.sparks
      |> Juice3d.flash white 20 |> Juice3d.shake 1.
  | _, Over g ->
      (* the wreck burning *)
      let loser = if g.p1.armor <= 0 then g.p1 else g.p2 in
      if after.frames mod 8 = 0 then Juice3d.burst ~at:(at loser) Juice3d.smoke fx else fx
  | _ -> fx

(* the loser, not drawn once it is in pieces *)
let wrecked (fx : Juice3d.t) (s : sled) : bool = Juice3d.on fx && s.armor <= 0

let update (computer : computer) (m : model) : model =
  let fx = Juice3d.step computer m.fx in
  if Juice3d.frozen fx then { m with fx }
  else
    let scenes = update_rules computer m.scenes in
    { scenes; fx = juiced m.scenes.scene scenes fx }

(*****************************************************************************)
(* View: flat-shaded polygons *)
(*****************************************************************************)

(* [placed heading (x, z) shape]: a model facing -z turned to [heading]
 * and moved to (x, z) on the ground. rotate3d turns by the other hand
 * from our heading (which goes from -z towards +x), hence the minus *)
let placed (heading : number) ((x, z) : number * number) (s : shape3d) : shape3d =
  s |> rotate3d 0. (-.heading) 0. |> move3d x 0. z

(* A sled, facing -z: a wedge of a hull, low at the nose, between two
 * skirts, and a cannon on top. Every face flat, as on the System 21.
 *
 *            back                     front
 *        tl +-------+ tr           ftl +---+ ftr
 *           |       |                  |   |
 *        bl +-------+ br            fl +---+ fr
 *
 * seen from the side, the top slopes down from the back to the nose. *)
let sled_shape (color : color) : shape3d =
  let y0 = 0.5 in
  let bl = (-1.1, y0, 1.6) and br = (1.1, y0, 1.6) and tl = (-0.9, 1.4, 1.6) and tr = (0.9, 1.4, 1.6) in
  let fl = (-0.8, y0, -2.2) and fr = (0.8, y0, -2.2) and ftl = (-0.5, 0.8, -2.2) and ftr = (0.5, 0.8, -2.2) in
  let dark = rgb 50 50 60 in
  group3d
    [ polygon3d color [ tl; tr; ftr; ftl ];
      polygon3d color [ fl; fr; ftr; ftl ];
      polygon3d color [ bl; br; tr; tl ];
      polygon3d color [ bl; tl; ftl; fl ];
      polygon3d color [ br; fr; ftr; tr ];
      polygon3d dark [ bl; fl; fr; br ];
      box dark 0.5 0.7 4. |> move3d (-1.4) 0.45 (-0.2);
      box dark 0.5 0.7 4. |> move3d 1.4 0.45 (-0.2);
      box (rgb 200 200 210) 0.25 0.25 2. |> move3d 0. 1.35 (-1.6) ]

(* the floor in big tiles, two colors, for the eye to feel the speed
 * by; and the four walls. Built once. *)
let arena : shape3d =
  let n = 12 in
  let size = 2. *. half /. float_of_int n in
  let tiles =
    List.concat
      (List.init n (fun i ->
           List.init n (fun j ->
               let x0 = -.half +. (float_of_int i *. size) and z0 = -.half +. (float_of_int j *. size) in
               let color = if (i + j) mod 2 = 0 then rgb 90 100 120 else rgb 110 120 140 in
               polygon3d color [ (x0, 0., z0); (x0, 0., z0 +. size); (x0 +. size, 0., z0 +. size); (x0 +. size, 0., z0) ])))
  in
  let wall = rgb 140 70 170 in
  let walls =
    [ box wall (2. *. half) 5. 1. |> move3d 0. 2.5 (-.half -. 0.5);
      box wall (2. *. half) 5. 1. |> move3d 0. 2.5 (half +. 0.5);
      box wall 1. 5. (2. *. half) |> move3d (-.half -. 0.5) 2.5 0.;
      box wall 1. 5. (2. *. half) |> move3d (half +. 0.5) 2.5 0. ]
  in
  let pillar p = box p.color p.w p.h p.d |> move3d p.px (p.h /. 2.) p.pz in
  cached3d (tiles @ walls @ List.map pillar pillars)

(* a shell: a bolt along its way, thin, so that the one just fired is
 * not a wall in front of the cockpit; a missile, a fatter one *)
let shell (sh : shell) : shape3d =
  (if sh.homing then box (rgb 255 90 200) 0.35 0.35 1.2
   else box (if sh.from_p1 then rgb 120 240 255 else rgb 255 200 60) 0.12 0.12 1.5)
  |> move_y3d sh.sy
  |> placed sh.sh (sh.sx, sh.sz)

let sled (color : color) (s : sled) : shape3d =
  placed s.heading (s.x, s.z) (sled_shape (if s.hit > 0 && s.hit mod 4 < 2 then white else color) |> move_y3d s.y)

(* the night over the arena, and the ground outside it, seen over the
 * walls from above *)
let sky (cam : camera) : shape3d list =
  let outside = rgb 30 20 50 in
  Camera3d.floor ~color:outside cam :: Camera3d.sky ~sky:(rgb 10 12 30) ~horizon:outside cam

let pose (s : sled) : Camera3d.pose = { x = s.x; y = 0.5 +. s.y; z = s.z; heading = s.heading }

(* the view from behind the sled, its eye kept inside the arena: backed
 * against a wall, the camera would be in the wall, seeing only it *)
let behind (s : sled) : camera =
  let cam = Camera3d.behind ~back:10. ~height:4.5 ~ahead:10. ~look:1. (pose s) in
  let x, y, z = cam.eye and inside a = Float.max (1. -. half) (Float.min (half -. 1.) a) in
  { cam with eye = (inside x, y, inside z) }

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* an armor bar, from full (100) to empty *)
let armor_bar (color : color) (x : number) (y : number) (label : string) (armor : int) : shape list =
  let w = 240. *. float_of_int (max 0 armor) /. 100. in
  [ rectangle (rgb 30 30 30) 244. 22. |> move x y;
    rectangle color w 18. |> move (x -. 120. +. (w /. 2.)) y;
    text color 2. label |> move x (y +. 28.) ]

(* the radar: the other sled's blip, relative to our heading (ahead is
 * up), as in TinyBattlezone *)
let radar (screen : screen) (me : sled) (other : sled) (color : color) : shape list =
  let cy = screen.bottom +. 90. and r = 60. in
  let rel = (bearing me.x me.z other.x other.z -. me.heading) *. Float.pi /. 180. in
  let d = Float.min 1. (distance me.x me.z other.x other.z /. (2. *. half)) in
  [ circle (rgb 20 30 50) r |> fade 0.8 |> move_y cy; square color 8. |> move (r *. d *. sin rel) (cy +. (r *. d *. cos rel)) ]

let sight : shape list =
  List.map
    (fun (w, h, x, y) -> rectangle (rgb 255 255 120) w h |> move x y)
    [ (30., 2., -30., 0.); (30., 2., 30., 0.); (2., 30., 0., 30.); (2., 30., 0., -30.) ]

(* what one player sees, in their part of the window: the camera at
 * their sled, the HUD laid out on [screen], that part's own *)
let view_player (v : variants) (fx : Juice3d.t) (screen : screen) (g : game) (p1 : bool) : camera * shape3d list =
  let me, other = if p1 then (g.p1, g.p2) else (g.p2, g.p1) in
  let color, other_color = if p1 then (blue, red_sled) else (red_sled, blue) in
  let cam = if g.behind then behind me else Camera3d.cockpit (pose me) in
  (* in the cockpit, our own sled is around the eye: not drawn *)
  let sleds = sled other_color other :: (if g.behind then [ sled color me ] else []) in
  let names = if v.players = 1 then ("YOU", "CPU") else if p1 then ("P1", "P2") else ("P2", "P1") in
  let huds =
    sight @ radar screen me other other_color
    @ armor_bar color (screen.left +. 160.) (screen.top -. 60.) (fst names) me.armor
    @ armor_bar other_color (screen.right -. 160.) (screen.top -. 60.) (snd names) other.armor
    @ (if v.missiles then [ text white 2. (Printf.sprintf "MISSILES %d" me.missiles) |> move (screen.left +. 160.) (screen.top -. 100.) ]
       else [])
    @ if me.hit > 0 then [ rectangle red screen.width screen.height |> fade 0.25 ] else []
  in
  ( Juice3d.camera fx cam,
    sky cam @ Juice3d.view fx ((arena :: ramps_view v) @ sleds @ List.map shell g.shells) @ List.map hud huds )

(* the camera turning round the arena's center, slowly *)
let turning (s : scene Scene2d.t) : camera =
  let cam = Camera3d.orbit ~distance:28. ~height:9. ~look:1. (float_of_int s.frames *. 0.5) (0., 0., 0.) in
  (* far enough for the sky's horizon, 1400 away *)
  { cam with far = 3000. }

let view_title (v : variants) (s : scene Scene2d.t) : camera * shape3d list =
  let cam = turning s in
  let g = new_game () in
  (* the two sleds face to face, beside the center's pillar, for the
   * title *)
  let p1 = { g.p1 with x = 14.; z = 8. } and p2 = { g.p2 with x = 14.; z = -8. } in
  let keys =
    if v.players = 1 then [ "left stick w/s a/d   right stick arrows   space: fire   v: view" ]
    else [ "P1: left stick w/s a/d   right stick t/g f/h   q: fire   e: missile"; "P2: left stick i/k j/l   right stick arrows   enter: fire   shift: missile" ]
  in
  let keys = if v.missiles && v.players = 1 then keys @ [ "enter: missile" ] else keys in
  ( cam,
    sky cam @ (arena :: ramps_view v) @ [ sled blue p1; sled red_sled p2 ]
    @ List.map hud
        ((text (rgb 255 255 120) 6. "TINY CYBER SLED" |> move_y 300.)
         :: List.mapi (fun i k -> text white 2.2 k |> move_y (-230. -. (35. *. float_of_int i))) keys
        @ Scene2d.blink 1. s [ text (rgb 255 255 120) 3. "PRESS SPACE" |> move_y (-340.) ]) )

let view_over (v : variants) (fx : Juice3d.t) (s : scene Scene2d.t) (g : game) : camera * shape3d list =
  let winner = if g.p2.armor <= 0 then g.p1 else g.p2 in
  (* from above the pillars, which would come between *)
  let cam = Camera3d.orbit ~distance:16. ~height:10. ~look:1. (float_of_int s.frames) (winner.x, winner.y, winner.z) in
  let cam = { cam with far = 3000. } in
  let message, color =
    match (v.players, g.p2.armor <= 0) with
    | 1, true -> ("YOU WIN", rgb 255 255 120)
    | 1, false -> ("YOU LOSE", red_sled)
    | _, true -> ("P1 WINS", blue)
    | _, false -> ("P2 WINS", red_sled)
  in
  (* the last hit's flash stopped; the loser gone, with the juice, in
   * pieces *)
  let still (s : sled) = { s with hit = 0 } in
  let sleds = List.filter_map (fun (c, s) -> if wrecked fx s then None else Some (sled c (still s))) [ (blue, g.p1); (red_sled, g.p2) ] in
  ( Juice3d.camera fx cam,
    sky cam @ Juice3d.view fx ((arena :: ramps_view v) @ sleds)
    @ List.map hud ((text color 6. message |> move_y 250.) :: Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-300.) ]) )

(* one view, the whole window, but with players=2 while playing: a
 * view each, player 1 above (Playground3d.split) *)
let view (computer : computer) (m : model) : view list =
  let v = variants computer and s = m.scenes in
  let whole (camera, shapes) = [ { camera; area = whole; shapes } ] in
  match s.scene with
  | Title -> whole (view_title v s)
  | Over g -> whole (view_over v m.fx s g)
  | Playing g ->
      List.mapi
        (fun i area ->
          let camera, shapes = view_player v m.fx (area_screen computer.screen area) g (i = 0) in
          { camera; area; shapes })
        (split v.players)

let app = split3d view update initial_model

(* flat shading, the System 21's look; the back faces drawn, for the sky
 * (seen from below, see Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d ~flags:(Playground_platform.flags ())
    ~rendering:{ default_rendering with shading = Flat; backface_culling = false }
    app
