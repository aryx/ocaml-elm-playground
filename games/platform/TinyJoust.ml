(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Joust (John Newcomer, Williams Electronics, 1982):
 * you ride a flying ostrich over a pit of lava, the enemy knights ride
 * buzzards, and when two riders meet the higher lance wins. The loser
 * leaves an egg, which falls, bounces along the ledges, and hatches
 * into a new and faster rider -- unless you get to it first.
 *
 *   left/right   push (and you keep going: there are no brakes)
 *   space, up    flap, once per press: hammer the key to climb
 *
 * Joust was the arcade's answer to a question nobody had asked: what
 * if the platform game let go of the ground? Its flap is Newcomer's
 * one idea, and everything else in the game comes out of it -- you
 * cannot stop, you cannot turn on the spot, and the fight is not about
 * hitting but about *being higher when you meet*. It was also the first
 * arcade game two people could play at once on the same screen without
 * taking turns, and the joy of it was that co-operation was optional:
 * flying into your friend jousted him just as well. (Names, dates and
 * the rest from memory, to check.)
 *
 * What this one has that the others in this directory do not is,
 * honestly, very little -- and that is why it is here. A whole arcade
 * game (waves, lives, a score, an enemy with a mind of its own, eggs
 * that hatch) in about 200 lines of code, the size of TinyBreakout,
 * and that only because the physics layer is already most of it:
 *
 *  - every bird, egg and ledge is a [Physics.body], and the entire
 *    flight model is four verbs on it ([fly], five lines): [fall] for
 *    gravity, [slow] for the air, [push] for the direction held, and a
 *    flap that adds to the rise. Momentum, the top speed, the long
 *    turn, the sag when you stop flapping -- none of those is written
 *    anywhere: they are what those four verbs do together;
 *  - the ledges are not tiles and there is no collision code in this
 *    file at all. One call to [Physics.bounce_all] does every pair in
 *    the arena at once ([bounced]): bird against ledge, bird against
 *    bird, egg against ledge;
 *  - so the game itself is one rule, [meet]: of two riders who touch,
 *    the higher one wins.
 *
 * Compare TinyFlappyBird, the other flying game here: its flap *sets*
 * the rise (vy becomes flap_speed, so every flap is identical, which
 * is what makes it learnable by rhythm), and it has no physics layer
 * under it at all -- two numbers and a line of gravity. Joust's flap
 * *adds* to the rise, which is the harder, more physical choice, and
 * is why flapping has a rhythm of its own: hammer it and you hit the
 * ceiling, let it go and you sink into the lava.
 *
 * Both riders sit the same distance above their mount, so comparing
 * lances is comparing the two bodies' y -- which is why the drawing
 * puts the rider's head exactly at the height the comparison uses
 * ([lance]): what you see is what is compared. Within [level] pixels
 * neither is higher, and then the engine simply bounces them apart,
 * which is the arcade's rule too.
 *
 * What it uses: no kit (there is one Joust); Scene2d (title, play,
 * game over, and [pressed] for the flap: a *press*, not a key held --
 * holding space would be a jet, not a wing); and Physics for
 * everything that moves -- body, at, upright, rough, bouncy,
 * immovable, fall, slow, push, step, touching, bounce_all, draw,
 * debug. Not Physics.world and its solver: nothing here rests on
 * anything for long, and a pile of birds is not a thing. Not Tilemap:
 * seven ledges are seven bodies. Not Camera2d: the arena is one
 * screen, as it was in 1982. Not Random: the waves come from the
 * ledges they are laid on, so a run replays exactly.
 *
 * Left as exercises: the second player on the same keyboard (a/d and
 * w, as TinyXpilot.ml does it) -- which is the real Joust, and
 * needs only a second flyer with [Player]'s role, the two of them
 * jousting each other by the same [meet]; the pterodactyl that comes
 * for you when a wave lasts too long, killable only through its open
 * beak; the lava troll's hand, which grabs you if you fly too low over
 * the pit (an upward [Physics.push] on a body that is holding you);
 * the egg that must be collected before it hatches *in the air*, worth
 * more the higher you take it; the survival and egg waves; sounds (the
 * flap, the clash, the shell).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The arena *)
(*****************************************************************************)

let sky = rgb 18 16 38
let rock = rgb 118 92 68
let lava_top = -420.
let ceiling = 460.

let ledge (w : number) (x : number) (y : number) : Physics.body =
  Physics.body (rectangle rock w 24.) |> Physics.at x y |> Physics.immovable |> Physics.rough 0.8

(* the two banks of the lava pit, two half-height ledges, the big one
 * in the middle, and two eyries at the top corners (which the wrap
 * joins into one) *)
let ledges : Physics.body list =
  [ ledge 400. (-300.) (-390.); ledge 400. 300. (-390.);
    ledge 300. (-250.) (-150.); ledge 300. 250. (-150.);
    ledge 340. 0. 110.;
    ledge 200. (-400.) 300.; ledge 200. 400. 300. ]

(* where the buzzards of a wave are laid, in this order *)
let nests = [ (300., -340.); (-250., -100.); (250., -100.); (0., 160.); (-400., 350.); (400., 350.); (-450., -340.) ]

(* the player's own ledge, which no nest is laid on: waking up inside a
 * buzzard would be a joust you never saw coming *)
let home = (-300., -340.)

(*****************************************************************************)
(* What flies *)
(*****************************************************************************)

type role = Player | Buzzard | Egg

type flyer = {
  b : Physics.body;
  role : role;
  (* the arcade's three mounts, 0 to 2: faster, and worth more (the
   * bounder, the hunter and the shadow lord, 500, 750 and 1500 points;
   * from memory, to check) *)
  tier : int;
  (* 1 right, -1 left: the drawing only, the physics has no facing *)
  facing : number;
  (* frames as what it is now: an egg hatches, a buzzard beats its wings *)
  age : int;
}

let tier_color = [| rgb 200 70 60; rgb 175 175 185; rgb 120 110 225 |]
let tier_speed = [| 1.0; 1.15; 1.3 |]
let tier_points = [| 500; 750; 1500 |]
let player_color = rgb 235 200 80
let egg_color = rgb 225 220 195

let color_of (role : role) (tier : int) : color =
  match role with Player -> player_color | Buzzard -> tier_color.(tier) | Egg -> egg_color

let flyer (role : role) (tier : int) (x : number) (y : number) : flyer =
  let color = color_of role tier in
  (* the hitbox is the mount, not the lance: a lance that grazes
   * doesn't count. What the lance decides is who wins, not whether
   * they met *)
  let shape = match role with Egg -> oval color 24. 28. | Player | Buzzard -> oval color 48. 26. in
  {
    b =
      Physics.body shape |> Physics.at x y |> Physics.upright |> Physics.rough 0.5
      |> Physics.bouncy (match role with Egg -> 0.5 | Player | Buzzard -> 0.25);
    role;
    tier;
    facing = 1.;
    age = 0;
  }

(*****************************************************************************)
(* Flying *)
(*****************************************************************************)

let gravity = 900.
let drag = 1.6 (* the air: with the push below, a top speed of 700/1.6 *)
let push = 700. (* what holding a direction adds, per second per second *)
let flap_kick = 330. (* what one flap adds to the rise *)
let flap_top = 430. (* and the fastest anything ever rises *)

(* The whole flight model, for the player and the buzzards alike --
 * which is what makes a joust fair. Note that nothing here is a
 * position: [fall], [slow] and [push] only say what pulls on the body,
 * and [step] is the single place where it moves. *)
let fly ?(speed = 1.) (dx : number) (flapping : bool) (f : flyer) : flyer =
  let b : Physics.body = f.b |> Physics.fall gravity |> Physics.slow drag |> Physics.push (dx * push * speed) 0. in
  let b = if flapping then { b with vy = Float.min (flap_top * speed) (b.vy + flap_kick) } else b in
  { f with b = Physics.step b; facing = (if dx = 0. then f.facing else dx); age = f.age +.. 1 }

(* The whole AI: go the short way round towards the player, and flap
 * whenever you are below him. A buzzard that is above you stops
 * flapping and sinks onto you, which is exactly what you should be
 * doing to it. *)
let brain (screen : screen) (target : flyer) (f : flyer) : number * bool =
  let dx = target.b.x - f.b.x in
  let dx = if Float.abs dx > screen.width / 2. then -.dx else dx (* the short way is through the edge *) in
  ((if dx > 0. then 1. else -1.), f.b.y < target.b.y + 30. && f.age mod 11 = 0)

(* The arena is joined sideways (fly off the right, come back on the
 * left, as Joust and Asteroids do), the top is a ceiling you bump your
 * head on, and under the bottom is the lava. [Physics.wrap] wraps both
 * ways, which here would drop you in it. *)
let inside (screen : screen) (f : flyer) : flyer =
  let b : Physics.body = f.b in
  let x = if b.x < screen.left then b.x + screen.width else if b.x > screen.right then b.x - screen.width else b.x in
  let y, vy = if b.y > ceiling then (ceiling, Float.min 0. b.vy) else (b.y, b.vy) in
  { f with b = { b with x; y; vy } }

(* One call for every pair in the arena: bird against ledge, bird
 * against bird, egg against ledge. Only the pairs whose boxes overlap
 * are tested exactly (Physics.broad_phase), and the ledges, being
 * immovable, come back unchanged. *)
let bounced (flyers : flyer list) : flyer list =
  let moved = Physics.bounce_all (List.map (fun (f : flyer) -> f.b) flyers @ ledges) in
  List.map2 (fun f b -> { f with b }) flyers (List.filteri (fun i _ -> i < List.length flyers) moved)

(*****************************************************************************)
(* The one rule *)
(*****************************************************************************)

let lance = 30. (* how far above his mount the rider's head and lance are *)
let level = 10. (* closer than this, and neither of the two is higher *)

type meeting = Higher | Lower | Level

let meet (a : flyer) (b : flyer) : meeting =
  if a.b.y > b.b.y + level then Higher else if b.b.y > a.b.y + level then Lower else Level

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  player : flyer;
  (* the buzzards and the eggs *)
  others : flyer list;
  wave : int;
  score : int;
  lives : int;
  (* frames before the player comes back, 0 while he flies *)
  dead : int;
}

type scene = Title | Playing of play | Over of int (* score *)
type model = scene Scene2d.t

let hatch_after = 420 (* 7 seconds in the shell *)
let respawn = 90
let egg_points = 250

let wave_tier (wave : int) : int = min 2 ((wave -.. 1) /.. 2)

(* a wave is its ledges: no Random, so a run replays exactly *)
let spawn_wave (wave : int) : flyer list =
  nests
  |> List.filteri (fun i _ -> i < 2 +.. wave)
  |> List.map (fun (x, y) -> flyer Buzzard (wave_tier wave) x y)

let start () : play =
  { player = flyer Player 0 (fst home) (snd home); others = spawn_wave 1; wave = 1; score = 0; lives = 3; dead = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the egg the loser leaves, thrown on by half of what he was doing,
 * and holding the tier he will come back as *)
let egg_of (f : flyer) : flyer =
  let e = flyer Egg (min 2 (f.tier +.. 1)) f.b.x f.b.y in
  let b : Physics.body = e.b in
  { e with b = { b with vx = f.b.vx * 0.5; vy = f.b.vy * 0.5 } }

let hatched (f : flyer) : flyer =
  if f.role = Egg && f.age > hatch_after then flyer Buzzard f.tier f.b.x f.b.y else f

let update_play (computer : computer) (scenes : model) (p : play) : play =
  let screen = computer.screen in
  let flapping = Scene2d.pressed (fun k -> k.kspace || k.kup) scenes in
  let player = if p.dead > 0 then p.player else fly (to_x computer.keyboard) flapping p.player in
  let others =
    List.map
      (fun (f : flyer) ->
        match f.role with
        | Egg -> fly 0. false f
        | Buzzard ->
            let dx, flapping = brain screen player f in
            fly ~speed:tier_speed.(f.tier) dx flapping f
        | Player -> f)
      p.others
  in
  (* the meetings, on the positions they flew to: an egg the player
   * runs into is collected, a buzzard is a joust *)
  let kept, laid, points, lost =
    List.fold_left
      (fun (kept, laid, points, lost) (f : flyer) ->
        if p.dead > 0 || not (Physics.touching player.b f.b) then (f :: kept, laid, points, lost)
        else
          match f.role with
          | Egg -> (kept, laid, points +.. egg_points, lost)
          | Player -> (f :: kept, laid, points, lost)
          | Buzzard -> (
              match meet player f with
              | Higher -> (kept, egg_of f :: laid, points +.. tier_points.(f.tier), lost)
              | Lower -> (f :: kept, laid, points, true)
              | Level -> (f :: kept, laid, points, lost)))
      ([], [], 0, false) others
  in
  (* the lava keeps what falls in it, egg or rider, and leaves nothing *)
  let others = List.filter (fun (f : flyer) -> f.b.y > lava_top) (List.rev kept) @ laid in
  let lost = lost || (p.dead = 0 && player.b.y < lava_top) in
  (* then the engine pushes apart everything that is still overlapping *)
  let player, others =
    match bounced (player :: others) with p :: os -> (p, os) | [] -> (player, others)
  in
  let others = List.map (fun f -> hatched (inside screen f)) others in
  let wave = if others = [] then p.wave +.. 1 else p.wave in
  {
    player = (if lost then flyer Player 0 (fst home) (snd home) else inside screen player);
    others = (if others = [] then spawn_wave wave else others);
    wave;
    score = p.score +.. points;
    lives = (if lost then p.lives -.. 1 else p.lives);
    dead = (if lost then respawn else max 0 (p.dead -.. 1));
  }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed k = Scene2d.pressed k scenes in
  match scenes.scene with
  | Title | Over _ -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (start ())) scenes else scenes
  | Playing p ->
      let p = update_play computer scenes p in
      if p.lives <= 0 then Scene2d.go (Over p.score) scenes else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the rider, drawn where the rule looks: his head is at [lance], the
 * height [meet] compares *)
let rider (f : flyer) : shape list =
  let skin = if f.role = Player then rgb 250 240 210 else rgb 55 45 55 in
  [ rectangle skin 6. 18. |> move (-5. * f.facing) 16.;
    circle skin 7. |> move (-5. * f.facing) lance;
    rectangle (rgb 225 225 240) 34. 3. |> move (18. * f.facing) (lance - 5.) ]

(* the mount's head on its long neck, and the wing, which beats while
 * it climbs: one shape, two angles *)
let mount (f : flyer) : shape list =
  let color = color_of f.role f.tier in
  let up = f.b.vy > 0. && f.age mod 12 < 6 in
  [ rectangle color 5. 20. |> move (20. * f.facing) 12.;
    circle color 8. |> move (23. * f.facing) 22.;
    triangle (rgb 240 180 60) 7. |> rotate (if f.facing > 0. then -90. else 90.) |> move (32. * f.facing) 20.;
    rectangle (rgb 235 235 240) 30. 6. |> rotate (if up then 28. else -12.) |> move (-6. * f.facing) 6. ]

let draw_flyer (f : flyer) : shape =
  let parts = match f.role with Egg -> [] | Player | Buzzard -> mount f @ rider f in
  group (Physics.draw f.b :: List.map (fun s -> s |> move f.b.x f.b.y) parts)

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  [ rectangle sky screen.width screen.height;
    (* the lava, and its surface shimmering *)
    rectangle (rgb 190 60 25) screen.width (lava_top - screen.bottom) |> move_y ((lava_top + screen.bottom) / 2.);
    rectangle (rgb 250 160 40) screen.width 10. |> move_y (lava_top + wave (-4.) 4. 1.7 computer.time) ]
  @ List.map Physics.draw ledges
  @ List.map draw_flyer p.others
  @ (if p.dead > 0 then
       (* blinking back on his ledge *)
       if p.dead mod 16 < 8 then [ draw_flyer p.player |> fade 0.4 ] else []
     else [ draw_flyer p.player ])
  @ (if List.mem_assoc "hitboxes" computer.flags then List.map (fun (f : flyer) -> Physics.debug f.b) (p.player :: p.others) @ List.map Physics.debug ledges
     else [])
  @ [ text white 2.5 (Printf.sprintf "score %d    wave %d" p.score p.wave) |> move_y (screen.top - 40.);
      text player_color 2.5 (String.concat " " (List.init (max 0 p.lives) (fun _ -> "*"))) |> move_y (screen.top - 80.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      [ rectangle sky screen.width screen.height;
        text white 6. "TINY JOUST" |> move_y 220.;
        text white 2. "left/right: push (there are no brakes)" |> move_y 90.;
        text white 2. "space or up: flap, once per press -- hammer it to climb" |> move_y 50.;
        text white 2. "the higher lance wins. collect the eggs before they hatch" |> move_y 10.;
        text (rgb 190 60 25) 2. "and stay out of the lava" |> move_y (-30.) ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-220.) ]
  | Playing p -> view_play computer p
  | Over score ->
      [ rectangle sky screen.width screen.height;
        text white 5. "GAME OVER" |> move_y 120.;
        text white 3. (Printf.sprintf "score %d" score) |> move_y 20. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-220.) ]

let help =
  {|TinyJoust
  left/right  push (and you keep going: there are no brakes)
  space, up   flap, once per press: hammer the key to climb
              (space also starts and restarts)
  the higher lance wins; the loser leaves an egg, which hatches
  flags: hitboxes   draw what the physics sees
  e.g.   dune exec games/platform/TinyJoust.exe -- hitboxes
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
