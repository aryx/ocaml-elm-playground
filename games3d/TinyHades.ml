(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Hades (Supergiant, 2020): out of the underworld one
 * chamber at a time, a boon from a god after each one, and back to the
 * start when you die -- a little stronger than you were.
 *
 *   arrows   move
 *   space    strike (a short arc in front of you)
 *   x        dash, and for a few frames nothing can touch you
 *   1 2 3    between chambers, the boon to take
 *
 * This is the third game of a line the other two are in already:
 * games/TinyRogue.ml (Rogue, 1980), games2.5d/TinyDiablo.ml (Diablo,
 * 1996), and Hades. They share a dungeon made anew each time and they
 * differ in what death is, which turns out to be the whole design:
 *
 *   TinyRogue     death is final, and the dungeon is forgotten
 *   TinyDiablo    death ends a life; the character keeps what it
 *                 carried, and goes back down
 *   TinyHades     death ends a *run*, and pays for the next one
 *
 * That last one is the roguelite, and it is the only game in this
 * repository where losing is a way of making progress: every run adds
 * to [kept], which is health you start the next one with, so the
 * player who cannot win a fight can still win the war by losing it
 * enough times. Supergiant's other idea, which this toy only points
 * at, is that the story is told *through* those deaths -- every one
 * sends you home to people who have something new to say.
 *
 * What is between chambers is the other half: three boons, one taken
 * ([boons], [take_boon]). A boon is not a weapon, it is a *number* in
 * the model changed for the rest of the run -- more damage, a longer
 * reach, a quicker dash, more health -- so a run is a little machine
 * you build out of whatever the gods happen to offer, and no two runs
 * are the same build. Hades' real boons combine (fire that spreads,
 * dashes that strike); combining is left as an exercise, and the place
 * to do it is [strike].
 *
 * The third thing it has, and the reason an action game of the 2010s
 * feels different from one of the 1990s, is the **dash with
 * invulnerability** ([dashing]): for eight frames of it nothing lands
 * on you. Once a game gives you that, every fight becomes a question
 * of timing rather than of positioning, which is Dark Souls' roll
 * (2011) and everything after it, Hades included. Compare
 * games3d/TinyBoomerangFu.ml, whose dash is only speed: there, being
 * somewhere else is the whole of the defence.
 *
 * Why it is here and not in games2.5d/: Hades is drawn in 2D from a
 * fixed angle, as Diablo is, so the honest twin of TinyDiablo would be
 * the same dungeon with a camera over it. This is not that -- it keeps
 * no world in common with it -- and it is in games3d/ to be the same
 * genre with the engine doing the drawing: a camera, triangles and a
 * z-buffer, where games2.5d/TinyDiablo.ml has two lines of arithmetic
 * and a sort. Read them together and the isometric trick is exactly
 * what the camera in [chamber_camera] does for free.
 *
 * What it uses: playground3d (boxes and spheres, no textures), Camera3d
 * (one fixed camera over the chamber, the arcade angle Hades itself
 * uses), Scene2d, and nothing else -- no kit: an arena is a floor and
 * four walls, and there is no second game to share one with yet. Not
 * Physics3d: nothing here falls, and the dash is a speed, not a force.
 *
 * Exercises: boons that combine rather than add up; a cast (the ranged
 * attack Hades gives you, on a resource you pick up off the floor); a
 * boss at the end of a depth; the mirror in the house, where the
 * [kept] of this toy would become a list of upgrades to spend on; and
 * the dialogue, which is the game everyone actually remembers.
 *)
open Playground
open Playground3d
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The chamber *)
(*****************************************************************************)

let half = 10. (* the arena is 20 by 20, and all of it is on screen *)
let wall_h = 2.2
let floor_color = rgb 58 46 62
let wall_color = rgb 40 32 46
let blood = rgb 190 50 60
let gold_color = rgb 235 195 90

(* the same shift register as the other toys: a run is a pure function
 * of its seed *)
let roll (seed : int) (n : int) : int * int =
  let bit = (seed lxor (seed lsr 2) lxor (seed lsr 3) lxor (seed lsr 5)) land 1 in
  let seed = (seed lsr 1) lor (bit lsl 15) in
  (seed mod n, seed)

(*****************************************************************************)
(* What fights *)
(*****************************************************************************)

type kind = Shade | Wretch | Brute

let hp_of = function Shade -> 10. | Wretch -> 14. | Brute -> 30.
let speed_of = function Shade -> 0.10 | Wretch -> 0.05 | Brute -> 0.07
let hit_of = function Shade -> 6. | Wretch -> 8. | Brute -> 14.
let color_of = function Shade -> rgb 130 150 210 | Wretch -> rgb 150 200 140 | Brute -> rgb 210 110 90
let size_of = function Shade -> 0.55 | Wretch -> 0.6 | Brute -> 0.9

type foe = { fx : number; fz : number; hp : number; kind : kind; cool : int; hurt : int }

(* what a wretch throws *)
type shot = { sx : number; sz : number; svx : number; svz : number }

(* {1 The boons} a boon is a number in the model, changed for the rest
 * of the run *)
type boon = Fury | Reach | Swift | Vitality

let boon_name = function
  | Fury -> "FURY: every strike hits harder"
  | Reach -> "REACH: the arc is wider and longer"
  | Swift -> "SWIFT: the dash comes back sooner"
  | Vitality -> "VITALITY: more life, and some of it now"

let boon_god = function Fury -> "Ares" | Reach -> "Artemis" | Swift -> "Hermes" | Vitality -> "Demeter"

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  seed : int;
  chamber : int; (* how deep this run has got *)
  run : int; (* how many runs have been made *)
  kept : number; (* the health every run starts with, earned by dying *)
  px : number;
  pz : number;
  face : number; (* where the strike goes, in degrees *)
  hp : number;
  max_hp : number;
  damage : number;
  reach : number;
  dash_wait : int; (* frames between dashes *)
  dash : int; (* frames of dash left; the first of them are invulnerable *)
  cool : int; (* frames before the next strike *)
  swing : int; (* frames of the strike being drawn *)
  hurt : int;
  foes : foe list;
  shots : shot list;
  (* between chambers: the three on offer *)
  offer : boon list;
}

type scene = Title | Playing of play | Dead of play
type model = scene Scene2d.t

let dash_frames = 14
let dash_safe = 8 (* of those, the ones nothing can touch you in *)

let spawn (chamber : int) (seed : int) : foe list * int =
  let n = 2 +.. min 5 (chamber /.. 2 +.. 1) in
  let seed = ref seed and out = ref [] in
  for i = 1 to n do
    let a, s = roll !seed 360 in
    let d, s = roll s 6 in
    let k, s = roll s 10 in
    seed := s;
    let kind = if chamber >= 3 && k > 7 then Brute else if k > 4 then Wretch else Shade in
    let angle = float_of_int a * Float.pi / 180. in
    let r = 4. + float_of_int d in
    ignore i;
    out := { fx = r * cos angle; fz = r * sin angle; hp = hp_of kind * (1. + (0.1 * float_of_int chamber)); kind; cool = 60; hurt = 0 } :: !out
  done;
  (!out, !seed)

let three_boons (seed : int) : boon list * int =
  let all = [ Fury; Reach; Swift; Vitality ] in
  let a, s = roll seed 4 in
  let b, s = roll s 4 in
  let c, s = roll s 4 in
  let pick i = List.nth all (i mod 4) in
  let chosen = [ pick a; pick (b +.. 1); pick (c +.. 2) ] in
  (chosen, s)

let new_run (run : int) (kept : number) (seed : int) : play =
  let foes, seed = spawn 1 seed in
  { seed; chamber = 1; run; kept;
    px = 0.; pz = 6.; face = 90.; hp = 50. + kept; max_hp = 50. + kept;
    damage = 8.; reach = 2.2; dash_wait = 40; dash = 0; cool = 0; swing = 0; hurt = 0;
    foes; shots = []; offer = [] }

let start () : play = new_run 1 0. 4242
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let apart (ax : number) (az : number) (bx : number) (bz : number) : number = Float.hypot (bx - ax) (bz - az)
let inside (x : number) (z : number) : number * number =
  (Float.max (-.half + 0.6) (Float.min (half - 0.6) x), Float.max (-.half + 0.6) (Float.min (half - 0.6) z))

let take_boon (b : boon) (p : play) : play =
  match b with
  | Fury -> { p with damage = p.damage * 1.35 }
  | Reach -> { p with reach = p.reach + 0.9 }
  | Swift -> { p with dash_wait = max 12 (p.dash_wait -.. 10) }
  | Vitality -> { p with max_hp = p.max_hp + 15.; hp = Float.min (p.max_hp + 15.) (p.hp + 15.) }

(* the strike: everything in the arc in front of you, at once. Hades'
 * own boons make this line do more -- catch fire, chain, knock back --
 * which is the exercise the header names *)
let strike (p : play) : foe list * bool =
  let a = p.face * Float.pi / 180. in
  let hit (f : foe) : bool =
    let d = apart p.px p.pz f.fx f.fz in
    d < p.reach + size_of f.kind
    &&
    let towards = Float.atan2 (f.fz - p.pz) (f.fx - p.px) in
    let diff = Float.abs (Float.rem (towards - a + (3. * Float.pi)) (2. * Float.pi) - Float.pi) in
    diff < 1.1
  in
  let any = List.exists hit p.foes in
  (List.map (fun (f : foe) -> if hit f then { f with hp = f.hp - p.damage; hurt = 8 } else { f with hurt = max 0 (f.hurt -.. 1) }) p.foes, any)

let update_play (computer : computer) (scenes : model) (p : play) : play =
  let k = computer.keyboard in
  (* between chambers: the doors are shut until a boon is taken *)
  if p.offer <> [] then
    let picked =
      List.filteri (fun i _ -> Scene2d.pressed (fun k -> Set_.mem (string_of_int (i +.. 1)) k.keys) scenes) p.offer
    in
    match picked with
    | b :: _ ->
        let foes, seed = spawn (p.chamber +.. 1) p.seed in
        { (take_boon b p) with chamber = p.chamber +.. 1; offer = []; foes; shots = []; seed; px = 0.; pz = 6. }
    | [] -> p
  else
    let dx, dz = to_xy k in
    let dashing = p.dash > 0 in
    let start_dash = (not dashing) && p.cool >= 0 && p.dash = 0 && Scene2d.pressed (fun k -> Set_.mem "x" k.keys) scenes in
    let speed = if dashing then 0.38 else 0.11 in
    let face = if dx = 0. && dz = 0. then p.face else Float.atan2 dz dx * 180. / Float.pi in
    let px, pz = inside (p.px + (dx * speed)) (p.pz + (dz * speed)) in
    (* the strike *)
    let swinging = p.cool = 0 && Scene2d.pressed (fun k -> k.kspace) scenes in
    let p = { p with px; pz; face } in
    let foes, _landed = if swinging then strike p else (p.foes, false) in
    let dead, alive = List.partition (fun (f : foe) -> f.hp <= 0.) foes in
    ignore dead;
    (* the foes come at you, and the wretches throw *)
    let shots = ref (List.filter_map (fun (s : shot) ->
        let sx = s.sx + s.svx and sz = s.sz + s.svz in
        if Float.abs sx > half || Float.abs sz > half then None else Some { s with sx; sz }) p.shots)
    in
    let foes =
      List.map
        (fun (f : foe) ->
          let d = apart f.fx f.fz p.px p.pz in
          let want = if f.kind = Wretch then 6.5 else 0.8 in
          let step = if d > want then speed_of f.kind else if d < want - 1. then -.speed_of f.kind else 0. in
          let fx, fz = inside (f.fx + ((p.px - f.fx) / Float.max 0.001 d * step)) (f.fz + ((p.pz - f.fz) / Float.max 0.001 d * step)) in
          let f = { f with fx; fz; cool = max 0 (f.cool -.. 1) } in
          if f.kind = Wretch && f.cool = 0 && d < 9. then begin
            shots := { sx = f.fx; sz = f.fz; svx = (p.px - f.fx) / d * 0.16; svz = (p.pz - f.fz) / d * 0.16 } :: !shots;
            { f with cool = 90 }
          end
          else f)
        alive
    in
    (* what lands on you, unless you are in the first frames of a dash *)
    let safe = p.dash > dash_frames -.. dash_safe in
    let by_foe =
      List.fold_left (fun n (f : foe) -> if (not safe) && f.cool = 0 && apart f.fx f.fz p.px p.pz < 1.1 + size_of f.kind then n + hit_of f.kind else n) 0. foes
    in
    let foes = List.map (fun (f : foe) -> if (not safe) && f.cool = 0 && apart f.fx f.fz p.px p.pz < 1.1 + size_of f.kind then { f with cool = 60 } else f) foes in
    let struck, rest = List.partition (fun (s : shot) -> (not safe) && apart s.sx s.sz p.px p.pz < 0.9) !shots in
    let by_shot = float_of_int (List.length struck) * 7. in
    let cleared = foes = [] in
    let offer, seed = if cleared then three_boons p.seed else ([], p.seed) in
    {
      p with
      hp = p.hp - by_foe - by_shot;
      hurt = (if by_foe + by_shot > 0. then 10 else max 0 (p.hurt -.. 1));
      dash = (if start_dash then dash_frames else max 0 (p.dash -.. 1));
      cool = (if swinging then 22 else if start_dash then p.dash_wait else max 0 (p.cool -.. 1));
      swing = (if swinging then 10 else max 0 (p.swing -.. 1));
      foes; shots = rest; offer; seed;
    }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go (Playing (start ())) scenes else scenes
  | Dead p ->
      (* death is not a reset: it pays for the next run *)
      if Scene2d.pressed (fun k -> k.kspace) scenes then
        Scene2d.go (Playing (new_run (p.run +.. 1) (p.kept + 6.) p.seed)) scenes
      else scenes
  | Playing p ->
      let p = update_play computer scenes p in
      if p.hp <= 0. then Scene2d.go (Dead p) scenes else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* one fixed camera over the chamber: the angle Hades is drawn at, and
 * what games2.5d/TinyDiablo.ml's two lines of arithmetic do by hand *)
let chamber_camera : camera = Camera3d.from_far ~fov:42. ~offset:(0., 21., 20.) (0., 0., -1.)

let arena : shape3d list =
  let wall (x : number) (z : number) (w : number) (d : number) : shape3d =
    box wall_color w wall_h d |> move3d x (wall_h / 2.) z
  in
  (* the underworld around the chamber: without it the sky is white,
   * and this one is not that sort of place *)
  [ Camera3d.floor ~color:(rgb 12 9 16) ~ground:(-4.) chamber_camera;
    plane floor_color (2. * half) (2. * half);
    wall 0. (-.half) (2. * half) 0.8; wall 0. half (2. * half) 0.8;
      wall (-.half) 0. 0.8 (2. * half); wall half 0. 0.8 (2. * half) ]

let hero_shape (p : play) : shape3d =
  let c = if p.hurt > 0 then rgb 250 160 150 else if p.dash > dash_frames -.. dash_safe then rgb 250 240 160 else rgb 220 220 235 in
  group3d
    [ box c 0.8 1.4 0.8 |> move3d 0. 0.7 0.;
      sphere c 0.38 |> move3d 0. 1.7 0.;
      (* the blade, out in front while the strike is drawn *)
      (if p.swing > 0 then box (rgb 250 230 140) (p.reach * 1.6) 0.16 0.5 |> move3d (p.reach * 0.6) 1. 0. else box (rgb 200 200 210) 0.9 0.14 0.2 |> move3d 0.5 0.9 0.2)
      |> fun s -> s ]
  |> rotate3d 0. (-.p.face) 0.
  |> move3d p.px 0. p.pz

let foe_shape (f : foe) : shape3d =
  let c = if f.hurt > 0 then white else color_of f.kind in
  let s = size_of f.kind in
  group3d [ box c (s * 1.3) (s * 2.2) (s * 1.3) |> move3d 0. (s * 1.1) 0.; sphere c (s * 0.6) |> move3d 0. (s * 2.6) 0. ]
  |> move3d f.fx 0. f.fz

let bar (color : color) (full : number) (w : number) : shape =
  group [ rectangle (rgb 30 26 32) (w + 6.) 22.; rectangle color (w * Float.max 0. full) 16. |> move_x (-.w / 2. * (1. - Float.max 0. full)) ]

let view_play (computer : computer) (p : play) : shape3d list =
  let screen = computer.screen in
  arena
  @ List.map foe_shape p.foes
  @ List.map (fun (s : shot) -> sphere (rgb 160 230 150) 0.22 |> move3d s.sx 0.8 s.sz) p.shots
  @ [ hero_shape p ]
  @ List.map hud
      ([ bar blood (p.hp / p.max_hp) 320. |> move_y (screen.top - 45.);
         text white 2. (Printf.sprintf "chamber %d    run %d    kept %.0f" p.chamber p.run p.kept) |> move_y (screen.top - 85.);
         text (rgb 170 160 180) 1.7 "arrows: move   space: strike   x: dash (it makes you untouchable)"
         |> move_y (screen.bottom + 30.) ]
      @
      if p.offer = [] then []
      else
        [ rectangle (rgb 20 16 26) 760. 260. |> fade 0.92;
          text gold_color 2.4 "A BOON, AND THE DOOR OPENS" |> move_y 90. ]
        @ List.mapi
            (fun i b ->
              text (if i = 0 then rgb 230 180 120 else if i = 1 then rgb 160 200 230 else rgb 180 230 170) 1.9
                (Printf.sprintf "%d  %s  --  %s" (i +.. 1) (boon_god b) (boon_name b))
              |> move_y (30. - (float_of_int i * 45.)))
            p.offer)

let view (computer : computer) (model : model) : camera * shape3d list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      ( chamber_camera,
        arena
        @ List.map hud
            ([ text blood 6. "TINY HADES" |> move_y 180.;
               text white 2. "out of the underworld, one chamber at a time" |> move_y 60.;
               text white 2. "arrows move, space strikes, x dashes -- and the dash is your defence" |> move_y 20.;
               text gold_color 2. "when you die, you keep a little of it: the next run starts stronger" |> move_y (-40.) ]
            @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-180.) ]) )
  | Playing p -> (chamber_camera, view_play computer p)
  | Dead p ->
      ( chamber_camera,
        arena
        @ List.map hud
            ([ text blood 5. "YOU DIED" |> move_y 120.;
               text white 2.5 (Printf.sprintf "chamber %d of run %d" p.chamber p.run) |> move_y 40.;
               text gold_color 2.2 (Printf.sprintf "and you keep %.0f more life for the next one" 6.) |> move_y (-20.) ]
            @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-180.) ])
        @ [ hud (rectangle (rgb 0 0 0) screen.width screen.height |> fade 0.35) ] )

let help =
  {|TinyHades
  arrows   move
  space    strike (an arc in front of you)
  x        dash -- its first frames are invulnerable, and that is the game
  1 2 3    between chambers, the boon to take
  dying is not the end of it: every run starts with more life than the last
|}

let app = game3d view update initial_model

let main =
  print_string help;
  Playground3d_platform.run_app3d app
