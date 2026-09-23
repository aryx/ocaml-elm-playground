(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Flappy Bird (Dong Nguyen, .GEARS, 2013), the
 * one-button game: a bird falls, each press of the button makes it flap
 * up, and it must fly through the gaps between pipes, forever. One
 * point per pipe passed; the first pipe you touch ends the game. Space
 * (or up, or a click) to flap.
 *
 * Flappy Bird came out in May 2013 on phones, unnoticed, and in January
 * 2014 it was suddenly the most downloaded game of the App Store, its
 * ads said to earn its author $50,000 a day; in February he took it
 * down, saying it had become an addiction for its players. It was
 * written in a few days, and its ideas are older: the "helicopter
 * games" of the 2000s (hold the button to rise, release to fall,
 * through an endless cave: SFCave, Helicopter Game), and the endless
 * runners (Canabalt, Adam Saltsman, 2009: one button to jump, running
 * across roofs until you miss one). (Names and dates from memory, to
 * check.)
 *
 * A game of one button is the smallest complete game there is, and a
 * good first game to read after Pong. Three ideas in it are new to the
 * games of this directory:
 *
 *  - A flap *sets* the velocity, it doesn't add to it: vy becomes
 *    [flap_speed], whether the bird was falling fast or rising already.
 *    That's why every flap looks the same, and why the game can be
 *    learned by the rhythm of the fingers alone. Adding an impulse
 *    (vy + flap_speed) would be more physical, and much harder to play:
 *    try it (see [update_bird]). What makes a game feel good isn't
 *    always physics: see Steve Swink's "Game Feel" (2009).
 *
 *  - The world is endless, so it can't be in the model as a level: it
 *    is made as the bird flies, a pipe appearing at the right edge of
 *    the screen when there's room for it, and dropped once it has left
 *    by the left edge ([spawn_and_drop]). The model never holds more
 *    than 4 pipes, however far the bird goes: the memory used is the
 *    screen's, not the world's.
 *
 *  - Randomness without Random: each pipe's gap is at a height drawn
 *    from a linear-feedback shift register ([lfsr]), whose state is in
 *    the model, passed from pipe to pipe and from game to game. The
 *    whole run is a pure function of the first state (the seed= flag),
 *    so it replays exactly (golden frames, a replay), unlike OCaml's
 *    global Random (see docs/claude_notes/plan_playground_other.md,
 *    section 1). The Atari 2600 games did this out of necessity: 128
 *    bytes of memory can't hold a world, so Activision's Pitfall!
 *    (David Crane, 1982) computes its 255 jungle screens from an 8-bit
 *    LFSR, stepped forward or backward as you walk right or left, and
 *    River Raid (Carol Shaw, 1982) its endless river from a 16-bit one,
 *    the same river every game. (Details from memory, to check.)
 *
 * Juice: the bird squashes at each flap, flattened by the push and
 * springing back taller, the rhythm of the fingers seen in its body; a
 * point pops the score, which swells and settles, in a burst of
 * sparks; and the crash shakes the screen hard, flashes it white (as
 * Flappy Bird does) and throws feathers of the bird's colors where it
 * hit. The juice is on by default; the flag juice=off gives the dry
 * game (dune exec games/arcade/TinyFlappyBird.exe -- juice=off). The
 * effects watch the game from outside ([juiced]): the rules don't know
 * about them, and the same keys give the same flight, dry or juiced.
 * No hitstop (Juice.freeze) at the crash, on purpose: it would change
 * *when* things happen, and a replay's flaps would come too early.
 *
 * What it uses: Scene2d (title, play, game over), Camera2d (the camera
 * following the bird, [look_at]; the city and the clouds behind with
 * [parallax]), Audio (flap, point, hit, fall), Juice (squash, stretch,
 * tween, shake, flash, burst). Not Physics: the bird is
 * two numbers, y and vy, one line of gravity (semi-implicit Euler, as in
 * TinyMario.ml), and its flap is not a force; its collisions are a
 * circle against a few boxes ([circle_hits_box]), which the engine
 * would do as well, but not more clearly. Not Tilemap: the pipes aren't
 * on a grid, they come from the LFSR.
 *
 * The bird's hitbox (a circle of 17) is smaller than its picture (the
 * body alone is 26 by 20): a pipe grazing a feather doesn't count. Games
 * are forgiving this way, on purpose; the flag hitboxes draws it.
 *
 * Left as exercises: difficulty rising with the score (the gaps closing
 * up, the pipes coming faster: Flappy Bird has none, its difficulty is
 * constant), the pipes moving up and down, a flap that adds instead of
 * setting (see above), the best score kept between runs (a file; the
 * web's localStorage).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The numbers of the game *)
(*****************************************************************************)

(* In pixels and seconds; the screen is 1000 x 1000. Tuning these is
 * most of the work of making such a game: a gap 40 pixels smaller, a
 * gravity 20% stronger, and it's another game. *)

let gravity = 1900. (* pixels per second, per second, down *)
let flap_speed = 620. (* the velocity a flap gives, up *)
let max_fall = 900. (* the fastest the bird falls (Flappy's terminal velocity) *)
let speed = 230. (* the bird flies forward at this speed, always *)

let pipe_width = 110.
let pipe_gap = 250. (* the hole's height *)
let pipe_spacing = 340. (* from a pipe to the next, center to center *)

let ground_y = -380. (* the top of the ground *)
let sky_y = 520. (* the bird can't fly higher (a bit above the screen) *)

let bird_radius = 17. (* its hitbox *)

(* where the bird is on the screen: left of center, to see the pipes
 * coming *)
let bird_screen_x = -250.

(*****************************************************************************)
(* Randomness in the model: a linear-feedback shift register *)
(*****************************************************************************)

(* A 16-bit Galois LFSR: shift the state right by one bit, and if the bit
 * that fell out was a 1, flip the bits of the "taps" (xor with 0xB400:
 * the bits 16, 14, 13 and 11 of the polynomial x^16 + x^14 + x^13 +
 * x^11 + 1).
 *
 *    state   1010 1100 1110 0001  (0xACE1)
 *    >> 1    0101 0110 0111 0000  and a 1 fell out, so
 *    xor     1011 0100 0000 0000  (0xB400)
 *    =       1110 0010 0111 0000  (0xE270)
 *
 * With these taps, the states go through all the 65535 non-zero 16-bit
 * numbers before coming back (a "maximal" LFSR); 0 would stay 0 forever,
 * so it's never a seed. In hardware it's a shift register and a few xor
 * gates, the cheapest random numbers there are: the NES's noise channel
 * is one (Noise.mli), and the Atari 2600 games had it in software.
 * Not good randomness: a state is the previous one shifted by a bit, 15
 * of their 16 bits the same, which is why [random_byte] steps it 8
 * times, so that all 8 bits of a byte are new.
 *
 * Reference: Wikipedia's "Linear-feedback shift register", whose example
 * is this one, from the same 0xACE1; Knuth, The Art of Computer
 * Programming vol. 2, section 3.2.2. *)
let lfsr (state : int) : int =
  let shifted = state lsr 1 in
  if state land 1 = 1 then shifted lxor 0xB400 else shifted

let default_seed = 0xACE1

(* a number from 0 to 255, and the next state *)
let random_byte (state : int) : int * int =
  let rec steps n s = if n = 0 then s else steps (n -.. 1) (lfsr s) in
  let s = steps 8 state in
  (s land 0xFF, s)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a pipe: its x in the world, and the height of its gap's center *)
type pipe = { px : number; gap_y : number }

type bird = {
  x : number; (* in the world: it grows forever, at [speed] *)
  y : number;
  vy : number;
}

type game = {
  bird : bird;
  pipes : pipe list; (* from left to right *)
  seed : int; (* the LFSR's state: where the next pipe's height comes from *)
  score : int;
  (* the bird hit something: it falls to the ground, the world stops *)
  dead : bool;
  (* before the first flap, the bird waits, bobbing: the time to put a
   * finger on the button *)
  started : bool;
  cam : Camera2d.t;
}

type scene = Title | Playing of game | Game_over of game

type model = {
  scenes : scene Scene2d.t;
  best : int;
  (* the juice: the effects, when the bird last flapped (it squashes),
   * when the last point was scored (the score pops) *)
  fx : Juice.t;
  flapped : time;
  scored : time;
}

(* the next pipe, [pipe_spacing] after [x], its gap's center between 60
 * pixels above the ground plus half the gap and 60 below the top of the
 * screen minus half the gap *)
let new_pipe (x : number) (seed : int) : pipe * int =
  let byte, seed = random_byte seed in
  let low = ground_y + (pipe_gap / 2.) + 60. and high = 500. - (pipe_gap / 2.) - 60. in
  ({ px = x + pipe_spacing; gap_y = low + ((high - low) * float_of_int byte / 255.) }, seed)

let new_game (seed : int) : game =
  { bird = { x = 0.; y = 50.; vy = 0. };
    (* the first pipe comes after 2 seconds of flight *)
    pipes = [];
    seed;
    score = 0;
    dead = false;
    started = false;
    cam = Camera2d.origin |> Camera2d.look_at (0. - bird_screen_x) 0. }

(* the seed= flag, e.g. seed=42; an LFSR's seed can't be 0, nor more than
 * 16 bits *)
let flag_seed (computer : computer) : int =
  match Option.bind (List.assoc_opt "seed" computer.flags) int_of_string_opt with
  | Some n when n land 0xFFFF <> 0 -> n land 0xFFFF
  | _ -> default_seed

let initial_model : model =
  { scenes = Scene2d.start Title; best = 0; fx = Juice.none ~seed:1; flapped = Time (-10.); scored = Time (-10.) }

(*****************************************************************************)
(* The endless world *)
(*****************************************************************************)

(* Pipes appear just beyond the screen's right edge and are dropped just
 * after its left one: [Camera2d.visible] says where the edges are in
 * the world.
 *
 *       dropped              the screen                 spawned
 *    |  |    |       +----------------------------+  |  |    |
 *    |__|    |       |  ||       ||        ||     |  |__|    |
 *            |       |  @        ||        ||     |          |
 *     __     |       |  ||                 ||     |   __     |
 *    |  |    |       |  ||       ||        ||     |  |  |    |
 *            +-- left edge                 right edge --+
 *
 * The first pipe is just beyond the right edge (3.7 seconds of flight
 * away), the next ones [pipe_spacing] after the last. *)
let spawn_and_drop (screen : screen) (g : game) : game =
  let visible = Camera2d.visible screen g.cam in
  let rec spawn pipes seed =
    let last_x = match List.rev pipes with p :: _ -> p.px | [] -> visible.right + pipe_width - pipe_spacing in
    if last_x < visible.right + pipe_width then
      let p, seed = new_pipe last_x seed in
      spawn (pipes @ [ p ]) seed
    else (pipes, seed)
  in
  let pipes = List.filter (fun p -> p.px + (pipe_width / 2.) > visible.left) g.pipes in
  let pipes, seed = spawn pipes g.seed in
  { g with pipes; seed }

(*****************************************************************************)
(* Collisions: a circle against boxes *)
(*****************************************************************************)

(* Whether the circle of center (cx, cy) and radius r overlaps the box
 * from (left, bottom) to (right, top): the point of the box closest to
 * the center is the center clamped into the box; the circle hits the
 * box if that point is less than r away.
 *
 *     +--------+
 *     |        |
 *     |        * <-- closest point: (right, cy)
 *     |        |    \
 *     +--------+     ( o )  the circle, center (cx, cy)
 *
 * E.g. a box from (0, 0) to (10, 10) and a circle of radius 5: centered
 * on (13, 5), the closest point is (10, 5), 3 away: a hit; on (14, 14),
 * the closest point is the corner (10, 10), sqrt 32 = 5.66 away: no hit,
 * although the circle's own bounding box would overlap the box's
 * corner. (And a center inside the box is its own closest point: 0
 * away.) *)
let circle_hits_box (cx : number) (cy : number) (r : number) (left : number) (bottom : number) (right : number)
    (top : number) : bool =
  let px = clamp left right cx and py = clamp bottom top cy in
  ((cx - px) * (cx - px)) + ((cy - py) * (cy - py)) < r * r

(* a pipe is two boxes, above and below its gap; the top one goes up to
 * the sky, so the bird can't fly over it *)
let hits_pipe (b : bird) (p : pipe) : bool =
  let left = p.px - (pipe_width / 2.) and right = p.px + (pipe_width / 2.) in
  circle_hits_box b.x b.y bird_radius left (p.gap_y + (pipe_gap / 2.)) right (sky_y + 1000.)
  || circle_hits_box b.x b.y bird_radius left (ground_y - 1000.) right (p.gap_y - (pipe_gap / 2.))

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let flap_sound = Audio.square 440. |> Audio.sliding 880. |> Audio.lasting 0.08 |> Audio.fading
let fall_sound = Audio.square 600. |> Audio.sliding 100. |> Audio.lasting 0.5 |> Audio.fading

(* One button: space, up, or a click; pressed, not held, see
 * Scene2d.pressed: holding the button flaps once *)
let flapped (computer : computer) (scenes : scene Scene2d.t) : bool =
  Scene2d.pressed (fun k -> k.kspace) scenes || Scene2d.pressed (fun k -> k.kup) scenes || computer.mouse.mclick

(* One tick of the bird, semi-implicit Euler (the velocity first, the
 * position with the new velocity): falling faster and faster, down to
 * [max_fall]; a flap sets vy (the alternative, adding to vy, is the
 * comment). Its x always grows at [speed]. *)
let update_bird (dt : number) (flap : bool) (b : bird) : bird =
  let vy = if flap then flap_speed (* b.vy + flap_speed *) else max (-.max_fall) (b.vy - (gravity * dt)) in
  { x = b.x + (speed * dt); y = min sky_y (b.y + (vy * dt)); vy }

let dt = 1. / 60.

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let flap = flapped computer scenes in
  if g.dead then
    (* falling, the world stopped: no more flaps, no forward move *)
    let b = { g.bird with vy = max (-.max_fall) (g.bird.vy - (gravity * dt)) } in
    { g with bird = { b with y = max (ground_y + bird_radius) (b.y + (b.vy * dt)) } }
  else if not g.started then
    (* bobbing in place until the first flap, no pipes yet *)
    let g = { g with bird = { g.bird with y = 50. + wave (-10.) 10. 1. computer.time } } in
    if flap then begin
      Audio.play flap_sound;
      { g with started = true; bird = { g.bird with vy = flap_speed } }
    end
    else g
  else begin
    if flap then Audio.play flap_sound;
    let bird = update_bird dt flap g.bird in
    (* the camera: position-locked on the bird's x, never moving up or down *)
    let g = { g with bird; cam = g.cam |> Camera2d.look_at (bird.x - bird_screen_x) 0. } |> spawn_and_drop computer.screen in
    (* a point for each pipe whose middle the bird passed during this tick *)
    let passed = List.length (List.filter (fun p -> g.bird.x >= p.px && g.bird.x - (speed * dt) < p.px) g.pipes) in
    if passed > 0 then Audio.play Audio.coin;
    let g = { g with score = g.score +.. passed } in
    if List.exists (hits_pipe g.bird) g.pipes || g.bird.y - bird_radius <= ground_y then begin
      Audio.play Audio.hit;
      Audio.play fall_sound;
      { g with dead = true; bird = { g.bird with vy = 0. } }
    end
    else g
  end

let update_rules (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  match scenes.scene with
  | Title ->
      if flapped computer scenes then { model with scenes = Scene2d.go (Playing (new_game (flag_seed computer))) scenes }
      else { model with scenes }
  | Playing g ->
      let g = update_game computer scenes g in
      let best = max model.best g.score in
      if g.dead && g.bird.y <= ground_y + bird_radius then { model with best; scenes = Scene2d.go (Game_over g) scenes }
      else { model with best; scenes = { scenes with scene = Playing g } }
  | Game_over g ->
      (* the next game goes on from the LFSR's state: other pipes, but
       * the whole session is still a function of the first seed; after
       * a second, so that a panicked last flap doesn't restart at once *)
      if scenes.elapsed > 1. && flapped computer scenes then { model with scenes = Scene2d.go (Playing (new_game g.seed)) scenes }
      else if scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes }
      else { model with scenes }

(*****************************************************************************)
(* The juice (juice=off: none of it) *)
(*****************************************************************************)

(* Everything the juice does is here, and the rules above don't know
 * about it: [update] runs them, then [juiced] looks at what they just
 * did -- the game before and after -- and turns it into effects; the
 * view calls [flap_squash] where it draws the bird, [pop] where it
 * draws the score, and [Juice.view] around the picture. *)

(* the crash, if this update was it: the screen shaken hard and
 * flashed white, and feathers of the bird's three colors thrown from
 * where it hit (on the screen: the camera doesn't move after a crash,
 * the world stopped) *)
let crashed (g : game) (g' : game) (fx : Juice.t) : Juice.t =
  if g'.dead && not g.dead then
    let at = Camera2d.to_screen g'.cam g'.bird.x g'.bird.y in
    fx |> Juice.shake 0.8 |> Juice.flash white 12
    |> Juice.burst ~at (Juice.debris yellow)
    |> Juice.burst ~at (Juice.debris (rgb 250 250 200))
    |> Juice.burst ~at (Juice.debris orange)
  else fx

let juiced (before : scene) (model : model) : model =
  let now = Juice.now model.fx in
  match (before, model.scenes.scene) with
  | Playing g, Playing g' ->
      (* a flap: the velocity just set (a flap *sets* it, see
       * [update_bird]: exactly [flap_speed] only on the frame of one) *)
      let flapped = if g'.started && (not g'.dead) && g'.bird.vy = flap_speed then now else model.flapped in
      (* a point: the score pops, in sparks *)
      let scored, fx =
        if g'.score > g.score then (now, Juice.burst ~at:(0., 400.) Juice.sparks model.fx) else (model.scored, model.fx)
      in
      { model with flapped; scored; fx = crashed g g' fx }
  (* on the ground: the crash, if it was the ground, else a thud *)
  | Playing g, Game_over g' ->
      let fx = if g.dead then Juice.shake 0.3 model.fx else model.fx in
      { model with fx = crashed g g' fx }
  | _ -> model

let update (computer : computer) (model : model) : model =
  let model = { model with fx = Juice.step computer model.fx } in
  juiced model.scenes.scene (update_rules computer model)

(* the bird at a flap: flattened by the push, springing back past round
 * (the elastic stretch), settled in 0.3 s; the bird built around its
 * center, since it squashes against the air *)
let flap_squash (model : model) (bird : shape) : shape = Juice.stretch (Juice.squash 0.35 0.3 model.flapped model.fx) bird

(* the score's size just after a point: swollen, and settling *)
let pop (model : model) : number = Juice.tween Juice.out_back 1.5 1. 0.3 model.scored model.fx

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let sky = rgb 112 197 206
let pipe_green = rgb 115 191 46
let pipe_dark = rgb 84 128 36
let ground_color = rgb 222 216 149
let grass = rgb 115 191 46

(* The scenery repeats: a layer of copies of [piece i], one every [w]
 * pixels, only those the camera (turned into this layer's camera by
 * [parallax]) can see; [piece i] depends only on i, so the same
 * building comes back at the same place. *)
let repeated (screen : screen) (cam : Camera2d.t) (w : number) (piece : int -> shape) : shape =
  let visible = Camera2d.visible screen cam in
  let first = int_of_float (Float.floor (visible.left / w)) and last = int_of_float (Float.ceil (visible.right / w)) in
  Camera2d.view cam (List.init (last -.. first +.. 1) (fun k -> let i = first +.. k in piece i |> move_x (float_of_int i * w)))

(* [hash i]: a number from 0 to 255 that looks random, but depends only
 * on i: Knuth's multiplicative hash (TAOCP vol. 3, section 6.4), i times
 * 40503 (2^16 divided by the golden ratio), keeping the upper byte of
 * the lower 16 bits. Pure like the LFSR, but at random access: building
 * i doesn't need building i - 1. (16 bits, because a browser's OCaml
 * integers have 32 bits: the product overflows there, but its lower 16
 * bits are the same as natively.) *)
let hash (i : int) : int = ((i *.. 40503) land 0xFFFF) lsr 8

(* the city, far: a quarter of the world's speed *)
let building (i : int) : shape =
  let h = 60. + float_of_int (hash i /.. 2) in
  group
    [ rectangle (rgb 160 215 200) 70. h |> move_y (ground_y + (h / 2.));
      rectangle (rgb 190 230 215) 10. 10. |> move (-15.) (ground_y + h - 20.);
      rectangle (rgb 190 230 215) 10. 10. |> move 15. (ground_y + h - 20.) ]

let cloud (i : int) : shape =
  let y = 200. + float_of_int (hash (i +.. 1000)) in
  group [ oval white 160. 50.; oval white 90. 60. |> move 20. 20. ] |> move_y y |> fade 0.8

let view_pipe (p : pipe) : shape list =
  let top_bottom = p.gap_y + (pipe_gap / 2.) and bottom_top = p.gap_y - (pipe_gap / 2.) in
  let body = 1000. in
  [ rectangle pipe_green pipe_width body |> move p.px (top_bottom + (body / 2.));
    rectangle pipe_dark (pipe_width + 16.) 36. |> move p.px (top_bottom + 18.);
    rectangle pipe_green pipe_width body |> move p.px (bottom_top - (body / 2.));
    rectangle pipe_dark (pipe_width + 16.) 36. |> move p.px (bottom_top - 18.) ]

(* the ground's stripes scroll with the world: the eye reads the speed
 * from them *)
let stripe (_ : int) : shape = rectangle (rgb 200 190 120) 20. 30. |> move_y (ground_y - 40.) |> rotate 30.

(* the bird: a body, an eye, a beak, and a wing flapping when it goes up;
 * squashed at a flap (the juice); tilted up when rising, diving when
 * falling fast *)
let view_bird (model : model) (time : time) (b : bird) : shape =
  let wing_y = if b.vy > 0. then wave (-8.) 8. 0.15 time else 0. in
  group
    [ oval yellow 52. 40.;
      oval (rgb 250 250 200) 22. 14. |> move (-14.) wing_y;
      circle white 9. |> move 12. 8.;
      circle black 4. |> move 15. 8.;
      oval orange 22. 10. |> move 26. (-4.) ]
  |> flap_squash model
  |> rotate (clamp (-80.) 25. (b.vy / 12.))
  |> move b.x b.y

(* white text on a sky with white clouds: a dark shadow keeps it
 * readable (Flappy Bird outlines its digits in black) *)
let text (color : color) (size : number) (s : string) : shape =
  group [ words (rgb 60 60 60) s |> move 0.6 (-0.6); words color s ] |> scale size

(* the world as the camera sees it, the city and the clouds behind *)
let view_world (computer : computer) (model : model) (g : game) : shape list =
  let screen = computer.screen in
  let hitbox = if List.mem_assoc "hitboxes" computer.flags then [ circle red bird_radius |> fade 0.5 |> move g.bird.x g.bird.y ] else [] in
  [ repeated screen (Camera2d.parallax 0.1 g.cam) 400. cloud;
    repeated screen (Camera2d.parallax 0.25 g.cam) 90. building;
    Camera2d.view g.cam (List.concat_map view_pipe g.pipes);
    (* the ground, in front of the pipes: glued to the screen, only its
     * stripes scrolling; 100 pixels past the screen's sides and bottom,
     * so that a shake (the juice) doesn't show the sky under it *)
    rectangle ground_color (screen.width + 200.) (ground_y - screen.bottom + 100.)
    |> move_y ((ground_y + screen.bottom - 100.) / 2.);
    rectangle grass (screen.width + 200.) 16. |> move_y (ground_y - 8.);
    repeated screen g.cam 60. stripe;
    Camera2d.view g.cam (view_bird model computer.time g.bird :: hitbox) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let scenes = model.scenes in
  rectangle sky screen.width screen.height
  (* the sky still, everything else shaken (the juice) *)
  :: Juice.view model.fx
       (match scenes.scene with
       | Title ->
           (* the world of a game not started, the bird bobbing *)
           let g = new_game default_seed in
           view_world computer model { g with bird = { g.bird with y = 50. + wave (-10.) 10. 1. computer.time } }
           @ [ text white 5. "TINY FLAPPY BIRD" |> move_y 300.;
               text white 2.5 "space, up or click: flap" |> move_y 200. ]
           @ Scene2d.blink 1. scenes [ text white 3. "PRESS SPACE" |> move_y (-200.) ]
       | Playing g ->
           view_world computer model g
           @ [ text white 8. (string_of_int g.score) |> scale (pop model) |> move_y 380. ]
           @ if g.started then [] else [ text white 3. "GET READY" |> move_y 200. ]
       | Game_over g ->
           (* Flappy Bird's medals: bronze from 10 points, silver from 20,
            * gold from 30, platinum from 40 *)
           let medal =
             if g.score >= 40 then [ circle (rgb 229 228 226) 30. ]
             else if g.score >= 30 then [ circle (rgb 255 215 0) 30. ]
             else if g.score >= 20 then [ circle (rgb 192 192 192) 30. ]
             else if g.score >= 10 then [ circle (rgb 205 127 50) 30. ]
             else []
           in
           view_world computer model g
           @ [ text orange 6. "GAME OVER" |> move_y 280.;
               rectangle (rgb 222 216 149) 400. 180. |> move_y 80.;
               text black 3. (Printf.sprintf "SCORE %d" g.score) |> move 30. 120.;
               text black 3. (Printf.sprintf "BEST %d" model.best) |> move 30. 50. ]
           @ List.map (move (-130.) 85.) medal
           @ if scenes.elapsed > 1. then Scene2d.blink 1. scenes [ text white 3. "PRESS SPACE" |> move_y (-150.) ] else [])

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
