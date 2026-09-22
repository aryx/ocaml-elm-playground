(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A little Super Mario Bros. (Shigeru Miyamoto, Nintendo, 1985). Not the
 * first platformer -- Space Panic (Universal, 1980) had ladders, Donkey
 * Kong (1981) jumping and Mario himself, Jump Bug (1981) scrolling --
 * but the one that defined the side-scrolling kind for a decade, and
 * the pieces used here: '?' blocks, coins, pits, the flag at the end.
 *
 * A level bigger than the screen, typed as
 * strings (see Tilemap), seen through a camera following the player (see
 * Camera2d), with parallax hills and clouds behind, and a HUD in front.
 * Arrows to run, up to jump; take the coins ('$'), bump the '?' blocks
 * from below, reach the flag ('F'). The player moves against the tiles
 * one pixel at a time, with the platformer kit's Tile_move
 * (gamekits/platformer/, with TinyLodeRunner and TinyRick).
 *
 * Two flags (see Playground.flags) to compare the ways a camera can
 * follow the player (see Camera2d.mli), and to see more of the level:
 *
 *   dune exec games/platform/TinyMario.exe -- camera=lock zoom=0.5
 *   http://localhost:8001/games/platform/web/TinyMario.html?camera=lerp
 *
 * camera= is window (the default), lock, or lerp.
 *
 * A third flag chooses how the hero is drawn: artwork=shapes for a red
 * square instead of the pixel art (Sprite.mli).
 *
 * A fourth chooses the physics engine: physics=engine for the
 * playground's (playground/Physics.mli), the dumb one, this file's own
 * two lines of arithmetic, by default; see [fall] below.
 *
 * Sounds (playground/Audio.mli), played in [update] when things happen:
 * a jump, the steps (a foot lands every 30 pixels of the walk cycle),
 * the coins, a fall into a pit, and an arpeggio at the flag (a plain
 * C major chord, up: not Nintendo's fanfare).
 *
 * And music, looping, in ABC notation (audio/Abc.mli): by default an
 * original tune in the NES's style, [original_tune] below (Koji
 * Kondo's famous theme is Nintendo's, not ours to copy); the flag
 * music= plays your own tune instead, from a file or a URL (a MIDI file
 * if it ends in .mid, an ABC one in .abc, else solfège: see
 * Audio.loop_from; in a browser, a plain name is fetched from the
 * page's server), music=off none:
 *
 *   dune exec games/platform/TinyMario.exe -- music=mytune.abc
 *   dune exec games/platform/TinyMario.exe -- music=https://example.com/song.mid
 *   http://localhost:8001/games/platform/web/TinyMario.html?music=song.mid *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The level *)
(*****************************************************************************)

(* mario_level.xpm, beside this file: 60 cells by 25, one character
 * each, drawn in the map editor (apps/gamedev/TinyTiled) or in any
 * text editor, and embedded by dune (Mario_xpm, see the dune file).
 * '#' ground, 'B' a brick, '?' a block with a coin inside ('X' once
 * bumped), '$' a coin, 'F' the flag, '@' where the player starts:
 *
 *      $  $                   #                    #
 *  @         #       $$     # #       $            #      F
 *  ####################  ###########   ######   ###############
 *)
let level = Tilemap.of_xpm 50. Mario_xpm.level

let solid (c : char) : bool = c = '#' || c = 'B' || c = '?' || c = 'X'

let tile (c : char) : shape =
  match c with
  | '#' -> group [ square (rgb 120 60 20) 50.; square (rgb 160 90 40) 42. ]
  | 'B' -> group [ square (rgb 180 80 30) 50.; rectangle (rgb 90 40 10) 50. 4. ]
  | '?' -> group [ square (rgb 240 180 30) 50.; words black "?" |> scale 3. ]
  | 'X' -> square (rgb 130 100 60) 50.
  | '$' -> oval yellow 20. 30.
  | 'F' ->
      group
        [ rectangle darkGray 6. 150. |> move_y 50.;
          triangle green 30. |> rotate (-90.) |> move 20. 100. ]
  | _ -> group []

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the player is a 40x40 box, smaller than a tile, so it fits in a
 * one-tile gap *)
let player_size = 40.

type model = {
  map : Tilemap.t; (* changes when a coin is taken, a block bumped *)
  x : number;
  y : number;
  vy : number;
  (* for the sprite: running, and which way he faces *)
  vx : number;
  facing_left : bool;
  coins : int;
  won : bool;
  cam : Camera2d.t;
  (* the music started (at the first frame, when the flags are known) *)
  music_on : bool;
}

let start : number * number =
  match Tilemap.find level '@' with
  | (col, row) :: _ -> Tilemap.center level col row
  | [] -> (0., 0.)

let initial_model =
  let x, y = start in
  let col, row = Tilemap.cell level x y in
  { map = Tilemap.set level col row ' ';
    x; y; vy = 0.; vx = 0.; facing_left = false; coins = 0; won = false; music_on = false;
    cam = Camera2d.origin |> Camera2d.look_at x y }

(*****************************************************************************)
(* Moving against the tiles *)
(*****************************************************************************)

(* the player's box against the solid tiles, one pixel at a time: see
 * gamekits/platformer/Tile_move.mli (move_by once for x, then once for y,
 * so that running into a wall while falling stops only the running) *)
let blocked (map : Tilemap.t) (x : number) (y : number) : bool = Tile_move.hits solid map (player_size, player_size) x y
let move_by (map : Tilemap.t) (x, y) (dx, dy) : (number * number) * bool = Tile_move.move_by solid map (player_size, player_size) (x, y) (dx, dy)

(* the tile over the player's head, bumped from below: a '?' gives a coin *)
let bump (model : model) : model =
  let col, row = Tilemap.cell model.map model.x (model.y + (player_size / 2.) + 1.) in
  match Tilemap.get model.map col row with
  | Some '?' -> { model with map = Tilemap.set model.map col row 'X'; coins = succ model.coins }
  | _ -> model

(* the tile the player's center is in: a coin, the flag *)
let touch (model : model) : model =
  let col, row = Tilemap.cell model.map model.x model.y in
  match Tilemap.get model.map col row with
  | Some '$' -> { model with map = Tilemap.set model.map col row ' '; coins = succ model.coins }
  | Some 'F' -> { model with won = true }
  | _ -> model

(*****************************************************************************)
(* The camera *)
(*****************************************************************************)

let flag (computer : computer) (name : string) : string option = List.assoc_opt name computer.flags

(* see Camera2d.mli for these three ways, and the camera= flag above *)
let move_camera (computer : computer) (model : model) : Camera2d.t =
  let zoom = Option.value (Option.bind (flag computer "zoom") float_of_string_opt) ~default:1. in
  let cam = { model.cam with zoom } in
  let cam =
    match flag computer "camera" with
    | Some "lock" -> Camera2d.look_at model.x model.y cam
    | Some "lerp" -> Camera2d.follow 0.1 model.x model.y cam
    | _ -> Camera2d.window 200. 300. model.x model.y cam
  in
  Camera2d.clamp computer.screen (Tilemap.bounds model.map) cam

(*****************************************************************************)
(* Audio *)
(*****************************************************************************)

(* at the flag: C E G up, then the C above, held (a plain C major
 * arpeggio, not Nintendo's fanfare) *)
let flag_arpeggio =
  Audio.after
    (List.map (fun n -> Audio.square (Music.frequency n) |> Audio.lasting 0.12) [ "C5"; "E5"; "G5" ]
    @ [ Audio.square (Music.frequency "C6") |> Audio.lasting 0.5 |> Audio.fading ])

(* the sounds of the things that happened between [before] and [after] *)
let sounds (on_ground : bool) (jumped : bool) (before : model) (after : model) : unit =
  if jumped then Audio.play Audio.jump;
  (* a foot lands every 30 pixels: two of the walk cycle's four poses *)
  if on_ground && (not jumped) && Float.floor (after.x / 30.) <> Float.floor (before.x / 30.) then Audio.play Audio.step;
  if after.coins > before.coins then Audio.play Audio.coin;
  if after.won && not before.won then (
    Audio.stop "music";
    Audio.play flag_arpeggio)

(* The music: an original tune, 8 bars, the melody on the arpeggios of
 * its chords (C, F, Dm G, C; Dm, G, F G, C), a triangle bass under it,
 * like an NES game's (Music.to_sound) *)
let original_tune =
  {|X:1
T:Tiny Plumber (original, for TinyMario)
L:1/8
Q:1/4=144
K:C
V:1
E2 G2 c2 G2 | A2 c2 e4 | d2 c2 A2 G2 | E4 z4 |
F2 A2 d2 A2 | G2 B2 d4 | c2 B2 A2 B2 | c4 z4 |
V:2
C,2 G,2 E,2 G,2 | F,2 A,2 C2 A,2 | F,2 A,2 D,2 G,2 | C,2 G,2 C,4 |
D,2 A,2 F,2 A,2 | G,,2 D,2 G,2 D,2 | F,2 G,2 F,2 G,2 | C,2 G,2 C,4 |
|}

(* the music= flag's tune (a file or a URL, see Audio.loop_from), or
 * the original *)
let start_music (computer : computer) : unit =
  match flag computer "music" with
  | Some "off" -> ()
  | Some source -> Audio.loop_from "music" source
  | None -> Audio.loop "music" (Audio.abc original_tune)

(* fell in a pit *)
let fall_sound = Audio.square 700. |> Audio.sliding 120. |> Audio.lasting 0.6 |> Audio.fading

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* The vertical speed after a frame of falling, in pixels per frame,
 * with either physics engine (the physics=engine flag):
 *
 *  - the dumb engine: 0.8 pixels per frame less at every frame;
 *  - the physics engine: the same as a body, in seconds: falling at
 *    0.8 * 60^2 = 2880 pixels per second, per second (Physics.fall),
 *    for one tick (Physics.step).
 *
 * The same numbers, because the dumb engine is semi-implicit Euler too
 * (the speed changed first, then the position moved with it, below, by
 * move_by); the engine only computes the speed here: moving through the
 * tiles stays move_by's job, until the physics plan's collisions (see
 * docs/claude_notes/plan_physics_teaching.md). Both then cap the fall
 * at 15 pixels per frame, a terminal speed (Physics.slow would give one
 * too, but would also slow the jump on its way up). *)
let fall (computer : computer) (vy : number) : number =
  match flag computer "physics" with
  | Some "engine" ->
      let body = Physics.body (square white 1.) |> Physics.moving 0. (vy * 60.) |> Physics.fall 2880. |> Physics.step in
      body.vy / 60.
  | _ -> vy - 0.8

let update (computer : computer) (model : model) : model =
  let model = if model.music_on then model else (start_music computer; { model with music_on = true }) in
  let before = model in
  let on_ground = blocked model.map model.x (model.y - 1.) in
  let vx = 6. * to_x computer.keyboard in
  let jumped = on_ground && computer.keyboard.kup in
  let vy =
    if jumped then 19.
    else max (-15.) (fall computer model.vy) (* gravity, and a terminal speed *)
  in
  let (x, _), _ = move_by model.map (model.x, model.y) (vx, 0.) in
  let (x, y), hit = move_by model.map (x, model.y) (0., vy) in
  let facing_left = if vx < 0. then true else if vx > 0. then false else model.facing_left in
  let model = { model with x; y; vy = (if hit then 0. else vy); vx; facing_left } in
  let model = if hit && vy > 0. then bump model else model in
  let model = touch model in
  sounds on_ground jumped before model;
  (* fallen in a pit: back to the start *)
  let model =
    if model.y < (Tilemap.bounds model.map).bottom - 200. then (
      Audio.play fall_sound;
      { model with x = fst start; y = snd start; vy = 0. })
    else model
  in
  { model with cam = move_camera computer model }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* Super Mario Bros. ran on sprites, so that is what this draws: a
 * little plumber in pixel art (see Sprite), animated. With
 * artwork=shapes he is a red square instead -- the simplest code, and
 * all a game needs to be played. *)
let use_sprites (computer : computer) = Sprite.artwork ~default:true computer.flags

(* Our hero, 10x10 pixels of 4 (the player's 40x40 box), facing right:
 * 'R' the cap and shirt, 'S' the skin, 'B' the overalls, 'K' the hair
 * and shoes. The poses are XPM files beside this one (mario_walk1.xpm,
 * ...), which a sprite editor writes and dune embeds (Mario_xpm), e.g.
 * walking:
 *
 *     ...RRRR...
 *     ..RRRRRRR.
 *     ..KKSSKS..
 *     .KSKSSSKS.
 *     ..SSSSSS..
 *     ..RRBRR...
 *     .RRRBBRRR.
 *     .SSBBBBSS.
 *     .BBB..BBB.
 *     KKK....KKK
 *)
let palette, stand = Sprite.of_xpm Mario_xpm.stand
let walk1 = snd (Sprite.of_xpm Mario_xpm.walk1)
let walk2 = snd (Sprite.of_xpm Mario_xpm.walk2)
let jump = snd (Sprite.of_xpm Mario_xpm.jump)

(* each pose drawn once, facing right, and mirrored (Sprite.flip) *)
let poses : (string list * (shape * shape)) list =
  List.map (fun rows -> (rows, (Sprite.pixels 4. palette rows, Sprite.pixels 4. palette (Sprite.flip rows)))) [ stand; walk1; walk2; jump ]

(* In the air: jumping. Running: the walk cycle, one pose every 15
 * pixels -- driven by the distance run, not by time, so the legs move
 * as fast as he goes, and stop when he stops. *)
let hero (computer : computer) (model : model) : shape =
  if not (use_sprites computer) then square red player_size
  else
    let on_ground = blocked model.map model.x (model.y - 1.) in
    let rows =
      if not on_ground then jump
      else if model.vx <> 0. then Sprite.cycle (int_of_float (Float.abs model.x / 15.)) [ walk1; stand; walk2; stand ]
      else stand
    in
    let right, left = List.assoc rows poses in
    if model.facing_left then left else right

let hills = List.init 8 (fun i -> oval (rgb 90 170 80) 500. 300. |> move ((float_of_int i * 400.) - 1400.) (-450.))
let clouds = List.init 8 (fun i -> oval white 160. 60. |> move ((float_of_int i * 350.) - 1300.) (250. + (float_of_int (i mod 3) * 60.)))

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let cam = model.cam in
  let world =
    [ Tilemap.view_visible (Camera2d.visible screen cam) tile model.map;
      hero computer model |> move model.x model.y ]
  in
  let hud =
    [ words black (Printf.sprintf "coins: %d" model.coins) |> scale 3. |> move (screen.left + 120.) (screen.top - 40.);
      words black ("camera: " ^ Option.value (flag computer "camera") ~default:"window")
      |> scale 2. |> move (screen.right - 150.) (screen.top - 40.) ]
    @ if model.won then [ words red "You win!" |> scale 8. ] else []
  in
  (* the sky, glued to the screen, then layers further and further *)
  [ rectangle (rgb 174 238 238) screen.width screen.height;
    Camera2d.view (Camera2d.parallax 0.3 cam) hills;
    Camera2d.view (Camera2d.parallax 0.6 cam) clouds;
    Camera2d.view cam world ]
  @ hud

let app = game view update initial_model

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
