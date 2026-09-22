(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Guitar Hero (Harmonix, 2005): notes come down a
 * highway towards you, and you play each one as it reaches the line --
 * holding its fret, and strumming, on the beat.
 *
 *   a s d f g  the five frets, green to orange
 *   space      strum
 *   up/down    on the title, the difficulty
 *   - =        the calibration, 10 ms at a time
 *
 * The second of this repository's three rhythm games, and each adds one
 * idea to the one before: TinyDDR.ml is the music's clock (the
 * steps judged by what you hear, not by the frames); this one is the
 * instrument; TinyRockBand.ml is the band. What this one adds:
 *
 *  - An instrument is a way of pressing. A note is two hands: the left
 *    holds the fret, the right strums, and only the strum, on the beat,
 *    plays -- the fret alone does nothing (Rhythm.strummed). Easy to
 *    say; it is what makes a plastic guitar feel like playing rather
 *    than pressing buttons.
 *  - A note can last. The tune knows every note's length, and a long
 *    one held -- the fret kept down after the strum -- goes on scoring
 *    for as long as it lasts (Rhythm.sustaining). DDR's "freeze arrow"
 *    exercise, made core.
 *  - A difficulty is the same song, reduced (Rhythm.reduce): Expert is
 *    the part as the tune has it; Hard keeps each chord's outline;
 *    Medium and Easy one note of it, on four frets and then three.
 *    Never a different song: an easy part is still the same part.
 *  - You hear what you play. The song runs with the guitar muted
 *    (Rhythm.muted); a note hit sounds when it is strummed, in the
 *    guitar's own sound (Rhythm.struck), and a note missed is silence.
 *    Guitar Hero mutes its guitar track on a miss for the same reason:
 *    the ear judges before the score does.
 *
 * The highway -- the trick of this game -- is TinyOutRun.ml's
 * road, straightened. A flat strip seen from above and behind: a point
 * z ahead of the line is drawn at
 *
 *     x' = x f / (z + near)     y' = horizon - h f / (z + near)
 *
 * one division, no camera, no matrix ([project]). The lanes, straight
 * on the ground, become lines meeting at the horizon, and a gem far
 * away is drawn small and high, near and big at the bottom:
 *
 *                     horizon  . . . . . .
 *                             /  |  |  |  \
 *                            /   |  |  |   \      far: small, high
 *                           /    o  |  |    \
 *                          /     |  |  |     \
 *                         /  o   |  |  o      \   near: big, low
 *                        [G] [R] [Y] [B] [O]      the line
 *
 * A road that cannot bend still has to *move*: as Out Run's rumble
 * strips do, the lines across it do -- one on every beat, sliding down
 * from the horizon at the song's own tempo ([beat_lines]), so the
 * highway visibly carries the music towards you.
 *
 * What it uses: gamekits/rhythm (the music's clock and calibration, the
 * grades, the performance, the chart from a voice on frets, the
 * difficulty reduction, the strum, the sustains -- all of them shared
 * with TinyRockBand), Audio (the band, a note per hit, Audio.position,
 * Audio.of_tune), audio's ABC
 * percussion (the backing band's drummer: a clef=perc voice, played
 * with real drum sounds), Scene2d. No Camera2d: the road is drawn by
 * [project] alone.
 *
 * Exercises: star power (a streak charges it; spent, it doubles the
 * score -- a number in the model, as TinyHades' boons are), the exact
 * frets (Rock Band's rule: an extra fret held at the strum is a wrong
 * chord), hammer-ons and pull-offs (a note right after another on a
 * higher or lower fret, played by the fret alone, no strum), and the
 * highway bending like Out Run's road -- which would take nothing but
 * TinyOutRun's curve, and a sustain let go of cut short (here a hit
 * note always sounds its full length).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The song *)
(*****************************************************************************)

(* An original tune, 132 beats a minute: two bars of count-in (the
 * hi-hat alone), then a riff with power chords over bass and drums.
 * The guitar is voice 1; the drums, voice 3, are percussion. *)
let tune_text =
  {|X:1
T:Tiny Riff
L:1/8
Q:1/4=132
K:Em
V:1
z8 | z8 |
E2 G2 A2 [EB]2 | G2 A2 [DA]4 | E2 G2 A2 B2 | [DA]2 G2 E4 |
E2 G2 A2 [EB]2 | c2 B2 [GD]4 | A2 G2 E2 D2 | [EB]4 z4 |
V:2
z8 | z8 |
E,4 E,4 | C,4 D,4 | E,4 E,4 | D,4 E,4 |
E,4 E,4 | C,4 G,,4 | A,,4 B,,4 | E,8 |
V:3 clef=perc
^F,,2 ^F,,2 ^F,,2 ^F,,2 | ^F,,2 ^F,,2 ^F,,2 ^F,,2 |
C,,2 ^F,,2 D,,2 ^F,,2 | C,,2 ^F,,2 D,,2 ^F,,2 | C,,2 ^F,,2 D,,2 ^F,,2 | C,,2 C,,2 D,,4 |
C,,2 ^F,,2 D,,2 ^F,,2 | C,,2 ^F,,2 D,,2 ^F,,2 | C,,2 ^F,,2 D,,2 ^F,,2 | C,,2 D,,2 C,,4 |
|}

let tune : Abc.tune = match Abc.parse tune_text with Ok t -> t | Error e -> failwith ("TinyGuitarHero's tune: " ^ e)
(* the backing band: the song with the guitar, the part you play,
 * muted *)
let band : Audio.sound = Audio.of_tune (Rhythm.muted 0 tune)
let song_length : number = Abc.duration tune
let beat : number = 60. / 132.

(* the guitar's part, at a difficulty *)
let part (level : Rhythm.difficulty) : int Rhythm.note list = Rhythm.reduce level (Rhythm.on_frets tune 0)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  level : Rhythm.difficulty;
  perf : int Rhythm.performance;
  sustain : int; (* the points held notes have earned, on top of the kit's score *)
  rock : number; (* the rock meter, 0 to 1: empty, and the song stops *)
}

type scene = Title of Rhythm.difficulty | Playing of play | Over of play * bool (* the song finished *)
type model = scene Scene2d.t

let start (level : Rhythm.difficulty) (offset : number) (started : number) : play =
  { level; perf = Rhythm.start ~offset ~started (part level); sustain = 0; rock = 0.5 }

let initial_model : model = Scene2d.start (Title Rhythm.Medium)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let fret_keys = [ "a"; "s"; "d"; "f"; "g" ]

let held (k : keyboard) : int list =
  List.concat (List.mapi (fun i key -> if Set_.mem key k.keys then [ i ] else []) fret_keys)

let hits (p : int Rhythm.performance) : int * int =
  List.fold_left
    (fun (h, m) (_, j) -> match j with Some Rhythm.Miss -> (h, m +.. 1) | Some _ -> (h +.. 1, m) | None -> (h, m))
    (0, 0) p.judged

(* one frame of playing, given the song's time and the hands: the pure
 * part, which the tests call directly *)
let play_step (now : number) ~(strum : bool) ~(held : int list) (p : play) : play =
  let perf = Rhythm.play now (Rhythm.strummed ~strum ~held) p.perf in
  let h0, m0 = hits p.perf and h1, m1 = hits perf in
  { p with
    perf;
    sustain = p.sustain +.. Rhythm.sustaining now held perf;
    rock = Float.max 0. (Float.min 1. (p.rock + (0.03 * float_of_int (h1 -.. h0)) - (0.08 * float_of_int (m1 -.. m0)))) }

let finished (p : play) : bool = p.perf.now > song_length + 0.5

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let key f = Scene2d.pressed f scenes in
  let down k = key (fun kb -> Set_.mem k kb.keys) in
  match scenes.scene with
  | Title level ->
      let pos = let rec find i = function [] -> 1 | l :: r -> if l = level then i else find (i +.. 1) r in find 0 Rhythm.difficulties in
      let pos = pos +.. (if key (fun k -> k.kdown) then 1 else 0) -.. if key (fun k -> k.kup) then 1 else 0 in
      let level = List.nth Rhythm.difficulties (max 0 (min 3 pos)) in
      if key (fun k -> k.kspace) then begin
        Audio.stop "guitar";
        Scene2d.go (Playing (start level 0.05 (match computer.time with Time t -> t))) scenes
      end
      else { scenes with scene = Title level }
  | Over (p, _) ->
      if key (fun k -> k.kspace) then begin
        Audio.stop "guitar";
        Scene2d.go (Title p.level) scenes
      end
      else scenes
  | Playing p ->
      Audio.loop "guitar" band;
      let offset = p.perf.offset + (if down "=" then 0.01 else 0.) - if down "-" then 0.01 else 0. in
      let now = Rhythm.song_time ~position:(Option.value ~default:0. (Audio.position "guitar")) ~offset in
      let before = p in
      let p = play_step now ~strum:(key (fun k -> k.kspace)) ~held:(held computer.keyboard) { p with perf = { p.perf with offset } } in
      (* the notes just hit, heard; the ones missed never are *)
      List.iter (fun at -> Audio.play (Audio.of_tune (Rhythm.struck tune 0 at))) (Rhythm.newly_hit before.perf p.perf);
      if p.rock <= 0. then begin
        Audio.stop "guitar";
        Scene2d.go (Over (p, false)) scenes
      end
      else if finished p then begin
        Audio.stop "guitar";
        Scene2d.go (Over (p, true)) scenes
      end
      else { scenes with scene = Playing p }

(*****************************************************************************)
(* The highway -- the trick of this game, in 23 lines (see the header) *)
(*****************************************************************************)

let horizon = 330. (* where the road meets the sky, on the screen *)
let focal = 200. (* pixels a road unit is wide at depth 1 *)
let height = 6.3 (* the eye above the road, in road units *)
let near = 2. (* the line's depth: 630 px below the horizon, 500 px wide *)
let lane_w = 1. (* the lanes' width on the ground, in the road's units *)
let speed = 4. (* road units a second: a note is on the road ~3 s ahead *)
let far_z = 14.

(* a point of the road, [x] across (0 the middle) and [z] ahead of the
 * line, on the screen, and how many pixels a road unit is there: Out
 * Run's one division, on a road that does not bend *)
let project (x : number) (z : number) : number * number * number =
  let s = focal / (z + near) in
  (x * s, horizon - (height * s), s)

(* the lines across the road on every beat, sliding down it at the
 * song's own tempo: all that makes a straight road move *)
let beat_lines (now : number) : shape list =
  List.filter_map
    (fun k ->
      let t = (Float.floor (now / beat) + float_of_int k) * beat in
      let z = (t - now) * speed in
      if z < 0. || z > far_z then None
      else
        let x0, y, _ = project (-2.5 * lane_w) z and x1, _, _ = project (2.5 * lane_w) z in
        Some (rectangle (rgb 90 80 120) (x1 - x0) (Float.max 1. (focal * 0.03 / (z + near))) |> move 0. y))
    (List.init 40 Fun.id)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let fret_color = function 0 -> rgb 60 200 90 | 1 -> rgb 230 60 60 | 2 -> rgb 240 220 60 | 3 -> rgb 60 130 240 | _ -> rgb 250 150 40
let lane_x (lane : int) : number = (float_of_int lane - 2.) * lane_w

let road : shape list =
  let corner x z = let sx, sy, _ = project x z in (sx, sy) in
  [ polygon (rgb 26 22 38) [ corner (-2.5) 0.; corner 2.5 0.; corner 2.5 far_z; corner (-2.5) far_z ] ]
  @ List.init 6 (fun k ->
        let x = (float_of_int k - 2.5) * lane_w in
        let x0, y0 = corner x 0. and x1, y1 = corner x far_z in
        let len = Float.hypot (x1 - x0) (y1 - y0) in
        rectangle (rgb 70 64 92) 2. len |> rotate (Float.atan2 (x1 - x0) (y1 - y0) * -180. / Float.pi) |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.))

(* a note on the road: its gem where it is, drawn at its distance's
 * size, and a long note's tail running back up the road *)
let note_shapes (now : number) ((n : int Rhythm.note), j) : shape list =
  let z = (n.at - now) * speed in
  let tail_end = (n.at + n.length - now) * speed in
  if z > far_z || tail_end < -0.5 || j = Some Rhythm.Miss && z < -0.3 then []
  else
    let tail =
      if n.length < Rhythm.sustain_min then []
      else
        let z0 = Float.max 0. z and z1 = Float.min far_z tail_end in
        if z1 <= z0 then []
        else
          let a, ya, sa = project (lane_x n.lane) z0 and b, yb, sb = project (lane_x n.lane) z1 in
          [ polygon (fret_color n.lane) [ (a - (0.1 * sa), ya); (a + (0.1 * sa), ya); (b + (0.1 * sb), yb); (b - (0.1 * sb), yb) ] |> fade 0.7 ]
    in
    let head =
      match j with
      | Some _ when j <> Some Rhythm.Miss -> []
      | _ ->
          if z < -0.3 then []
          else
            let x, y, s = project (lane_x n.lane) z in
            [ oval (fret_color n.lane) (0.7 * s) (0.36 * s) |> move x y;
              oval white (0.3 * s) (0.12 * s) |> fade 0.6 |> move x (y + (0.04 * s)) ]
    in
    tail @ head

let judgement_color = function
  | Rhythm.Perfect -> rgb 250 240 120 | Great -> rgb 120 230 140 | Good -> rgb 120 180 250 | Almost -> rgb 200 160 230
  | Miss -> rgb 230 80 80

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  let now = p.perf.now in
  let frets_held = held computer.keyboard in
  let frets = Rhythm.frets_at p.level in
  [ rectangle (rgb 10 8 18) screen.width screen.height ]
  @ road @ beat_lines now
  @ List.concat_map (note_shapes now) (List.rev p.perf.judged)
  @ List.init 5 (fun lane ->
        let x, y, s = project (lane_x lane) 0. in
        let on = lane < frets in
        circle (if List.mem lane frets_held then white else if on then fret_color lane else rgb 50 46 62) (0.36 * s)
        |> fade (if on then 1. else 0.4) |> move x y)
  @ (match p.perf.last with
    | Some (j, at) when now - at < 0.5 -> [ text (judgement_color j) 3.5 (Rhythm.name j) |> move_y 90. ]
    | _ -> [])
  @ [ text white 2.4 (Printf.sprintf "%d" (p.perf.score +.. p.sustain)) |> move 380. (screen.top - 40.);
      text white 1.9 (if p.perf.combo >= 3 then Printf.sprintf "%d in a row" p.perf.combo else "") |> move_y 50.;
      rectangle (rgb 40 36 50) 16. 260. |> move (-440.) 0.;
      rectangle (if p.rock < 0.25 then rgb 230 80 80 else rgb 120 230 140) 16. (260. * p.rock) |> move (-440.) (-130. + (130. * p.rock));
      text (rgb 150 140 170) 1.5 "rock" |> move (-440.) (-150.);
      text (rgb 150 140 170) 1.6
        (Printf.sprintf "%s    frets a s d f g, strum space    calibration %+.0f ms (- =)" (Rhythm.difficulty_name p.level)
           (p.perf.offset * 1000.))
      |> move_y (screen.bottom + 25.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let back = rectangle (rgb 10 8 18) screen.width screen.height in
  match model.scene with
  | Title level ->
      [ back ] @ road
      @ [ text (rgb 240 220 60) 6. "TINY GUITAR HERO" |> move_y 400.;
          text white 2. "hold the fret (a s d f g), strum (space) on the beat" |> move_y 300.;
          text (rgb 200 190 220) 2. "up/down: the difficulty -- the same song, reduced" |> move_y 260. ]
      @ List.mapi
          (fun i l ->
            text (if l = level then white else rgb 110 100 130) (if l = level then 2.6 else 2.)
              (Printf.sprintf "%s   %d frets, %s" (Rhythm.difficulty_name l) (Rhythm.frets_at l)
                 (match Rhythm.chord_at l with 1 -> "one note a chord" | 2 -> "chords as two notes" | _ -> "every note"))
            |> move_y (150. - (float_of_int i * 40.)))
          Rhythm.difficulties
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-420.) ]
  | Playing p -> view_play computer p
  | Over (p, finished) ->
      let count j = List.length (List.filter (fun (_, j') -> j' = Some j) p.perf.judged) in
      [ back;
        text (if finished then rgb 120 230 140 else rgb 230 80 80) 5. (if finished then "YOU ROCK" else "BOOED OFF") |> move_y 250.;
        text white 2.4
          (Printf.sprintf "%s    score %d    held notes %d" (Rhythm.difficulty_name p.level) (p.perf.score +.. p.sustain) p.sustain)
        |> move_y 170. ]
      @ List.mapi
          (fun k j -> text (judgement_color j) 2.2 (Printf.sprintf "%-8s %3d" (Rhythm.name j) (count j)) |> move_y (100. - (float_of_int k * 36.)))
          [ Rhythm.Perfect; Great; Good; Almost; Miss ]
      @ [ text (rgb 170 160 200) 2.
            (match Rhythm.average_error p.perf with
             | Some e when Float.abs e > 0.012 ->
                 Printf.sprintf "on average %.0f ms %s: set the calibration to %+.0f ms" (Float.abs e * 1000.)
                   (if e > 0. then "late" else "early") ((p.perf.offset + e) * 1000.)
             | Some _ -> "on the beat, on average"
             | None -> "")
          |> move_y (-110.) ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-330.) ]

let help =
  {|TinyGuitarHero
  a s d f g  the five frets    space  strum
  up/down    on the title, the difficulty
  - =        the calibration, 10 ms at a time
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
