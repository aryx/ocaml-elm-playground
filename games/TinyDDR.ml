(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Dance Dance Revolution (Konami, 1998): arrows rise
 * to the top of the screen in time with the music, and you press the
 * same arrow as each one reaches its outline -- on the beat.
 *
 *   arrows   the four steps
 *   - =      the calibration: your machine's latency, 10 ms at a time
 *   space    start, and again after the song
 *
 * DDR put a dance mat in front of the arcade cabinet, and before it
 * PaRappa the Rapper (1996) had made a game of pressing buttons in
 * time; Guitar Hero (2005) is the same game with a plastic guitar. What
 * the genre is *about*, underneath, is a question no other game in
 * this directory has had to ask: what time is it?
 *
 * Every game here runs on computer.time, advanced once a frame. But the
 * player of a rhythm game is not watching the frames, they are
 * listening to the music, and the music runs on another clock -- the
 * sound card's, 44,100 samples a second, which never waits for a late
 * frame (it would click). The two drift apart, and the game has to pick
 * one:
 *
 *     frame clock   |------|------|---------|------|------|  a late
 *                                     ^ one frame late          frame
 *     music clock   ||||||||||||||||||||||||||||||||||||||||  never late
 *                                     ^ the player hears this
 *
 * So here the steps are judged, and the arrows placed on the screen, by
 * the music's clock: Audio.position "song", how far into the song the
 * sound card has been fed ([song_time]). The flag clock=frame judges by
 * the frame clock instead, for comparison.
 *
 * And even the music's clock is early. It counts what was *given* to
 * the sound card, which plays it a little later -- natively this
 * playground keeps its queue ~50 ms ahead, a browser ~100 ms, a
 * Bluetooth headset far more -- and that gap belongs to the machine,
 * not the program. Which is why every rhythm game has a calibration
 * setting ([offset], the - and = keys), and why the results screen
 * here tells you your average error ([average_error]): a player who
 * dances steadily to what they hear is early or late by exactly the
 * latency, so the average *is* the calibration to set. The game
 * measures its own machine through its player.
 *
 * The chart -- which arrow, when -- is not typed in. It is computed
 * from the song's own melody ([chart]): a step on every note, its time
 * the note's start in the tune (audio/Abc.mli parses them), its arrow
 * following the melody's shape: up for a step up in pitch, down for a
 * step down, a leap is a sideways arrow, a repeated note the same arrow
 * again. So the arrows land exactly where the notes are, and the song
 * is danced by its tune. (DDR's own charts are written by hand, by
 * people who dance; auto-charting from the notes is what the free
 * clones, StepMania among them, offered for songs nobody had charted.)
 *
 * What it uses: the playground's Audio (the song, played as a loop,
 * and its clock -- Audio.position, added for this game: the mixer
 * counts, for each loop, the samples it has sent), audio/Abc (the
 * song's notes and their times, for the chart), Scene2d. The tune is
 * original, in ABC, a few lines at the top of the file.
 *
 * Exercises: freeze arrows (hold the step for the length of the note:
 * the tune already knows how long each is); jumps (two arrows at once,
 * from the tune's chords); a second difficulty, the chart made from the
 * bass line instead; a calibration screen of its own (a click every
 * beat, and the player taps along); the arrows' speed as a setting,
 * which DDR calls a speed mod and players swear by.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The song, and the steps in it *)
(*****************************************************************************)

(* An original tune, 128 beats a minute: two bars of nothing so that the
 * first arrows have time to rise, then eight bars of melody over a
 * bass. *)
let tune_text =
  {|X:1
T:Tiny Steps
L:1/8
Q:1/4=128
K:C
V:1
z8 | z8 |
c2 e2 g2 e2 | f2 a2 g4 | e2 g2 c'2 g2 | a2 g2 e4 |
d2 f2 a2 f2 | g2 e2 c4 | d2 e2 f2 g2 | c'4 z4 |
V:2
z8 | z8 |
C,4 G,,4 | F,,4 C,4 | C,4 G,,4 | F,,4 C,4 |
D,4 A,,4 | G,,4 C,4 | F,,4 G,,4 | C,8 |
|}

let tune : Abc.tune = match Abc.parse tune_text with Ok t -> t | Error e -> failwith ("TinyDDR's tune: " ^ e)
let song : Audio.sound = Audio.abc tune_text
let song_length : number = Abc.duration tune

type lane = Left | Down | Up | Right

let lanes = [ Left; Down; Up; Right ]
let lane_index = function Left -> 0 | Down -> 1 | Up -> 2 | Right -> 3

type step = { at : number; (* seconds into the song *) lane : lane }

(* The chart, from the melody: a step on each note, the arrow following
 * the tune's shape -- up for a step up, down for a step down, a leap
 * (a fifth or more) sideways, a repeated note the same arrow again. *)
let chart (t : Abc.tune) : step list =
  let melody = match t.voices with v :: _ -> v | [] -> [] in
  let notes = List.filter_map (fun (e : Abc.event) -> match e.notes with n :: _ -> Some (e.start, n) | [] -> None) melody in
  let rec go previous lane acc = function
    | [] -> List.rev acc
    | (at, pitch) :: rest ->
        let lane =
          match previous with
          | None -> Left
          | Some p ->
              let leap = pitch -.. p in
              if leap = 0 then lane
              else if leap >= 7 then Right
              else if leap <= -7 then Left
              else if leap > 0 then Up
              else Down
        in
        go (Some pitch) lane ({ at; lane } :: acc) rest
  in
  go None Left [] notes

let steps : step list = chart tune

(*****************************************************************************)
(* Judging *)
(*****************************************************************************)

type judgement = Perfect | Great | Good | Almost | Miss

(* how far from the beat a press may be, in seconds, for each grade;
 * our own windows, near DDR's (whose exact ones changed from version
 * to version -- from memory, to check) *)
let window = function Perfect -> 0.030 | Great -> 0.060 | Good -> 0.100 | Almost -> 0.135 | Miss -> infinity

let judge (error : number) : judgement option =
  let e = Float.abs error in
  if e <= window Perfect then Some Perfect
  else if e <= window Great then Some Great
  else if e <= window Good then Some Good
  else if e <= window Almost then Some Almost
  else None

let points = function Perfect -> 100 | Great -> 70 | Good -> 40 | Almost -> 10 | Miss -> 0
let name = function Perfect -> "PERFECT" | Great -> "GREAT" | Good -> "GOOD" | Almost -> "ALMOST" | Miss -> "MISS"

(* The music's time, as the player hears it: how far into the song the
 * sound card has been fed, less the machine's latency. *)
let song_time ~(position : number) ~(offset : number) : number = position - offset

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type dance = {
  (* each step, and how it went: None until it is judged *)
  judged : (step * judgement option) list;
  (* the signed errors of the steps hit, early < 0 < late *)
  errors : number list;
  combo : int;
  best_combo : int;
  score : int;
  (* the last judgement, and when, for the flash in the middle *)
  last : (judgement * number) option;
  offset : number; (* the calibration, seconds *)
  (* the song time of the last frame, for the frame-clock comparison *)
  now : number;
  started : number; (* computer.time when the dance began *)
}

type scene = Title | Dancing of dance | Results of dance
type model = scene Scene2d.t

let start_dance (offset : number) (started : number) : dance =
  { judged = List.map (fun s -> (s, None)) steps; errors = []; combo = 0; best_combo = 0; score = 0; last = None;
    offset; now = 0.; started }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* A press in a lane: the nearest step not yet judged in that lane, if
 * it is close enough to be one at all. *)
let press (now : number) (lane : lane) (d : dance) : dance =
  let candidates =
    List.filter_map
      (fun (s, j) -> if j = None && s.lane = lane then Some (s, now - s.at) else None)
      d.judged
  in
  let nearest =
    List.fold_left
      (fun best (s, e) ->
        match best with Some (_, be) when Float.abs be <= Float.abs e -> best | _ -> Some (s, e))
      None candidates
  in
  match nearest with
  | Some (s, e) -> (
      match judge e with
      | Some j ->
          let combo = if j = Almost then 0 else d.combo +.. 1 in
          { d with
            judged = List.map (fun (s', j') -> if s' == s then (s', Some j) else (s', j')) d.judged;
            errors = e :: d.errors; combo; best_combo = max d.best_combo combo; score = d.score +.. points j;
            last = Some (j, now) }
      | None -> d)
  | None -> d

(* the steps that went by unpressed are misses *)
let misses (now : number) (d : dance) : dance =
  let late (s, j) = j = None && now - s.at > window Almost in
  if not (List.exists late d.judged) then d
  else
    { d with judged = List.map (fun (s, j) -> if late (s, j) then (s, Some Miss) else (s, j)) d.judged; combo = 0;
             last = Some (Miss, now) }

(* one frame of dancing, given the song's time and the lanes pressed:
 * the pure part, which the tests call directly *)
let dance_step (now : number) (pressed : lane list) (d : dance) : dance =
  let d = List.fold_left (fun d lane -> press now lane d) { d with now } pressed in
  misses now d

let average_error (d : dance) : number option =
  match d.errors with [] -> None | l -> Some (List.fold_left ( + ) 0. l / float_of_int (List.length l))

let finished (d : dance) : bool = d.now > song_length + 0.5

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let key k = Scene2d.pressed k scenes in
  let clock_now = match computer.time with Time t -> t in
  match scenes.scene with
  | Title | Results _ ->
      if key (fun k -> k.kspace) then begin
        Audio.stop "song";
        let offset = match scenes.scene with Results d -> d.offset | _ -> 0.05 in
        Scene2d.go (Dancing (start_dance offset clock_now)) scenes
      end
      else scenes
  | Dancing d ->
      Audio.loop "song" song;
      let offset =
        d.offset
        + (if key (fun k -> Set_.mem "=" k.keys) then 0.01 else 0.)
        - if key (fun k -> Set_.mem "-" k.keys) then 0.01 else 0.
      in
      let position =
        if List.assoc_opt "clock" computer.flags = Some "frame" then clock_now - d.started
        else Option.value ~default:0. (Audio.position "song")
      in
      let now = song_time ~position ~offset in
      let pressed =
        List.filter
          (fun lane ->
            key (fun k ->
                match lane with Left -> k.kleft | Down -> k.kdown | Up -> k.kup | Right -> k.kright))
          lanes
      in
      let d = dance_step now pressed { d with offset } in
      if finished d then begin
        Audio.stop "song";
        Scene2d.go (Results d) scenes
      end
      else { scenes with scene = Dancing d }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let lane_color = function Left -> rgb 230 90 200 | Down -> rgb 90 170 250 | Up -> rgb 100 230 130 | Right -> rgb 250 170 60
let lane_x (l : lane) : number = (float_of_int (lane_index l) * 110.) - 165.
let target_y = 300.
let speed = 420. (* pixels a second: an arrow is on screen ~1.7 s before its beat *)

let arrow (color : color) (l : lane) : shape =
  let turn = match l with Up -> 0. | Left -> 90. | Down -> 180. | Right -> -90. in
  group [ triangle color 30. |> move_y 10.; rectangle color 20. 30. |> move_y (-18.) ] |> rotate turn

let judgement_color = function
  | Perfect -> rgb 250 240 120 | Great -> rgb 120 230 140 | Good -> rgb 120 180 250 | Almost -> rgb 200 160 230
  | Miss -> rgb 230 80 80

(* the targets flash on each beat of the song's own clock: the picture
 * keeping time with what is heard *)
let beat = 60. / 128.

let view_dance (computer : computer) (d : dance) : shape list =
  let screen = computer.screen in
  let pulse = 1. - (Float.rem (Float.max 0. d.now) beat / beat) in
  [ rectangle (rgb 16 12 28) screen.width screen.height ]
  @ List.map (fun l -> rectangle (rgb 30 24 48) 96. screen.height |> move_x (lane_x l)) lanes
  @ List.map (fun l -> arrow (rgb 200 200 220) l |> fade (0.35 + (0.35 * pulse)) |> move (lane_x l) target_y) lanes
  @ List.filter_map
      (fun ((s : step), j) ->
        let y = target_y - ((s.at - d.now) * speed) in
        if j <> None || y < screen.bottom - 60. || y > screen.top + 60. then None
        else Some (arrow (lane_color s.lane) s.lane |> move (lane_x s.lane) y))
      d.judged
  @ (match d.last with
    | Some (j, at) when d.now - at < 0.5 -> [ text (judgement_color j) 4. (name j) |> move_y 60. ]
    | _ -> [])
  @ [ text white 2.5 (Printf.sprintf "%d" d.score) |> move (340.) (screen.top - 40.);
      text white 2. (if d.combo >= 3 then Printf.sprintf "%d combo" d.combo else "") |> move_y (-20.);
      text (rgb 170 160 200) 1.8
        (Printf.sprintf "calibration %+.0f ms  (- =)    song %.1f s" (d.offset * 1000.) (Float.max 0. d.now))
      |> move_y (screen.bottom + 30.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let back = rectangle (rgb 16 12 28) screen.width screen.height in
  match model.scene with
  | Title ->
      [ back;
        text (rgb 250 170 60) 6. "TINY DDR" |> move_y 220.;
        text white 2. "the arrows rise with the music: press each one as it reaches its outline" |> move_y 90.;
        text white 2. "- and = move the calibration by 10 ms, for this machine's latency" |> move_y 50.;
        text (rgb 170 160 200) 2. "the steps are judged by the music's clock, not the picture's" |> move_y (-10.) ]
      @ List.map (fun l -> arrow (lane_color l) l |> move (lane_x l) (-120.)) lanes
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-260.) ]
  | Dancing d -> view_dance computer d
  | Results d ->
      let count j = List.length (List.filter (fun (_, j') -> j' = Some j) d.judged) in
      [ back; text (rgb 250 170 60) 5. "RESULTS" |> move_y 260. ]
      @ List.mapi
          (fun i j ->
            text (judgement_color j) 2.6 (Printf.sprintf "%-8s %3d" (name j) (count j)) |> move_y (170. - (float_of_int i * 42.)))
          [ Perfect; Great; Good; Almost; Miss ]
      @ [ text white 2.4 (Printf.sprintf "score %d    best combo %d" d.score d.best_combo) |> move_y (-60.);
          text (rgb 170 160 200) 2.2
            (match average_error d with
             | Some e when Float.abs e > 0.012 ->
                 Printf.sprintf "on average %.0f ms %s: set the calibration to %+.0f ms" (Float.abs e * 1000.)
                   (if e > 0. then "late" else "early") ((d.offset + e) * 1000.)
             | Some _ -> "on the beat, on average: the calibration is right"
             | None -> "no steps hit")
          |> move_y (-110.) ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-260.) ]

let help =
  {|TinyDDR
  arrows   the four steps, as each arrow reaches its outline at the top
  - =      the calibration, 10 ms at a time (this machine's latency)
  space    start, and again after the song
  flags:   clock=frame   judge by the frame clock instead of the music's
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
