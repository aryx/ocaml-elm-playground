(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Rock Band (Harmonix, 2007): a band on four
 * highways, one song, and you play one of the parts while the others
 * play theirs.
 *
 *   1 2 3 4    on the title: guitar, bass, drums or keys
 *   up/down    on the title: the difficulty
 *   a s d f g  the five frets, or the five keys
 *   space      strum (guitar and bass), the kick pedal (drums)
 *   s d f g    the drums' four pads: red, yellow, blue, green
 *   - =        the calibration, 10 ms at a time
 *
 * Harmonix made Frequency (2001) and Amplitude (2003), then Guitar Hero
 * (2005), the note highway with a plastic guitar in front of it; Rock
 * Band put the whole band in the living room -- guitar, bass, drums, a
 * microphone -- and Rock Band 3 (2010) added the keyboard. (Names and
 * dates from memory, to check.)
 *
 * The third of the repository's rhythm games, and each adds one idea to
 * the one before: TinyDDR.ml is the music's clock,
 * TinyGuitarHero.ml the instrument (fret and strum, long
 * notes, a difficulty as the same song reduced), and this one the band.
 * All of the first two is in gamekits/rhythm, shared. What is new here is
 * what makes it a *band*:
 *
 *  - The parts are the voices of one tune. The song is written once, in
 *    ABC, with four voices -- a melody, a bass line, chords, a beat --
 *    and each instrument's chart is its own voice ([chart]): a note on
 *    every note, its fret given by where the pitch sits in that part's
 *    range (Rhythm.on_frets), a chord as several frets at once. The
 *    guitar plays the tune, the bass the bass, the keys the chords, the
 *    drums the beat, because that is what the song says they play.
 *
 *  - Four instruments, four ways of pressing ([presses]): guitar and
 *    bass are fret-then-strum (TinyGuitarHero's rule); a keyboard has
 *    no strum, the key *is* the note -- the same five keys, a different
 *    instrument, which is what Rock Band 3 had to teach its players;
 *    and the drums are hits, four pads and a pedal, the kick drawn
 *    across the whole highway because a foot plays it.
 *
 *  - The drums are a voice of hits, not notes. The song's fourth voice
 *    is marked clef=perc (audio/Abc.mli), so the sound card hears a
 *    bass drum, a snare, a hi-hat -- the drum sounds of the MIDI player
 *    -- and the chart reads each drum's key as a pad ([pad_of_key]).
 *
 *  - Everyone plays at their own difficulty (Rhythm.reduce): yours is
 *    chosen on the title, the band's own parts are played as written.
 *    The drums are reduced their own way ([drum_part]), since folding
 *    frets means nothing on a drum kit: below Hard the pedal is left
 *    out, below Expert one pad is struck at a time, and Easy keeps only
 *    the beats.
 *
 *  - The band: the parts you are not playing play themselves, their
 *    highways beside yours, so the four are always seen together; and
 *    the crowd is one meter for all of it ([crowd]): hits fill it,
 *    misses drain it, and empty, the band is booed off. And the band is
 *    heard without you ([bands]: your part muted); your notes sound
 *    only when you hit them (TinyGuitarHero's rule, Rhythm.struck), so
 *    a miss is a hole in the song -- a missing chord, a missing snare.
 *
 * And it is a 3D game because the highway is a road into the
 * distance, which a camera draws for free: four of them in real 3D,
 * each looking like its instrument -- coloured fret pads for guitar and
 * bass, drum pads and a pedal bar for the drums, a row of piano keys
 * for the keyboard. TinyGuitarHero.ml draws one highway by
 * hand, Out Run's road straightened; a camera is what makes four
 * cheap.
 *
 * What it uses: gamekits/rhythm (the clock, the grades, the performance,
 * the charts on frets, the difficulty, the strum, the sustains),
 * playground3d (the highways, a perspective camera), Audio (the band,
 * a note per hit, Audio.position -- which in 3D needed the 3D loop to feed the
 * sound card at all, plan_audio_teaching.md's phase 4 item, done for
 * this game), audio's ABC percussion (the drums), Scene2d.
 *
 * One simplification, worth knowing: here the frets held at a strum are
 * all played, so holding every fret and strumming every beat cannot
 * miss a single note. Rock Band wants the *exact* frets -- an extra one
 * held is a wrong chord -- and that one check (Rhythm.strummed) is the
 * first exercise, and the real difficulty of the guitar.
 *
 * Exercises: vocals (which need a microphone and pitch detection,
 * audio/Spectrum's peaks), two players on one keyboard, overdrive (Rock
 * Band's star power: a streak charges it, spending it doubles the score
 * and can save a failing bandmate), hammer-ons, and the drum fills.
 *)
open Playground
open Playground3d
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The song, and its four parts *)
(*****************************************************************************)

(* An original tune, 120 beats a minute: two bars for the highways to
 * fill (the drummer counting in on the hi-hat), then eight of melody
 * (the guitar), bass line (the bass), chords (the keys), and a rock
 * beat (hi-hat eighths, bass drum on 1 and 3, snare on 2 and 4):
 * the fourth voice is drums, a clef=perc voice whose notes are drums --
 * C,, the bass drum, D,, the snare, ^F,, the hi-hat, A,, B,, the toms,
 * ^C, the crash cymbal (audio/Abc.mli). *)
let tune_text =
  {|X:1
T:Tiny Anthem
L:1/8
Q:1/4=120
K:G
V:1
z8 | z8 |
G2 B2 d2 B2 | c2 e2 d4 | B2 d2 g2 d2 | e2 d2 B4 |
A2 c2 e2 c2 | d2 B2 G4 | A2 B2 c2 d2 | g4 z4 |
V:2
z8 | z8 |
G,,4 D,4 | C,4 G,,4 | G,,4 D,4 | C,4 G,,4 |
A,,4 E,4 | D,4 G,,4 | C,4 D,4 | G,,8 |
V:3
z8 | z8 |
[GBd]8 | [CEG]4 [DGB]4 | [GBd]8 | [CEG]4 [GBd]4 |
[Ace]8 | [DFA]4 [GBd]4 | [CEG]4 [DFA]4 | [GBd]8 |
V:4 clef=perc
z8 | ^F,,2 ^F,,2 ^F,,2 ^F,,2 |
[C,,^C,] ^F,, [D,,^F,,] ^F,, [C,,^F,,] ^F,, [D,,^F,,] ^F,, | [C,,^F,,] ^F,, [D,,^F,,] ^F,, [C,,^F,,] ^F,, [D,,^F,,] ^F,, |
[C,,^F,,] ^F,, [D,,^F,,] ^F,, [C,,^F,,] ^F,, [D,,^F,,] ^F,, | [C,,^F,,] ^F,, [D,,^F,,] ^F,, [C,,^F,,] [C,,^F,,] [D,,^F,,] D,, |
[C,,^C,] ^F,, [D,,^F,,] ^F,, [C,,^F,,] ^F,, [D,,^F,,] ^F,, | [C,,^F,,] ^F,, [D,,^F,,] ^F,, [C,,^F,,] ^F,, [D,,^F,,] ^F,, |
[C,,^F,,] ^F,, [D,,^F,,] ^F,, [C,,^F,,] ^F,, [D,,^F,,] ^F,, | D,, D,, A,, A,, B,, B,, [C,,^C,]2 |
|}

let tune : Abc.tune = match Abc.parse tune_text with Ok t -> t | Error e -> failwith ("TinyRockBand's tune: " ^ e)
let song_length : number = Abc.duration tune

type instrument = Guitar | Bass | Drums | Keys

let instruments = [ Bass; Guitar; Drums; Keys ] (* as the band stands, left to right *)
let voice_of = function Guitar -> 0 | Bass -> 1 | Keys -> 2 | Drums -> 3
let name_of = function Guitar -> "GUITAR" | Bass -> "BASS" | Drums -> "DRUMS" | Keys -> "KEYS"

(* A drum kit's lanes: the kick pedal (0), then the four pads, red,
 * yellow, blue, green (1 to 4), each drum of the song on the pad nearest
 * where a kit has it -- the snare, the hi-hat, the toms, the cymbals. *)
let pad_of_key (key : int) : int =
  match key with
  | 35 | 36 -> 0
  | 38 | 40 -> 1
  | 42 | 44 | 46 -> 2
  | 41 | 43 | 45 | 47 | 48 | 50 -> 3
  | _ -> 4

(* the drums' chart: a hit per drum struck, and hits do not last *)
let drum_chart (t : Abc.tune) : int Rhythm.note list =
  List.concat_map
    (fun (at, _, keys) ->
      List.sort_uniq compare (List.map pad_of_key keys) |> List.map (fun lane -> { Rhythm.at; lane; length = 0. }))
    (Rhythm.sounding t (voice_of Drums))

let beat = 0.5 (* Q:1/4=120 *)

(* A drum part reduced: folding frets (Rhythm.reduce) means nothing on
 * a kit, so each level takes something away instead. Below Hard the
 * pedal is left out; below Expert one pad at a time, the lowest; Easy
 * only on the beats.
 *
 *            kick   pads at once   when
 *   Easy      -         1          on the beat
 *   Medium    -         1          all
 *   Hard      yes       1          all
 *   Expert    yes      all         all
 *)
let drum_part (level : Rhythm.difficulty) (chart : int Rhythm.note list) : int Rhythm.note list =
  let on_beat at = Float.abs (at - (Float.round (at / beat) * beat)) < 0.01 in
  let alone (n : int Rhythm.note) =
    not (List.exists (fun (m : int Rhythm.note) -> m.at = n.at && m.lane > 0 && m.lane < n.lane) chart)
  in
  List.filter
    (fun (n : int Rhythm.note) ->
      match level with
      | Expert -> true
      | Hard -> n.lane = 0 || alone n
      | Medium -> n.lane > 0 && alone n
      | Easy -> n.lane > 0 && alone n && on_beat n.at)
    chart

(* A part's chart at a level: the kit's frets and its reduction for
 * the pitched parts, the drums their own. The band's parts are played
 * as written, at Expert. *)
let chart (i : instrument) (level : Rhythm.difficulty) : int Rhythm.note list =
  match i with
  | Drums -> drum_part level (drum_chart tune)
  | Guitar | Bass | Keys -> Rhythm.reduce level (Rhythm.on_frets tune (voice_of i))

(* the band as each player hears it: the song with that player's part
 * muted, made once *)
let bands : (instrument * Audio.sound) list =
  List.map (fun i -> (i, Audio.of_tune (Rhythm.muted (voice_of i) tune))) instruments

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  mine : instrument;
  level : Rhythm.difficulty;
  perf : int Rhythm.performance;
  (* the points held notes have earned, on top of the kit's score *)
  sustain : int;
  (* how the crowd feels, 0 to 1: at 0 the band is booed off *)
  crowd : number;
}

type scene = Title of instrument * Rhythm.difficulty | Playing of play | Over of play * bool (* the song finished *)
type model = scene Scene2d.t

let start (mine : instrument) (level : Rhythm.difficulty) (offset : number) (started : number) : play =
  { mine; level; perf = Rhythm.start ~offset ~started (chart mine level); sustain = 0; crowd = 0.5 }

let initial_model : model = Scene2d.start (Title (Guitar, Rhythm.Medium))

(*****************************************************************************)
(* Playing *)
(*****************************************************************************)

(* the keys of an instrument's lanes: five frets or keys, or the four
 * pads (the kick is the space bar, the strum of the others) *)
let lane_keys = function
  | Drums -> [ ("s", 1); ("d", 2); ("f", 3); ("g", 4) ]
  | Guitar | Bass | Keys -> [ ("a", 0); ("s", 1); ("d", 2); ("f", 3); ("g", 4) ]

(* the lanes whose key is [down] *)
let lanes (i : instrument) (down : string -> bool) : int list =
  List.filter_map (fun (key, lane) -> if down key then Some lane else None) (lane_keys i)

(* An instrument is a way of pressing. A guitar note is the fret held
 * and then the strum: the frets down *at the strum* are the notes
 * played. A keyboard has no strum: a key pressed is its note. A drum
 * is struck: a pad pressed, or the pedal. *)
let presses (i : instrument) ~(strum : bool) ~(held : int list) ~(pressed : int list) : int list =
  match i with
  | Guitar | Bass -> Rhythm.strummed ~strum ~held
  | Keys -> pressed
  | Drums -> if strum then 0 :: pressed else pressed

(* the crowd: a hit fills it a little, a miss drains it more *)
let crowd (before : int Rhythm.performance) (after : int Rhythm.performance) (c : number) : number =
  let count g (p : int Rhythm.performance) = List.length (List.filter (fun (_, j) -> g j) p.judged) in
  let hit j = j <> None && j <> Some Rhythm.Miss and miss j = j = Some Rhythm.Miss in
  let hits = count hit after -.. count hit before and misses = count miss after -.. count miss before in
  Float.max 0. (Float.min 1. (c + (0.025 * float_of_int hits) - (0.07 * float_of_int misses)))

(* one frame of playing, given the song's time and the hands: the pure
 * part, which the tests call directly *)
let play_step (now : number) ~(strum : bool) ~(held : int list) ~(pressed : int list) (p : play) : play =
  let played = presses p.mine ~strum ~held ~pressed in
  let perf = Rhythm.play now played p.perf in
  { p with perf; sustain = p.sustain +.. Rhythm.sustaining now held perf; crowd = crowd p.perf perf p.crowd }

let finished (p : play) : bool = p.perf.now > song_length + 0.5
let booed (p : play) : bool = p.crowd <= 0.

(* up and down through a list, stopping at its ends *)
let step (l : 'a list) (x : 'a) (by : int) : 'a =
  let rec find i = function [] -> 0 | y :: r -> if y = x then i else find (i +.. 1) r in
  List.nth l (max 0 (min (List.length l -.. 1) (find 0 l +.. by)))

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let key f = Scene2d.pressed f scenes in
  let digit d = key (fun k -> Set_.mem d k.keys) in
  match scenes.scene with
  | Title (mine, level) ->
      let mine =
        if digit "1" then Guitar else if digit "2" then Bass else if digit "3" then Drums else if digit "4" then Keys else mine
      in
      let level =
        step Rhythm.difficulties level ((if key (fun k -> k.kdown) then 1 else 0) -.. if key (fun k -> k.kup) then 1 else 0)
      in
      if key (fun k -> k.kspace) then begin
        Audio.stop "rockband";
        Scene2d.go (Playing (start mine level 0.05 (match computer.time with Time t -> t))) scenes
      end
      else { scenes with scene = Title (mine, level) }
  | Over (p, _) ->
      if key (fun k -> k.kspace) then begin
        Audio.stop "rockband";
        Scene2d.go (Title (p.mine, p.level)) scenes
      end
      else scenes
  | Playing p ->
      Audio.loop "rockband" (List.assoc p.mine bands);
      let offset =
        p.perf.offset + (if digit "=" then 0.01 else 0.) - if digit "-" then 0.01 else 0.
      in
      let now = Rhythm.song_time ~position:(Option.value ~default:0. (Audio.position "rockband")) ~offset in
      let before = p in
      let p =
        play_step now ~strum:(key (fun k -> k.kspace))
          ~held:(lanes p.mine (fun k -> Set_.mem k computer.keyboard.keys))
          ~pressed:(lanes p.mine digit)
          { p with perf = { p.perf with offset } }
      in
      (* your notes just hit, heard; the ones missed never are *)
      List.iter
        (fun at -> Audio.play (Audio.of_tune (Rhythm.struck tune (voice_of p.mine) at)))
        (Rhythm.newly_hit before.perf p.perf);
      if booed p then begin
        Audio.stop "rockband";
        Scene2d.go (Over (p, false)) scenes
      end
      else if finished p then begin
        Audio.stop "rockband";
        Scene2d.go (Over (p, true)) scenes
      end
      else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let lane_w = 0.6
let highway_x = function Bass -> -5.1 | Guitar -> -1.7 | Drums -> 1.7 | Keys -> 5.1
let label_x = function Bass -> -390. | Guitar -> -130. | Drums -> 130. | Keys -> 390.

(* where a lane is: five across, or on the drums four wider pads, and
 * the kick across the whole highway *)
let pad_w = lane_w * 1.25
let lane_x (i : instrument) (lane : int) : number =
  match i with
  | Drums -> if lane = 0 then highway_x i else highway_x i + ((float_of_int lane - 2.5) * pad_w)
  | Guitar | Bass | Keys -> highway_x i + ((float_of_int lane - 2.) * lane_w)

let speed = 7. (* units a second: a note is on the highway ~5 s ahead of its beat *)
let far = 36.

(* guitar and bass: Guitar Hero's colours, green to orange; the drums:
 * Rock Band's pads, red to green, and the pedal orange *)
let color_of (i : instrument) (lane : int) : color =
  match (i, lane) with
  | Drums, 0 -> rgb 250 150 40
  | Drums, 1 -> rgb 230 60 60
  | Drums, 2 -> rgb 240 220 60
  | Drums, 3 -> rgb 60 130 240
  | Drums, _ -> rgb 60 200 90
  | _, 0 -> rgb 60 200 90
  | _, 1 -> rgb 230 60 60
  | _, 2 -> rgb 240 220 60
  | _, 3 -> rgb 60 130 240
  | _ -> rgb 250 150 40

let highway (i : instrument) (mine : bool) : shape3d list =
  let x = highway_x i in
  let deck = if mine then rgb 46 40 60 else rgb 30 28 38 in
  let lines = match i with Drums -> List.init 5 (fun k -> (float_of_int k - 2.) * pad_w) | _ -> List.init 6 (fun k -> (float_of_int k - 2.5) * lane_w) in
  (box deck (5. * lane_w) 0.05 far |> move3d x (-0.03) (-.far / 2.))
  :: List.map (fun dx -> box (rgb 70 66 86) 0.02 0.06 far |> move3d (x + dx) 0. (-.far / 2.)) lines

(* what stands at the strike line says what the instrument is *)
let strike_line (i : instrument) (held : int list) (mine : bool) : shape3d list =
  let lit lane = mine && List.mem lane held in
  match i with
  | Guitar | Bass ->
      List.init 5 (fun lane ->
          box (if lit lane then white else color_of i lane) (lane_w * 0.8) 0.12 0.3 |> move3d (lane_x i lane) 0.06 0.)
  | Drums ->
      (* four drum heads, and the pedal's bar under them *)
      (box (if lit 0 then white else color_of i 0) (5. * lane_w) 0.04 0.12 |> move3d (lane_x i 0) 0.02 0.35)
      :: List.concat
           (List.init 4 (fun k ->
                let lane = k +.. 1 in
                [ box (rgb 50 48 60) (pad_w * 0.92) 0.1 0.5 |> move3d (lane_x i lane) 0.04 0.;
                  box (if lit lane then white else color_of i lane) (pad_w * 0.78) 0.14 0.38 |> move3d (lane_x i lane) 0.07 0. ]))
  | Keys ->
      (* a keyboard: five white keys, and the black ones between them *)
      List.init 5 (fun lane ->
          box (if lit lane then rgb 250 230 140 else rgb 240 240 245) (lane_w * 0.92) 0.14 0.9
          |> move3d (lane_x i lane) 0.07 0.2)
      @ List.filter_map
          (fun lane ->
            if lane = 2 then None
            else Some (box (rgb 20 20 26) (lane_w * 0.45) 0.2 0.5 |> move3d (lane_x i lane + (lane_w / 2.)) 0.12 0.))
          [ 0; 1; 2; 3 ]

let gem (i : instrument) (n : int Rhythm.note) : shape3d =
  match i with
  | Guitar | Bass -> box (color_of i n.lane) (lane_w * 0.7) 0.18 0.28
  | Drums -> if n.lane = 0 then box (color_of i 0) (5. * lane_w) 0.06 0.14 else box (color_of i n.lane) (pad_w * 0.8) 0.12 0.24
  | Keys -> box (rgb 245 245 250) (lane_w * 0.75) 0.14 0.4

(* a part on its highway: the notes still to come, their tails, and the
 * ones just played flashing at the line *)
let part (now : number) (i : instrument) (notes : (int Rhythm.note * Rhythm.judgement option) list) : shape3d list =
  List.concat_map
    (fun ((n : int Rhythm.note), j) ->
      let z = -.(n.at - now) * speed in
      let tail_len = if n.length >= Rhythm.sustain_min then n.length * speed else 0. in
      if z < -.far || z - tail_len > 1.5 then []
      else
        let tail =
          if tail_len = 0. then []
          else
            let top = Float.min z 0. in
            let len = Float.max 0. (top - (z - tail_len)) in
            if len = 0. then []
            else [ box (color_of i n.lane) 0.12 0.06 len |> move3d (lane_x i n.lane) 0.04 (top - (len / 2.)) ]
        in
        let head =
          match j with
          | Some Rhythm.Miss -> if z < 1.5 then [ gem i n |> move3d (lane_x i n.lane) 0.1 z ] else []
          | Some _ -> if now - n.at < 0.15 then [ box white (lane_w * 0.9) 0.3 0.4 |> move3d (lane_x i n.lane) 0.2 0. ] else []
          | None -> [ gem i n |> move3d (lane_x i n.lane) 0.1 z ]
        in
        tail @ head)
    notes

(* the parts you are not playing play themselves, on the beat *)
let band_part (now : number) (i : instrument) : shape3d list =
  part now i
    (List.map (fun (n : int Rhythm.note) -> (n, if n.at <= now then Some Rhythm.Perfect else None)) (chart i Rhythm.Expert))

(* behind the strike line and above it, looking down the highways: the
 * line near the bottom of the screen, the notes coming from the top *)
let stage_camera : camera = Camera3d.from_far ~fov:58. ~offset:(0., 6., 22.) (0., 0., -10.)

(* the stage around the band: a floor, and a wall far behind the
 * highways, so that above the horizon is night rather than the white
 * the renderer clears to *)
let stage : shape3d list =
  [ Camera3d.floor ~color:(rgb 14 10 20) ~ground:(-0.2) stage_camera;
    box (rgb 14 10 20) 600. 200. 1. |> move3d 0. 60. (-120.) ]

let judgement_color = function
  | Rhythm.Perfect -> rgb 250 240 120 | Great -> rgb 120 230 140 | Good -> rgb 120 180 250 | Almost -> rgb 200 160 230
  | Miss -> rgb 230 80 80

let how_to_play = function
  | Guitar | Bass -> "hold a fret (a s d f g), strum with space"
  | Keys -> "the keys a s d f g are the notes: no strum"
  | Drums -> "the pads s d f g, the kick pedal space"

let view_play (computer : computer) (p : play) : shape3d list =
  let screen = computer.screen in
  let now = p.perf.now in
  let held_now = lanes p.mine (fun k -> Set_.mem k computer.keyboard.keys) @ if computer.keyboard.kspace && p.mine = Drums then [ 0 ] else [] in
  stage
  @ List.concat_map (fun i -> highway i (i = p.mine) @ strike_line i held_now (i = p.mine)) instruments
  @ part now p.mine p.perf.judged
  @ List.concat_map (fun i -> if i = p.mine then [] else band_part now i) instruments
  @ List.map hud
      ([ text white 2.4 (Printf.sprintf "%d" (p.perf.score +.. p.sustain)) |> move 380. (screen.top - 40.);
         (* the crowd *)
         rectangle (rgb 40 36 50) 300. 16. |> move_y (screen.top - 40.);
         rectangle (if p.crowd < 0.25 then rgb 230 80 80 else rgb 120 230 140) (300. * p.crowd) 16.
         |> move (-150. + (150. * p.crowd)) (screen.top - 40.);
         text (rgb 170 160 200) 1.6 "the crowd" |> move_y (screen.top - 65.) ]
      @ List.map
          (fun i ->
            text (if i = p.mine then white else rgb 120 110 140) 1.8
              (if i = p.mine then name_of i ^ " (you)" else name_of i)
            |> move (label_x i) (screen.bottom + 70.))
          instruments
      @ (match p.perf.last with
        | Some (j, at) when now - at < 0.5 -> [ text (judgement_color j) 3.5 (Rhythm.name j) |> move_y 160. ]
        | _ -> [])
      @ [ text white 1.9 (if p.perf.combo >= 3 then Printf.sprintf "%d in a row" p.perf.combo else "") |> move_y 120.;
          text (rgb 150 140 170) 1.6
            (Printf.sprintf "%s    %s    calibration %+.0f ms (- =)" (Rhythm.difficulty_name p.level) (how_to_play p.mine)
               (p.perf.offset * 1000.))
          |> move_y (screen.bottom + 25.) ])

let view (computer : computer) (model : model) : camera * shape3d list =
  let screen = computer.screen in
  match model.scene with
  | Title (mine, level) ->
      ( stage_camera,
        stage @ List.concat_map (fun i -> highway i (i = mine) @ strike_line i [] (i = mine)) instruments
        @ List.map hud
            ([ text (rgb 250 170 60) 6. "TINY ROCK BAND" |> move_y 260.;
               text white 2. "pick your part: 1 guitar   2 bass   3 drums   4 keys" |> move_y 170.;
               text (rgb 200 190 220) 2. (Printf.sprintf "you: %s -- %s" (name_of mine) (how_to_play mine))
               |> move_y 130. ]
            @ List.mapi
                (fun k l ->
                  text (if l = level then white else rgb 110 100 130) (if l = level then 2.4 else 1.9)
                    (if l = level then "> " ^ Rhythm.difficulty_name l ^ " <" else Rhythm.difficulty_name l)
                  |> move_y (70. - (float_of_int k * 34.)))
                Rhythm.difficulties
            @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-330.) ]) )
  | Playing p -> (stage_camera, view_play computer p)
  | Over (p, finished) ->
      let count j = List.length (List.filter (fun (_, j') -> j' = Some j) p.perf.judged) in
      ( stage_camera,
        stage
        @ List.map hud
            ([ text (if finished then rgb 120 230 140 else rgb 230 80 80) 5.
                 (if finished then "THE CROWD GOES WILD" else "BOOED OFF")
               |> move_y 250.;
               text white 2.4
                 (Printf.sprintf "%s (%s)    score %d    held notes %d" (name_of p.mine) (Rhythm.difficulty_name p.level)
                    (p.perf.score +.. p.sustain) p.sustain)
               |> move_y 170. ]
            @ List.mapi
                (fun k j ->
                  text (judgement_color j) 2.2 (Printf.sprintf "%-8s %3d" (Rhythm.name j) (count j))
                  |> move_y (100. - (float_of_int k * 36.)))
                [ Rhythm.Perfect; Great; Good; Almost; Miss ]
            @ [ text (rgb 170 160 200) 2.
                  (match Rhythm.average_error p.perf with
                   | Some e when Float.abs e > 0.012 ->
                       Printf.sprintf "on average %.0f ms %s: set the calibration to %+.0f ms" (Float.abs e * 1000.)
                         (if e > 0. then "late" else "early") ((p.perf.offset + e) * 1000.)
                   | Some _ -> "on the beat, on average"
                   | None -> "")
                |> move_y (-110.) ]
            @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-330.) ])
        @ [ hud (rectangle black screen.width screen.height |> fade 0.3) ] )

let help =
  {|TinyRockBand
  1 2 3 4    on the title: guitar, bass, drums or keys
  up/down    on the title: the difficulty
  a s d f g  the five frets, or the five keys
  space      strum (guitar and bass), the kick pedal (drums)
  s d f g    the drums' four pads
  - =        the calibration, 10 ms at a time
|}

let app = game3d view update initial_model

let main =
  print_string help;
  Playground3d_platform.run_app3d app
