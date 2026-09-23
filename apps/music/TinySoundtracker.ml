(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Ultimate Soundtracker (Karsten Obarski, Amiga, 1987),
 * the first tracker: a song as a grid of notes, four columns for the
 * Amiga's four channels, rows going down in time, scrolling up the
 * screen as it plays -- the interface of every tracker since
 * (ProTracker, FastTracker, Impulse Tracker, Renoise, OpenMPT), and a
 * file format, the MOD, that carries its instruments with its notes.
 * The song is audio/formats/mod's Mod.song, played by its Mod_player;
 * this is the editor.
 *
 * A cell is a note, an instrument and an effect: "C-3 01 C20" plays
 * instrument 1 at C-3, its volume set to 0x20. The cursor is on a cell's
 * note or on one of its effect's three hex digits:
 *
 *   - on the note, two rows of the keyboard are a piano, as in every
 *     tracker: z s x d c v g b h n j m the lower octave (from C), q 2 w
 *     3 e r 5 t 6 y 7 u i the upper one; the note gets the current
 *     instrument, and the cursor moves a row down;
 *   - on the effect, 0-9 and a-f type its digits;
 *   - Backspace clears what the cursor is on.
 *
 * The arrows move the cursor, across the channels too; Tab switches the
 * piano's octave (C-1 and C-2 below, C-2 and C-3 above); [ and ] choose
 * the instrument. The order list: , and . the previous and next
 * position, - and = the pattern it plays (a new, empty one past the
 * last), Enter a new position after this one. Space plays the song from
 * the current position, the grid following it, or stops it; the edits
 * are heard as they are made (Mod_player.set_song). Control-S exports
 * the song as song.mod: natively in the current directory, in the
 * browser as a download -- a MOD file every tracker opens.
 *
 * The song it starts with is ours: four instruments synthesized here (a
 * pulse lead, a triangle bass, a kick, a snare: a MOD's samples can be
 * anything) and two patterns. mod= opens yours, from a file or a URL
 * (dune exec apps/music/TinySoundtracker.exe -- mod=song.mod).
 *
 * Uses: audio/formats/mod (Mod, Mod_player: the file, the player), Audio
 * (the player as an instrument the mixer pulls; fetch, for mod=),
 * Playground_platform.export with its capability. Not: gui/, the
 * physics, Scene2d.
 *
 * Exercises: editing the instruments (a sample drawn with the mouse, or
 * one of the synthesizer's sounds recorded, Audio.recorded); an edit
 * step other than 1; copying and pasting a pattern's block; the channels
 * muted one by one; reading an 8SVX file into an instrument (the
 * Amiga's own, plan_audio_formats.md).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The song we start with *)
(*****************************************************************************)

let instrument name (a : float array) ~loop : Mod.instrument =
  let data = Mod.data_of_floats a in
  { name; finetune = 0; volume = 64; loop_start = 0; loop_length = (if loop then String.length data else 0); data }

let blank = instrument "" [||] ~loop:false
let note_period (s : string) : int = Option.value (Mod.period_of_name s) ~default:0
let cell ?(i = 1) ?(e = 0) ?(x = 0) (n : string) : Mod.cell = { instrument = i; period = note_period n; effect = e; param = x }

let our_song : Mod.song =
  let pi2 = 2. * Float.pi in
  let lead = instrument "lead" (Array.init 32 (fun i -> if i < 8 then 0.6 else -0.2)) ~loop:true in
  let bass = instrument "bass" (Array.init 64 (fun i -> let u = float_of_int i / 64. in 0.8 * (if u < 0.5 then (4. * u) - 1. else 3. - (4. * u)))) ~loop:true in
  let kick =
    instrument "kick"
      (Array.init 2400 (fun i ->
           let t = float_of_int i / 8287. in
           0.9 * exp (-.t * 18.) * sin (pi2 * ((60. * t) + (140. * (1. - exp (-.t * 30.)) / 30.)))))
      ~loop:false
  in
  let seed = ref 7 in
  let snare =
    instrument "snare"
      (Array.init 2000 (fun i ->
           seed := ((!seed *.. 1103515245) +.. 12345) land 0x7FFFFFFF;
           0.7 * exp (-.float_of_int i / 350.) * ((float_of_int ((!seed lsr 8) land 0xFF) / 128.) - 1.)))
      ~loop:false
  in
  let drums = List.concat_map (fun r -> [ (r, 2, cell ~i:3 "C-2"); (r +.. 4, 3, cell ~i:4 ~e:0xC ~x:0x30 "C-2") ]) [ 0; 8; 16; 24; 32; 40; 48; 56 ] in
  let bass_line = List.mapi (fun k n -> (k *.. 8, 1, cell ~i:2 n)) [ "A-1"; "A-1"; "F-1"; "F-1"; "C-2"; "C-2"; "G-1"; "G-1" ] in
  let melody notes = List.mapi (fun k n -> (k *.. 4, 0, cell ~i:1 n)) notes in
  let first = melody [ "A-2"; "C-3"; "E-3"; "A-3"; "G-3"; "E-3"; "C-3"; "D-3"; "F-2"; "A-2"; "C-3"; "F-3"; "E-3"; "C-3"; "B-2"; "G-2" ] in
  (* the second time, chords: the arpeggio's minor and major thirds *)
  let second =
    List.mapi (fun k (n, x) -> (k *.. 8, 0, cell ~i:1 ~e:0 ~x n)) [ ("A-2", 0x37); ("A-2", 0x37); ("F-2", 0x47); ("F-2", 0x47); ("C-3", 0x47); ("C-3", 0x47); ("G-2", 0x47); ("G-2", 0x47) ]
  in
  let pattern cells =
    Array.init 64 (fun r -> Array.init 4 (fun c -> match List.find_opt (fun (r', c', _) -> r = r' && c = c') cells with Some (_, _, x) -> x | None -> Mod.empty_cell))
  in
  {
    title = "tiny soundtracker";
    instruments = Array.init 31 (fun k -> match k with 0 -> lead | 1 -> bass | 2 -> kick | 3 -> snare | _ -> blank);
    restart = 127;
    positions = [| 0; 1 |];
    patterns = [| pattern (first @ bass_line @ drums); pattern (second @ bass_line @ drums) |];
    tag = "M.K.";
  }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  song : Mod.song;
  position : int; (* the order list's position shown *)
  row : int;
  channel : int;
  column : int; (* 0 the note, 1 to 3 the effect's digits *)
  octave : int; (* the piano's lower row: 1 or 2 *)
  instrument : int; (* 1 to 31 *)
  playing : bool;
  held : string list; (* the keys held at the last frame *)
  said : string;
  loaded : bool; (* mod= asked for *)
}

let initial_model : model =
  { song = our_song; position = 0; row = 0; channel = 0; column = 0; octave = 2; instrument = 1; playing = false; held = []; said = ""; loaded = false }

(* the player lives with the sound, not in the model: the mixer pulls
 * its blocks between frames; a song fetched by mod= arrives here *)
let player : Mod_player.t option ref = ref None
let fetched : string option ref = ref None
let pattern_of (m : model) : int = m.song.positions.(m.position)
let cell_at (m : model) : Mod.cell = m.song.patterns.(pattern_of m).(m.row).(m.channel)

(* the song with one cell changed: the pattern and its row copied, the
 * rest shared -- the song stays a value, and the player is given it *)
let with_cell (m : model) (c : Mod.cell) : model =
  let p = pattern_of m in
  let patterns = Array.copy m.song.patterns in
  let rows = Array.copy patterns.(p) in
  let row = Array.copy rows.(m.row) in
  row.(m.channel) <- c;
  rows.(m.row) <- row;
  patterns.(p) <- rows;
  { m with song = { m.song with patterns } }

let empty_pattern : Mod.cell array array = Array.init 64 (fun _ -> Array.make 4 Mod.empty_cell)

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let lower = [ "z"; "s"; "x"; "d"; "c"; "v"; "g"; "b"; "h"; "n"; "j"; "m" ]
let upper = [ "q"; "2"; "w"; "3"; "e"; "r"; "5"; "t"; "6"; "y"; "7"; "u"; "i" ]

let index_of (x : string) (l : string list) : int option =
  let rec go i = function [] -> None | y :: rest -> if y = x then Some i else go (i +.. 1) rest in
  go 0 l

(* the semitone above the octave's C a piano key plays *)
let piano (k : string) : int option =
  match index_of k lower with Some s -> Some s | None -> Option.map (fun s -> s +.. 12) (index_of k upper)

let hex (k : string) : int option = if String.length k = 1 then int_of_string_opt ("0x" ^ k) else None
let down_a_row (m : model) : model = { m with row = (m.row +.. 1) mod 64 }

let type_digit (m : model) (d : int) : model =
  let c = cell_at m in
  let c : Mod.cell =
    match m.column with
    | 1 -> { c with effect = d }
    | 2 -> { c with param = (d lsl 4) lor (c.param land 0x0F) }
    | _ -> { c with param = (c.param land 0xF0) lor d }
  in
  let m = with_cell m c in
  if m.column = 3 then down_a_row { m with column = 1 } else { m with column = m.column +.. 1 }

let play (m : model) : unit =
  ignore
    (Audio.instrument "soundtracker" (fun () ->
         let p = Mod_player.create m.song in
         Mod_player.seek p ~position:m.position ~row:0;
         player := Some p;
         { Instrument.note_on = (fun _ _ -> ()); note_off = ignore; set = (fun _ _ -> ()); fill = Mod_player.fill p }))

let stop () : unit =
  Audio.stop "soundtracker";
  player := None

let keys (caps : < Cap.open_out >) (computer : computer) (m : model) : model =
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) in
  let control = List.mem "Control" now in
  let first = List.find_opt pressed now in
  let m = { m with held = now } in
  match first with
  | None -> m
  | Some _ when control && pressed "s" ->
      let bytes = Mod.to_string m.song in
      Playground_platform.export caps "song.mod" bytes;
      { m with said = Printf.sprintf "exported song.mod, %d bytes" (String.length bytes) }
  | Some _ when pressed "space" ->
      if m.playing then (
        stop ();
        { m with playing = false })
      else (
        play m;
        { m with playing = true })
  | Some _ when pressed "ArrowUp" -> { m with row = (m.row +.. 63) mod 64 }
  | Some _ when pressed "ArrowDown" -> down_a_row m
  | Some _ when pressed "ArrowRight" ->
      if m.column = 3 then { m with column = 0; channel = (m.channel +.. 1) mod 4 } else { m with column = m.column +.. 1 }
  | Some _ when pressed "ArrowLeft" ->
      if m.column = 0 then { m with column = 3; channel = (m.channel +.. 3) mod 4 } else { m with column = m.column -.. 1 }
  | Some _ when pressed "Tab" -> { m with octave = (if m.octave = 1 then 2 else 1) }
  | Some _ when pressed "[" -> { m with instrument = max 1 (m.instrument -.. 1) }
  | Some _ when pressed "]" -> { m with instrument = min 31 (m.instrument +.. 1) }
  | Some _ when pressed "," -> { m with position = max 0 (m.position -.. 1) }
  | Some _ when pressed "." -> { m with position = min (Array.length m.song.positions -.. 1) (m.position +.. 1) }
  | Some _ when pressed "=" || pressed "-" ->
      let p = pattern_of m +.. if pressed "=" then 1 else -1 in
      let p = max 0 (min (Array.length m.song.patterns) p) in
      let patterns = if p = Array.length m.song.patterns then Array.append m.song.patterns [| empty_pattern |] else m.song.patterns in
      let positions = Array.copy m.song.positions in
      positions.(m.position) <- p;
      { m with song = { m.song with patterns; positions } }
  | Some _ when pressed "Enter" && Array.length m.song.positions < 128 ->
      let at = m.position +.. 1 in
      let ps = m.song.positions in
      let positions = Array.init (Array.length ps +.. 1) (fun i -> if i < at then ps.(i) else if i = at then ps.(m.position) else ps.(i -.. 1)) in
      { m with song = { m.song with positions }; position = at }
  | Some _ when pressed "Backspace" ->
      let c = cell_at m in
      with_cell m (if m.column = 0 then { c with period = 0; instrument = 0 } else { c with effect = 0; param = 0 })
  | Some k -> (
      match (m.column, piano k, hex k) with
      | 0, Some s, _ ->
          let i = ((m.octave -.. 1) *.. 12) +.. s in
          if i >= Array.length Mod.periods then m
          else down_a_row (with_cell m { (cell_at m) with instrument = m.instrument; period = Mod.periods.(i) })
      | c, _, Some d when c > 0 -> type_digit m d
      | _ -> m)

let update (caps : < Cap.open_out >) (computer : computer) (m : model) : model =
  (* mod=: asked for once, taken when it arrives *)
  let m =
    if m.loaded then m
    else (
      Option.iter (fun src -> Audio.fetch src (fun bytes -> fetched := bytes)) (List.assoc_opt "mod" computer.flags);
      { m with loaded = true })
  in
  let m =
    match !fetched with
    | None -> m
    | Some bytes -> (
        fetched := None;
        match Mod.of_string bytes with
        | Ok song -> { m with song; position = 0; row = 0; said = "opened " ^ song.title }
        | Error e -> { m with said = e })
  in
  let before = m.song in
  let m = keys caps computer m in
  (* the edits heard as they are made; the grid following the playhead *)
  match !player with
  | Some p when m.playing ->
      if m.song != before then Mod_player.set_song p m.song;
      let position, row = Mod_player.position p in
      { m with position; row }
  | _ -> m

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let grey = rgb 170 170 170
let dark = rgb 20 20 28
let ink = rgb 225 225 205
let dim = rgb 120 120 130

(* [txt size color s]: [s], [size] pixels high (words are 10 at scale 1) *)
let txt (size : number) (color : color) (s : string) : shape = words color s |> scale (size / 10.)
let channel_x (c : int) : number = -320. + (225. * float_of_int c)
let row_height = 32.
let rows_shown = 19
let cell_size = 18.

(* a cell's three fields, centred at x - 55, x + 5, x + 60 *)
let cell_view (c : Mod.cell) (x : number) (y : number) (bright : bool) : shape list =
  let col = if bright then ink else dim in
  [
    txt cell_size col (if c.period = 0 then "---" else Mod.note_name c.period) |> move (x - 55.) y;
    txt cell_size col (if c.instrument = 0 then ".." else Printf.sprintf "%02d" c.instrument) |> move (x + 5.) y;
    txt cell_size col (if c.effect = 0 && c.param = 0 then "..." else Printf.sprintf "%X%02X" c.effect c.param) |> move (x + 60.) y;
  ]

let pattern_view (m : model) : shape list =
  let pattern = m.song.patterns.(pattern_of m) in
  let cy = -90. in
  (* the current row in the middle, a bar under it; the cursor's field:
   * the note, or one of the effect's digits (11 pixels apart) *)
  let bar = rectangle (if m.playing then rgb 100 40 40 else rgb 55 55 90) 930. row_height |> move 15. cy in
  let fx = channel_x m.channel + (match m.column with 0 -> -55. | 1 -> 49. | 2 -> 60. | _ -> 71.) in
  let cursor = rectangle (rgb 160 160 60) (if m.column = 0 then 48. else 13.) (row_height - 6.) |> move fx cy in
  let rows =
    List.concat
      (List.init rows_shown (fun k ->
           let r = m.row +.. k -.. (rows_shown /.. 2) in
           if r < 0 || r > 63 then []
           else
             let y = cy - (float_of_int (k -.. (rows_shown /.. 2)) * row_height) in
             (txt cell_size (if r mod 4 = 0 then ink else dim) (Printf.sprintf "%02d" r) |> move (-465.) y)
             :: List.concat (List.init 4 (fun c -> cell_view pattern.(r).(c) (channel_x c) y (r = m.row)))))
  in
  let headers =
    List.init 4 (fun c ->
        let v = match !player with Some p when m.playing -> Mod_player.channel_volume p c | _ -> 0 in
        group
          [
            rectangle (rgb 40 40 55) 205. 38.;
            rectangle (rgb 90 200 90) (float_of_int v * 195. / 64.) 6. |> move 0. (-15.);
            txt 16. grey (Printf.sprintf "channel %d" (c +.. 1)) |> move 0. 4.;
          ]
        |> move (channel_x c) 245.)
  in
  [ rectangle dark 960. 730. |> move 0. (-85.); bar; cursor ] @ headers @ rows

let instruments_view (m : model) : shape list =
  let i = m.song.instruments.(m.instrument -.. 1) in
  (* its sample, its first 400 bytes at most, across the box *)
  let n = min 400 (String.length i.data) in
  let step = 280. / float_of_int (max 1 (n -.. 1)) in
  let wave =
    List.init (max 0 (n -.. 1)) (fun k ->
        let x0 = 190. + (float_of_int k * step) and x1 = 190. + (float_of_int (k +.. 1) * step) in
        let y0 = 385. + (Mod.sample i k * 55.) and y1 = 385. + (Mod.sample i (k +.. 1) * 55.) in
        let len = Float.hypot (x1 - x0) (y1 - y0) in
        rectangle (rgb 80 220 120) (len + 1.) 2. |> rotate (Float.atan2 (y1 - y0) (x1 - x0) * 180. / Float.pi) |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.))
  in
  let names =
    List.init 5 (fun k ->
        let j = m.instrument -.. 2 +.. k in
        if j < 1 || j > 31 then []
        else
          let name = m.song.instruments.(j -.. 1).name in
          [ txt 16. (if j = m.instrument then ink else dim) (Printf.sprintf "%02d %s" j name) |> move 70. (440. - (float_of_int k * 26.)) ])
  in
  [ rectangle dark 300. 130. |> move 330. 385. ] @ wave @ List.concat names

let order_view (m : model) : shape list =
  List.mapi
    (fun k p -> txt 18. (if k = m.position then ink else dim) (Printf.sprintf "%02d" p) |> move (-450. + (float_of_int k * 44.)) 300.)
    (Array.to_list (Array.sub m.song.positions 0 (min 20 (Array.length m.song.positions))))

let view (_computer : computer) (m : model) : shape list =
  [ rectangle (rgb 60 60 72) 1000. 1000. ]
  @ [
      txt 26. grey "TinySoundtracker" |> move (-300.) 462.;
      txt 16. grey
        (Printf.sprintf "position %02d/%02d   pattern %02d   octave %d   instrument %02d" m.position (Array.length m.song.positions) (pattern_of m) m.octave m.instrument)
      |> move (-220.) 420.;
      txt 16. ink m.song.title |> move (-300.) 385.;
      txt 16. (rgb 230 200 120) m.said |> move (-220.) 355.;
      txt 14. grey "order list:  , .  - =  Enter" |> move (-340.) 328.;
      txt 14. dim "piano z-m q-i   hex 0-f   arrows   Tab octave   [ ] instrument   space play   ctrl-s export" |> move 0. (-470.);
    ]
  @ order_view m @ instruments_view m @ pattern_view m

let app (caps : < Cap.open_out >) = game view (update caps) initial_model
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> < Cap.open_out >)))
