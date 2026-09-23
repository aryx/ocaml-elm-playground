(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy media player, after Windows' Media Player (Microsoft, 1991,
 * playing .wav and .mid files through MCI) and what VLC (VideoLAN,
 * 2001) made of the idea: one program for every kind of file, a
 * playlist, and whatever the file is shown its own way -- here every
 * format this repository reads: a recording (WAV), tunes (MIDI, ABC,
 * solfege), a song with its instruments (MOD), pictures (PNG, JPEG,
 * XPM) and movies (so far an animated GIF's frames; the video formats
 * as graphics/videos/ grows, plan_video_teaching.md).
 *
 * The file's kind is found from its bytes, not its name (Media.mli: the
 * magic numbers file(1) and VLC's demuxers look for), and each kind is
 * shown as what it is:
 *
 *   - a tune: a piano roll, its notes as bars going by the playhead, a
 *     color per channel (its notes read from the MIDI file it is, or
 *     becomes: Midi.of_tune);
 *   - a recording: its whole wave, the playhead crossing it;
 *   - a module: its four channels around the row playing, as a tracker
 *     shows them (TinySoundtracker.ml);
 *   - a picture: fitted to the screen, for 5 s; a movie, each frame at
 *     its time, decoded when shown (Movie.mli), looped for 5 s if shorter;
 *     d shows it as what changed from a frame to the next, the rest
 *     dimmed: what a delta frame (FLC's) stores.
 *
 * Under it, what just played, as an oscilloscope and a spectrum; then
 * the position (a slider: drag it to seek), the buttons, and the
 * playlist -- click an item to play it. Keys: space play or pause, n
 * and p the next and the previous item, s stop, the arrows left and
 * right 5 s back and on. When an item ends, the next one plays.
 *
 * The playlist starts with media of our own (Our_media.ml); file= adds
 * yours in front, from a file or a URL, several separated by commas:
 *   dune exec apps/media/TinyMediaPlayer.exe -- file=song.mid,photo.png
 *
 * Uses: Media (the kinds, opening them), Our_media, Audio (the player as
 * an instrument the mixer pulls; fetch, for file=), Mod_player, Gui (the
 * slider, the buttons, the list), Sprite.of_rgba (the pictures),
 * Spectrum. Not: Scene2d, the physics, the File menu.
 *
 * Exercises: MIDI's pitch bend and control changes (a channel's volume
 * and pan) heard; the channels of a tune muted from the piano roll; a
 * playlist saved and opened; the formats of plan_audio_formats.md as
 * they come (AU, AIFF, FLAC, MML): a line each in Media.ml.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The deck: what plays, on the sound's side *)
(*****************************************************************************)

(* the item playing, where it is, paused or not; the last samples, for
 * the scope and the spectrum. It lives with the sound, not in the model:
 * the mixer pulls its blocks between frames. *)
type deck = {
  mutable media : Media.media option;
  mutable pos : int; (* a sound's samples played *)
  mutable player : Mod_player.t option;
  mutable paused : bool;
  mutable finished : bool;
  ring : float array;
  mutable at : int;
}

let deck = { media = None; pos = 0; player = None; paused = true; finished = false; ring = Array.make 2048 0.; at = 0 }

let fill (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  Array.fill out.left 0 n 0.;
  Array.fill out.right 0 n 0.;
  if not deck.paused then (
    match (deck.media, deck.player) with
    | Some (Sound s), _ ->
        let len = Array.length s.samples.left in
        let k = max 0 (min n (len -.. deck.pos)) in
        Array.blit s.samples.left deck.pos out.left 0 k;
        Array.blit s.samples.right deck.pos out.right 0 k;
        deck.pos <- deck.pos +.. k;
        if deck.pos >= len then deck.finished <- true
    | Some (Module _), Some p ->
        Mod_player.fill p out;
        if Mod_player.finished p then deck.finished <- true
    | _ -> ());
  for i = 0 to n -.. 1 do
    deck.ring.(deck.at) <- (out.left.(i) + out.right.(i)) / 2.;
    deck.at <- (deck.at +.. 1) mod 2048
  done

let recent () : Signal.t = Array.init 2048 (fun i -> deck.ring.((deck.at +.. i) mod 2048))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  items : (string * string) list; (* the playlist: names and bytes *)
  current : int;
  opened : (Media.kind * Media.media, string) result;
  playing : bool;
  shown : int; (* the frames the current item has been on the screen *)
  held : string list;
  asked : bool; (* file= asked for *)
  changes : bool; (* a movie shown as what changed from a frame to the next *)
}

(* files given by file=, arriving (now natively, later in a browser) *)
let arrived : (string * string) list ref = ref []

(* the item [i] put on the deck, playing or not *)
let load (items : (string * string) list) (i : int) ~(playing : bool) : model -> model =
 fun m ->
  let name, bytes = List.nth items i in
  let opened = Media.open_ ~name bytes in
  deck.media <- (match opened with Ok (_, media) -> Some media | Error _ -> None);
  deck.pos <- 0;
  deck.player <- (match opened with Ok (_, Module song) -> Some (Mod_player.create ~loop:false song) | _ -> None);
  deck.finished <- false;
  deck.paused <- not playing;
  { m with items; current = i; opened; playing; shown = 0 }

let initial_model : model =
  let m = { items = []; current = 0; opened = Error ""; playing = true; shown = 0; held = []; asked = false; changes = false } in
  load Our_media.playlist 0 ~playing:true m

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let picture_frames = 300 (* 5 s *)

let seek (m : model) (fraction : float) : unit =
  match (deck.media, deck.player) with
  | Some (Sound s), _ -> deck.pos <- int_of_float (fraction * float_of_int (Array.length s.samples.left))
  | Some (Module song), Some p ->
      Mod_player.seek p ~position:(int_of_float (fraction * float_of_int (Array.length song.positions))) ~row:0;
      deck.finished <- false
  | _ -> ignore m

(* where it is, from 0 to 1 *)
let fraction () : float =
  match (deck.media, deck.player) with
  | Some (Sound s), _ -> float_of_int deck.pos / float_of_int (max 1 (Array.length s.samples.left))
  | Some (Module song), Some p ->
      let position, row = Mod_player.position p in
      (float_of_int position + (float_of_int row / 64.)) / float_of_int (Array.length song.positions)
  | _ -> 0.

let next (m : model) (step : int) : model =
  let n = List.length m.items in
  let i = m.current +.. step in
  if i < 0 || i >= n then (
    deck.paused <- true;
    { m with playing = false })
  else load m.items i ~playing:m.playing m

let playlist_box : Widget.box = { Widget.x = 370.; y = 170.; w = 230.; h = 460. }

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "mediaplayer" (fun () -> { Instrument.note_on = (fun _ _ -> ()); note_off = ignore; set = (fun _ _ -> ()); fill }));
  (* file=: asked for once; each file arriving goes in front *)
  let m =
    if m.asked then m
    else (
      (match List.assoc_opt "file" computer.flags with
      | Some files -> List.iter (fun f -> Audio.fetch f (fun b -> Option.iter (fun b -> arrived := !arrived @ [ (Filename.basename f, b) ]) b)) (String.split_on_char ',' files)
      | None -> ());
      { m with asked = true })
  in
  let m =
    if !arrived = [] then m
    else
      let items = !arrived @ m.items in
      arrived := [];
      load items 0 ~playing:true m
  in
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) in
  let m = { m with held = now; shown = m.shown +.. 1 } in
  (* the widgets: the playlist, the slider, the buttons *)
  let chosen = Gui.list_in computer playlist_box (List.map fst m.items) (Some m.current) in
  let before = fraction () in
  let after = Gui.slider computer ~at:(-120., -225.) ~from:0. ~to_:1. before in
  if after <> before then seek m after;
  let button label x = Gui.button computer ~at:(x, -290.) label in
  let prev = button "prev" (-330.) and play = button (if m.playing then "pause" else "play") (-220.) in
  let stop = button "stop" (-110.) and nxt = button "next" 0. in
  let m =
    match chosen with
    | Some i when i <> m.current -> load m.items i ~playing:true m
    | _ ->
        if play || pressed "space" then (
          deck.paused <- m.playing;
          { m with playing = not m.playing })
        else if stop || pressed "s" then (
          seek m 0.;
          deck.paused <- true;
          { m with playing = false })
        else if nxt || pressed "n" then next m 1
        else if prev || pressed "p" then next m (-1)
        else if pressed "d" then { m with changes = not m.changes }
        else if pressed "ArrowRight" || pressed "ArrowLeft" then (
          (match deck.media with
          | Some (Sound s) ->
              let d = if pressed "ArrowRight" then 5. else -5. in
              let len = Array.length s.samples.left in
              deck.pos <- max 0 (min (len -.. 1) (deck.pos +.. int_of_float (d * float_of_int Signal.rate)))
          | _ -> ());
          m)
        else m
  in
  (* the item over: the next one; a picture after its time *)
  let over =
    match m.opened with
    | Ok (_, Picture _) -> m.playing && m.shown >= picture_frames
    | Ok (_, Movie movie) -> m.playing && m.shown >= max picture_frames (int_of_float (movie.duration * 60.))
    | Ok _ -> deck.finished
    | Error _ -> m.playing && m.shown >= 60
  in
  if over then next m 1 else m

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let txt (size : number) (color : color) (s : string) : shape = words color s |> scale (size / 10.)
let ink = rgb 230 230 220
let dim = rgb 140 140 150
let panel = rgb 22 22 30

(* the view's box: x from -480 to 240, y from -80 to 400 *)
let vx = -120.
let vy = 160.
let vw = 700.
let vh = 460.

let segment (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 - x1) (y2 - y1) + 1.) 2.
  |> rotate (Float.atan2 (y2 - y1) (x2 - x1) * 180. / Float.pi)
  |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let channel_colors = [| rgb 240 120 90; rgb 90 180 240; rgb 130 220 120; rgb 230 200 90; rgb 200 130 230; rgb 90 220 210 |]

(* 8 s of notes, the playhead a quarter of the way *)
let piano_roll (notes : Midi.note list) (now : float) : shape list =
  let keys = List.map (fun (n : Midi.note) -> n.key) notes in
  let lo = List.fold_left min 127 keys -.. 2 and hi = List.fold_left max 0 keys +.. 2 in
  let span = float_of_int (max 12 (hi -.. lo)) in
  let x_of t = vx - (vw / 2.) + ((t - now + 2.) * vw / 8.) in
  let y_of k = vy - (vh / 2.) + 20. + (float_of_int (k -.. lo) * (vh - 40.) / span) in
  let bar (n : Midi.note) =
    let x0 = Float.max (vx - (vw / 2.)) (x_of n.start) and x1 = Float.min (vx + (vw / 2.)) (x_of (n.start + n.length)) in
    if x1 <= x0 then []
    else
      let lit = n.start <= now && now < n.start + n.length in
      let color = if lit then ink else channel_colors.(n.channel mod Array.length channel_colors) in
      [ rectangle color (x1 - x0 - 1.) (Float.max 4. ((vh - 40.) / span - 2.)) |> move ((x0 + x1) / 2.) (y_of n.key) ]
  in
  List.concat_map bar notes @ [ rectangle (rgb 250 250 250) 2. vh |> move (x_of now) vy ]

(* the whole recording, a column's lowest and highest sample *)
let waveform (samples : Signal.t) (fraction : float) : shape list =
  let cols = 350 in
  let n = Array.length samples in
  let col c =
    let a = c *.. n /.. cols and b = (c +.. 1) *.. n /.. cols in
    let lo = ref 0. and hi = ref 0. in
    for i = a to max a (b -.. 1) do
      if i < n then (
        lo := Float.min !lo samples.(i);
        hi := Float.max !hi samples.(i))
    done;
    let x = vx - (vw / 2.) + ((float_of_int c + 0.5) * vw / float_of_int cols) in
    rectangle (rgb 120 200 250) (vw / float_of_int cols) (Float.max 1. ((!hi - !lo) * vh * 0.45)) |> move x (vy + ((!hi + !lo) * vh * 0.225))
  in
  List.init cols col @ [ rectangle (rgb 250 250 250) 2. vh |> move (vx - (vw / 2.) + (fraction * vw)) vy ]

(* the module's four channels, nine rows around the one playing *)
let tracker (song : Mod.song) : shape list =
  match deck.player with
  | None -> []
  | Some p ->
      let position, row = Mod_player.position p in
      let pattern = song.patterns.(song.positions.(min position (Array.length song.positions -.. 1))) in
      let cell (c : Mod.cell) =
        Printf.sprintf "%s %s %s" (if c.period = 0 then "---" else Mod.note_name c.period) (if c.instrument = 0 then ".." else Printf.sprintf "%02d" c.instrument)
          (if c.effect = 0 && c.param = 0 then "..." else Printf.sprintf "%X%02X" c.effect c.param)
      in
      (rectangle (rgb 70 40 40) vw 34. |> move vx vy)
      :: List.concat
           (List.init 11 (fun k ->
                let r = row +.. k -.. 5 in
                if r < 0 || r > 63 then []
                else
                  let y = vy - (float_of_int (k -.. 5) * 38.) in
                  (txt 18. dim (Printf.sprintf "%02d" r) |> move (vx - 320.) y)
                  :: List.init 4 (fun c -> txt 18. (if r = row then ink else dim) (cell pattern.(r).(c)) |> move (vx - 190. + (float_of_int c * 160.)) y)))
      @ List.init 4 (fun c ->
            rectangle (rgb 90 200 90) (float_of_int (Mod_player.channel_volume p c) * 140. / 64.) 6. |> move (vx - 190. + (float_of_int c * 160.)) (vy + (vh / 2.) - 20.))

(* a whole number of pixels a pixel: at 8.75, the rows' edges fall
 * between pixels and the rasterizer leaves seams *)
let picture (img : Rgba_image.t) : shape list =
  let size = Float.floor (Float.min ((vw - 40.) / float_of_int img.width) ((vh - 40.) / float_of_int img.height)) in
  [ Sprite.of_rgba size img |> move vx vy ]

(* the frame the movie is at, [shown] frames (1/60 s) in, looping *)
let movie_frame (movie : Movie.t) (shown : int) : int = Movie.index_at movie (Float.rem (float_of_int shown / 60.) movie.duration)

(* what changed: the pixels the same as the frame before's at a quarter
 * of their brightness, the ones that changed as they are -- what a
 * delta frame stores (Fli.mli), seen; the first frame all changed *)
let changes (movie : Movie.t) (i : int) : Rgba_image.t =
  let img = movie.frame i in
  if i = 0 then img
  else
    let before = movie.frame (i -.. 1) in
    let out = Rgba_image.create ~width:img.width ~height:img.height in
    for p = 0 to (img.width *.. img.height) -.. 1 do
      let same = img.rgba.{4 *.. p} = before.rgba.{4 *.. p} && img.rgba.{(4 *.. p) +.. 1} = before.rgba.{(4 *.. p) +.. 1} && img.rgba.{(4 *.. p) +.. 2} = before.rgba.{(4 *.. p) +.. 2} in
      for c = 0 to 2 do out.rgba.{(4 *.. p) +.. c} <- (if same then img.rgba.{(4 *.. p) +.. c} /.. 4 else img.rgba.{(4 *.. p) +.. c}) done;
      out.rgba.{(4 *.. p) +.. 3} <- 255
    done;
    out

let movie (m : model) (movie : Movie.t) : shape list =
  let i = movie_frame movie m.shown in
  picture (if m.changes then changes movie i else movie.frame i)

let scope_and_spectrum () : shape list =
  let samples = recent () in
  let w = 440. and h = 70. and y = -140. in
  let start = ref 0 in
  (try
     for i = 1 to 1023 do
       if samples.(i -.. 1) < 0. && samples.(i) >= 0. then (
         start := i;
         raise Exit)
     done
   with Exit -> ());
  let point j = (-250. - (w / 2.) + (float_of_int j * w / 128.), y + (samples.(!start +.. (j *.. 8)) * h)) in
  let mags = Spectrum.of_signal samples in
  let n = 2 *.. (Array.length mags -.. 1) in
  let bars = 44 in
  let freq b = 20. * (1000. ** (float_of_int b / float_of_int bars)) in
  let bar b =
    let lo = freq b and hi = freq (b +.. 1) in
    let top = ref 0. in
    Array.iteri (fun k v -> let f = Spectrum.bin_frequency ~n k in if f >= lo && f < hi && v > !top then top := v) mags;
    let db = if !top <= 0. then -80. else Float.max (-80.) (20. * log10 !top) in
    let bh = (db + 80.) / 80. * h in
    rectangle (rgb 250 190 80) (w / float_of_int bars - 2.) (Float.max 1. bh) |> move (10. + ((float_of_int b + 0.5) * w / float_of_int bars)) (y - (h / 2.) + (bh / 2.))
  in
  [ rectangle panel w h |> move (-250.) y; rectangle panel w h |> move 230. y ]
  @ List.init 128 (fun j -> segment (rgb 120 255 140) (point j) (point (j +.. 1)))
  @ List.init bars bar

let clock (seconds : float) : string = Printf.sprintf "%d:%02d" (int_of_float seconds /.. 60) (int_of_float seconds mod 60)

let where (m : model) : string =
  match m.opened with
  | Error e -> e
  | Ok (_, Sound s) -> Printf.sprintf "%s / %s" (clock (float_of_int deck.pos / float_of_int Signal.rate)) (clock (Float.round (float_of_int (Array.length s.samples.left) / float_of_int Signal.rate)))
  | Ok (_, Module song) -> (
      match deck.player with Some p -> let pos, row = Mod_player.position p in Printf.sprintf "position %d/%d, row %d" pos (Array.length song.positions) row | None -> "")
  | Ok (_, Picture img) -> Printf.sprintf "%d x %d pixels" img.width img.height
  | Ok (_, Movie movie) -> Printf.sprintf "frame %d of %d   d: %s" (movie_frame movie m.shown +.. 1) (Movie.frame_count movie) (if m.changes then "the frames" else "what changed")

let view (_computer : computer) (m : model) : shape list =
  let name = fst (List.nth m.items m.current) in
  let kind = match m.opened with Ok (k, _) -> Media.kind_name k | Error _ -> "?" in
  let shown =
    match m.opened with
    | Ok (_, Sound s) when s.notes <> [] -> piano_roll s.notes (float_of_int deck.pos / float_of_int Signal.rate)
    | Ok (_, Sound s) -> waveform s.samples.left (fraction ())
    | Ok (_, Module song) -> tracker song
    | Ok (_, Picture img) -> picture img
    | Ok (_, Movie mv) -> movie m mv
    | Error e -> [ txt 18. ink e |> move vx vy ]
  in
  [ rectangle (rgb 50 50 62) 1000. 1000.; rectangle panel vw vh |> move vx vy ]
  @ shown
  @ [
      txt 26. ink "TinyMediaPlayer" |> move (-310.) 462.;
      txt 18. ink (Printf.sprintf "%s   (%s)" name kind) |> move (-120.) 420.;
      txt 16. dim (where m) |> move 250. (-225.);
      txt 14. dim "space play/pause   n p next previous   s stop   arrows seek   click the playlist" |> move 0. (-470.);
    ]
  @ scope_and_spectrum () @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
