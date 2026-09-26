(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Winamp, after Winamp 2 (Justin Frankel's Nullsoft, 1998; the
 * first Winamp, 1997): the MP3 player of the MP3 years, "it really
 * whips the llama's ass".
 * Where TinyMediaPlayer shows each file as what it is, Winamp shows
 * every song the same way, in three small windows stacked, drawn here
 * in its double-size mode (Ctrl-D: each of the skin's pixels a 2 x 2
 * square), 275 pixels wide:
 *
 *   +--------------------------------------+
 *   | WINAMP                               |  the main window: the time
 *   | > 00:42   1. OUR SYNTH - BELL AND...  |  in an LCD's digits, the
 *   | ||||:.|   128 KBPS 44 KHZ  MONO STEREO|  title scrolling, the
 *   | [volume][bal]              [EQ][PL] |  analyzer, kbps and kHz,
 *   | [========O=====================]     |  the sliders, the seek
 *   | |<  >  ||  []  >|  ^   SHUFFLE REP   |  bar, the buttons
 *   +--------------------------------------+
 *   | WINAMP EQUALIZER                     |  ten bands and the preamp
 *   | ON AUTO   ~~~curve~~~       PRESETS  |  (Graphic_eq.mli), the
 *   | |  |  |  |  |  |  |  |  |  |  |     |  curve they make
 *   +--------------------------------------+
 *   | WINAMP PLAYLIST                      |  the songs, the one
 *   | 1. OUR SYNTH - BELL AND CHIRPS  0:01 |  playing white, the
 *   | 2. ...                               |  selected one on blue
 *   +--------------------------------------+
 *
 * What is Winamp's here, and what each teaches:
 *
 *   - the tags: an MP3's title is not its file's name but its ID3 tag
 *     (Id3.mli): "artist - title", read when the file is added;
 *   - the skin: every color, place and size is data (the [skin] record
 *     and the constants beside each widget), as Winamp's were bitmaps
 *     in a zip (a .wsz: main.bmp, cbuttons.bmp, numbers.bmp,
 *     text.bmp...) -- whose text.bmp, a 5 x 6 cell per character,
 *     uppercase only, is our [font];
 *   - the analyzer: 19 bars from the spectrum (Spectrum.mli) of the last
 *     samples, on a log scale of frequencies, in decibels, falling
 *     slower than they rise, and a peak on each bar that falls slower
 *     still -- the look of a hi-fi's LED meters; clicked, the
 *     oscilloscope, clicked again, nothing. Winamp handed its plug-ins
 *     576 samples and 576 bins a frame (its vis API); the view here
 *     reads 1024 samples. The analyzer shows the sound before the
 *     volume, as Winamp's did: turned down, the bars stay;
 *   - the equalizer (Graphic_eq.mli): applied as the sound is pulled,
 *     so a slider moved is heard at once;
 *   - the time: an LCD's seven segments, the unlit ones faintly there;
 *     clicked, the time remaining (a minus); blinking when paused;
 *   - dragging a slider writes what it does where the title scrolls
 *     ("VOLUME: 72%", "EQ: 3KHZ: +4.0 DB"), and the seek bar seeks
 *     only when let go.
 *
 * Keys, Winamp's: z x c v b the buttons prev, play, pause, stop and
 * next (the bottom row of a QWERTY keyboard, left to right, as they sit
 * on the screen); the arrows left and right 5 s back and on, up and
 * down the volume; s shuffle, r repeat. A click on the playlist selects,
 * a double click plays; the EQ and PL buttons show and hide their
 * windows. When a song ends the next plays, a random one with shuffle,
 * the first after the last with repeat.
 *
 * The playlist starts with the sounds of our own (Our_media.ml), the
 * MP3 and MP2 tagged with ID3v1 here (the MP3 already has ffmpeg's
 * ID3v2, with only its encoder: the title comes from the v1); file=
 * adds yours in front, and so does a bare argument, as in
 * TinyMediaPlayer:
 *   dune exec apps/media/TinyWinamp.exe -- song.mp3 other.mp3
 *
 * Uses: Media (opening: MP3 and MP2 decoded whole, tunes rendered,
 * modules played live by Mod_player), Our_media, Id3, Graphic_eq,
 * Spectrum, Audio (the deck as an instrument the mixer pulls; fetch,
 * for file=), Lehmer (shuffle). Not: Gui (the skin draws its own
 * widgets), Sprite, the File menu.
 *
 * Exercises: skins read from a real .wsz (a zip: compression's Inflate;
 * then BMP, a format to add to graphics/images); the playlist saved and
 * read as .m3u ("#EXTINF:seconds,title" before each file) or .pls; the
 * windowshade mode (a window rolled up to its title bar, the time and
 * the seek bar in it); the EQ's AUTO (a preset remembered per song);
 * the crossfade and the gap between songs removed (Mpeg_audio.mli: the
 * encoder's delay and padding); the llama's "demo.mp3", with our own
 * voice synthesis. *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The skin *)
(*****************************************************************************)

(* Winamp 2's base skin, its colors: the frames' grey-blue metal, the
 * LCD's green on black, the playlist's own four (pledit.txt's Normal,
 * Current, NormalBG, SelectedBG) *)
type skin = {
  body : color;
  light : color; (* a bevel's lit edges, up and left *)
  shade : color; (* its dark ones *)
  face : color; (* a button's *)
  icon : color;
  title : color; (* the title bars' stripes *)
  lcd : color;
  ghost : color; (* an LCD's unlit segment *)
  text : color; (* the playlist's *)
  current : color;
  selected : color;
  peak : color;
}

let skin : skin =
  {
    body = rgb 44 44 62;
    light = rgb 110 110 140;
    shade = rgb 16 16 24;
    face = rgb 150 150 170;
    icon = rgb 30 30 44;
    title = rgb 190 170 100;
    lcd = rgb 0 232 0;
    ghost = rgb 0 44 0;
    text = rgb 0 255 0;
    current = rgb 255 255 255;
    selected = rgb 0 0 198;
    peak = rgb 200 200 210;
  }

(* the windows in the skin's pixels, x right and y down from the main
 * window's top left, each pixel [px] of ours: the three windows 275
 * wide, 116, 116 and 232 high, 464 in all *)
let px = 2.
let sx (x : number) : number = (x - 137.5) * px
let sy (y : number) : number = (232. - y) * px

(* a rectangle at [x], [y] (its top left), [w] by [h] skin pixels *)
let box (color : color) (x : number) (y : number) (w : number) (h : number) : shape =
  rectangle color (w * px) (h * px) |> move (sx (x + (w / 2.))) (sy (y + (h / 2.)))

let poly (color : color) (points : (number * number) list) : shape = polygon color (List.map (fun (x, y) -> (sx x, sy y)) points)

(* raised (a button, a frame) or sunk (pressed, an LCD's hole) *)
let bevel ?(fill = skin.face) ~(up : bool) (x : number) (y : number) (w : number) (h : number) : shape list =
  let a, b = if up then (skin.light, skin.shade) else (skin.shade, skin.light) in
  [ box a x y w h; box b (x + 1.) (y + 1.) (w - 1.) (h - 1.); box fill (x + 1.) (y + 1.) (w - 2.) (h - 2.) ]

let inside ((mx, my) : number * number) (x : number) (y : number) (w : number) (h : number) : bool =
  mx >= x && mx < x + w && my >= y && my < y + h

(*****************************************************************************)
(* The font: text.bmp's *)
(*****************************************************************************)

(* each character 4 x 5 dots in a 5 x 6 cell, uppercase only: Winamp
 * wrote every title in capitals, having no others *)
let glyphs : (char * string) list =
  [
    ('A', ".xx.x..xxxxxx..xx..x"); ('B', "xxx.x..xxxx.x..xxxx."); ('C', ".xxxx...x...x....xxx");
    ('D', "xxx.x..xx..xx..xxxx."); ('E', "xxxxx...xxx.x...xxxx"); ('F', "xxxxx...xxx.x...x...");
    ('G', ".xxxx...x.xxx..x.xxx"); ('H', "x..xx..xxxxxx..xx..x"); ('I', "xxx..x...x...x..xxx.");
    ('J', "..xx...x...xx..x.xx."); ('K', "x..xx.x.xx..x.x.x..x"); ('L', "x...x...x...x...xxxx");
    ('M', "x..xxxxxxxxxx..xx..x"); ('N', "x..xxx.xx.xxx..xx..x"); ('O', ".xx.x..xx..xx..x.xx.");
    ('P', "xxx.x..xxxx.x...x..."); ('Q', ".xx.x..xx..xx.x..x.x"); ('R', "xxx.x..xxxx.x.x.x..x");
    ('S', ".xxxx....xx....xxxx."); ('T', "xxx..x...x...x...x.."); ('U', "x..xx..xx..xx..x.xx.");
    ('V', "x..xx..xx..x.xx..xx."); ('W', "x..xx..xxxxxxxxxx..x"); ('X', "x..xx..x.xx.x..xx..x");
    ('Y', "x.x.x.x..x...x...x.."); ('Z', "xxxx...x.xx.x...xxxx");
    ('0', ".xx.x.xxxx.xx..x.xx."); ('1', ".x..xx...x...x..xxx."); ('2', "xxx....x.xx.x...xxxx");
    ('3', "xxx....x.xx....xxxx."); ('4', "x..xx..xxxxx...x...x"); ('5', "xxxxx...xxx....xxxx.");
    ('6', ".xx.x...xxx.x..x.xx."); ('7', "xxxx...x..x..x...x.."); ('8', ".xx.x..x.xx.x..x.xx.");
    ('9', ".xx.x..x.xxx...x.xx.");
    ('-', "........xxx........."); ('.', ".................x.."); (':', ".....x.......x......");
    ('(', "..x..x...x...x....x."); (')', ".x....x...x...x..x.."); ('/', "...x..x..x..x...x...");
    ('*', "....x.x..x..x.x....."); ('\'', ".x...x.............."); (',', ".............x..x...");
    ('_', "................xxxx"); ('&', ".x..x.x..x..x.x..x.x"); ('!', ".x...x...x.......x..");
    ('?', "xxx....x.xx.......x."); ('+', ".....x..xxx..x......"); ('=', "....xxx.....xxx.....");
    ('#', ".x.xxxxx.x.xxxxx.x.x"); ('[', "xx..x...x...x...xx.."); (']', "..xx...x...x...x..xx");
    ('%', "x..x...x..x..x..x..x");
  ]

(* the glyphs as runs of dots, a row's each: (row, first column, length) *)
let runs : (char, (int * int * int) list) Hashtbl.t =
  let t = Hashtbl.create 64 in
  List.iter
    (fun (c, dots) ->
      let rs = ref [] in
      for row = 0 to 4 do
        let col = ref 0 in
        while !col < 4 do
          if dots.[(row *.. 4) +.. !col] = 'x' then (
            let start = !col in
            while !col < 4 && dots.[(row *.. 4) +.. !col] = 'x' do incr col done;
            rs := (row, start, !col -.. start) :: !rs)
          else incr col
        done
      done;
      Hashtbl.replace t c !rs)
    glyphs;
  t

(* a string as Winamp shows it: capitals, a multi-byte UTF-8 character
 * a '?' (text.bmp has no others) *)
let shown (s : string) : string =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> if Char.code c >= 0xC0 then Buffer.add_char b '?' else if Char.code c < 0x80 then Buffer.add_char b (Char.uppercase_ascii c)) s;
  Buffer.contents b

(* [text color x y s]: [s] from [x], [y], 5 pixels a character; what
 * falls outside [clip] (from, to) cut, a dot at a time *)
let text ?clip (color : color) (x : number) (y : number) (s : string) : shape list =
  let lo, hi = match clip with Some c -> c | None -> (-1000., 1000.) in
  let s = shown s in
  List.concat
    (List.init (String.length s) (fun i ->
         let cx = x + (5. * float_of_int i) in
         if cx + 4. < lo || cx > hi then []
         else
           List.filter_map
             (fun (row, col, len) ->
               let a = Float.max lo (cx + float_of_int col) and b = Float.min hi (cx + float_of_int (col +.. len)) in
               if b <= a then None else Some (box color a (y + float_of_int row) (b - a) 1.))
             (Option.value (Hashtbl.find_opt runs s.[i]) ~default:[])))

(*****************************************************************************)
(* The deck: what plays, on the sound's side *)
(*****************************************************************************)

(* the song playing, where it is, and what the sound goes through: the
 * EQ, the volume, the balance. It lives with the sound, not in the
 * model: the mixer pulls its blocks between frames, and [update] sets
 * the knobs. The ring keeps the last samples for the analyzer. *)
type deck = {
  mutable media : Media.media option;
  mutable pos : int;
  mutable played : int; (* samples played: a module's time *)
  mutable player : Mod_player.t option;
  mutable running : bool;
  mutable finished : bool;
  eq : Graphic_eq.t;
  mutable eq_on : bool;
  mutable preamp : float;
  mutable gains : float array;
  mutable volume : float;
  mutable balance : float;
  ring : float array;
  mutable at : int;
}

let ring_size = 2048

let deck =
  {
    media = None; pos = 0; played = 0; player = None; running = false; finished = false;
    eq = Graphic_eq.create (); eq_on = true; preamp = 0.; gains = Array.make 10 0.; volume = 0.8; balance = 0.;
    ring = Array.make ring_size 0.; at = 0;
  }

let samples_of (media : Media.media option) : Signal.stereo option = match media with Some (Sound s) -> Some s.samples | _ -> None

let fill (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  Array.fill out.left 0 n 0.;
  Array.fill out.right 0 n 0.;
  if deck.running then (
    deck.played <- deck.played +.. n;
    match (samples_of deck.media, deck.player) with
    | Some samples, _ ->
        let len = Array.length samples.left in
        let k = max 0 (min n (len -.. deck.pos)) in
        Array.blit samples.left deck.pos out.left 0 k;
        Array.blit samples.right deck.pos out.right 0 k;
        deck.pos <- deck.pos +.. k;
        if deck.pos >= len then deck.finished <- true
    | None, Some p ->
        Mod_player.fill p out;
        if Mod_player.finished p then deck.finished <- true
    | None, None -> ());
  if deck.eq_on then Graphic_eq.process deck.eq ~preamp:deck.preamp ~gains:deck.gains out;
  (* the analyzer's samples, before the volume *)
  for i = 0 to n -.. 1 do
    deck.ring.(deck.at) <- (out.left.(i) + out.right.(i)) / 2.;
    deck.at <- (deck.at +.. 1) mod ring_size
  done;
  (* the volume squared: loudness grows slower than the amplitude; the
   * balance turns one side down, never the other up *)
  let v = deck.volume * deck.volume in
  let l = v * Float.min 1. (1. - deck.balance) and r = v * Float.min 1. (1. + deck.balance) in
  for i = 0 to n -.. 1 do
    out.left.(i) <- out.left.(i) * l;
    out.right.(i) <- out.right.(i) * r
  done

let recent (n : int) : Signal.t = Array.init n (fun i -> deck.ring.((deck.at +.. ring_size -.. n +.. i) mod ring_size))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type item = {
  name : string;
  bytes : string Lazy.t;
  title : string; (* the tag's, or the file's name *)
  duration : float option; (* known once played *)
}

type status = Playing | Paused | Stopped
type vis = Analyzer | Scope | Off

type model = {
  items : item list;
  current : int;
  selected : int;
  opened : (Media.kind * Media.media, string) result;
  status : status;
  kbps : int option;
  khz : int option;
  stereo : bool;
  volume : float; (* 0 to 1 *)
  balance : float; (* -1 left to 1 right *)
  eq_on : bool;
  preamp : float;
  gains : float array;
  preset : int;
  eq_shown : bool;
  pl_shown : bool;
  shuffle : bool;
  repeat : bool;
  seed : Lehmer.t;
  vis : vis;
  remaining : bool;
  bars : float array; (* the analyzer's 19, 0 to 16 pixels *)
  peaks : float array;
  press : (number * number) option; (* where the button went down, in skin pixels *)
  held : string list;
  frame : int;
  asked : bool;
}

let item (name : string) (bytes : string Lazy.t) : item =
  let b = Lazy.force bytes in
  let title =
    match Media.sniff ~name b with
    | Some (Mp3 | Mp2) -> Id3.display ~name (Id3.read b)
    (* a module's title, its first 20 bytes: Winamp's MOD plug-in's *)
    | Some Mod -> ( match Media.open_ ~name b with Ok (_, Module { title; _ }) when String.trim title <> "" -> String.trim title | _ -> Id3.display ~name None)
    | _ -> Id3.display ~name None
  in
  { name; bytes; title; duration = None }

(* our sounds, the MPEG ones first and tagged: Winamp's own *)
let our_playlist : item list =
  let ours name = List.assoc name Our_media.playlist in
  let tagged name (tag : Id3.t) = item name (lazy (Lazy.force (ours name) ^ Id3.v1_to_string tag)) in
  let tag title track : Id3.t = { title; artist = "Our Synth"; album = "Tiny Hits"; year = "2026"; track = Some track } in
  [
    tagged "lame_encoded.mp3" (tag "Bell and Chirps" 1);
    tagged "twolame_encoded.mp2" (tag "Bell and Chirps (MP2)" 2);
  ]
  @ List.map
      (fun name -> item name (ours name))
      [ "tiny_soundtracker.mod"; "bell.wav"; "frere_jacques.mid"; "frere_jacques.abc"; "la_lune.doremi" ]

(* the numbers under the title: the bitrate and sample rate, from an
 * MPEG file's first frame; a WAV's from its header (bytes 24 and 28:
 * the rate and bytes a second); none for what is synthesized *)
let about (kind : Media.kind) (bytes : string) (samples : Signal.stereo option) : int option * int option * bool =
  let le32 at = if String.length bytes < at +.. 4 then 0 else Char.code bytes.[at] lor (Char.code bytes.[at +.. 1] lsl 8) lor (Char.code bytes.[at +.. 2] lsl 16) lor (Char.code bytes.[at +.. 3] lsl 24) in
  let stereo = match samples with Some s -> not (s.left == s.right || s.left = s.right) | None -> true in
  match kind with
  | Mp2 | Mp3 -> (
      match Mpeg_audio_header.first_frame bytes with
      | Some (_, h) -> (Some (h.bitrate /.. 1000), Some (h.sample_rate /.. 1000), h.channels = 2)
      | None -> (None, None, stereo))
  | Wav -> (Some (le32 28 *.. 8 /.. 1000), Some (le32 24 /.. 1000), stereo)
  | _ -> (None, None, stereo)

let replace (items : item list) (i : int) (it : item) : item list = List.mapi (fun k x -> if k = i then it else x) items

(* the song [i] put on the deck, playing or not *)
let load (m : model) (i : int) ~(status : status) : model =
  let it = List.nth m.items i in
  let bytes = Lazy.force it.bytes in
  let opened = Media.open_ ~name:it.name bytes in
  let opened = match opened with Ok (_, (Picture _ | Movie _)) -> Error "not a sound" | o -> o in
  deck.media <- (match opened with Ok (_, media) -> Some media | Error _ -> None);
  deck.pos <- 0;
  deck.played <- 0;
  deck.player <- (match opened with Ok (_, Module song) -> Some (Mod_player.create ~loop:false song) | _ -> None);
  (* a file that isn't a sound: over at once, skipped *)
  deck.finished <- Result.is_error opened;
  deck.running <- status = Playing;
  let kbps, khz, stereo = match opened with Ok (kind, media) -> about kind bytes (samples_of (Some media)) | Error _ -> (None, None, false) in
  let duration = match opened with Ok (_, media) -> Media.duration media | Error _ -> None in
  let items = replace m.items i { it with duration } in
  { m with items; current = i; selected = i; opened; status; kbps; khz; stereo }

let initial_model : model =
  let m =
    {
      items = our_playlist; current = 0; selected = 0; opened = Error ""; status = Playing; kbps = None; khz = None; stereo = false;
      volume = 0.8; balance = 0.; eq_on = true; preamp = 0.; gains = Array.make 10 0.; preset = 0; eq_shown = true; pl_shown = true;
      shuffle = false; repeat = false; seed = Lehmer.of_int 1997; vis = Analyzer; remaining = false;
      bars = Array.make 19 0.; peaks = Array.make 19 0.; press = None; held = []; frame = 0; asked = false;
    }
  in
  load m 0 ~status:Playing

(*****************************************************************************)
(* Where it is *)
(*****************************************************************************)

(* a module's time is what has played: it has no samples to count *)
let seconds () : float = float_of_int (if deck.player = None then deck.pos else deck.played) / float_of_int Signal.rate

(* from 0 to 1; a module's by its positions and rows *)
let fraction () : float =
  match (samples_of deck.media, deck.media, deck.player) with
  | Some samples, _, _ -> float_of_int deck.pos / float_of_int (max 1 (Array.length samples.left))
  | None, Some (Module song), Some p ->
      let position, row = Mod_player.position p in
      (float_of_int position + (float_of_int row / 64.)) / float_of_int (Array.length song.positions)
  | _ -> 0.

let seek (fraction : float) : unit =
  match (samples_of deck.media, deck.media, deck.player) with
  | Some samples, _, _ -> deck.pos <- int_of_float (fraction * float_of_int (Array.length samples.left -.. 1))
  | None, Some (Module song), Some p ->
      Mod_player.seek p ~position:(min (Array.length song.positions -.. 1) (int_of_float (fraction * float_of_int (Array.length song.positions)))) ~row:0;
      deck.finished <- false
  | _ -> ()

let clock (s : float) : string = Printf.sprintf "%d:%02d" (int_of_float s /.. 60) (int_of_float s mod 60)

(*****************************************************************************)
(* update *)
(*****************************************************************************)

(* the widgets' places, in skin pixels: Winamp 2's base skin *)
let vis_box = (24., 43., 76., 16.)
let time_box = (36., 26., 63., 13.)
let volume_box = (107., 57., 68., 13.)
let balance_box = (177., 57., 38., 13.)
let seek_box = (16., 72., 248., 10.)
let buttons = [ ("prev", 16., 23.); ("play", 39., 23.); ("pause", 62., 23.); ("stop", 85., 23.); ("next", 108., 22.) ]
let eject_box = (136., 89., 22., 16.)
let shuffle_box = (164., 89., 47., 15.)
let repeat_box = (210., 89., 28., 15.)
let eq_button = (219., 58., 23., 12.)
let pl_button = (242., 58., 23., 12.)

(* the equalizer, 116 lower *)
let eq_y = 116.
let on_box = (14., eq_y + 18., 26., 12.)
let presets_box = (217., eq_y + 18., 44., 12.)
let graph_box = (86., eq_y + 17., 113., 19.)
let slider_x (k : int) : number = if k < 0 then 21. else 78. + (18. * float_of_int k) (* -1: the preamp *)
let slider_y = eq_y + 38.
let slider_h = 63.

(* the playlist, 232 lower *)
let pl_y = 232.
let rows = 18
let row_h = 9.
let row_y (r : int) : number = pl_y + 22. + (row_h * float_of_int r)

let in_box (p : number * number) ((x, y, w, h) : number * number * number * number) : bool = inside p x y w h

(* what's under the point where the button went down: what a drag moves *)
type target = Volume | Balance | Seek | Band of int (* -1: the preamp *) | Nothing

let target_at (m : model) (p : number * number) : target =
  if in_box p volume_box then Volume
  else if in_box p balance_box then Balance
  else if in_box p seek_box && m.status <> Stopped then Seek
  else if m.eq_shown then
    match List.find_opt (fun k -> inside p (slider_x k) slider_y 14. slider_h) (List.init 11 (fun k -> k -.. 1)) with
    | Some k -> Band k
    | None -> Nothing
  else Nothing

(* a slider's value under the pointer: its thumb's center on it *)
let along (x : number) (from : number) (w : number) (thumb : number) : number = clamp 0. 1. ((x - from - (thumb / 2.)) / (w - thumb))
let volume_at (x : number) : number = let bx, _, bw, _ = volume_box in along x bx bw 14.
let balance_at (x : number) : number =
  let bx, _, bw, _ = balance_box in
  let b = (along x bx bw 14. * 2.) - 1. in
  (* the middle sticky, as Winamp's *)
  if Float.abs b < 0.12 then 0. else b
let seek_at (x : number) : number = let bx, _, bw, _ = seek_box in along x bx bw 29.
let gain_at (y : number) : number = Graphic_eq.range * (1. - (2. * clamp 0. 1. ((y - slider_y - 5.5) / (slider_h - 11.))))

(* the next song: a random other one with shuffle; past the last, the
 * first with repeat, else the end *)
let next (m : model) (step : int) : model =
  let n = List.length m.items in
  if m.shuffle && n > 1 then (
    let seed = Lehmer.next m.seed in
    let i = int_of_float (Lehmer.to_unit seed * float_of_int (n -.. 1)) in
    let i = if i >= m.current then i +.. 1 else i in
    load { m with seed } i ~status:m.status)
  else
    let i = m.current +.. step in
    if i >= 0 && i < n then load m i ~status:m.status
    else if i < 0 then load m 0 ~status:m.status
    else if m.repeat then load m ((i +.. n) mod n) ~status:m.status
    else (
      deck.running <- false;
      seek 0.;
      { m with status = Stopped })

let play (m : model) : model =
  match m.status with
  | Paused ->
      deck.running <- true;
      { m with status = Playing }
  | Playing | Stopped -> load m m.current ~status:Playing

let pause (m : model) : model =
  match m.status with
  | Playing ->
      deck.running <- false;
      { m with status = Paused }
  | Paused ->
      deck.running <- true;
      { m with status = Playing }
  | Stopped -> m

let stop (m : model) : model =
  deck.running <- false;
  seek 0.;
  { m with status = Stopped }

(* the analyzer's bars: the loudest bin in each of 19 bands, spaced
 * alike on a log scale from 40 Hz to 16 kHz, in dB from -60 to 0 as 0
 * to 16 pixels; a bar falls at most 0.6 pixel a frame, a peak 0.15 *)
let analyze (m : model) : model =
  let mags = Spectrum.of_signal (recent 1024) in
  let n = 2 *.. (Array.length mags -.. 1) in
  let f b = 40. * (400. ** (float_of_int b / 19.)) in
  let target b =
    let top = ref 0. in
    Array.iteri (fun k v -> let fk = Spectrum.bin_frequency ~n k in if fk >= f b && fk < f (b +.. 1) && v > !top then top := v) mags;
    if !top <= 0. then 0. else clamp 0. 16. (((20. * log10 !top) + 60.) * 16. / 60.)
  in
  let bars = Array.mapi (fun b old -> Float.max (target b) (old - 0.6)) m.bars in
  let peaks = Array.mapi (fun b old -> Float.max bars.(b) (old - 0.15)) m.peaks in
  { m with bars; peaks }

(* file=: asked once; each file arriving goes in front and plays *)
let arrived : (string * string Lazy.t) list ref = ref []

let files (computer : computer) (m : model) : model =
  let m =
    if m.asked then m
    else (
      let fs =
        List.concat_map (fun (k, v) -> match (k, v) with "file", fs -> String.split_on_char ',' fs | f, "" -> [ f ] | _ -> []) computer.flags
      in
      List.iter (fun f -> Audio.fetch f (fun b -> Option.iter (fun b -> arrived := !arrived @ [ (Filename.basename f, Lazy.from_val b) ]) b)) fs;
      { m with asked = true })
  in
  if !arrived = [] then m
  else
    let items = List.map (fun (name, bytes) -> item name bytes) !arrived @ m.items in
    arrived := [];
    load { m with items } 0 ~status:Playing


(* the first row shown: a longer playlist scrolls to show the selected *)
let first_row (m : model) : int = max 0 (m.selected -.. rows +.. 1)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "winamp" (fun () -> { Instrument.note_on = (fun _ _ -> ()); note_off = ignore; set = (fun _ _ -> ()); fill }));
  let m = files computer m in
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) in
  (* the pointer in skin pixels *)
  let p = ((computer.mouse.mx / px) + 137.5, 232. - (computer.mouse.my / px)) in
  let before = m.press in
  let press = if computer.mouse.mdown then (match before with None -> Some p | q -> q) else None in
  (* a click: let go where it went down *)
  let clicked b = computer.mouse.mclick && in_box p b && match before with Some q -> in_box q b | None -> true in
  let m = { m with held = now; press; frame = m.frame +.. 1 } in
  (* a drag moves its slider while the button is down; the seek bar
   * seeks when let go *)
  let m =
    match (press, before) with
    | Some q, _ -> (
        match target_at m q with
        | Volume -> { m with volume = volume_at (fst p) }
        | Balance -> { m with balance = balance_at (fst p) }
        | Band k when k < 0 -> { m with preamp = gain_at (snd p) }
        | Band k -> { m with gains = Array.mapi (fun j g -> if j = k then gain_at (snd p) else g) m.gains }
        | Seek | Nothing -> m)
    | None, Some q when target_at m q = Seek ->
        seek (seek_at (fst p));
        m
    | None, _ -> m
  in
  let button name = List.exists (fun (n, x, w) -> n = name && clicked (x, 88., w, 18.)) buttons in
  let row = if m.pl_shown then List.find_opt (fun r -> clicked (12., row_y r - 1.5, 243., row_h)) (List.init rows Fun.id) else None in
  let m =
    if button "prev" || pressed "z" then next m (-1)
    else if button "play" || pressed "x" then play m
    else if button "pause" || pressed "c" then pause m
    else if button "stop" || pressed "v" then stop m
    else if button "next" || pressed "b" then next m 1
    else if clicked shuffle_box || pressed "s" then { m with shuffle = not m.shuffle }
    else if clicked repeat_box || pressed "r" then { m with repeat = not m.repeat }
    else if clicked eq_button then { m with eq_shown = not m.eq_shown }
    else if clicked pl_button then { m with pl_shown = not m.pl_shown }
    else if clicked vis_box then { m with vis = (match m.vis with Analyzer -> Scope | Scope -> Off | Off -> Analyzer) }
    else if clicked time_box then { m with remaining = not m.remaining }
    else if m.eq_shown && clicked on_box then { m with eq_on = not m.eq_on }
    else if m.eq_shown && clicked presets_box then
      let k = (m.preset +.. 1) mod List.length Graphic_eq.presets in
      { m with preset = k; gains = Array.copy (snd (List.nth Graphic_eq.presets k)) }
    else if pressed "ArrowRight" || pressed "ArrowLeft" then (
      (match samples_of deck.media with
      | Some samples ->
          let d = if pressed "ArrowRight" then 5. else -5. in
          deck.pos <- max 0 (min (Array.length samples.left -.. 1) (deck.pos +.. int_of_float (d * float_of_int Signal.rate)))
      | None -> ());
      m)
    else
      match row with
      | Some r when first_row m +.. r < List.length m.items ->
          let i = first_row m +.. r in
          if computer.mouse.mdouble then load m i ~status:Playing else { m with selected = i }
      | _ -> m
  in
  (* the volume held up or down: a step a frame *)
  let m =
    if List.mem "ArrowUp" now then { m with volume = Float.min 1. (m.volume + 0.01) }
    else if List.mem "ArrowDown" now then { m with volume = Float.max 0. (m.volume - 0.01) }
    else m
  in
  (* the knobs, to the deck *)
  deck.volume <- m.volume;
  deck.balance <- m.balance;
  deck.eq_on <- m.eq_on;
  deck.preamp <- m.preamp;
  deck.gains <- m.gains;
  let m = analyze m in
  if m.status = Playing && deck.finished then
    (* a module's length, known once it has played to its end *)
    let m =
      match m.opened with
      | Ok (_, Module _) -> { m with items = replace m.items m.current { (List.nth m.items m.current) with duration = Some (seconds ()) } }
      | _ -> m
    in
    next m 1
  else m

(*****************************************************************************)
(* view: the main window *)
(*****************************************************************************)

let black = rgb 0 0 0

(* a title bar: the stripes, the name in the middle, three buttons *)
let title_bar (y : number) (name : string) : shape list =
  let w = 5. * float_of_int (String.length name) in
  let x0 = 137.5 - (w / 2.) in
  List.concat_map (fun k -> let ly = y + 3. + (2. * float_of_int k) in [ box skin.title 6. ly (x0 - 10.) 1.; box skin.title (x0 + w + 4.) ly (237. - x0 - w - 4.) 1. ]) [ 0; 1; 2; 3 ]
  @ text skin.title x0 (y + 4.) name
  @ List.concat_map (fun x -> bevel ~up:true x (y + 3.) 9. 9.) [ 245.; 254.; 264. ]

(* the LCD's seven segments, 9 by 13, the unlit ones faint *)
let digit (x : number) (y : number) (d : int option) : shape list =
  let lit =
    match d with
    | None -> ""
    | Some d -> [| "abcdef"; "bc"; "abdeg"; "abcdg"; "bcfg"; "acdfg"; "acdefg"; "abc"; "abcdefg"; "abcdfg" |].(d)
  in
  List.map
    (fun (s, (sx0, sy0, w, h)) -> box (if String.contains lit s then skin.lcd else skin.ghost) (x + sx0) (y + sy0) w h)
    [
      ('a', (1.5, 0., 6., 1.5)); ('b', (7.5, 1.5, 1.5, 4.25)); ('c', (7.5, 7.25, 1.5, 4.25)); ('d', (1.5, 11.5, 6., 1.5));
      ('e', (0., 7.25, 1.5, 4.25)); ('f', (0., 1.5, 1.5, 4.25)); ('g', (1.5, 5.75, 6., 1.5));
    ]

(* the time, or what's left (a minus); blank when stopped, blinking
 * when paused *)
let time (m : model) : shape list =
  let t =
    match (m.remaining, (List.nth m.items m.current).duration) with
    | true, Some d -> Float.max 0. (d - seconds ())
    | _ -> seconds ()
  in
  let on = m.status = Playing || (m.status = Paused && m.frame mod 60 < 30) in
  let t = int_of_float t in
  let ds = if on then [ Some (t /.. 600 mod 10); Some (t /.. 60 mod 10); Some (t mod 60 /.. 10); Some (t mod 10) ] else [ None; None; None; None ] in
  List.concat (List.map2 (fun x d -> digit x 26. d) [ 48.; 60.; 78.; 90. ] ds)
  @ [ box (if on && m.remaining then skin.lcd else skin.ghost) 37. 32. 7. 1.5 ]
  @ if on then [ box skin.lcd 73.5 29. 1.5 1.5; box skin.lcd 73.5 34. 1.5 1.5 ] else []

let status_icon (m : model) : shape list =
  match m.status with
  | Playing -> [ poly skin.lcd [ (26., 28.); (26., 36.); (32., 32.) ] ]
  | Paused -> [ box skin.lcd 26. 29. 2. 7.; box skin.lcd 30. 29. 2. 7. ]
  | Stopped -> [ box skin.lcd 26. 29. 7. 7. ]

(* the analyzer's colors, a row's each, from green at the bottom to red
 * at the top: viscolor.txt's *)
let bar_color (row : int) : color =
  let mix (r1, g1, b1) (r2, g2, b2) t = let c a b = int_of_float (a + ((b - a) * t)) in rgb (c r1 r2) (c g1 g2) (c b1 b2) in
  let t = float_of_int row / 15. in
  if t < 0.5 then mix (40., 210., 20.) (230., 220., 40.) (t * 2.) else mix (230., 220., 40.) (240., 40., 16.) ((t - 0.5) * 2.)

let vis (m : model) : shape list =
  let x0, y0, _, h = vis_box in
  match m.vis with
  | Off -> []
  | Analyzer ->
      List.concat
        (List.init 19 (fun b ->
             let x = x0 + (4. * float_of_int b) in
             let n = int_of_float m.bars.(b) in
             List.init n (fun r -> box (bar_color r) x (y0 + h - 1. - float_of_int r) 3. 1.)
             @ if m.peaks.(b) >= 1. then [ box skin.peak x (y0 + h - Float.ceil m.peaks.(b)) 3. 1. ] else []))
  | Scope ->
      let s = recent 576 in
      let y c = clamp 0. (h - 1.) (Float.round ((h / 2.) - (s.(c *.. 576 /.. 76) * h))) in
      List.init 76 (fun c ->
          let a = y c and b = if c = 0 then y c else y (c -.. 1) in
          box skin.peak (x0 + float_of_int c) (y0 + Float.min a b) 1. (Float.abs (a - b) + 1.))

(* what the title's place says: a slider's value while dragged, else
 * the song, "1. artist - title (1:23)" *)
let marquee (m : model) (p : number * number) : string =
  let it = List.nth m.items m.current in
  let song () =
    Printf.sprintf "%d. %s (%s)" (m.current +.. 1) it.title (match it.duration with Some d -> clock d | None -> "?:??")
  in
  match m.press with
  | None -> ( match m.opened with Error e when e <> "" -> Printf.sprintf "%d. %s: %s" (m.current +.. 1) it.name e | _ -> song ())
  | Some q -> (
      match target_at m q with
      | Volume -> Printf.sprintf "VOLUME: %d%%" (int_of_float (Float.round (m.volume * 100.)))
      | Balance ->
          if m.balance = 0. then "BALANCE: CENTER"
          else Printf.sprintf "BALANCE: %d%% %s" (int_of_float (Float.abs m.balance * 100.)) (if m.balance < 0. then "LEFT" else "RIGHT")
      | Seek ->
          let d = Option.value it.duration ~default:0. in
          let f = seek_at (fst p) in
          Printf.sprintf "SEEK TO: %s/%s (%d%%)" (clock (f * d)) (clock d) (int_of_float (f * 100.))
      | Band k when k < 0 -> Printf.sprintf "EQ: PREAMP: %+.1f DB" m.preamp
      | Band k ->
          let f = Graphic_eq.frequencies.(k) in
          Printf.sprintf "EQ: %s: %+.1f DB" (if f >= 1000. then Printf.sprintf "%gKHZ" (f / 1000.) else Printf.sprintf "%gHZ" f) m.gains.(k)
      | Nothing -> song ())

(* scrolling when it doesn't fit: a pixel every other frame, around and
 * around, "***" between the ends *)
let title_text (m : model) (s : string) : shape list =
  let x0 = 111. and x1 = 265. in
  let w = 5. * float_of_int (String.length s) in
  if w <= x1 - x0 || m.press <> None then text ~clip:(x0, x1) skin.lcd x0 27. s
  else
    let s = s ^ "  ***  " in
    let w = 5. * float_of_int (String.length s) in
    let off = Float.rem (float_of_int (m.frame /.. 2)) w in
    text ~clip:(x0, x1) skin.lcd (x0 - off) 27. s @ text ~clip:(x0, x1) skin.lcd (x0 - off + w) 27. s

(* a slider's bar, its color by its value: green, yellow, red *)
let level_color (t : float) : color = bar_color (int_of_float (clamp 0. 15. (t * 15.)))

let thumb ~(pressed : bool) (x : number) (y : number) (w : number) (h : number) : shape list = bevel ~up:(not pressed) x y w h

let icon (name : string) (cx : number) (cy : number) : shape list =
  let c = skin.icon in
  let tri_r x = poly c [ (x, cy - 4.); (x, cy + 4.); (x + 5., cy) ] and tri_l x = poly c [ (x + 5., cy - 4.); (x + 5., cy + 4.); (x, cy) ] in
  match name with
  | "prev" -> [ box c (cx - 7.) (cy - 4.) 2. 8.; tri_l (cx - 5.); tri_l cx ]
  | "play" -> [ poly c [ (cx - 3., cy - 5.); (cx - 3., cy + 5.); (cx + 4., cy) ] ]
  | "pause" -> [ box c (cx - 4.) (cy - 4.) 3. 8.; box c (cx + 1.) (cy - 4.) 3. 8. ]
  | "stop" -> [ box c (cx - 4.) (cy - 4.) 8. 8. ]
  | "next" -> [ tri_r (cx - 5.); tri_r cx; box c (cx + 5.) (cy - 4.) 2. 8. ]
  | _ (* eject *) -> [ poly c [ (cx - 5., cy + 1.); (cx + 5., cy + 1.); (cx, cy - 5.) ]; box c (cx - 5.) (cy + 3.) 10. 2. ]

let main_window (m : model) (p : number * number) : shape list =
  let down b = match m.press with Some q -> in_box q b && in_box p b | None -> false in
  let dragging t = match m.press with Some q -> target_at m q = t | None -> false in
  let vx, vy, vw, vh = volume_box and bx, by, bw, bh = balance_box and kx, ky, kw, kh = seek_box in
  let seek_f = if dragging Seek then seek_at (fst p) else fraction () in
  bevel ~fill:skin.body ~up:true 0. 0. 275. 116.
  @ title_bar 0. "WINAMP"
  (* the clutterbar: Winamp's options, O A I D V, a letter each *)
  @ List.concat (List.mapi (fun k c -> text skin.face 12. (24. + (7. * float_of_int k)) c) [ "O"; "A"; "I"; "D"; "V" ])
  (* the LCD: the status, the time, the analyzer *)
  @ bevel ~fill:black ~up:false 20. 22. 84. 40.
  @ status_icon m @ time m @ vis m
  (* the title, kbps and kHz, mono and stereo *)
  @ bevel ~fill:black ~up:false 108. 24. 159. 11.
  @ title_text m (marquee m p)
  @ bevel ~fill:black ~up:false 108. 41. 19. 8.
  @ bevel ~fill:black ~up:false 153. 41. 14. 8.
  @ (match m.kbps with Some k -> text skin.lcd 111. 42. (Printf.sprintf "%3d" (min 999 k)) | None -> [])
  @ (match m.khz with Some k -> text skin.lcd 156. 42. (Printf.sprintf "%2d" (min 99 k)) | None -> [])
  @ text skin.face 130. 42. "KBPS" @ text skin.face 170. 42. "KHZ"
  @ (let loaded = Result.is_ok m.opened && m.status <> Stopped in
     text (if loaded && not m.stereo then skin.lcd else skin.ghost) 206. 42. "MONO"
     @ text (if loaded && m.stereo then skin.lcd else skin.ghost) 232. 42. "STEREO")
  (* the volume and the balance, the bars colored by how far *)
  @ bevel ~fill:(level_color m.volume) ~up:false vx (vy + 4.) vw 5.
  @ thumb ~pressed:(dragging Volume) (vx + (m.volume * (vw - 14.))) (vy + 1.) 14. (vh - 2.)
  @ bevel ~fill:(level_color (Float.abs m.balance)) ~up:false bx (by + 4.) bw 5.
  @ thumb ~pressed:(dragging Balance) (bx + ((m.balance + 1.) / 2. * (bw - 14.))) (by + 1.) 14. (bh - 2.)
  @ List.concat_map
      (fun ((x, y, w, h), label, on) -> bevel ~up:(not (down (x, y, w, h))) x y w h @ text (if on then skin.lcd else skin.icon) (x + 7.) (y + 3.) label)
      [ (eq_button, "EQ", m.eq_shown); (pl_button, "PL", m.pl_shown) ]
  (* the seek bar: its thumb only when there is a song on *)
  @ bevel ~fill:skin.shade ~up:false kx ky kw kh
  @ (if m.status = Stopped then [] else thumb ~pressed:(dragging Seek) (kx + (seek_f * (kw - 29.))) ky 29. kh)
  (* the buttons *)
  @ List.concat_map (fun (name, x, w) -> bevel ~up:(not (down (x, 88., w, 18.))) x 88. w 18. @ icon name (x + (w / 2.)) 97.) buttons
  @ (let x, y, w, h = eject_box in bevel ~up:true x y w h @ icon "eject" (x + (w / 2.)) (y + 8.))
  @ List.concat_map
      (fun ((x, y, w, h), label, on) -> bevel ~up:(not (down (x, y, w, h))) x y w h @ text (if on then skin.lcd else skin.icon) (x + ((w - (5. * float_of_int (String.length label))) / 2.)) (y + 5.) label)
      [ (shuffle_box, "SHUFFLE", m.shuffle); (repeat_box, "REP", m.repeat) ]
  (* the logo: a bolt *)
  @ [ poly skin.title [ (258., 91.); (255., 99.); (258., 99.); (256., 106.); (262., 96.); (259., 96.); (261., 91.) ] ]

(*****************************************************************************)
(* view: the equalizer *)
(*****************************************************************************)

let eq_window (m : model) (p : number * number) : shape list =
  let down b = match m.press with Some q -> in_box q b && in_box p b | None -> false in
  let gx, gy, gw, gh = graph_box in
  (* the curve: the real response, on a log scale from 30 Hz to 20 kHz
   * (Winamp drew a spline through the sliders instead) *)
  let y_of db = gy + (gh / 2.) - (clamp (-.Graphic_eq.range) Graphic_eq.range db / Graphic_eq.range * ((gh / 2.) - 1.)) in
  let curve =
    let ys = Array.init (int_of_float gw -.. 2) (fun c -> y_of (Graphic_eq.response ~preamp:0. ~gains:m.gains (30. * ((20000. / 30.) ** (float_of_int c / (gw - 3.)))))) in
    Array.to_list
      (Array.mapi
         (fun c y ->
           let prev = if c = 0 then y else ys.(c -.. 1) in
           let color = level_color ((gy + gh - y) / gh) in
           box color (gx + 1. + float_of_int c) (Float.round (Float.min y prev)) 1. (Float.round (Float.abs (y - prev)) + 1.))
         ys)
  in
  let slider k =
    let x = slider_x k and g = if k < 0 then m.preamp else m.gains.(k) in
    let t = (g + Graphic_eq.range) / (2. * Graphic_eq.range) in
    let label = if k < 0 then "PREAMP" else let f = Graphic_eq.frequencies.(k) in if f >= 1000. then Printf.sprintf "%gK" (f / 1000.) else Printf.sprintf "%g" f in
    bevel ~fill:(level_color t) ~up:false (x + 4.) slider_y 6. slider_h
    @ thumb ~pressed:false (x + 1.5) (slider_y + ((1. - t) * (slider_h - 11.))) 11. 11.
    @ text skin.face (x + 7. - (2.5 * float_of_int (String.length label))) (slider_y + slider_h + 3.) label
  in
  bevel ~fill:skin.body ~up:true 0. eq_y 275. 116.
  @ title_bar eq_y "WINAMP EQUALIZER"
  @ List.concat_map
      (fun ((x, y, w, h), label, on) -> bevel ~up:(not (down (x, y, w, h))) x y w h @ [ box (if on then skin.lcd else skin.ghost) (x + 3.) (y + 4.) 3. 4. ] @ text skin.icon (x + 8.) (y + 4.) label)
      [ (on_box, "ON", m.eq_on); ((40., eq_y + 18., 32., 12.), "AUTO", false) ]
  @ (let x, y, w, h = presets_box in bevel ~up:(not (down presets_box)) x y w h @ text skin.icon (x + 4.) (y + 4.) "PRESETS")
  @ bevel ~fill:black ~up:false gx gy gw gh
  @ [ box skin.ghost (gx + 1.) (gy + (gh / 2.)) (gw - 2.) 1. ]
  @ [ box skin.peak (gx + 1.) (Float.round (y_of m.preamp)) 3. 1. ]
  @ curve
  (* the preset's name, in the graph's corner *)
  @ text skin.ghost (gx + 2.) (gy + 2.) (fst (List.nth Graphic_eq.presets m.preset))
  @ text skin.face 44. slider_y "+12DB" @ text skin.face 44. (slider_y + (slider_h / 2.) - 2.) "+0DB" @ text skin.face 44. (slider_y + slider_h - 5.) "-12DB"
  @ List.concat_map slider (List.init 11 (fun k -> k -.. 1))

(*****************************************************************************)
(* view: the playlist *)
(*****************************************************************************)

let pl_window (m : model) : shape list =
  let first = first_row m in
  let n = List.length m.items in
  let row r =
    let i = first +.. r in
    if i >= n then []
    else
      let it = List.nth m.items i in
      let y = row_y r in
      let color = if i = m.current then skin.current else skin.text in
      let d = match it.duration with Some d -> clock d | None -> "" in
      let t = Printf.sprintf "%d. %s" (i +.. 1) it.title in
      let t = if String.length t > 40 then String.sub t 0 40 else t in
      (if i = m.selected then [ box skin.selected 12. (y - 1.5) 243. row_h ] else [])
      @ text color 14. y t
      @ text color (253. - (5. * float_of_int (String.length d))) y d
  in
  let known = List.filter_map (fun it -> it.duration) m.items in
  let total = List.fold_left ( + ) 0. known in
  let sel = match (List.nth m.items m.selected).duration with Some d -> clock d | None -> "?:??" in
  bevel ~fill:skin.body ~up:true 0. pl_y 275. 232.
  @ title_bar pl_y "WINAMP PLAYLIST"
  @ [ box black 12. (pl_y + 20.) 243. 174. ]
  @ List.concat (List.init rows row)
  (* the scrollbar, its thumb where the rows shown are *)
  @ bevel ~fill:skin.shade ~up:false 258. (pl_y + 20.) 9. 174.
  @ thumb ~pressed:false 258.5 (pl_y + 20. + (float_of_int first / float_of_int (max 1 n) * 156.)) 8. 18.
  (* the bottom: the buttons, the times *)
  @ List.concat (List.mapi (fun k l -> let x = 12. + (27. * float_of_int k) in bevel ~up:true x (pl_y + 202.) 25. 18. @ text skin.icon (x + 12.5 - (2.5 * float_of_int (String.length l))) (pl_y + 208.) l) [ "ADD"; "REM"; "SEL"; "MISC" ])
  @ bevel ~fill:black ~up:false 125. (pl_y + 204.) 80. 12.
  @ text skin.lcd 128. (pl_y + 207.) (Printf.sprintf "%s/%s%s" sel (clock total) (if List.length known < n then "+" else ""))
  @ bevel ~up:true 238. (pl_y + 202.) 25. 18. @ text skin.icon 241. (pl_y + 208.) "LIST"

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let view (computer : computer) (m : model) : shape list =
  let p = ((computer.mouse.mx / px) + 137.5, 232. - (computer.mouse.my / px)) in
  (* the desktop under the windows: Windows 95's teal *)
  [ rectangle (rgb 0 128 128) 2000. 2000. ]
  @ main_window m p
  @ (if m.eq_shown then eq_window m p else [])
  @ if m.pl_shown then pl_window m else []

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
