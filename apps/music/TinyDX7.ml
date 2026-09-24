(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of the Yamaha DX7 (1983), the FM synthesizer of the
 * 1980s: six operators, 32 algorithms, 145 parameters a voice. The
 * voice is Voice_dx7.ml over Fm_algorithm and Dx_envelope; this is its
 * panel and a keyboard.
 *
 * The DX7 was edited as its front panel allowed: a two-line display, a
 * button to choose one parameter, a slider and two buttons (-1, +1) to
 * change it -- 145 parameters one at a time, with nothing to show how
 * they fit together, which is why most people played the presets and
 * why every synthesizer after it had knobs again. The left half of the
 * panel is that: the LCD, < PARAM > and < OP > to walk the parameters
 * (the second jumping an operator at a time), the data slider, -1 and
 * +1. The right half is what the DX7 hid: the algorithm drawn as the
 * graph it is (the carriers at the bottom, each modulator above what it
 * modulates, the fed-back one marked), each operator lit by how loud it
 * is now, a click on one choosing its output level; and the six
 * envelopes drawn, their four rates and levels as a shape.
 *
 * The keys play with the mouse or the letters (a s d f g h j k the
 * white keys from C, w e t y u the black ones, z and x an octave down
 * and up). With the mouse, the velocity is where the key is pressed:
 * soft at its back, hard at its front -- with FM, velocity changes the
 * timbre as much as the loudness (the electric piano's bark). Under the
 * panel, the spectrum and the scope.
 *
 * A cartridge: cart= a .syx file of 32 voices (a DX7 bulk dump, the
 * kind shared on the web), natively for now, its voices added to the
 * menu after ours, < and > beside it stepping through them:
 *   dune exec apps/music/TinyDX7.exe -- cart=rom1a.syx
 *
 * Uses: Voice_dx7 (the voice, its patches, the cartridge), Fm_algorithm
 * (the graph), Polyphony (16 voices, stealing), Audio's instruments and
 * fetch, Gui (the buttons, the slider, the menu), Spectrum. Not: the
 * effects rack, Scene2d, Sprite, File_menu.
 *
 * Exercises: the Reface DX's mode (four operators, its twelve
 * algorithms); saving a voice or a cartridge (File_menu, Voice_dx7's
 * to_cartridge); the operators switched on and off (the DX7's buttons
 * 1 to 6, for hearing one at a time); a MIDI keyboard's velocity and
 * its pitch bend.
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Voice_dx7.patch;
  voices : (string * Voice_dx7.patch) list; (* ours, then a cartridge's *)
  voice : int; (* an index in [voices] *)
  param : int; (* an index in Voice_dx7.knobs: the one the LCD shows *)
  octave : int;
  held : string list; (* the letters held at the last frame *)
  mouse_note : int option;
  loaded : bool; (* the cart= flag looked at *)
  message : string;
}

let initial_model : model =
  {
    patch = snd (List.hd Voice_dx7.presets);
    voices = Voice_dx7.presets;
    voice = 0;
    param = 0;
    octave = 4;
    held = [];
    mouse_note = None;
    loaded = false;
    message = "";
  }

(* the synthesizer lives with the sound, not in the model: the mixer
 * pulls its blocks between frames (Instrument.mli) *)
let dx7 = Voice_dx7.create initial_model.patch
let inst : Instrument.t = Voice_dx7.instrument dx7
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

(* a cartridge fetched by cart= arrives here *)
let fetched : string option option ref = ref None
let knobs = Array.of_list Voice_dx7.knobs

(*****************************************************************************)
(* The keyboard *)
(*****************************************************************************)

let keys_count = 25 (* two octaves and a C *)
let is_black (s : int) : bool = List.mem (s mod 12) [ 1; 3; 6; 8; 10 ]
let white_width = 56.
let keyboard_left = -420.
let keyboard_top = -175.
let white_height = 270.
let black_height = 165.
let whites_before (s : int) : int = List.length (List.filter (fun i -> not (is_black i)) (List.init s (fun i -> i)))

let key_x (s : int) : number =
  let w = float_of_int (whites_before s) in
  if is_black s then keyboard_left + (w * white_width) else keyboard_left + ((w + 0.5) * white_width)

(* the key under the mouse (a black key first, it's on top), and the
 * velocity: how far down the key, 0.2 at its back to 1 at its front *)
let key_at (x : number) (y : number) : (int * number) option =
  let keys = List.init keys_count (fun s -> s) in
  let height s = if is_black s then black_height else white_height in
  let hit s =
    let w = if is_black s then white_width * 0.6 else white_width in
    Float.abs (x - key_x s) <= w / 2. && y <= keyboard_top && y >= keyboard_top - height s
  in
  let found = match List.find_opt (fun s -> is_black s && hit s) keys with Some s -> Some s | None -> List.find_opt hit keys in
  Option.map (fun s -> (s, 0.2 + (0.8 * (keyboard_top - y) / height s))) found

(*****************************************************************************)
(* The parameters, one at a time *)
(*****************************************************************************)

(* a control's positions: a selector's labels, a switch's two *)
let positions (k : Voice_dx7.knob) : int =
  match k.control with Selector labels -> List.length labels | Switch -> 2 | Knob _ -> 100

(* the next parameter of another operator: [dir] 1 or -1 *)
let jump_operator (param : int) (dir : int) : int =
  let prefix i = match String.index_opt knobs.(i).name '.' with Some j -> String.sub knobs.(i).name 0 j | None -> "" in
  let n = Array.length knobs in
  let rec go i = if i < 0 || i >= n then param else if prefix i <> prefix param && String.length (prefix i) = 3 && (prefix i).[0] = 'o' then i else go (i +.. dir) in
  (* backwards, to the first parameter of the operator found *)
  let found = go (param +.. dir) in
  if dir > 0 || found = param then found
  else
    let rec first i = if i > 0 && prefix (i -.. 1) = prefix found then first (i -.. 1) else i in
    first found

(* the LCD's two lines, 16 characters: the voice, the parameter *)
let lcd (m : model) : string * string =
  let k = knobs.(m.param) in
  let value = Control.to_string k.control (k.get m.patch) in
  let name = String.uppercase_ascii (String.map (fun c -> if c = '.' || c = '_' then ' ' else c) k.name) in
  let width = 16 -.. String.length value -.. 1 in
  let name = if String.length name > width then String.sub name 0 width else name in
  (Printf.sprintf "%02d %s" (m.voice +.. 1) (String.trim m.patch.name), Printf.sprintf "%-*s %s" width name value)

(*****************************************************************************)
(* The algorithm as a graph *)
(*****************************************************************************)

(* each operator's place, in columns and rows: the carriers on row 0 in
 * order, a modulator a row above what it modulates, over its first
 * target -- a DX7 panel's drawings *)
let layout (alg : Fm_algorithm.t) : (int * (int * int)) list =
  let places = Hashtbl.create 6 and column = ref 0 in
  let rec place (op : int) (row : int) : unit =
    if not (Hashtbl.mem places op) then begin
      let mods = List.sort compare (Fm_algorithm.modulators alg op) in
      let fresh = List.filter (fun m -> not (Hashtbl.mem places m)) mods in
      (* placed now, before its modulators, so a loop can't recurse *)
      Hashtbl.replace places op (0, row);
      List.iter (fun m -> place m (row +.. 1)) fresh;
      let col =
        match fresh with
        | [] ->
            incr column;
            !column -.. 1
        | _ ->
            let cols = List.map (fun m -> fst (Hashtbl.find places m)) fresh in
            List.fold_left ( +.. ) 0 cols /.. List.length cols
      in
      Hashtbl.replace places op (col, row)
    end
  in
  List.iter (fun c -> place c 0) (List.sort compare alg.carriers);
  List.map (fun op -> (op, Hashtbl.find places op)) [ 1; 2; 3; 4; 5; 6 ]

let graph_x = -420.
let graph_y = 30.
let cell = 54.
let box = 38.

let op_position (col, row) : number * number = (graph_x + (float_of_int col * cell), graph_y + (float_of_int row * cell))

(* a line from one point to another, [w] wide *)
let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let lit (level : number) : color =
  (* 0 to 2 cycles, on a log scale: 60 dB of glow *)
  let x = if level <= 0. then 0. else Float.max 0. (Float.min 1. (1. + (log10 (level / 2.) / 3.))) in
  let c base range = int_of_float (base + (range * x)) in
  rgb (c 40. 200.) (c 40. 170.) (c 30. 60.)

let graph_view (m : model) : shape list =
  let alg = Fm_algorithm.get m.patch.algorithm in
  let places = layout alg in
  let at op = op_position (List.assoc op places) in
  let levels = Voice_dx7.levels dx7 in
  let edge (a, b) = segment (rgb 200 190 160) 3. (at a) (at b) in
  let from, into = alg.feedback in
  let fx, fy = at from and ix, iy = at into in
  let feedback =
    (* the loop: out of [from]'s right, up and over to [into] *)
    let c = if m.patch.feedback > 0 then rgb 230 120 60 else rgb 110 100 80 in
    let right = Float.max fx ix + (box * 0.8) and top = fy + (box * 0.8) in
    [ segment c 3. (fx, fy) (right, fy); segment c 3. (right, fy) (right, top); segment c 3. (right, top) (ix, top); segment c 3. (ix, top) (ix, iy) ]
  in
  let bottom = graph_y - (cell * 0.7) in
  let out =
    List.map (fun c -> segment (rgb 200 190 160) 3. (at c) (fst (at c), bottom)) alg.carriers
    @
    let xs = List.map (fun c -> fst (at c)) alg.carriers in
    [ segment (rgb 200 190 160) 3. (List.fold_left Float.min 1e9 xs, bottom) (List.fold_left Float.max (-1e9) xs, bottom) ]
  in
  let op_box op =
    let x, y = at op in
    group
      [
        rectangle (rgb 20 20 20) (box + 4.) (box + 4.);
        rectangle (lit levels.(op -.. 1)) box box;
        words (rgb 20 20 20) (string_of_int op) |> scale 1.6;
      ]
    |> move x y
  in
  [ words (rgb 230 220 190) (Printf.sprintf "ALGORITHM %d   FEEDBACK %d" m.patch.algorithm m.patch.feedback) |> scale 1.3 |> move (graph_x + 110.) (bottom - 22.) ]
  @ feedback @ List.map edge alg.edges @ out
  @ List.map op_box [ 1; 2; 3; 4; 5; 6 ]

(* the operator under the mouse, in the graph *)
let op_at (m : model) (x : number) (y : number) : int option =
  let places = layout (Fm_algorithm.get m.patch.algorithm) in
  List.find_map
    (fun (op, p) ->
      let ox, oy = op_position p in
      if Float.abs (x - ox) <= box / 2. && Float.abs (y - oy) <= box / 2. then Some op else None)
    places

(*****************************************************************************)
(* The envelopes *)
(*****************************************************************************)

(* an operator's envelope as a shape: from L4 to L1, L2, L3, held, then
 * back to L4; each segment as long as its time, square-rooted so a
 * slow rate doesn't hide the others *)
let envelope_view (o : Voice_dx7.operator) (cx : number) (cy : number) : shape list =
  let w = 150. and h = 60. in
  let time k from to_ =
    let q = float_of_int (o.rates.(k) *.. 41 /.. 64) in
    sqrt ((Float.abs (float_of_int (to_ -.. from)) + 1.) / (2. ** (q / 4.)))
  in
  let l = o.levels in
  let times = [ time 0 l.(3) l.(0); time 1 l.(0) l.(1); time 2 l.(1) l.(2); time 3 l.(2) l.(3) ] in
  let hold = 0.25 * List.fold_left ( + ) 0. times in
  let total = List.fold_left ( + ) hold times in
  let px t = cx - (w / 2.) + (t / total * w) and py level = cy - (h / 2.) + (float_of_int level / 99. * h) in
  let t1 = List.nth times 0 in
  let t2 = t1 + List.nth times 1 in
  let t3 = t2 + List.nth times 2 in
  let t4 = t3 + hold in
  let points = [ (0., l.(3)); (t1, l.(0)); (t2, l.(1)); (t3, l.(2)); (t4, l.(2)); (total, l.(3)) ] in
  let rec lines = function
    | (ta, la) :: ((tb, lb) :: _ as rest) -> segment (rgb 120 220 160) 2. (px ta, py la) (px tb, py lb) :: lines rest
    | _ -> []
  in
  (rectangle (rgb 25 30 25) w h |> move cx cy) :: lines points

let envelopes_view (m : model) : shape list =
  List.concat
    (List.init 6 (fun i ->
         let cx = 20. + (float_of_int (i mod 3) * 170.) and cy = 200. - (float_of_int (i /.. 3) * 95.) in
         let o = m.patch.operators.(i) in
         let title = words (rgb 230 220 190) (Printf.sprintf "OP%d  %s %d" (i +.. 1) (if o.fixed then "FIXED" else "RATIO") o.level) in
         (title |> scale 1.1 |> move cx (cy + 42.)) :: envelope_view o cx cy))

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "dx7" (fun () -> inst));
  (* cart=: fetched once, its voices added when they arrive *)
  let m =
    if m.loaded then m
    else begin
      Option.iter (fun src -> Audio.fetch src (fun bytes -> fetched := Some bytes)) (List.assoc_opt "cart" computer.flags);
      { m with loaded = true }
    end
  in
  let m =
    match !fetched with
    | None -> m
    | Some bytes -> (
        fetched := None;
        match Option.map Voice_dx7.cartridge bytes with
        | Some (Ok voices) ->
            let named = Array.to_list (Array.map (fun (p : Voice_dx7.patch) -> (String.trim p.name, p)) voices) in
            { m with voices = Voice_dx7.presets @ named; message = "cartridge: 32 voices" }
        | Some (Error e) -> { m with message = "cartridge: " ^ e }
        | None -> { m with message = "cartridge: can't be read" })
  in
  (* the menu, and < > for a cartridge's 32, more than the screen holds *)
  let count = List.length m.voices in
  let voice = Gui.menu computer ~at:(330., 482.) (List.map fst m.voices) m.voice in
  let voice = if Gui.button computer ~at:(250., 482.) "<" then (voice +.. count -.. 1) mod count else voice in
  let voice = if Gui.button computer ~at:(410., 482.) ">" then (voice +.. 1) mod count else voice in
  let patch = if voice <> m.voice then snd (List.nth m.voices voice) else m.patch in
  (* the DX7's way: one parameter, chosen with the buttons, changed with
   * the slider or -1 and +1 *)
  let n = Array.length knobs in
  let param = m.param in
  let param = if Gui.button computer ~at:(-420., 300.) "< PARAM" then (param +.. n -.. 1) mod n else param in
  let param = if Gui.button computer ~at:(-310., 300.) "PARAM >" then (param +.. 1) mod n else param in
  let param = if Gui.button computer ~at:(-200., 300.) "< OP" then jump_operator param (-1) else param in
  let param = if Gui.button computer ~at:(-110., 300.) "OP >" then jump_operator param 1 else param in
  let k = knobs.(param) in
  let top = float_of_int (positions k -.. 1) in
  let v = k.get patch in
  let v = if Gui.button computer ~at:(-420., 255.) "-1" then Float.max 0. (v - 1.) else v in
  let v = if Gui.button computer ~at:(-360., 255.) "+1" then Float.min top (v + 1.) else v in
  let v = Float.round (Gui.slider computer ~at:(-190., 255.) ~from:0. ~to_:top v) in
  let patch = if v <> k.get patch then k.put patch v else patch in
  (* a click on an operator in the graph: its output level *)
  let mouse = computer.mouse in
  let param =
    match (mouse.mclick, op_at m mouse.mx mouse.my) with
    | true, Some op ->
        let name = Printf.sprintf "op%d.output" op in
        let rec find i = if i >= n then param else if knobs.(i).name = name then i else find (i +.. 1) in
        find 0
    | _ -> param
  in
  (* the letters, several at once *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 1 (m.octave -.. 1) else if pressed "x" then min 6 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, semitone) ->
      let n = note m.octave semitone in
      if pressed k then inst.note_on n 0.8;
      if released k then inst.note_off n)
    letters;
  (* the mouse on the keyboard: the velocity where the key is pressed *)
  let under = if mouse.mdown then key_at mouse.mx mouse.my else None in
  let under_note = Option.map (fun (s, _) -> note m.octave s) under in
  if under_note <> m.mouse_note then begin
    Option.iter inst.note_off m.mouse_note;
    Option.iter (fun (s, velocity) -> inst.note_on (note m.octave s) velocity) under
  end;
  Voice_dx7.set_patch dx7 patch;
  { m with patch; voice; param; octave; held = now; mouse_note = under_note }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 230 220 190

(* the DX7's front: brown and black, its LCD, the membrane's colours *)
let panel_view (m : model) : shape list =
  let line1, line2 = lcd m in
  let lcd_green = rgb 150 190 110 in
  [
    rectangle (rgb 35 30 28) 960. 480. |> move 0. 190.;
    rectangle (rgb 90 60 40) 960. 14. |> move 0. 437.;
    words (rgb 200 170 120) "DX7   DIGITAL PROGRAMMABLE ALGORITHM SYNTHESIZER" |> scale 1.1 |> move (-250.) 415.;
    rectangle (rgb 20 20 18) 330. 78. |> move (-265.) 363.;
    rectangle lcd_green 316. 64. |> move (-265.) 363.;
    words (rgb 30 40 25) line1 |> scale 1.7 |> move (-265.) 378.;
    words (rgb 30 40 25) line2 |> scale 1.7 |> move (-265.) 348.;
    words ink "DATA ENTRY" |> scale 1.1 |> move (-190.) 228.;
  ]

(* the spectrum of the last 2048 samples, 20 Hz to 20 kHz on a log axis,
 * -80 to 0 dB, and the scope *)
let spectrum_view (samples : Signal.t) : shape list =
  let cx = -160. and cy = -95. and w = 620. and h = 110. in
  let mags = Spectrum.of_signal samples in
  let n = 2 *.. (Array.length mags -.. 1) in
  let bars = 90 in
  let freq b = 20. * (1000. ** (float_of_int b / float_of_int bars)) in
  let bar b =
    let lo = freq b and hi = freq (b +.. 1) in
    let top = ref 0. in
    Array.iteri
      (fun k v ->
        let f = Spectrum.bin_frequency ~n k in
        if f >= lo && f < hi && v > !top then top := v)
      mags;
    let db = if !top <= 0. then -80. else Float.max (-80.) (20. * log10 !top) in
    let bh = (db + 80.) / 80. * h in
    let bw = w / float_of_int bars in
    rectangle (rgb 120 220 160) (bw - 1.) (Float.max 1. bh) |> move (cx - (w / 2.) + ((float_of_int b + 0.5) * bw)) (cy - (h / 2.) + (bh / 2.))
  in
  (rectangle (rgb 20 25 20) w h |> move cx cy) :: List.init bars bar

let scope_view (samples : Signal.t) : shape list =
  let cx = 330. and cy = -95. and w = 300. and h = 110. in
  let points = 150 in
  let at i = samples.(Array.length samples -.. 1024 +.. (i *.. 1024 /.. points)) in
  (rectangle (rgb 20 25 20) w h |> move cx cy)
  :: List.init (points -.. 1) (fun i ->
         let x i = cx - (w / 2.) + (float_of_int i / float_of_int points * w) in
         let y i = cy + (Float.max (-1.) (Float.min 1. (at i * 3.)) * h / 2.) in
         segment (rgb 120 220 160) 2. (x i, y i) (x (i +.. 1), y (i +.. 1)))

let keyboard_view (computer : computer) (m : model) : shape list =
  let letter_of s = List.find_map (fun (k, s') -> if s' = s then Some k else None) letters in
  let down s =
    m.mouse_note = Some (note m.octave s) || match letter_of s with Some k -> Set_.mem k computer.keyboard.keys | None -> false
  in
  let key s =
    let black = is_black s in
    let w = if black then white_width * 0.6 else white_width - 3. in
    let h = if black then black_height else white_height in
    let color = if down s then rgb 120 220 160 else if black then rgb 20 20 20 else rgb 250 250 245 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 120 120 120) k |> scale 1.6 |> move_y ((-.h / 2.) + 18.) ] | None -> [] in
    group (rectangle color w h :: label) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys)
  @ List.map key (List.filter is_black keys)
  @ [ words black (Printf.sprintf "C%d" m.octave) |> scale 1.4 |> move (keyboard_left + 20.) (keyboard_top + 14.) ]

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyDX7" |> scale 2.4 |> move (-380.) 482.; words black "voice" |> scale 1.5 |> move 190. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-190.) 482. ]
  @ [ words (rgb 70 70 70) m.message |> scale 1.3 |> move 20. 482. ]
  @ panel_view m @ graph_view m @ envelopes_view m
  @ spectrum_view (Voice_dx7.recent dx7)
  @ scope_view (Voice_dx7.recent dx7)
  @ [
      words (rgb 70 70 70)
        (Printf.sprintf "voices %d   the mouse: soft at a key's back, hard at its front" (Voice_dx7.voices dx7))
      |> scale 1.3 |> move 0. (-162.);
    ]
  @ keyboard_view computer m @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
