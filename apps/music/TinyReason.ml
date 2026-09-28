(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of Propellerhead's Reason (2000), the studio as a rack:
 * a mixer, synthesizers, a drum machine, a pattern sequencer and
 * effects, stacked between the rack's rails -- and Tab turns the rack
 * round, to its back, where every device has its jacks and the sound
 * goes where the cables say, cables dragged from jack to jack, sagging
 * and swinging (plan_tiny_reason.md).
 *
 * The idea to teach is the architecture. A device is a *module*
 * (Rack_module.mli), two halves over one sound: its front, a part
 * (Component.mli, the office's embedded parts) -- the very panels
 * TinyJuno, TinyHammond and TinyTR808 show full size, scaled here to
 * the rack's width -- and its back and sound, a Rack_device (jacks,
 * stages). This program knows no module in particular: the Create menu
 * is its catalogue, a name and a maker each, and a new module is a line
 * there. The rack is a graph (Studio_reason.mli): each chunk of 64
 * samples, the devices' stages in topological order, a cable closing a
 * loop refused; a device added cabled by itself, as Reason does it.
 *
 * The cables (Rack_cable.mli) are ropes of particles, Jakobsen's
 * position Verlet: pinned at their jacks, under gravity, they sag,
 * trail behind a dragged end, bounce when plugged, swing and settle.
 *
 * Under the rack, as in Reason, the sequencer: a track per instrument,
 * the selected one's notes on a piano roll -- the keys down its left,
 * a row per key, a column per sixteenth, two bars looping, the playhead
 * running -- a note drawn by a click (a drag makes it longer), taken
 * out by another; the song (Song.mli) played by the rack on its clock.
 * The top bar's button hides it, the rack taking the whole screen.
 *
 * On the front: the panels, each played with the mouse in place; a
 * click selects a device, the letters play it (a s d f g h j k, w e t
 * y u, z x an octave), its presets in the top bar. On the back: a jack
 * pressed pulls a new cable out of it, or picks up the cable in it;
 * shift and a press pulls a cable off, falling; let go on a jack, it
 * plugs in (the jacks it may go into ringed green, the others dimmed,
 * why not said by the pointer). Anywhere: Tab turns the rack round,
 * space starts and stops every sequencer on one clock, Backspace takes
 * the selected device out (an effect inserted leaving its two sides
 * joined), the wheel, the arrows, the page keys and the scrollbar
 * scroll.
 *
 * Uses: Studio_reason (the rack, its graph and its sound), Song (the
 * sequencer's), Rack_module,
 * Rack_device, Rack_mixer, Rack_matrix, Rack_cable (over Particles),
 * Part_juno, Part_hammond, Part_tr808, Part_mixer, Part_matrix,
 * Part_effect (the fronts), Voice_juno, Voice_hammond, Voice_tr808,
 * Delay, Reverb, Drive, Dynamics, Eq, Modulation (the effects),
 * Component, Gui (the menus, the transport). Not:
 * Scene2d, Sprite, File_menu.
 *
 * Ours, and said so: the SubTractor's place taken by the Juno-106 (a
 * polyphonic subtractive synthesizer, as it is) and the Redrum's by the
 * TR-808; a stereo cable per jack; one aux send; the rack's width 880,
 * its unit 40 pixels; the flip a box narrowing, not a turn in 3D.
 *
 * Exercises: the rack saved and loaded (each front's save, a registry
 * of kinds, Component's placeholder for a kind this build lacks); the
 * Spider, one output into several inputs; a device dragged to another
 * place, its cables following; more modules -- the other Tiny voices
 * (Part_minimoog, Part_dx7...), a line each, within the budget;
 * TinyReBirth plugged in whole, Reason's ReBirth Input Machine.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The catalogue *)
(*****************************************************************************)

let voice_module ~name ~color ~(front : Component.part) ?cv ?transport (inst : Instrument.t) ~kind : Rack_module.t =
  { name; color; front; device = Rack_device.of_instrument ~kind ?cv ?transport inst }

let juno () : Rack_module.t =
  let v = Voice_juno.create (snd (List.hd Voice_juno.presets)) in
  voice_module ~kind:"juno" ~name:"Juno-106" ~color:(rgb 235 120 50) ~front:(Part_juno.make v)
    ~cv:[ ("Filter CV", "vcf.cutoff", (0., 1.)) ]
    (Voice_juno.instrument v)

let hammond () : Rack_module.t =
  let v = Voice_hammond.create (snd (List.hd Voice_hammond.presets)) in
  voice_module ~kind:"hammond" ~name:"Hammond B-3" ~color:(rgb 150 90 50) ~front:(Part_hammond.make v) (Voice_hammond.instrument v)

(* the Redrum's place: the 808, its own sequencer on the rack's clock *)
let redrum () : Rack_module.t =
  let v = Voice_tr808.create (snd (List.hd Voice_tr808.presets)) in
  let tempo bpm = Voice_tr808.set_patch v { (Voice_tr808.patch v) with tempo = bpm } in
  let step () = if Voice_tr808.running v then Some (Voice_tr808.step v) else None in
  voice_module ~kind:"tr808" ~name:"TR-808" ~color:(rgb 205 55 45) ~front:(Part_tr808.make v)
    ~transport:(Voice_tr808.run v, tempo, step)
    (Voice_tr808.instrument v)

let effect kind name color fx () = Rack_module.of_effect ~kind ~name ~color (fx ())

(* the rack, the studio: made when first asked for (tinybox) *)
let studio = lazy (Studio_reason.create Studio_reason.empty)

let catalogue : Rack_module.catalogue =
  [
    ("Mixer 14:2", Rack_module.mixer);
    ("Juno-106", juno);
    ("Hammond B-3", hammond);
    ("TR-808 (Redrum)", redrum);
    ("Matrix", Rack_module.matrix);
    ("DDL-1 Delay", effect "ddl1" "DDL-1" (rgb 90 170 230) Delay.fx);
    ("RV-7 Reverb", effect "rv7" "RV-7" (rgb 120 200 150) Reverb.fx);
    ("D-11 Distortion", effect "d11" "D-11" (rgb 230 80 60) Drive.fx);
    ("COMP-01", effect "comp01" "COMP-01" (rgb 200 200 90) Dynamics.fx);
    ("PEQ-2", effect "peq2" "PEQ-2" (rgb 170 130 220) Eq.fx);
    ("CF-101 Chorus", effect "cf101" "CF-101" (rgb 90 210 210) Modulation.fx);
  ]

(* the Hardware Interface's front: its name and the level going out *)
let rec hardware_front : Component.part =
  {
    kind = "hardware";
    height = (fun _ -> 40.);
    natural = Some (880., 40.);
    draw =
      (fun b ~active:_ ->
        let level = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. (Studio_reason.recent (Lazy.force studio)) in
        [
          rectangle (rgb 35 35 40) b.w b.h |> move b.x b.y;
          words (rgb 220 220 220) "HARDWARE INTERFACE" |> scale 1.1 |> move (b.x - (b.w / 2.) + 110.) b.y;
          rectangle (rgb 20 20 20) 200. 10. |> move (b.x + 250.) b.y;
          rectangle (rgb 90 220 110) (200. * Float.min 1. level) 10. |> move (b.x + 150. + (100. * Float.min 1. level)) b.y;
        ]);
    input = (fun _ _ -> hardware_front);
    menu = [];
    command = (fun _ -> hardware_front);
    save = (fun () -> "");
  }

let hardware_module : Rack_module.t = { name = "Hardware Interface"; color = rgb 200 200 200; front = hardware_front; device = Studio_reason.hardware_device }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type side = Front | Back

(* a cable in the hand: the end still plugged, and its rope *)
type drag = { fixed : Studio_reason.port; rope : Rack_cable.t }

type model = {
  patch : Studio_reason.patch;
  modules : (int * Rack_module.t) list; (* by id, their fronts as they are now *)
  selected : int option;
  side : side;
  flip : int; (* frames left of the rack turning *)
  scroll : number;
  ropes : (Studio_reason.cable * Rack_cable.t) list;
  drag : drag option;
  falling : (Rack_cable.t * int) list; (* cables pulled off, frames left *)
  active : int option; (* the front holding the mouse *)
  octave : int; (* the letter a's C *)
  message : (string * int) option;
  was_down : bool;
  keys : string list; (* held at the last frame *)
  seq : bool; (* the sequencer shown, under the rack *)
  song : Song.t;
  low : int; (* the sequencer's lowest key *)
  drawing : Song.note option; (* a note being drawn, longer as dragged *)
  preview : int option; (* a key of the sequencer's held *)
}

(* a module made, put in the rack under [below], cabled by itself *)
let add_module (patch : Studio_reason.patch) (modules : (int * Rack_module.t) list) (name : string) ~(below : int option) ~(selected : int option) =
  let s = Lazy.force studio in
  let m = (List.assoc name catalogue) () in
  let patch, id = Studio_reason.add patch ~kind:m.device.kind ~below in
  Studio_reason.attach s id m.device;
  let patch = Studio_reason.route (Studio_reason.lookup s) patch id ~selected in
  (patch, modules @ [ (id, m) ], id)

(* the sequencer's song at first: the Hammond's chords, C, A minor, F,
 * G, half a bar each *)
let demo_chords () : Song.note list =
  List.concat
    (List.mapi
       (fun k chord -> List.map (fun pitch -> { Song.start = float_of_int (8 *.. k); length = 8.; pitch; velocity = 0.8 }) chord)
       [ [ 48; 52; 55 ]; [ 45; 48; 52 ]; [ 41; 45; 48 ]; [ 43; 47; 50 ] ])

(* the default rack: a mixer, the Juno played by the Matrix, the 808,
 * the Hammond, a delay on the mixer's send *)
let initial_model : model =
  let add (p, ms, _) name ~selected = add_module p ms name ~below:None ~selected in
  let r = (Studio_reason.empty, [ (Studio_reason.hardware, hardware_module) ], 0) in
  let r = add r "Mixer 14:2" ~selected:None in
  let _, _, mixer = r in
  let r = add r "Juno-106" ~selected:None in
  let _, _, juno = r in
  let r = add r "Matrix" ~selected:(Some juno) in
  let r = add r "TR-808 (Redrum)" ~selected:None in
  let r = add r "Hammond B-3" ~selected:None in
  let _, _, hammond = r in
  let patch, modules, _ = add r "DDL-1 Delay" ~selected:(Some mixer) in
  Studio_reason.set_patch (Lazy.force studio) patch;
  {
    patch;
    modules;
    selected = Some hammond;
    side = Front;
    flip = 0;
    scroll = 0.;
    ropes = [];
    drag = None;
    falling = [];
    active = None;
    octave = 4;
    message = None;
    was_down = false;
    keys = [];
    seq = true;
    song = Song.set_notes Song.empty hammond (demo_chords ());
    low = 40;
    drawing = None;
    preview = None;
  }

(*****************************************************************************)
(* The layout *)
(*****************************************************************************)

let width = 880.
let unit = 40.
let rack_top = 430.
(* the rack's bottom: over the sequencer when it shows *)
let seq_top = -70.
let rack_bottom (m : model) : number = if m.seq then seq_top else -430.
let scrollbar_x = 485.

(* a front as wide as the rack, unless that makes it taller than
 * [tallest]: then narrower, centred -- so that the mixer, a synthesizer
 * and the Matrix fit on the screen together *)
let tallest = 320.

let front_scale (m : Rack_module.t) : number =
  match m.front.natural with Some (nw, nh) -> Float.min (width / nw) (tallest / nh) | None -> 1.

let height (m : Rack_module.t) : number =
  match m.front.natural with
  | Some (_, nh) -> Float.ceil (nh * front_scale m / unit) * unit
  | None -> Float.ceil (Component.fitted_height ~scaled:true m.front width / unit) * unit

(* the room a front is drawn in, inside its device's box *)
let front_box (m : Rack_module.t) (b : Widget.box) : Widget.box =
  match m.front.natural with Some (nw, _) -> { b with w = nw * front_scale m } | None -> b

(* each device's box, top to bottom, scrolled *)
let boxes (m : model) : (int * Widget.box) list =
  let y = ref (rack_top + m.scroll) in
  List.filter_map
    (fun (id, _) ->
      match List.assoc_opt id m.modules with
      | None -> None
      | Some md ->
          let h = height md in
          let b = { Widget.x = 0.; y = !y - (h / 2.); w = width; h } in
          y := !y - h;
          Some (id, b))
    m.patch.devices

let total_height (m : model) : number = List.fold_left (fun t (_, md) -> t + height md) 0. m.modules
let visible (m : model) (b : Widget.box) : bool = b.y - (b.h / 2.) < rack_top && b.y + (b.h / 2.) > rack_bottom m

(* a jack's place on a device's back: in rows of 14, centred *)
let jack_pos (b : Widget.box) (count : int) (j : int) : number * number =
  let rows = (count +.. 13) /.. 14 in
  let row = j /.. 14 and col = j mod 14 in
  let in_row = if row = rows -.. 1 then count -.. (row *.. 14) else 14 in
  (b.x - (float_of_int (in_row -.. 1) * 29.) + (float_of_int col * 58.), b.y + (float_of_int (rows -.. 1) * 21.) - (float_of_int row * 42.) + 4.)

let port_pos (m : model) (p : Studio_reason.port) : (number * number) option =
  match (List.assoc_opt p.device (boxes m), List.assoc_opt p.device m.modules) with
  | Some b, Some md -> Some (jack_pos b (Array.length md.device.jacks) p.jack)
  | _ -> None

(* the jack under the mouse *)
let jack_at (m : model) (x : number) (y : number) : Studio_reason.port option =
  List.find_map
    (fun (id, b) ->
      match List.assoc_opt id m.modules with
      | None -> None
      | Some md ->
          let n = Array.length md.device.jacks in
          List.find_map
            (fun j ->
              let jx, jy = jack_pos b n j in
              if Float.abs (x - jx) <= 13. && Float.abs (y - jy) <= 13. then Some { Studio_reason.device = id; jack = j } else None)
            (List.init n (fun j -> j)))
    (boxes m)

(* the cable from the jack in the hand to [p], its ends the right way *)
let cable_to (fixed : Studio_reason.port) (p : Studio_reason.port) (m : model) : Studio_reason.cable =
  match List.assoc_opt fixed.device m.modules with
  | Some md when md.device.jacks.(fixed.jack).dir = Out -> { out = fixed; into = p }
  | _ -> { out = p; into = fixed }

(* the ropes following the patch's cables: kept for the cables still
 * there, made for the new, stepped with their ends where their jacks
 * are now *)
let ropes_of (m : model) : (Studio_reason.cable * Rack_cable.t) list =
  List.filter_map
    (fun (c : Studio_reason.cable) ->
      match (port_pos m c.out, port_pos m c.into) with
      | Some a, Some b -> Some (c, match List.assoc_opt c m.ropes with Some r -> Rack_cable.step r a b | None -> Rack_cable.make a b)
      | _ -> None)
    m.patch.cables

(*****************************************************************************)
(* update *)
(*****************************************************************************)

(* the letters: each the semitone above the octave's C; they play the
 * selected device, several at once, z and x an octave down and up *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

let play_letters (d : Rack_device.t) (octave : int) ~(now : string list) ~(was : string list) : int =
  let pressed k = List.mem k now && not (List.mem k was) and released k = List.mem k was && not (List.mem k now) in
  List.iter
    (fun (k, semitone) ->
      if pressed k then d.note_on ((12 *.. octave) +.. semitone) 0.9;
      if released k then d.note_off ((12 *.. octave) +.. semitone))
    letters;
  if pressed "z" then max 1 (octave -.. 1) else if pressed "x" then min 7 (octave +.. 1) else octave

let transport_y = -470.

(* the cables in the hand, plugged, picked up or pulled off *)
let back_input (computer : computer) (m : model) : model =
  let mouse = computer.mouse in
  let press = mouse.mdown && not m.was_down and release = (not mouse.mdown) && m.was_down in
  let s = Lazy.force studio in
  let hand = (mouse.mx, mouse.my) in
  match (m.drag, press, release) with
  | None, true, _ -> (
      match jack_at m mouse.mx mouse.my with
      | None -> m
      | Some p -> (
          match Studio_reason.cable_at m.patch p with
          | Some c when computer.keyboard.kshift ->
              (* pulled off: it falls *)
              let rope = match List.assoc_opt c m.ropes with Some r -> Rack_cable.release r | None -> Rack_cable.release (Rack_cable.make hand hand) in
              { m with patch = Studio_reason.disconnect m.patch p; falling = (rope, 60) :: m.falling }
          | Some c ->
              (* picked up by the end pressed: the other stays plugged *)
              let fixed = if c.out = p then c.into else c.out in
              let rope = match List.assoc_opt c m.ropes with Some r -> r | None -> Rack_cable.make hand hand in
              { m with patch = Studio_reason.disconnect m.patch p; drag = Some { fixed; rope } }
          | None -> (
              match port_pos m p with Some a -> { m with drag = Some { fixed = p; rope = Rack_cable.make a hand } } | None -> m)))
  | Some d, _, true -> (
      let dropped = { m with drag = None; falling = (Rack_cable.release d.rope, 60) :: m.falling } in
      match jack_at m mouse.mx mouse.my with
      | None -> dropped
      | Some p -> (
          let c = cable_to d.fixed p m in
          match Studio_reason.connect (Studio_reason.lookup s) m.patch c with
          | Ok patch -> { m with patch; drag = None; ropes = (c, d.rope) :: m.ropes }
          | Error why -> { dropped with message = Some (why, 90) }))
  | Some d, _, _ -> (
      match port_pos m d.fixed with
      | Some a ->
          (* near an edge, the rack scrolls under the cable *)
          let scroll = if mouse.my > rack_top - 40. then m.scroll - 8. else if mouse.my < rack_bottom m + 40. then m.scroll + 8. else m.scroll in
          { m with scroll; drag = Some { d with rope = Rack_cable.step d.rope a hand } }
      | None -> m)
  | _ -> m

(* the front under the mouse, in place; it keeps the mouse while held *)
let front_input (computer : computer) (m : model) : model =
  let mouse = computer.mouse in
  let press = mouse.mdown && not m.was_down in
  let bs = boxes m in
  let under = List.find_map (fun (id, (b : Widget.box)) -> if visible m b && Widget.contains b mouse.mx mouse.my then Some id else None) bs in
  let active = if press then under else if mouse.mdown then m.active else under in
  let selected = if press && under <> None && under <> Some Studio_reason.hardware then under else m.selected in
  let modules =
    List.map
      (fun (id, (md : Rack_module.t)) ->
        match (Some id = active, List.assoc_opt id bs) with
        | true, Some b -> (id, { md with front = Component.input_in ~scaled:true md.front computer (front_box md b) })
        | _ -> (id, md))
      m.modules
  in
  { m with modules; active; selected }

(*****************************************************************************)
(* The sequencer *)
(*****************************************************************************)

(* Reason's sequencer under the rack: the tracks down the left, one per
 * instrument, the selected one's notes on a grid -- a piano's keys
 * along its left edge, a row per key, a column per sixteenth -- the
 * playhead running over it. A click on the grid puts a note there (a
 * drag makes it longer), a click on a note takes it out; a key clicked
 * plays its note; the wheel moves the keys up and down. *)

let seq_bottom = -430.
let rows = 24
let row_h = (seq_top - 36. - seq_bottom) / float_of_int rows
let grid_left = -325.
let grid_right = 470.
let step_w (song : Song.t) : number = (grid_right - grid_left) / Song.length song
let row_y (m : model) (pitch : int) : number = seq_bottom + ((float_of_int (pitch -.. m.low) + 0.5) * row_h)
let is_black (pitch : int) : bool = List.mem (pitch mod 12) [ 1; 3; 6; 8; 10 ]

(* the instruments, in the rack's order: the tracks *)
let tracks (m : model) : (int * Rack_module.t) list =
  List.filter_map
    (fun (id, _) -> match List.assoc_opt id m.modules with Some md when md.device.role = Instrument -> Some (id, md) | _ -> None)
    m.patch.devices

let track_y (k : int) : number = seq_top - 50. - (float_of_int k * 28.)

let seq_input (computer : computer) (m : model) : model =
  let mouse = computer.mouse in
  let press = mouse.mdown && not m.was_down and release = (not mouse.mdown) && m.was_down in
  let track = Option.bind m.selected (fun id -> Option.map (fun md -> (id, md)) (List.assoc_opt id m.modules)) in
  let pitch = m.low +.. int_of_float (Float.of_int (truncate ((mouse.my - seq_bottom) / row_h))) in
  let time = Float.of_int (truncate ((mouse.mx - grid_left) / step_w m.song)) in
  let on_rows = mouse.my >= seq_bottom && mouse.my < seq_bottom + (float_of_int rows * row_h) in
  match (m.drawing, m.preview, track) with
  (* a note being drawn, longer as the mouse goes right; let go, it is in *)
  | Some n, _, Some (id, _) ->
      let n = { n with length = Float.max 1. (Float.min (Song.length m.song - n.start) (time - n.start + 1.)) } in
      if release || not mouse.mdown then { m with drawing = None; song = Song.add m.song id n } else { m with drawing = Some n }
  (* a key held: its note sounding until let go *)
  | _, Some p, Some (_, md) ->
      if mouse.mdown then m
      else begin
        md.device.note_off p;
        { m with preview = None }
      end
  | _ when not press -> m
  | _ when mouse.mx < grid_left - 70. ->
      (* a track clicked: its instrument selected *)
      let k = List.length (tracks m) in
      let hit = List.find_opt (fun (i, _) -> Float.abs (mouse.my - track_y i) <= 12.) (List.init k (fun i -> (i, ()))) in
      (match hit with Some (i, _) -> { m with selected = Some (fst (List.nth (tracks m) i)) } | None -> m)
  | _, _, Some (_, md) when mouse.mx < grid_left && on_rows ->
      md.device.note_on pitch 0.8;
      { m with preview = Some pitch }
  | _, _, Some (id, _) when on_rows && time >= 0. && time < Song.length m.song -> (
      match Song.note_at m.song id time pitch with
      | Some n -> { m with song = Song.remove m.song id n }
      | None -> { m with drawing = Some { start = time; length = 1.; pitch; velocity = 0.8 } })
  | _ -> m

let seq_view (m : model) : shape list =
  let sw = step_w m.song and len = Song.length m.song in
  let track = Option.bind m.selected (fun id -> Option.map (fun md -> (id, md)) (List.assoc_opt id m.modules)) in
  let color = match track with Some (_, md) -> md.color | None -> rgb 200 200 200 in
  let pane_h = seq_top - seq_bottom + 30. in
  let grid_w = grid_right - grid_left in
  let x_of t = grid_left + (t * sw) in
  (* the keys: a white column, the black keys over it *)
  let keys =
    (rectangle (rgb 235 235 230) 60. (float_of_int rows * row_h) |> move (grid_left - 35.) (seq_bottom + (float_of_int rows * row_h / 2.)))
    :: List.concat
      (List.init rows (fun r ->
           let p = m.low +.. r and y = row_y m (m.low +.. r) in
           [
             rectangle (if is_black p then rgb 36 36 40 else rgb 46 46 52) grid_w (row_h - 1.) |> move (grid_left + (grid_w / 2.)) y;
             rectangle (if m.preview = Some p then color else if is_black p then rgb 25 25 25 else rgb 235 235 230) (if is_black p then 38. else 60.) (if is_black p then row_h - 1. else row_h - 2.)
             |> move (if is_black p then grid_left - 46. else grid_left - 35.) y;
           ]
           @ if p mod 12 = 0 then [ words (rgb 80 80 80) (Printf.sprintf "C%d" ((p /.. 12) -.. 1)) |> scale 0.7 |> move (grid_left - 16.) y ] else []))
  in
  let lines =
    List.init (int_of_float len +.. 1) (fun k ->
        let bar = k mod 16 = 0 in
        rectangle (if bar then rgb 120 120 130 else if k mod 4 = 0 then rgb 75 75 82 else rgb 55 55 60) (if bar then 2. else 1.) (float_of_int rows * row_h)
        |> move (x_of (float_of_int k)) (seq_bottom + (float_of_int rows * row_h / 2.)))
  in
  let ruler = List.init (Song.(m.song.bars)) (fun b -> words (rgb 220 220 220) (string_of_int (b +.. 1)) |> scale 0.9 |> move (x_of (float_of_int (16 *.. b)) + 8.) (seq_top - 20.)) in
  let note_view (n : Song.note) =
    if n.pitch < m.low || n.pitch >= m.low +.. rows then []
    else [ rectangle color ((n.length * sw) - 2.) (row_h - 3.) |> move (x_of n.start + (n.length * sw / 2.)) (row_y m n.pitch); rectangle (rgb 20 20 20) 2. (row_h - 3.) |> move (x_of n.start + 1.) (row_y m n.pitch) ]
  in
  let notes = match track with Some (id, _) -> List.concat_map note_view (Song.notes m.song id) | None -> [] in
  let drawing = match m.drawing with Some n -> note_view n | None -> [] in
  let playhead =
    let s = Lazy.force studio in
    if Studio_reason.running s then [ rectangle (rgb 250 220 80) 2. (float_of_int rows * row_h + 20.) |> move (x_of (Studio_reason.position s)) (seq_bottom + (float_of_int rows * row_h / 2.) + 10.) ] else []
  in
  let track_list =
    List.concat
      (List.mapi
         (fun k (id, (md : Rack_module.t)) ->
           let chosen = m.selected = Some id in
           [ rectangle (if chosen then md.color else rgb 60 60 66) 104. 24. |> move (-440.) (track_y k); words (if chosen then black else rgb 220 220 220) md.name |> scale 0.8 |> move (-440.) (track_y k) ])
         (tracks m))
  in
  [ rectangle (rgb 30 30 34) 1000. pane_h |> move 0. (seq_bottom - 30. + (pane_h / 2.)); rectangle (rgb 70 70 76) 1000. 2. |> move 0. seq_top ]
  @ [ words (rgb 220 220 220) "SEQUENCER" |> scale 1.1 |> move (-420.) (seq_top - 20.) ]
  @ List.mapi (fun k line -> words (rgb 160 160 160) line |> scale 0.75 |> move (-440.) (seq_bottom + 60. - (float_of_int k * 16.)))
      [ "click: a note"; "drag: longer"; "again: out"; "a key: heard"; "wheel: octaves" ]
  @ track_list @ keys @ lines @ ruler @ notes @ drawing @ playhead

let update (computer : computer) (m : model) : model =
  let s = Lazy.force studio in
  ignore (Audio.instrument "reason" (fun () -> Studio_reason.instrument s));
  Gui.set_theme Theme.default;
  let kb = computer.keyboard in
  let now = Set_.elements kb.keys in
  let pressed k = List.mem k now && not (List.mem k m.keys) in
  (* the top bar: Create, and the selected device's presets *)
  let created = Gui.menu computer ~at:(-80., 475.) ("Create..." :: List.map fst catalogue) 0 in
  let m =
    if created > 0 then
      let patch, modules, id = add_module m.patch m.modules (fst (List.nth catalogue (created -.. 1))) ~below:m.selected ~selected:m.selected in
      { m with patch; modules; selected = Some id }
    else m
  in
  let m =
    match m.selected with
    | Some id -> (
        match List.assoc_opt id m.modules with
        | Some md when md.front.menu <> [] ->
            let names = List.tl md.front.menu in
            let chosen = Gui.menu computer ~at:(250., 475.) ("Preset..." :: names) 0 in
            if chosen > 0 then { m with modules = List.map (fun (i, x) -> if i = id then (i, { x with Rack_module.front = md.front.command (List.nth names (chosen -.. 1)) }) else (i, x)) m.modules } else m
        | _ -> m)
    | None -> m
  in
  let m = if Gui.button computer ~at:(90., 475.) (if m.seq then "Hide seq" else "Sequencer") then { m with seq = not m.seq; drawing = None } else m in
  (* the transport *)
  let running = Studio_reason.running s in
  if Gui.button computer ~at:(-400., transport_y) (if running then "STOP" else "PLAY") || pressed "space" then
    Studio_reason.run s (not running);
  let tempo = Float.round (Gui.knob computer ~at:(-280., transport_y + 4.) ~from:60. ~to_:180. m.patch.tempo) in
  let volume = Gui.knob computer ~at:(-160., transport_y + 4.) ~from:0. ~to_:1. m.patch.volume in
  let m = { m with patch = { m.patch with tempo; volume } } in
  (* Tab turns the rack round; Backspace takes the selected device out *)
  let m = if pressed "Tab" && m.flip = 0 then { m with side = (if m.side = Front then Back else Front); flip = 12; drag = None } else { m with flip = max 0 (m.flip -.. 1) } in
  let m =
    match m.selected with
    | Some id when kb.kbackspace && not (List.mem "Backspace" m.keys) && id <> Studio_reason.hardware ->
        { m with patch = Studio_reason.remove (Studio_reason.lookup s) m.patch id; modules = List.remove_assoc id m.modules; selected = None }
    | _ -> m
  in
  (* the wheel scrolls (60 pixels a notch), the arrows and the page keys
   * too, and the scrollbar pressed puts that part of the rack in view *)
  let rack_bottom = rack_bottom m in
  let room = Float.max 0. (total_height m - (rack_top - rack_bottom)) in
  let mouse = computer.mouse in
  let keys = (if List.mem "ArrowDown" now then 20. else 0.) - (if List.mem "ArrowUp" now then 20. else 0.) + (if pressed "PageDown" then 400. else 0.) - if pressed "PageUp" then 400. else 0. in
  let on_bar = mouse.mdown && Float.abs (mouse.mx - scrollbar_x) <= 12. && mouse.my <= rack_top && mouse.my >= rack_bottom in
  let scroll = if on_bar then ((rack_top - mouse.my) / (rack_top - rack_bottom) * total_height m) - ((rack_top - rack_bottom) / 2.) else m.scroll - (if m.seq && mouse.my < seq_top then 0. else mouse.mwheel * 60.) + keys in
  let m = { m with scroll = Float.max 0. (Float.min room scroll) } in
  (* the mouse, unless a menu has it or the rack is turning *)
  let in_seq = m.seq && (mouse.my < seq_top || m.drawing <> None || m.preview <> None) && m.drag = None in
  let m =
    if Gui.modal () || m.flip > 0 then m
    else if in_seq then seq_input computer m
    else if m.side = Back then back_input computer m
    else front_input computer m
  in
  let m = if m.seq && mouse.my < seq_top then { m with low = max 12 (min 96 (m.low +.. int_of_float (Float.round mouse.mwheel *. 2.))) } else m in
  (* the letters play the selected device *)
  let octave = match Option.bind m.selected (fun id -> List.assoc_opt id m.modules) with Some md -> play_letters md.device m.octave ~now ~was:m.keys | None -> m.octave in
  if m.patch != Studio_reason.patch s then Studio_reason.set_patch s m.patch;
  Studio_reason.set_song s m.song;
  let ropes = if m.side = Back then ropes_of m else m.ropes in
  let falling = List.filter_map (fun (r, n) -> if n <= 0 then None else Some (Rack_cable.step r (0., 0.) (0., 0.), n -.. 1)) m.falling in
  let message = match m.message with Some (w, n) when n > 0 -> Some (w, n -.. 1) | _ -> None in
  { m with ropes; falling; octave; message; was_down = computer.mouse.mdown; keys = now }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let rail = rgb 70 70 76

(* the rack's rails and each device's ears, screwed *)
let ears (b : Widget.box) : shape list =
  List.concat_map
    (fun x ->
      [ rectangle (rgb 150 150 158) 30. b.h |> move x b.y ]
      @ List.map (fun dy -> circle (rgb 90 90 96) 4. |> move x (b.y + dy)) (if b.h > unit then [ (b.h / 2.) - 12.; 12. - (b.h / 2.) ] else [ 0. ]))
    [ -.((width / 2.) + 15.); (width / 2.) + 15. ]

let front_view (m : model) (id : int) (b : Widget.box) : shape list =
  match List.assoc_opt id m.modules with
  | None -> []
  | Some md ->
      [ rectangle (rgb 25 25 28) b.w b.h |> move b.x b.y ]
      @ Component.draw_in ~scaled:true md.front (front_box md b) ~active:(m.selected = Some id)
      @ (if m.selected = Some id then [ rectangle md.color 4. b.h |> move (b.x - (b.w / 2.) + 2.) b.y ] else [])
      @ ears b

(* a jack: a hole in a silver ring, its label under; while a cable is in
 * the hand, ringed green if it may go in, dimmed if not *)
let jack_view (m : model) (id : int) (md : Rack_module.t) (b : Widget.box) (j : int) : shape list =
  let x, y = jack_pos b (Array.length md.device.jacks) j in
  let jack = md.device.jacks.(j) in
  let s = Lazy.force studio in
  let verdict =
    match m.drag with
    | Some d when d.fixed <> { device = id; jack = j } -> Some (Studio_reason.connect (Studio_reason.lookup s) m.patch (cable_to d.fixed { device = id; jack = j } m))
    | _ -> None
  in
  let ring = match verdict with Some (Ok _) -> rgb 90 230 110 | Some (Error _) -> rgb 70 70 70 | None -> rgb 190 190 195 in
  [
    circle ring 12. |> move x y;
    circle (rgb 10 10 10) 8. |> move x y;
    circle (match jack.signal with Audio -> rgb 200 60 50 | Cv -> rgb 230 200 60) 2. |> move (x + 9.) (y + 9.);
    words (rgb 210 210 210) jack.label |> scale 0.55 |> move x (y - 19.);
  ]

let back_view (m : model) (id : int) (b : Widget.box) : shape list =
  match List.assoc_opt id m.modules with
  | None -> []
  | Some md ->
      (* its back plate, edged, its name stencilled *)
      [
        rectangle (rgb 22 22 26) b.w b.h |> move b.x b.y;
        rectangle (rgb 52 52 58) (b.w - 8.) (b.h - 6.) |> move b.x b.y;
        words (rgb 110 110 118) (String.uppercase_ascii md.name) |> scale 1.6 |> move (b.x + 330.) (b.y + (b.h / 2.) - 18.);
      ]
      @ List.concat_map (jack_view m id md b) (List.init (Array.length md.device.jacks) (fun j -> j))
      @ ears b

(* the audio cables red and orange, the CV yellow and green, a shade a
 * cable, the same each time *)
let cable_color (c : Studio_reason.cable) (signal : Rack_device.signal) : color =
  let shades = match signal with Audio -> [| rgb 200 50 45; rgb 230 110 40; rgb 180 40 90 |] | Cv -> [| rgb 230 200 50; rgb 120 200 70; rgb 60 170 120 |] in
  shades.(((c.out.device *.. 7) +.. (c.out.jack *.. 3) +.. c.into.device) mod 3)

let rope_view (color : color) (r : Rack_cable.t) : shape list =
  let pts = Rack_cable.points r in
  let plug (x, y) (x', y') =
    let angle = atan2 (y' - y) (x' - x) * 180. / Float.pi in
    group [ rectangle (rgb 30 30 30) 26. 12.; rectangle color 8. 12. |> move 9. 0. ] |> rotate angle |> move x y
  in
  let ends = match (pts, List.rev pts) with a :: a' :: _, b :: b' :: _ -> [ plug a a'; plug b b' ] | _ -> [] in
  [
    polygon black (Rack_cable.ribbon r ~width:8.) |> move 6. (-8.) |> fade 0.3;
    polygon color (Rack_cable.ribbon r ~width:8.);
    polygon (rgb 255 255 255) (Rack_cable.ribbon r ~width:2.) |> move 1.5 1.5 |> fade 0.35;
  ]
  @ ends

let cables_view (m : model) : shape list =
  List.concat_map
    (fun ((c : Studio_reason.cable), r) ->
      let signal = match List.assoc_opt c.out.device m.modules with Some md -> md.device.jacks.(c.out.jack).signal | None -> Audio in
      rope_view (cable_color c signal) r)
    m.ropes
  @ List.concat_map (fun (r, _) -> rope_view (rgb 150 150 150) r) m.falling
  @ match m.drag with Some d -> rope_view (rgb 240 240 240) d.rope | None -> []

(* the rack turning: each device a box narrowing to nothing and
 * widening again, the other side's colour after the middle *)
let flip_view (m : model) : shape list =
  let k = 12 -.. m.flip in
  let w = width * Float.abs (cos (Float.pi * float_of_int k / 12.)) in
  let showing = if k < 6 then (if m.side = Front then Back else Front) else m.side in
  List.concat_map
    (fun (_, (b : Widget.box)) ->
      [ rectangle (if showing = Front then rgb 90 90 96 else rgb 48 48 54) w b.h |> move b.x b.y; rectangle (rgb 20 20 20) w 2. |> move b.x (b.y + (b.h / 2.)) ])
    (List.filter (fun (_, b) -> visible m b) (boxes m))

let view (computer : computer) (m : model) : shape list =
  let bs = List.filter (fun (_, b) -> visible m b) (boxes m) in
  let running = Studio_reason.running (Lazy.force studio) in
  [ rectangle (rgb 30 30 34) computer.screen.width computer.screen.height ]
  @ [ rectangle rail 18. 1000. |> move (-.((width / 2.) + 15.)) 0.; rectangle rail 18. 1000. |> move ((width / 2.) + 15.) 0. ]
  @ (if m.flip > 0 then flip_view m
     else if m.side = Front then List.concat_map (fun (id, b) -> front_view m id b) bs
     else List.concat_map (fun (id, b) -> back_view m id b) bs @ cables_view m)
  @ (if m.seq then seq_view m else [])
  (* the scrollbar *)
  @ (let total = total_height m and room = rack_top - rack_bottom m in
     if total <= room then []
     else
       let h = room * room / total in
       [ rectangle (rgb 60 60 66) 12. room |> move scrollbar_x (rack_top - (room / 2.)); rectangle (rgb 160 160 170) 12. h |> move scrollbar_x (rack_top - (h / 2.) - (m.scroll * room / total)) ])
  (* the bars *)
  @ [ rectangle (rgb 50 50 56) computer.screen.width 60. |> move 0. 470.; rectangle (rgb 50 50 56) computer.screen.width 60. |> move 0. transport_y ]
  @ [
      words white "TinyReason" |> scale 2. |> move (-380.) 475.;
      words (rgb 200 200 200) (if m.side = Front then "Tab: the back" else "Tab: the front") |> scale 1.1 |> move 400. 475.;
      words (rgb 200 200 200) (Printf.sprintf "TEMPO %.0f" m.patch.tempo) |> scale 0.9 |> move (-280.) (transport_y - 22.);
      words (rgb 200 200 200) "VOLUME" |> scale 0.9 |> move (-160.) (transport_y - 22.);
      words (rgb 200 200 200)
        (Printf.sprintf "%s   space: play   letters: the %s   Backspace: out" (if running then "playing" else "stopped")
           (match Option.bind m.selected (fun id -> List.assoc_opt id m.modules) with Some md -> md.name | None -> "selected"))
      |> scale 1. |> move 180. transport_y;
    ]
  @ (match m.message with Some (why, _) -> [ words (rgb 250 120 100) why |> scale 1.3 |> move (computer.mouse.mx + 10.) (computer.mouse.my + 28.) ] | None -> [])
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
