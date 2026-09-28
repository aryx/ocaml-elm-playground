(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground
open Basics (* float arithmetics *)

let kind = "juno"
let natural = (960., 420.)

(* the panel's own coordinates are TinyJuno's screen's, the panel
 * centred at (0, 250): a box elsewhere moves it there *)
let centre_y = 250.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)

(*****************************************************************************)
(* The sliders *)
(*****************************************************************************)

(* a slider: its control's name, its x, its label; the tracks all
 * between [bottom] and [bottom + track] *)
type slider = { name : string; x : number; label : string }

let bottom = 210.
let track = 160.

let sections : (string * (string * string) list) list =
  [
    ("LFO", [ ("lfo.rate", "RATE"); ("lfo.delay", "DELAY") ]);
    ("DCO", [ ("dco.lfo", "LFO"); ("dco.pwm", "PWM"); ("dco.sub", "SUB"); ("dco.noise", "NOISE") ]);
    ("HPF", [ ("hpf", "FREQ") ]);
    ("VCF", [ ("vcf.cutoff", "FREQ"); ("vcf.resonance", "RES"); ("vcf.env", "ENV"); ("vcf.lfo", "LFO"); ("vcf.key", "KYBD") ]);
    ("VCA", [ ("vca.level", "LEVEL") ]);
    ("ENV", [ ("env.attack", "A"); ("env.decay", "D"); ("env.sustain", "S"); ("env.release", "R") ]);
  ]

let step = 44.
let gap = 24.

(* the sliders' x, section after section, a gap between them *)
let sliders : slider list =
  let x = ref (-440.) in
  List.concat_map
    (fun (_, controls) ->
      let s = List.map (fun (name, label) -> let sl = { name; x = !x; label } in x := !x + step; sl) controls in
      x := !x + gap;
      s)
    sections

let knob (name : string) : Voice_juno.knob = List.find (fun (k : Voice_juno.knob) -> k.name = name) Voice_juno.knobs

(* a control's value as a slider's position, 0 to 1, and back (a
 * selector snapping to its positions) *)
let position (p : Voice_juno.patch) (name : string) : number =
  let k = knob name in
  match k.control with
  | Selector labels -> k.get p / float_of_int (List.length labels -.. 1)
  | _ -> k.get p

let set_position (p : Voice_juno.patch) (name : string) (pos : number) : Voice_juno.patch =
  let k = knob name in
  let pos = Float.max 0. (Float.min 1. pos) in
  match k.control with
  | Selector labels -> k.put p (Float.round (pos * float_of_int (List.length labels -.. 1)))
  | _ -> k.put p pos

let slider_at (x : number) (y : number) : string option =
  List.find_map (fun s -> if Float.abs (x - s.x) <= 14. && y >= bottom - 12. && y <= bottom + track + 12. then Some s.name else None) sliders

(* the buttons: a label, where, whether lit, and what a click does *)
type button = { text : string; bx : number; by : number; lit : Voice_juno.patch -> bool; click : Voice_juno.patch -> Voice_juno.patch }

let x_of name = (List.find (fun s -> s.name = name) sliders).x

let buttons : button list =
  let b text bx by lit click = { text; bx; by; lit; click } in
  let row = 150. in
  List.mapi (fun i r -> b r (x_of "dco.lfo" + (float_of_int i * 40.)) row (fun p -> p.range = i) (fun p -> { p with range = i })) Voice_juno.ranges
  @ [
      b "PULSE" (x_of "dco.lfo" + 10.) 95. (fun p -> p.pulse) (fun p -> { p with pulse = not p.pulse });
      b "SAW" (x_of "dco.lfo" + 80.) 95. (fun p -> p.saw) (fun p -> { p with saw = not p.saw });
      b "PWM LFO" (x_of "dco.sub" + 40.) 150. (fun p -> p.pwm_lfo) (fun p -> { p with pwm_lfo = not p.pwm_lfo });
      b "ENV -" (x_of "vcf.env") 150. (fun p -> p.env_invert) (fun p -> { p with env_invert = not p.env_invert });
      b "GATE" (x_of "vca.level") 150. (fun p -> p.gate) (fun p -> { p with gate = not p.gate });
    ]
  @ List.mapi
      (fun i c -> b c (x_of "env.attack" - 10. + (float_of_int i * 44.)) 95. (fun p -> p.chorus = i) (fun p -> { p with chorus = i }))
      Voice_juno.choruses

(*****************************************************************************)
(* input *)
(*****************************************************************************)

type state = {
  voice : Voice_juno.t;
  sliding : string option; (* the slider the mouse holds *)
  was_down : bool;
}

(* a frame, the mouse [i] in the panel's coordinates *)
let step_ui (i : Widget.input) (st : state) : state =
  let patch = Voice_juno.patch st.voice in
  (* a slider pressed follows the mouse until let go *)
  let sliding =
    if not i.mdown then None else match st.sliding with Some s -> Some s | None -> if st.was_down then None else slider_at i.mx i.my
  in
  let patch = match sliding with Some name -> set_position patch name ((i.my - bottom) / track) | None -> patch in
  (* the buttons, on a click *)
  let patch =
    if i.mclick then
      List.fold_left (fun p b -> if Float.abs (i.mx - b.bx) <= 20. && Float.abs (i.my - b.by) <= 12. then b.click p else p) patch buttons
    else patch
  in
  Voice_juno.set_patch st.voice patch;
  { st with sliding; was_down = i.mdown }

let input (computer : computer) (b : Widget.box) (st : state) : state =
  let dx, dy = offset b in
  step_ui (Panel.input computer ~dx ~dy) st

(*****************************************************************************)
(* draw *)
(*****************************************************************************)

let ink = rgb 230 230 230
let orange = rgb 235 120 50

let slider_view (p : Voice_juno.patch) (s : slider) : shape list =
  let y = bottom + (position p s.name * track) in
  [
    rectangle (rgb 10 10 10) 6. track |> move s.x (bottom + (track / 2.));
    rectangle (rgb 240 240 240) 26. 12. |> move s.x y;
    rectangle (rgb 30 30 30) 26. 2. |> move s.x y;
    words ink s.label |> scale 0.9 |> move s.x (bottom - 22.);
  ]

let button_view (p : Voice_juno.patch) (b : button) : shape list =
  [
    circle (if b.lit p then rgb 255 60 40 else rgb 70 20 15) 4. |> move b.bx (b.by + 18.);
    rectangle (rgb 90 90 95) 38. 20. |> move b.bx b.by;
    words ink b.text |> scale 0.8 |> move b.bx b.by;
  ]

(* the 106's front: black, its sections named in orange over lines *)
let panel_view (p : Voice_juno.patch) : shape list =
  let titles =
    List.map
      (fun (title, controls) ->
        let xs = List.map (fun (name, _) -> x_of name) controls in
        let lo = List.fold_left Float.min 1e9 xs and hi = List.fold_left Float.max (-1e9) xs in
        group [ rectangle orange (hi - lo + 32.) 2. |> move ((lo + hi) / 2.) 400.; words orange title |> scale 1.2 |> move ((lo + hi) / 2.) 414. ])
      sections
  in
  [ rectangle (rgb 30 30 32) 960. 420. |> move 0. 250.; rectangle (rgb 150 150 155) 960. 16. |> move 0. 452. ]
  @ [ words (rgb 30 30 30) "Roland  JUNO-106" |> scale 1.3 |> move 330. 452.; words orange "CHORUS" |> scale 1.1 |> move (x_of "env.attack" + 56.) 125. ]
  @ titles
  @ List.concat_map (slider_view p) sliders
  @ List.concat_map (button_view p) buttons

let draw (st : state) (b : Widget.box) ~active:_ : shape list =
  let dx, dy = offset b in
  List.map (move dx dy) (panel_view (Voice_juno.patch st.voice))

(*****************************************************************************)
(* The part *)
(*****************************************************************************)

let rec part (st : state) : Component.part =
  {
    kind;
    height = (fun w -> snd natural * w / fst natural);
    natural = Some natural;
    draw = draw st;
    input = (fun computer b -> part (input computer b st));
    menu = "Preset" :: List.map fst Voice_juno.presets;
    command =
      (fun c ->
        Option.iter (Voice_juno.set_patch st.voice) (List.assoc_opt c Voice_juno.presets);
        part st);
    save = (fun () -> Voice_juno.to_string (Voice_juno.patch st.voice));
  }

let make (voice : Voice_juno.t) : Component.part = part { voice; sliding = None; was_down = false }

let load (voice : Voice_juno.t) (text : string) : Component.part =
  (match Voice_juno.of_string text with Ok p -> Voice_juno.set_patch voice p | Error _ -> ());
  make voice
