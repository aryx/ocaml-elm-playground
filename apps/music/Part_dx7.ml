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

let kind = "dx7"
let natural = (960., 494.)

(* the panel's own coordinates are TinyDX7's screen's, the panel
 * centred at (0, 197): a box elsewhere moves it there *)
let centre_y = 197.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)
let knobs = Array.of_list Voice_dx7.knobs

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
let lcd (p : Voice_dx7.patch) (param : int) (number : int) : string * string =
  let k = knobs.(param) in
  let value = Control.to_string k.control (k.get p) in
  let name = String.uppercase_ascii (String.map (fun c -> if c = '.' || c = '_' then ' ' else c) k.name) in
  let width = 16 -.. String.length value -.. 1 in
  let name = if String.length name > width then String.sub name 0 width else name in
  (Printf.sprintf "%02d %s" (number +.. 1) (String.trim p.name), Printf.sprintf "%-*s %s" width name value)

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

let segment = Meters.segment

let lit (level : number) : color =
  (* 0 to 2 cycles, on a log scale: 60 dB of glow *)
  let x = if level <= 0. then 0. else Float.max 0. (Float.min 1. (1. + (log10 (level / 2.) / 3.))) in
  let c base range = int_of_float (base + (range * x)) in
  rgb (c 40. 200.) (c 40. 170.) (c 30. 60.)

let graph_view (voice : Voice_dx7.t) : shape list =
  let p = Voice_dx7.patch voice in
  let alg = Fm_algorithm.get p.algorithm in
  let places = layout alg in
  let at op = op_position (List.assoc op places) in
  let levels = Voice_dx7.levels voice in
  let edge (a, b) = segment (rgb 200 190 160) 3. (at a) (at b) in
  let from, into = alg.feedback in
  let fx, fy = at from and ix, iy = at into in
  let feedback =
    (* the loop: out of [from]'s right, up and over to [into] *)
    let c = if p.feedback > 0 then rgb 230 120 60 else rgb 110 100 80 in
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
  [ words (rgb 230 220 190) (Printf.sprintf "ALGORITHM %d   FEEDBACK %d" p.algorithm p.feedback) |> scale 1.3 |> move (graph_x + 110.) (bottom - 22.) ]
  @ feedback @ List.map edge alg.edges @ out
  @ List.map op_box [ 1; 2; 3; 4; 5; 6 ]

(* the operator under the mouse, in the graph *)
let op_at (p : Voice_dx7.patch) (x : number) (y : number) : int option =
  let places = layout (Fm_algorithm.get p.algorithm) in
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

let envelopes_view (p : Voice_dx7.patch) : shape list =
  List.concat
    (List.init 6 (fun i ->
         let cx = 20. + (float_of_int (i mod 3) * 170.) and cy = 200. - (float_of_int (i /.. 3) * 95.) in
         let o = p.operators.(i) in
         let title = words (rgb 230 220 190) (Printf.sprintf "OP%d  %s %d" (i +.. 1) (if o.fixed then "FIXED" else "RATIO") o.level) in
         (title |> scale 1.1 |> move cx (cy + 42.)) :: envelope_view o cx cy))

(*****************************************************************************)
(* input *)
(*****************************************************************************)

type state = {
  voice : Voice_dx7.t;
  ui : Immediate.t;
  param : int; (* an index in Voice_dx7.knobs: the one the LCD shows *)
  number : unit -> int;
}

let box (x, y) (w, h) : Widget.box = { Widget.x; y; w; h }

let button (ui : Immediate.t) (at : number * number) (s : string) : Immediate.t * bool =
  Immediate.button ui (box at (Immediate.button_size (Immediate.theme ui) s)) s

(* a frame, the mouse [i] in the panel's coordinates *)
let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let patch = Voice_dx7.patch st.voice in
  (* the DX7's way: one parameter, chosen with the buttons, changed with
   * the slider or -1 and +1 *)
  let n = Array.length knobs in
  let param = st.param in
  let ui, b = button ui (-420., 300.) "< PARAM" in
  let param = if b then (param +.. n -.. 1) mod n else param in
  let ui, b = button ui (-310., 300.) "PARAM >" in
  let param = if b then (param +.. 1) mod n else param in
  let ui, b = button ui (-200., 300.) "< OP" in
  let param = if b then jump_operator param (-1) else param in
  let ui, b = button ui (-110., 300.) "OP >" in
  let param = if b then jump_operator param 1 else param in
  let k = knobs.(param) in
  let top = float_of_int (positions k -.. 1) in
  let v = k.get patch in
  let ui, b = button ui (-420., 255.) "-1" in
  let v = if b then Float.max 0. (v - 1.) else v in
  let ui, b = button ui (-360., 255.) "+1" in
  let v = if b then Float.min top (v + 1.) else v in
  let ui, v = Immediate.slider ui (box (-190., 255.) (Immediate.slider_size (Immediate.theme ui))) ~from:0. ~to_:top v in
  let v = Float.round v in
  let patch = if v <> k.get patch then k.put patch v else patch in
  (* a click on an operator in the graph: its output level *)
  let param =
    match (i.mclick, op_at patch i.mx i.my) with
    | true, Some op ->
        let name = Printf.sprintf "op%d.output" op in
        let rec find j = if j >= n then param else if knobs.(j).name = name then j else find (j +.. 1) in
        find 0
    | _ -> param
  in
  Voice_dx7.set_patch st.voice patch;
  { st with ui; param }

let input (computer : computer) (b : Widget.box) (st : state) : state =
  let dx, dy = offset b in
  step_ui (Panel.input computer ~dx ~dy) st

(*****************************************************************************)
(* draw *)
(*****************************************************************************)

let ink = rgb 230 220 190

(* the DX7's front: brown and black, its LCD, the membrane's colours *)
let panel_view (st : state) : shape list =
  let line1, line2 = lcd (Voice_dx7.patch st.voice) st.param (st.number ()) in
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

let draw (st : state) (b : Widget.box) ~active:_ : shape list =
  let dx, dy = offset b in
  List.map (move dx dy) (panel_view st @ graph_view st.voice @ envelopes_view (Voice_dx7.patch st.voice)) @ Panel.shapes st.ui ~dx ~dy

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
    menu = "Voice" :: List.map fst Voice_dx7.presets;
    command =
      (fun c ->
        Option.iter (Voice_dx7.set_patch st.voice) (List.assoc_opt c Voice_dx7.presets);
        part st);
    save = (fun () -> Voice_dx7.to_string (Voice_dx7.patch st.voice));
  }

(* the voice's place among the presets, 0 if it is none of them *)
let preset_number (voice : Voice_dx7.t) () : int =
  let p = Voice_dx7.patch voice in
  let rec find i = function [] -> 0 | (_, q) :: rest -> if q = p then i else find (i +.. 1) rest in
  find 0 Voice_dx7.presets

(* painted once, untouched, so that a host can draw it before its first
 * input *)
let make ?number (voice : Voice_dx7.t) : Component.part =
  let number = match number with Some f -> f | None -> preset_number voice in
  part (step_ui Panel.neutral { voice; ui = Immediate.empty; param = 0; number })

let load (voice : Voice_dx7.t) (text : string) : Component.part =
  (match Voice_dx7.of_string text with Ok p -> Voice_dx7.set_patch voice p | Error _ -> ());
  make voice
