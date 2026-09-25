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

(* See Teletype.mli *)


(*****************************************************************************)
(* The keyboard *)
(*****************************************************************************)

let keyboard_bytes (computer : computer) ~(before : keyboard) : string =
  let now = computer.keyboard in
  let went_down = Set_.elements (Set_.diff now.keys before.keys) in
  if Set_.mem "Control" now.keys then
    (* Control and a letter: the letter's code, not the letter typed *)
    String.concat "" (List.filter_map (Vt.key ~ctrl:true) went_down)
  else
    (* the characters come typed; the named keys (longer than one
       character: "Enter", "ArrowUp") as the terminal sends them *)
    now.typed ^ String.concat "" (List.filter_map (fun k -> if String.length k > 1 then Vt.key ~ctrl:false k else None) went_down)

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(* the phosphor's green, and the roll's paper and ink *)
let phosphor = rgb 120 230 120
let glass = rgb 10 14 10
let ink = rgb 30 30 40
let roll = rgb 245 240 220

let palette (c : Vt.color) ~(default : color) : color =
  match c with
  | Default -> default
  | Black -> rgb 0 0 0
  | Red -> rgb 205 49 49
  | Green -> rgb 13 188 121
  | Yellow -> rgb 229 229 16
  | Blue -> rgb 36 114 200
  | Magenta -> rgb 188 63 188
  | Cyan -> rgb 17 168 205
  | White -> rgb 229 229 229

(* a cell is 0.6 as wide as it is high, the whole grid as large as the
   screen lets it be: a cell's height *)
let cell_height (computer : computer) (vt : Vt.t) : number =
  let screen = computer.screen in
  0.95 *. min (screen.height /. float_of_int (Vt.rows vt)) (screen.width /. (0.6 *. float_of_int (Vt.cols vt)))

let screen_size (computer : computer) (vt : Vt.t) : number * number =
  let h = cell_height computer vt in
  (0.6 *. h *. float_of_int (Vt.cols vt), h *. float_of_int (Vt.rows vt))

let size (computer : computer) (m : Talk.machine) : number * number = screen_size computer (Talk.screen m)

let draw_screen ?(paper = false) ?(capitals = paper) ?(phosphor = phosphor) ~(cursor : bool) (computer : computer) (vt : Vt.t) :
    shape list =
  let rows = Vt.rows vt and cols = Vt.cols vt in
  let fg0, bg0 = if paper then (ink, roll) else (phosphor, glass) in
  let h = cell_height computer vt in
  let w = 0.6 *. h in
  let x c = (-.w *. float_of_int cols /. 2.) +. ((float_of_int c +. 0.5) *. w) in
  let y r = (h *. float_of_int rows /. 2.) -. ((float_of_int r +. 0.5) *. h) in
  let size = h /. words_font_size *. 0.8 in
  let cells =
    List.init rows (fun r ->
        List.init cols (fun c ->
            let cell = Vt.cell vt r c in
            let fg = palette cell.attrs.fg ~default:fg0 and bg = palette cell.attrs.bg ~default:bg0 in
            let fg, bg = if cell.attrs.reverse then (bg, fg) else (fg, bg) in
            (* a pixel wider and higher, so that neighbours leave no seam *)
            let back = if cell.attrs.reverse || cell.attrs.bg <> Vt.Default then [ rectangle bg (w +. 1.) (h +. 1.) |> move (x c) (y r) ] else [] in
            let glyph = if capitals then String.uppercase_ascii cell.glyph else cell.glyph in
            let letter dx = words fg glyph |> scale size |> move (x c +. dx) (y r) in
            (* bold as a teletype did it: struck twice, a hair apart *)
            let front = if glyph = " " then [] else if cell.attrs.bold then [ letter 0.; letter (w *. 0.08) ] else [ letter 0. ] in
            back @ front)
        |> List.concat)
    |> List.concat
  in
  let cursor =
    let r, c = Vt.cursor vt in
    let (Time now) = computer.time in
    if cursor && Vt.cursor_visible vt && Float.rem now 1. < 0.5 then [ rectangle fg0 w h |> fade 0.6 |> move (x c) (y r) ] else []
  in
  (rectangle bg0 (w *. float_of_int cols) (h *. float_of_int rows) :: cells) @ cursor

(* the cursor blinks while the program waits for the keyboard *)
let draw ?paper ?capitals ?phosphor (computer : computer) (m : Talk.machine) : shape list =
  draw_screen ?paper ?capitals ?phosphor ~cursor:(Talk.reading m) computer (Talk.screen m)

(*****************************************************************************)
(* The application *)
(*****************************************************************************)

type state = {
  machine : Talk.machine option; (* None until the first frame, which has the flags *)
  seed : int; (* the next run's *)
  keys : unit Scene2d.t;
}

let teletype ?(rows = 24) ?(cols = 80) ?view:user_view (program : unit Talk.talk) : (state game, msg) app =
  let flag name = List.assoc_opt name in
  let make (computer : computer) (seed : int) : Talk.machine =
    let baud = Option.bind (flag "baud" computer.flags) int_of_string_opt in
    Talk.start ?baud ~seed ~rows ~cols program
  in
  let update (computer : computer) (s : state) : state =
    let before = s.keys.keys in
    let keys = Scene2d.update computer s.keys in
    let (Time now) = computer.time in
    let dt = match s.keys.last with None -> 0. | Some last -> now -. last in
    match s.machine with
    | None ->
        let seed = Option.value (Option.bind (flag "seed" computer.flags) int_of_string_opt) ~default:1 in
        { machine = Some (make computer seed); seed = seed + 1; keys }
    | Some m when Talk.finished m && Scene2d.pressed (fun k -> k.kenter) keys ->
        { machine = Some (make computer s.seed); seed = s.seed + 1; keys }
    | Some m -> { s with machine = Some (Talk.tick (Talk.input m (keyboard_bytes computer ~before)) dt); keys }
  in
  let view (computer : computer) (s : state) : shape list =
    match s.machine with
    | None -> []
    | Some m ->
        let paper = flag "paper" computer.flags <> None in
        (match user_view with
        | Some v -> v computer m
        | None -> rectangle (if paper then roll else glass) computer.screen.width computer.screen.height :: draw ~paper computer m)
        @ if Talk.finished m then [ words (if paper then ink else phosphor) "(the end -- Enter to run it again)" |> move_y (computer.screen.bottom +. 20.) ] else []
  in
  game view update { machine = None; seed = 1; keys = Scene2d.start () }
