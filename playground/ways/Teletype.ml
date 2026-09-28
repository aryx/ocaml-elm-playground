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
  else if Set_.mem "Alt" now.keys then
    (* claude: Alt and a key: Escape then the key, Meta as a terminal
       without a Meta key sends it (xterm's metaSendsEscape), what
       Emacs's M-x reads; the character typed when there is one, M-<
       being Alt and Shift and the comma *)
    let named = List.filter_map (fun k -> if String.length k > 1 then Vt.key ~alt:true ~ctrl:false k else None) went_down in
    let chars =
      if now.typed <> "" then List.init (String.length now.typed) (fun i -> String.make 1 now.typed.[i])
      else List.filter (fun k -> String.length k = 1) went_down
    in
    String.concat "" (List.map (fun k -> "\x1b" ^ k) chars @ named)
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

(* claude: the IBM PC's colour text mode (the CGA, 1981, and the
   EGA and VGA after it): eight colours, each with a bright twin that
   the foreground gets from the bold bit -- so yellow is bold brown, and
   white is bold light grey. Turbo Pascal's blue, grey and yellow are
   these, the same on every PC *)
let cga (c : Vt.color) ~(bright : bool) : color =
  let v k = if bright then (if k = 0 then 85 else 255) else if k = 0 then 0 else 170 in
  match c with
  | Black -> if bright then rgb 85 85 85 else rgb 0 0 0
  | Blue -> rgb (v 0) (v 0) (v 1)
  | Green -> rgb (v 0) (v 1) (v 0)
  | Cyan -> rgb (v 0) (v 1) (v 1)
  | Red -> rgb (v 1) (v 0) (v 0)
  | Magenta -> rgb (v 1) (v 0) (v 1)
  | Yellow -> if bright then rgb 255 255 85 else rgb 170 85 0 (* brown, the CGA's own *)
  | White | Default -> if bright then rgb 255 255 255 else rgb 170 170 170

(* claude: the PC's line-drawing characters (code page 437's, which
   text-mode programs framed their windows with), drawn as lines rather
   than by the font: which arms go from the cell's centre, left, right,
   up, down, and whether they are doubled *)
let box_arms (glyph : string) : (bool * bool * bool * bool * bool) option =
  match glyph with
  | "─" -> Some (true, true, false, false, false)
  | "│" -> Some (false, false, true, true, false)
  | "┌" -> Some (false, true, false, true, false)
  | "┐" -> Some (true, false, false, true, false)
  | "└" -> Some (false, true, true, false, false)
  | "┘" -> Some (true, false, true, false, false)
  | "├" -> Some (false, true, true, true, false)
  | "┤" -> Some (true, false, true, true, false)
  | "═" -> Some (true, true, false, false, true)
  | "║" -> Some (false, false, true, true, true)
  | "╔" -> Some (false, true, false, true, true)
  | "╗" -> Some (true, false, false, true, true)
  | "╚" -> Some (false, true, true, false, true)
  | "╝" -> Some (true, false, true, false, true)
  | _ -> None

(* the lines of a box character in a cell [w] by [h] centred at (x, y);
   a double corner is two corners, the outer one wider, so that the
   frame's two lines meet as two nested right angles *)
let box_lines (color : color) (w : number) (h : number) (x : number) (y : number) (left, right, up, down, double) : shape list =
  let t = max 1. (h *. 0.07) in
  let seg x1 y1 x2 y2 = rectangle color (abs_float (x2 -. x1) +. t) (abs_float (y2 -. y1) +. t) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.) in
  let hx = if right then 1. else -1. and vy = if up then 1. else -1. in
  let ex = w /. 2. +. 0.5 and ey = h /. 2. +. 0.5 in
  if not double then
    (if left then [ seg x y (x -. ex) y ] else [])
    @ (if right then [ seg x y (x +. ex) y ] else [])
    @ (if up then [ seg x y x (y +. ey) ] else [])
    @ if down then [ seg x y x (y -. ey) ] else []
  else
    let d = w *. 0.16 in
    match ((left || right) && not (left && right), (up || down) && not (up && down)) with
    | true, true ->
        (* a corner: the outer lines away from the arms, the inner ones
           towards them *)
        [ seg (x -. (hx *. d)) (y -. (vy *. d)) (x +. (hx *. ex)) (y -. (vy *. d));
          seg (x -. (hx *. d)) (y -. (vy *. d)) (x -. (hx *. d)) (y +. (vy *. ey));
          seg (x +. (hx *. d)) (y +. (vy *. d)) (x +. (hx *. ex)) (y +. (vy *. d));
          seg (x +. (hx *. d)) (y +. (vy *. d)) (x +. (hx *. d)) (y +. (vy *. ey)) ]
    | _ ->
        (if left || right then [ seg (x -. ex) (y +. d) (x +. ex) (y +. d); seg (x -. ex) (y -. d) (x +. ex) (y -. d) ] else [])
        @ if up || down then [ seg (x -. d) (y -. ey) (x -. d) (y +. ey); seg (x +. d) (y -. ey) (x +. d) (y +. ey) ] else []

(* a cell is 0.6 as wide as it is high, the whole grid as large as the
   screen lets it be: a cell's height *)
let cell_height (computer : computer) (vt : Vt.t) : number =
  let screen = computer.screen in
  0.95 *. min (screen.height /. float_of_int (Vt.rows vt)) (screen.width /. (0.6 *. float_of_int (Vt.cols vt)))

let screen_size (computer : computer) (vt : Vt.t) : number * number =
  let h = cell_height computer vt in
  (0.6 *. h *. float_of_int (Vt.cols vt), h *. float_of_int (Vt.rows vt))

let size (computer : computer) (m : Talk.machine) : number * number = screen_size computer (Talk.screen m)

let draw_screen ?(paper = false) ?(capitals = paper) ?(phosphor = phosphor) ?(pc = false) ~(cursor : bool) (computer : computer) (vt : Vt.t) :
    shape list =
  let rows = Vt.rows vt and cols = Vt.cols vt in
  let fg0, bg0 = if pc then (cga White ~bright:false, rgb 0 0 0) else if paper then (ink, roll) else (phosphor, glass) in
  (* claude: on a PC, bold is the bright colour, not a second stroke *)
  let palette (c : Vt.color) ~(default : color) ~(bold : bool) : color =
    if pc then (if c = Default && not bold then default else cga c ~bright:bold) else palette c ~default
  in
  let h = cell_height computer vt in
  let w = 0.6 *. h in
  let x c = (-.w *. float_of_int cols /. 2.) +. ((float_of_int c +. 0.5) *. w) in
  let y r = (h *. float_of_int rows /. 2.) -. ((float_of_int r +. 0.5) *. h) in
  let size = h /. words_font_size *. 0.8 in
  let cells =
    List.init rows (fun r ->
        List.init cols (fun c ->
            let cell = Vt.cell vt r c in
            let fg = palette cell.attrs.fg ~default:fg0 ~bold:cell.attrs.bold and bg = palette cell.attrs.bg ~default:bg0 ~bold:false in
            let fg, bg = if cell.attrs.reverse then (bg, fg) else (fg, bg) in
            (* a pixel wider and higher, so that neighbours leave no seam *)
            let back = if cell.attrs.reverse || cell.attrs.bg <> Vt.Default then [ rectangle bg (w +. 1.) (h +. 1.) |> move (x c) (y r) ] else [] in
            let glyph = if capitals then String.uppercase_ascii cell.glyph else cell.glyph in
            let letter dx = words fg glyph |> scale size |> move (x c +. dx) (y r) in
            (* bold as a teletype did it: struck twice, a hair apart *)
            let front =
              match box_arms glyph with
              | Some arms -> box_lines fg w h (x c) (y r) arms
              | None -> if glyph = " " then [] else if cell.attrs.bold && not pc then [ letter 0.; letter (w *. 0.08) ] else [ letter 0. ]
            in
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
