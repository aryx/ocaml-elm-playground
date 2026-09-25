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
(* Programs *)
(*****************************************************************************)

type status = Exited | Interrupted

type 'a talk =
  | Done of 'a
  | Print of string * 'a talk
  | Read_line of (string -> 'a talk)
  | Read_key of (string -> 'a talk)
  | Random of int * (int -> 'a talk)
  | Spawn of unit talk * (status -> 'a talk)

let return x = Done x
let print s = Print (s, Done ())
let read_line = Read_line (fun l -> Done l)
let read_key = Read_key (fun k -> Done k)
let random n = Random (n, fun i -> Done i)
let spawn child = Spawn (child, fun st -> Done st)

(* the rest of the program glued after the end of [m]: the requests
   stay the same, and where [m] was Done, [f] takes over *)
let rec ( let* ) (m : 'a talk) (f : 'a -> 'b talk) : 'b talk =
  match m with
  | Done x -> f x
  | Print (s, k) -> Print (s, ( let* ) k f)
  | Read_line k -> Read_line (fun l -> ( let* ) (k l) f)
  | Read_key k -> Read_key (fun key -> ( let* ) (k key) f)
  | Random (n, k) -> Random (n, fun i -> ( let* ) (k i) f)
  | Spawn (child, k) -> Spawn (child, fun st -> ( let* ) (k st) f)

let ask (question : string) : string talk =
  let* () = print question in
  read_line

(*****************************************************************************)
(* Running without a screen *)
(*****************************************************************************)

let run ?(seed = 1) (program : 'a talk) (answers : string list) : string =
  let out = Buffer.create 256 in
  (* the answers and the seed left, if the program reached its end: a
     spawned child's end is where its parent carries on *)
  let rec go : 'b. 'b talk -> string list -> seed -> (string list * seed) option =
   fun p answers seed ->
    match p, answers with
    | Done _, _ -> Some (answers, seed)
    | Print (s, k), _ -> Buffer.add_string out s; go k answers seed
    | Random (n, k), _ ->
        let i, seed = random_int 0 (n - 1) seed in
        go (k i) answers seed
    (* the answer echoed, as the paper would show it *)
    | Read_line k, a :: rest -> Buffer.add_string out (a ^ "\n"); go (k a) rest seed
    | Read_key k, a :: rest -> go (k a) rest seed
    | (Read_line _ | Read_key _), [] -> None
    | Spawn (child, k), _ -> (
        match go child answers seed with
        | Some (answers, seed) -> go (k Exited) answers seed
        | None -> None)
  in
  ignore (go program answers (initial_seed seed));
  Buffer.contents out

(*****************************************************************************)
(* The machine *)
(*****************************************************************************)

type machine = {
  program : unit talk;
  (* the parents waiting for their spawned child, the innermost first *)
  parents : (status -> unit talk) list;
  vt : Vt.t;
  tty : Line_discipline.t;
  (* bytes printed but not yet on the screen, at a baud rate *)
  outbox : string;
  (* characters a second, None: at once *)
  cps : float option;
  budget : float;
  seed : seed;
}

(* everything in the outbox on the screen, unless a baud rate
   rations it (then [tick] does it) *)
let flush (m : machine) : machine =
  match m.cps with
  | None -> { m with vt = Vt.feed m.vt m.outbox; outbox = "" }
  | Some _ -> m

(* the program run until it reads or ends: its prints into the outbox,
   its random numbers drawn, the tty set to the mode its read needs *)
let rec advance (m : machine) : machine =
  match m.program with
  | Print (s, k) -> advance { m with program = k; outbox = m.outbox ^ Line_discipline.output s }
  | Random (n, k) ->
      let i, seed = random_int 0 (n - 1) m.seed in
      advance { m with program = k i; seed }
  | Spawn (child, k) -> advance { m with program = child; parents = k :: m.parents }
  | Done () when m.parents <> [] -> exit_child m Exited
  | Read_key _ -> flush { m with tty = Line_discipline.set_mode m.tty Raw }
  | Read_line _ | Done () -> flush { m with tty = Line_discipline.set_mode m.tty Cooked }

(* the innermost program over: its parent carries on, told how *)
and exit_child (m : machine) (st : status) : machine =
  match m.parents with
  | k :: parents -> advance { m with program = k st; parents }
  | [] -> advance { m with program = Done () }

let start ?baud ~(seed : int) ~(rows : int) ~(cols : int) (program : unit talk) : machine =
  advance
    {
      program;
      parents = [];
      vt = Vt.create ~rows ~cols;
      tty = Line_discipline.create ();
      outbox = "";
      (* a character is 10 bits on the line (a start bit, 8, a stop
         bit); the Model 33 had two stop bits, so its 110 baud were
         exactly 10 characters a second *)
      cps = Option.map (fun b -> float_of_int b /. if b = 110 then 11. else 10.) baud;
      budget = 0.;
      seed = initial_seed seed;
    }

(* a key at a time, since a line or a key given to the program can
   change the tty's mode for the next one *)
let input (m : machine) (bytes : string) : machine =
  let one (m : machine) (key : string) : machine =
    let tty, echo, events = Line_discipline.input m.tty key in
    let m = { m with tty; outbox = m.outbox ^ echo } in
    List.fold_left
      (fun (m : machine) (ev : Line_discipline.event) ->
        match ev, m.program with
        | Line l, Read_line k -> advance { m with program = k l }
        | Key s, Read_key k -> advance { m with program = k s }
        | (Interrupt | End_of_file), (Read_line _ | Read_key _) -> exit_child m Interrupted
        (* typed ahead of a question, or after the end: dropped *)
        | _ -> m)
      m events
  in
  flush (List.fold_left one m (Line_discipline.split_keys bytes))

let tick (m : machine) (dt : float) : machine =
  match m.cps with
  | None -> m
  | Some _ when m.outbox = "" -> { m with budget = 0. }
  | Some cps ->
      let budget = m.budget +. (dt *. cps) in
      let n = min (int_of_float budget) (String.length m.outbox) in
      let len = String.length m.outbox in
      { m with vt = Vt.feed m.vt (String.sub m.outbox 0 n); outbox = String.sub m.outbox n (len - n); budget = budget -. float_of_int n }

let screen (m : machine) = m.vt
let reading (m : machine) = m.outbox = "" && match m.program with Read_line _ | Read_key _ -> true | _ -> false
let finished (m : machine) = m.outbox = "" && match m.program with Done () -> true | _ -> false

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

let size (computer : computer) (m : machine) : number * number =
  let h = cell_height computer m.vt in
  (0.6 *. h *. float_of_int (Vt.cols m.vt), h *. float_of_int (Vt.rows m.vt))

let draw ?(paper = false) ?(phosphor = phosphor) (computer : computer) (m : machine) : shape list =
  let vt = m.vt in
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
            let glyph = if paper then String.uppercase_ascii cell.glyph else cell.glyph in
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
    if reading m && Vt.cursor_visible vt && Float.rem now 1. < 0.5 then [ rectangle fg0 w h |> fade 0.6 |> move (x c) (y r) ] else []
  in
  (rectangle bg0 (w *. float_of_int cols) (h *. float_of_int rows) :: cells) @ cursor

(*****************************************************************************)
(* The application *)
(*****************************************************************************)

type state = {
  machine : machine option; (* None until the first frame, which has the flags *)
  seed : int; (* the next run's *)
  keys : unit Scene2d.t;
}

let teletype ?(rows = 24) ?(cols = 80) ?view:user_view (program : unit talk) : (state game, msg) app =
  let flag name = List.assoc_opt name in
  let make (computer : computer) (seed : int) : machine =
    let baud = Option.bind (flag "baud" computer.flags) int_of_string_opt in
    start ?baud ~seed ~rows ~cols program
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
    | Some m when finished m && Scene2d.pressed (fun k -> k.kenter) keys ->
        { machine = Some (make computer s.seed); seed = s.seed + 1; keys }
    | Some m -> { s with machine = Some (tick (input m (keyboard_bytes computer ~before)) dt); keys }
  in
  let view (computer : computer) (s : state) : shape list =
    match s.machine with
    | None -> []
    | Some m ->
        let paper = flag "paper" computer.flags <> None in
        (match user_view with
        | Some v -> v computer m
        | None -> rectangle (if paper then roll else glass) computer.screen.width computer.screen.height :: draw ~paper computer m)
        @ if finished m then [ words (if paper then ink else phosphor) "(the end -- Enter to run it again)" |> move_y (computer.screen.bottom +. 20.) ] else []
  in
  game view update { machine = None; seed = 1; keys = Scene2d.start () }
