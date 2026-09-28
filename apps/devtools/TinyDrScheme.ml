(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of DrScheme (PLT: Matthias Felleisen, Robert Bruce
 * Findler, Matthew Flatt, Shriram Krishnamurthi and others, Rice
 * University, 1995; renamed DrRacket in 2010), in the look of version
 * 209 (2004): the program in the Definitions window above, a prompt in
 * the Interactions window below, and Execute between them. Control-T
 * (or the Execute button) runs the definitions; Enter at the prompt
 * evaluates what was typed; Break stops a program that runs away;
 * Step opens the stepper. Click "Language:" to switch between
 * Beginning Student and Standard (R5RS).
 *
 * DrScheme was made for teaching, with How to Design Programs (2001),
 * and everything in it follows from that: two windows, so that a
 * student defines, then experiments, as a mathematician does; teaching
 * languages that print a value as the expression making it ((list 1
 * 2), not (1 2)) and forbid what a beginner only gets wrong with;
 * errors that say what was expected, the culprit painted pink in the
 * program; images as values, printed as pictures; and the stepper,
 * evaluation shown as algebra. (Names and dates from memory, to
 * check.)
 *
 * What's new here:
 *
 *  - Two windows over one machine ([job], [work]): Execute makes a new
 *    Scheme machine (languages/scheme's Scheme_eval, a CESK machine),
 *    runs the definitions a form at a time, and leaves it for the
 *    prompt, whose expressions see the definitions -- the REPL, the
 *    oldest interface of Lisp, next to an editor.
 *
 *  - The program runs a budget of steps a frame ([fuel]): the machine
 *    stops where it is and goes on at the next frame, so an endless
 *    loop keeps the window alive, the running indicator turning, and
 *    Break is simply not running it any more. DrScheme needed a thread
 *    and a custodian for that; a machine whose stack is data needs a
 *    counter.
 *
 *  - Errors with their place ([pink]): the language keeps each
 *    expression's span in the text (languages/sexpr), so the one that
 *    failed is painted, DrScheme's pink.
 *
 *  - Images as values ([picture]): (circle 20 "solid" "red") at the
 *    prompt prints a red disc. The language only describes images
 *    (Scheme_image); drawing them is the Playground's Bigbang way,
 *    whose combinators are 2htdp/image's.
 *
 *  - big-bang in the IDE ([world]): the machine stops at (big-bang
 *    ...) and hands the world to the host, which opens a window, calls
 *    the handlers -- each a nested run of the machine -- on ticks, keys
 *    and the mouse, and gives the last world back as big-bang's value.
 *
 *  - The stepper ([stepper]): Beginning Student's evaluation as a
 *    series of rewritings (Scheme_step), the redex in green, what
 *    replaced it in purple.
 *
 * What it uses: languages/scheme and languages/sexpr (the language:
 * the reader, the machine, the stepper), gui's Text_edit (the two
 * texts: a piece table, undo for free), the Bigbang way (the images
 * drawn, HtDP's key and mouse events). Not Physics, not Camera2d.
 *
 * Exercises: Check Syntax (the arrows from each use to its binding,
 * the spans are there), check-expect and its report, Intermediate
 * Student (lambda, local), the teachpacks' own functions
 * (on-mouse's pointer shape, key-release), a file saved and opened,
 * the images inside a printed list drawn too, a garbage collector for
 * the machine's store.
 *)
open Playground

(*****************************************************************************)
(* The window's geometry *)
(*****************************************************************************)

(* from the window's top-left corner, y going down, to the Playground's
 * centre *)
let at (x : float) (y : float) (s : shape) : shape = s |> move (x -. 500.) (500. -. y)
let box (c : color) (x : float) (y : float) (w : float) (h : float) : shape = rectangle c w h |> at (x +. (w /. 2.)) (y +. (h /. 2.))

let frame (c : color) (x : float) (y : float) (w : float) (h : float) : shape list =
  [ box c x y w 1.; box c x (y +. h -. 1.) w 1.; box c x y 1. h; box c (x +. w -. 1.) y 1. h ]

(* a character's cell: the text drawn in columns, as a code editor's *)
let cw = 11.
let lh = 20.

let glyph (c : color) (x : float) (y : float) (ch : char) : shape list =
  if ch = ' ' || ch = '\n' then [] else [ words c (String.make 1 ch) |> scale 1.55 |> at (x +. (cw /. 2.)) (y +. (lh /. 2.)) ]

let label (c : color) (x : float) (y : float) (s : string) : shape list = List.concat (List.mapi (fun i ch -> glyph c (x +. (float_of_int i *. cw)) y ch) (List.of_seq (String.to_seq s)))

(* the text in a proportional font, for the window's own words *)
let caption (c : color) (x : float) (y : float) (size : float) (s : string) : shape = words c s |> scale (size /. 10.) |> at x y

let title_h = 34.
let toolbar_y = 34.
let defs_y = 96.
let defs_h = 468.
let inter_y = 572.
let inter_h = 386.
let status_y = 960.
let text_x = 10.
let cols = int_of_float ((1000. -. (2. *. text_x)) /. cw)

(* Windows XP's, which DrScheme 209 wore on it *)
let chrome = rgb 236 233 216
let title_blue = rgb 0 84 227
let dark = rgb 90 90 90

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type level = Beginning | Standard
type pane = Definitions | Interactions

(* what the Interactions window shows *)
type entry =
  | Banner of string
  | Echo of string (* what was typed at the prompt *)
  | Result of Scheme.t
  | Printed of string (* display's *)
  | Failure of string
  | Warning of string

(* forms to evaluate, a machine running one of them *)
type job = { forms : Sexpr.t list; from_defs : bool; running : bool }

type world = {
  spec : Scheme_eval.world;
  value : Scheme.t;
  picture : Scheme_image.t option;
  ticks : int;
  after : job; (* what to go on with, the last world returned *)
}

type status = Idle | Busy of job | World of world

type stepper = { steps : Scheme_step.step array; error : string option; shown : int }

type model = {
  defs : Text_edit.t;
  top : int; (* the Definitions' first line shown *)
  input : Text_edit.t; (* the prompt's *)
  focus : pane;
  entries : entry list; (* newest first *)
  machine : Scheme_eval.state option; (* made by the first Execute, or the prompt *)
  status : status;
  level : level;
  pink : (int * int) option; (* the failed expression's span in the definitions *)
  executed : string option; (* the definitions last executed, to warn when they changed *)
  stepper : stepper option;
  keyboard : keyboard; (* at the last frame *)
  mouse : mouse;
  held : (string * int) list; (* keys held, for how many frames: repeating *)
  dragging : bool;
  frames : int;
}

let program =
  {|;; Beginning Student: define, then try at the prompt below
(define (fact n)
  (cond [(= n 0) 1]
        [else (* n (fact (- n 1)))]))

(fact 5)

(beside (circle 20 "solid" "red")
        (circle 20 "solid" "orange")
        (circle 20 "solid" "gold"))

;; How to Design Programs' first world: a rocket landing
(define ROCKET
  (above (triangle 24 "solid" "red")
         (rectangle 14 34 "solid" "gray")))
(define (draw y) (place-image ROCKET 100 y (empty-scene 200 240)))
(define (landed? y) (>= y 200))
(big-bang 0 [on-tick add1] [to-draw draw] [stop-when landed?])
|}

let banner (level : level) : entry list =
  [ Banner (Printf.sprintf "Language: %s." (match level with Beginning -> "Beginning Student" | Standard -> "Standard (R5RS)")); Banner "Welcome to DrScheme, version 209." ]

let initial_model : model =
  { defs = Text_edit.of_string program; top = 0; input = Text_edit.of_string ""; focus = Definitions; entries = banner Beginning; machine = None;
    status = Idle; level = Beginning; pink = None; executed = None; stepper = None; keyboard = initial_computer.keyboard;
    mouse = initial_computer.mouse; held = []; dragging = false; frames = 0 }

let style (m : model) : Scheme.style = match m.level with Beginning -> Constructor | Standard -> Write
let running (m : model) : bool = match m.status with Idle -> false | Busy _ | World _ -> true

(*****************************************************************************)
(* Running: a budget of steps a frame *)
(*****************************************************************************)

let fuel = 100_000

let add (e : entry) (m : model) : model =
  match (e, m.entries) with
  (* display's pieces, one entry *)
  | Printed s, Printed t :: rest -> { m with entries = Printed (t ^ s) :: rest }
  | _ -> { m with entries = e :: m.entries }

let flush (m : model) (st : Scheme_eval.state) : model =
  let out, st = Scheme_eval.take_output st in
  let m = { m with machine = Some st } in
  if out = "" then m else add (Printed out) m

let fail (m : model) (from_defs : bool) (err : Scheme_eval.error) : model =
  let m = add (Failure err.message) { m with status = Idle } in
  if from_defs then { m with pink = Option.map (fun (sp : Sexpr.span) -> (sp.start, sp.stop)) err.at } else m

let machine (m : model) : Scheme_eval.state = match m.machine with Some st -> st | None -> Scheme_eval.create ()

(* [work m budget]: the job on, until its steps are spent, it waits
   for a world, or it is done *)
let rec work (m : model) (budget : int) : model =
  match m.status with
  | Idle | World _ -> m
  | Busy job when not job.running -> (
      match job.forms with
      | [] -> { m with status = Idle }
      | f :: rest -> (
          match Scheme_syntax.top f with
          | exception Scheme_syntax.Error (message, span) -> fail m job.from_defs { message; at = Some span }
          | e -> work { m with machine = Some (Scheme_eval.start (machine m) e); status = Busy { job with forms = rest; running = true } } budget))
  | Busy job -> (
      let st = machine m in
      let before = Scheme_eval.steps st in
      match Scheme_eval.run ~fuel:budget st with
      | Running, st -> { m with machine = Some st }
      | Done v, st ->
          let m = flush m st in
          let m = if v = Void then m else add (Result v) m in
          let left = budget - (Scheme_eval.steps st - before) in
          let m = { m with status = Busy { job with running = false } } in
          if left > 0 then work m left else m
      | Failed err, st -> fail (flush m st) job.from_defs err
      | World spec, st -> { (flush m st) with status = World { spec; value = spec.init; picture = None; ticks = 0; after = job } })

(* Execute: a new machine, the Interactions cleared, the definitions
   read and queued *)
let execute (m : model) : model =
  let text = Text_edit.to_string m.defs in
  let m = { m with entries = banner m.level; pink = None; executed = Some text; stepper = None; machine = Some (Scheme_eval.create ()); focus = Interactions } in
  match Sexpr_read.read_all Scheme text with
  | forms -> { m with status = Busy { forms; from_defs = true; running = false } }
  | exception Sexpr_read.Error (msg, pos) -> { (add (Failure ("read: " ^ msg)) m) with pink = Some (pos, min (String.length text) (pos + 1)) }

let break (m : model) : model = if running m then add (Failure "user break") { m with status = Idle } else m

(*****************************************************************************)
(* A world, run by the host *)
(*****************************************************************************)

let handler (w : world) (name : string) : Scheme.t option = List.assoc_opt name w.spec.handlers

(* [call m w name args]: the handler on the world, if there is one;
   Error ends the world *)
let call (m : model) (w : world) (name : string) (args : Scheme.t list) : (model * world, model) result =
  match handler w name with
  | None -> Ok (m, w)
  | Some f -> (
      match Scheme_eval.call (machine m) f args with
      | Ok v, st -> Ok ({ m with machine = Some st }, { w with value = v })
      | Error err, st -> Error (fail { m with machine = Some st } false err))

(* the world ended: big-bang returns the last one, the job goes on *)
let finish (m : model) (w : world) : model =
  { m with machine = Some (Scheme_eval.resume (machine m) w.value); status = Busy w.after }

let stopped (m : model) (w : world) : (bool * model, model) result =
  match handler w "stop-when" with
  | None -> Ok (false, m)
  | Some f -> (
      match Scheme_eval.call (machine m) f [ w.value ] with
      | Ok v, st -> Ok (Scheme.truthy v, { m with machine = Some st })
      | Error err, st -> Error (fail { m with machine = Some st } false err))

(* where the scene is: the window's centre, and the scene scaled to
   fit 800 by 760 *)
let world_scale (i : Scheme_image.t) : float = Float.min 1. (Float.min (800. /. Scheme_image.width i) (760. /. Scheme_image.height i))

let update_world (computer : computer) (m : model) (w : world) : model =
  let ( let* ) r f = match r with Ok x -> f x | Error m -> m in
  let w = { w with ticks = w.ticks + 1 } in
  (* a tick every other frame, 30 a second: HtDP's is 28 *)
  let* m, w = if w.ticks mod 2 = 0 then call m w "on-tick" [ w.value ] else Ok (m, w) in
  let pressed, _ = Bigbang.key_events m.keyboard computer.keyboard in
  let* m, w = List.fold_left (fun r k -> match r with Ok (m, w) -> call m w "on-key" [ w.value; Str k ] | e -> e) (Ok (m, w)) pressed in
  let* m, w =
    match (Bigbang.mouse_event m.mouse computer.mouse, w.picture) with
    | Some ev, Some i ->
        let s = world_scale i in
        let x = ((computer.mouse.mx /. s) +. (Scheme_image.width i /. 2.)) and y = ((Scheme_image.height i /. 2.) -. (computer.mouse.my /. s)) in
        call m w "on-mouse" [ w.value; Int (int_of_float x); Int (int_of_float y); Str ev ]
    | _ -> Ok (m, w)
  in
  let* m, w =
    match handler w "to-draw" with
    | None -> Ok (m, w)
    | Some f -> (
        match Scheme_eval.call (machine m) f [ w.value ] with
        | Ok (Image i), st -> Ok ({ m with machine = Some st }, { w with picture = Some i })
        | Ok v, st -> Error (add (Failure ("to-draw: expected an image, given " ^ Scheme.print (style m) v)) { m with machine = Some st; status = Idle })
        | Error err, st -> Error (fail { m with machine = Some st } false err))
  in
  let* stop, m = stopped m w in
  if stop || List.mem "escape" pressed then finish m w else { m with status = World w }

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

(* the lines of a text, each with the offset it starts at *)
let lines (s : string) : (int * string) list =
  let rec go start acc = match String.index_from_opt s start '\n' with Some j -> go (j + 1) ((start, String.sub s start (j - start)) :: acc) | None -> List.rev ((start, String.sub s start (String.length s - start)) :: acc) in
  go 0 []

(* the line and column of an offset *)
let place (s : string) (pos : int) : int * int =
  let ls = lines s in
  let rec go i = function (start, l) :: rest -> if pos <= start + String.length l || rest = [] then (i, pos - start) else go (i + 1) rest | [] -> (0, 0) in
  go 0 ls

let offset (s : string) (line : int) (col : int) : int =
  match List.nth_opt (lines s) line with Some (start, l) -> start + max 0 (min col (String.length l)) | None -> String.length s

let repeating = [ "Backspace"; "Delete"; "ArrowLeft"; "ArrowRight"; "ArrowUp"; "ArrowDown" ]

(* the keys pressed this frame, and those held long enough to repeat *)
let presses (m : model) (computer : computer) : string list * (string * int) list =
  let down = Set_.elements computer.keyboard.keys in
  let held = List.map (fun k -> (k, 1 + Option.value ~default:0 (List.assoc_opt k m.held))) down in
  (List.filter_map (fun (k, n) -> if n = 1 || (List.mem k repeating && n > 24 && n mod 3 = 0) then Some k else None) held, held)

let edit (t : Text_edit.t) (computer : computer) (keys : string list) ~(multiline : bool) : Text_edit.t =
  let typed = String.of_seq (Seq.filter (fun c -> Char.code c >= 32 && Char.code c < 127) (String.to_seq computer.keyboard.typed)) in
  let t = if typed <> "" then Text_edit.insert typed t else t in
  List.fold_left
    (fun t k ->
      let s = Text_edit.to_string t in
      let line, col = place s (Text_edit.caret t) in
      match k with
      | "Enter" when multiline ->
          (* the new line indented as the one it breaks *)
          let _, l = List.nth (lines s) line in
          let rec spaces i = if i < String.length l && l.[i] = ' ' then spaces (i + 1) else i in
          Text_edit.insert ("\n" ^ String.make (spaces 0) ' ') t
      | "Tab" -> Text_edit.insert "  " t
      | "Backspace" -> Text_edit.delete_backward t
      | "Delete" -> Text_edit.delete_forward t
      | "ArrowLeft" -> Text_edit.at (max 0 (Text_edit.caret t - 1)) t
      | "ArrowRight" -> Text_edit.at (min (String.length s) (Text_edit.caret t + 1)) t
      | "ArrowUp" when line > 0 -> Text_edit.at (offset s (line - 1) col) t
      | "ArrowDown" -> Text_edit.at (offset s (line + 1) col) t
      | "Home" -> Text_edit.at (offset s line 0) t
      | "End" -> Text_edit.at (offset s line max_int) t
      | _ -> t)
    t keys

(* whether the prompt holds whole expressions, or wants more lines *)
let complete (s : string) : bool =
  match Sexpr_read.read_all Scheme s with _ -> true | exception Sexpr_read.Error (msg, _) -> not (String.starts_with ~prefix:"end of input" msg)

let rows = int_of_float ((defs_h -. 8.) /. lh)

(* the definitions scrolled so that the caret shows *)
let reveal (m : model) : model =
  let line, _ = place (Text_edit.to_string m.defs) (Text_edit.caret m.defs) in
  if line < m.top then { m with top = line } else if line >= m.top + rows then { m with top = line - rows + 1 } else m

(*****************************************************************************)
(* The stepper *)
(*****************************************************************************)

let open_stepper (m : model) : model =
  match m.level with
  | Standard -> add (Failure "Step: the stepper knows Beginning Student only (click Language)") m
  | Beginning ->
      let steps, error = Scheme_step.steps (Text_edit.to_string m.defs) in
      { m with stepper = Some { steps = Array.of_list steps; error; shown = 0 } }

(* the stepper's window, and its buttons *)
let sx = 40.
let sy = 110.
let sw = 920.
let sh = 780.
let prev_button = (sx +. 20., sy +. sh -. 60., 150., 40.)
let next_button = (sx +. sw -. 170., sy +. sh -. 60., 150., 40.)
let inside (mx, my) (x, y, w, h) = mx >= x && mx < x +. w && my >= y && my < y +. h

(* the mouse from the Playground's centre to the window's corner *)
let mouse_at (computer : computer) : float * float = (computer.mouse.mx +. 500., 500. -. computer.mouse.my)

let update_stepper (computer : computer) (keys : string list) (m : model) (s : stepper) : model =
  let last = Array.length s.steps in
  let click b = computer.mouse.mclick && inside (mouse_at computer) b in
  let s = if List.mem "ArrowRight" keys || click next_button then { s with shown = min last (s.shown + 1) } else s in
  let s = if List.mem "ArrowLeft" keys || click prev_button then { s with shown = max 0 (s.shown - 1) } else s in
  if List.mem "Escape" keys then { m with stepper = None } else { m with stepper = Some s }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the toolbar's buttons *)
let step_button = (640., 42., 110., 46.)
let execute_button = (760., 42., 120., 46.)
let break_button = (890., 42., 100., 46.)
let language_button = (10., 964., 360., 32.)

let update (computer : computer) (m : model) : model =
  let keys, held = presses m computer in
  let control = Set_.mem "Control" computer.keyboard.keys in
  let mouse = mouse_at computer in
  let clicked b = computer.mouse.mclick && inside mouse b in
  let m' = { m with frames = m.frames + 1; held } in
  let m =
    match (m.stepper, m.status) with
    | Some s, _ -> update_stepper computer keys m' s
    | None, World w -> if control && List.mem "b" keys then break m' else update_world computer m' w
    | None, _ ->
        let m = m' in
        (* the buttons, and their keys: Control-T executes, Control-B
           breaks, as in DrScheme *)
        let m = if clicked execute_button || (control && List.mem "t" keys) then execute m else m in
        let m = if clicked break_button || (control && List.mem "b" keys) then break m else m in
        let m = if clicked step_button && not (running m) then open_stepper m else m in
        let m =
          if clicked language_button && not (running m) then
            { m with level = (match m.level with Beginning -> Standard | Standard -> Beginning); entries = Warning "Language changed: click Execute." :: m.entries }
          else m
        in
        (* the mouse: a click in a window gives it the keys, and in the
           definitions places the caret; a drag selects *)
        let mx, my = mouse in
        let in_defs = my >= defs_y && my < defs_y +. defs_h in
        let defs_offset () =
          let s = Text_edit.to_string m.defs in
          offset s (m.top + int_of_float ((my -. defs_y -. 4.) /. lh)) (int_of_float (Float.round ((mx -. text_x) /. cw)))
        in
        let m =
          if computer.mouse.mclick && in_defs then { m with focus = Definitions; defs = Text_edit.at (defs_offset ()) m.defs; dragging = true }
          else if computer.mouse.mclick && my >= inter_y && my < inter_y +. inter_h then { m with focus = Interactions }
          else if m.dragging && computer.mouse.mdown then { m with defs = Text_edit.to_ (defs_offset ()) m.defs }
          else { m with dragging = computer.mouse.mdown && m.dragging }
        in
        (* the keys, into the window that has them *)
        let m =
          if control then
            match (List.mem "z" keys, m.focus) with
            | true, Definitions -> { m with defs = Text_edit.undo m.defs; pink = None }
            | _ -> m
          else
            match m.focus with
            | Definitions ->
                let defs = edit m.defs computer keys ~multiline:true in
                if Text_edit.to_string defs = Text_edit.to_string m.defs then { m with defs }
                else
                  let m = { m with defs; pink = None } in
                  (* DrScheme's reminder that what runs is not what shows *)
                  if m.executed <> None && not (List.exists (function Warning _ -> true | _ -> false) m.entries) then
                    add (Warning "Warning: The definitions window has changed. Click Execute.") m
                  else m
            | Interactions ->
                if List.mem "Enter" keys && not (running m) && complete (Text_edit.to_string m.input) then
                  let text = Text_edit.to_string m.input in
                  let m = { (add (Echo text) m) with input = Text_edit.of_string "" } in
                  match Sexpr_read.read_all Scheme text with
                  | forms -> { m with machine = Some (machine m); status = Busy { forms; from_defs = false; running = false } }
                  | exception Sexpr_read.Error (msg, _) -> add (Failure ("read: " ^ msg)) m
                else if running m then m
                else { m with input = edit m.input computer keys ~multiline:true }
        in
        reveal m
  in
  let m = work m fuel in
  { m with keyboard = computer.keyboard; mouse = computer.mouse }

(*****************************************************************************)
(* View: the texts *)
(*****************************************************************************)

(* DrScheme's colours for its program text *)
let paren_color = rgb 132 60 36
let symbol_color = rgb 38 38 128
let constant_color = rgb 41 128 38
let comment_color = rgb 194 116 31

(* each character's colour: a small lexer over the whole text *)
let colors (s : string) : color array =
  let n = String.length s in
  let a = Array.make n symbol_color in
  let fill i j c = for k = i to min (n - 1) (j - 1) do a.(k) <- c done in
  let delim c = String.contains " \n\t()[]\";'" c in
  let rec go i =
    if i < n then
      match s.[i] with
      | ';' ->
          let j = match String.index_from_opt s i '\n' with Some j -> j | None -> n in
          fill i j comment_color; go j
      | '"' ->
          let rec close j = if j >= n then n else if s.[j] = '\\' then close (j + 2) else if s.[j] = '"' then j + 1 else close (j + 1) in
          let j = close (i + 1) in
          fill i j constant_color; go j
      | '(' | ')' | '[' | ']' -> a.(i) <- paren_color; go (i + 1)
      | '\'' -> a.(i) <- constant_color; go (i + 1)
      | c when delim c -> go (i + 1)
      | _ ->
          let j = ref i in
          while !j < n && not (delim s.[!j]) do incr j done;
          let tok = String.sub s i (!j - i) in
          if s.[i] = '#' || float_of_string_opt tok <> None || tok = "true" || tok = "false" || tok = "empty" then fill i !j constant_color;
          go !j
  in
  go 0;
  a

(* the parenthesis matching the one beside the caret: DrScheme greys
   the expression between them *)
let matching (s : string) (caret : int) : (int * int) option =
  let n = String.length s in
  let rec back i depth = if i < 0 then None else match s.[i] with ')' | ']' -> back (i - 1) (depth + 1) | '(' | '[' -> if depth = 1 then Some i else back (i - 1) (depth - 1) | _ -> back (i - 1) depth in
  let rec fwd i depth = if i >= n then None else match s.[i] with '(' | '[' -> fwd (i + 1) (depth + 1) | ')' | ']' -> if depth = 1 then Some (i + 1) else fwd (i + 1) (depth - 1) | _ -> fwd (i + 1) depth in
  if caret > 0 && (s.[caret - 1] = ')' || s.[caret - 1] = ']') then Option.map (fun i -> (i, caret)) (back (caret - 1) 0)
  else if caret < n && (s.[caret] = '(' || s.[caret] = '[') then Option.map (fun j -> (caret, j)) (fwd caret 0)
  else None

(* [text_view ...]: lines of text from line [top], with backgrounds over
   character ranges *)
let text_view ~(x : float) ~(y : float) ~(rows : int) ~(top : int) (s : string) (color_of : int -> color) (shades : ((int * int) * color) list) (caret : int option) : shape list =
  let shapes = ref [] in
  let push l = shapes := l @ !shapes in
  List.iteri
    (fun i (start, l) ->
      let row = i - top in
      if row >= 0 && row < rows then begin
        let ly = y +. (float_of_int row *. lh) in
        let len = min cols (String.length l) in
        (* the shades first, under the characters *)
        List.iter
          (fun ((a, b), c) ->
            let a = max a start and b = min b (start + len + 1) in
            if a < b then push [ box c (x +. (float_of_int (a - start) *. cw)) ly (float_of_int (b - a) *. cw) lh ])
          shades;
        for k = 0 to len - 1 do
          push (glyph (color_of (start + k)) (x +. (float_of_int k *. cw)) ly l.[k])
        done;
        match caret with
        | Some c when c >= start && c <= start + String.length l -> push [ box black (x +. (float_of_int (c - start) *. cw) -. 1.) (ly +. 1.) 2. (lh -. 2.) ]
        | _ -> ()
      end)
    (lines s);
  List.rev !shapes

(*****************************************************************************)
(* View: images *)
(*****************************************************************************)

let color_of_name (name : string) : color =
  match name with
  | "red" -> rgb 255 0 0
  | "green" -> rgb 0 160 0
  | "blue" -> rgb 0 0 255
  | "yellow" -> rgb 255 255 0
  | "gold" -> rgb 255 215 0
  | "orange" -> rgb 255 165 0
  | "purple" -> rgb 160 32 240
  | "black" -> black
  | "white" -> white
  | "gray" | "grey" -> rgb 190 190 190
  | "darkgray" | "darkgrey" -> rgb 110 110 110
  | "brown" -> rgb 165 42 42
  | "pink" -> rgb 255 192 203
  | "cyan" -> rgb 0 255 255
  | "magenta" -> rgb 255 0 255
  | "lightblue" -> rgb 173 216 230
  | "darkgreen" -> rgb 0 100 0
  | "navy" -> rgb 0 0 128
  | "tan" -> rgb 210 180 140
  | _ -> rgb 128 128 128

(* the language's description drawn by the Bigbang way's combinators *)
let rec picture (i : Scheme_image.t) : Bigbang.image =
  let mode (m : Scheme_image.mode) : Bigbang.mode = match m with Solid -> Solid | Outline -> Outline in
  match i with
  | Circle (r, md, c) -> Bigbang.circle r (mode md) (color_of_name c)
  | Ellipse (w, h, md, c) -> Bigbang.ellipse w h (mode md) (color_of_name c)
  | Rectangle (w, h, md, c) -> Bigbang.rectangle w h (mode md) (color_of_name c)
  | Triangle (s, md, c) -> Bigbang.triangle s (mode md) (color_of_name c)
  | Text (s, size, c) -> Bigbang.text s size (color_of_name c)
  | Scene (w, h) -> Bigbang.empty_scene w h
  | Beside (a, b) -> Bigbang.beside (picture a) (picture b)
  | Above (a, b) -> Bigbang.above (picture a) (picture b)
  | Overlay (a, b) -> Bigbang.overlay (picture a) (picture b)
  | Place (a, x, y, s) -> Bigbang.place_image (picture a) x y (picture s)

(*****************************************************************************)
(* View: the windows *)
(*****************************************************************************)

let result_color = rgb 0 0 175
let output_color = rgb 150 0 150
let error_color = rgb 200 0 0
let pink_color = rgb 255 182 193

(* a toolbar button: raised, its icon and its word *)
let button ((x, y, w, h) : float * float * float * float) (icon : shape list) (word : string) (enabled : bool) : shape list =
  [ box (rgb 250 250 245) x y w h ] @ frame (rgb 160 160 150) x y w h @ List.map (at (x +. 20.) (y +. (h /. 2.))) icon
  @ [ caption (if enabled then black else rgb 160 160 160) (x +. 24. +. ((w -. 24.) /. 2.)) (y +. (h /. 2.)) 16. word ]

let view_toolbar (m : model) : shape list =
  let busy = running m in
  [ box chrome 0. toolbar_y 1000. 62. ]
  @ [ caption black 60. (toolbar_y +. 30.) 17. "Untitled"; caption dark 200. (toolbar_y +. 30.) 15. "(define ...)" ]
  @ frame (rgb 160 160 150) 150. (toolbar_y +. 16.) 100. 28.
  @ button step_button [ circle (rgb 60 60 60) 5. |> move (-4.) 4.; circle (rgb 60 60 60) 5. |> move 4. (-5.) ] "Step" (not busy)
  @ button execute_button [ polygon (rgb 0 150 0) [ (-9., 10.); (-9., -10.); (10., 0.) ] ] "Execute" true
  @ button break_button [ circle (rgb 200 0 0) 10.; rectangle white 10. 3. ] "Break" busy

let view_definitions (m : model) : shape list =
  let s = Text_edit.to_string m.defs in
  let colors = colors s in
  let caret = Text_edit.caret m.defs in
  let a, b = Text_edit.range m.defs in
  let shades =
    (match m.pink with Some r -> [ (r, pink_color) ] | None -> [])
    @ (if a < b then [ ((a, b), rgb 180 200 250) ] else [])
    @ match matching s caret with Some r when m.focus = Definitions -> [ (r, rgb 225 225 225) ] | _ -> []
  in
  [ box white 0. defs_y 1000. defs_h ]
  @ text_view ~x:text_x ~y:(defs_y +. 4.) ~rows ~top:m.top s (fun i -> if i < Array.length colors then colors.(i) else black) shades
      (if m.focus = Definitions then Some caret else None)

(* the Interactions window's content, as blocks of lines or pictures,
   from the oldest *)
type block = Lines of color * string | Picture of Scheme_image.t

let wrap (s : string) : string list =
  List.concat_map (fun l -> if l = "" then [ "" ] else List.init ((String.length l + cols - 1) / cols) (fun i -> String.sub l (i * cols) (min cols (String.length l - (i * cols))))) (String.split_on_char '\n' s)

let blocks (m : model) : block list =
  List.rev_map
    (function
      | Banner s -> Lines (black, s)
      | Echo s -> Lines (black, "> " ^ s)
      | Result (Image i) -> Picture i
      | Result v -> Lines (result_color, Scheme.print (style m) v)
      | Printed s -> Lines (output_color, s)
      | Failure s -> Lines (error_color, s)
      | Warning s -> Lines (rgb 170 120 0, s))
    m.entries

let picture_scale (i : Scheme_image.t) : float = Float.min 1. (240. /. Float.max 1. (Scheme_image.height i))

let view_interactions (m : model) : shape list =
  let height = function Lines (_, s) -> lh *. float_of_int (List.length (wrap s)) | Picture i -> (Scheme_image.height i *. picture_scale i) +. 6. in
  let prompt = "> " ^ Text_edit.to_string m.input in
  let prompt_h = if running m then 0. else lh *. float_of_int (List.length (wrap prompt)) in
  let bs = blocks m in
  let total = List.fold_left (fun acc b -> acc +. height b) prompt_h bs in
  (* the bottom kept in view, as the window scrolls with its output *)
  let y0 = inter_y +. 4. -. Float.max 0. (total -. (inter_h -. 8.)) in
  (* only what fits whole: no clipping in the Playground *)
  let visible y h = y >= inter_y && y +. h <= inter_y +. inter_h in
  let shapes, y =
    List.fold_left
      (fun (acc, y) b ->
        let h = height b in
        let s =
          if not (visible y h) then []
          else
            match b with
            | Lines (c, s) -> List.concat (List.mapi (fun i l -> let ly = y +. (float_of_int i *. lh) in if visible ly lh then label c text_x ly l else []) (wrap s))
            | Picture i ->
                let k = picture_scale i in
                [ Bigbang.to_shape (picture i) |> scale k |> at (text_x +. (Scheme_image.width i *. k /. 2.)) (y +. 3. +. (Scheme_image.height i *. k /. 2.)) ]
        in
        (s @ acc, y +. h))
      ([], y0) bs
  in
  let prompt_shapes =
    if running m then []
    else
      let input = Text_edit.to_string m.input in
      let caret = Text_edit.caret m.input in
      List.concat
        (List.mapi
           (fun i l ->
             let ly = y +. (float_of_int i *. lh) in
             if not (visible ly lh) then [] else label black text_x ly l)
           (wrap prompt))
      @
      if m.focus = Interactions then
        let line, col = place input caret in
        let col = if line = 0 then col + 2 else col in
        [ box black (text_x +. (float_of_int col *. cw) -. 1.) (y +. (float_of_int line *. lh) +. 1.) 2. (lh -. 2.) ]
      else []
  in
  (* the window's white, its content clipped by the chrome drawn over it *)
  [ box white 0. inter_y 1000. inter_h ] @ List.rev shapes @ prompt_shapes

let view_status (m : model) : shape list =
  let s = Text_edit.to_string m.defs in
  let line, col = place s (Text_edit.caret m.defs) in
  let lang = match m.level with Beginning -> "Beginning Student" | Standard -> "Standard (R5RS)" in
  [ box chrome 0. status_y 1000. 40.; caption black 190. (status_y +. 20.) 15. ("Language: " ^ lang ^ "  (click)") ]
  @ [ caption dark 820. (status_y +. 20.) 15. (Printf.sprintf "%d:%d" (line + 1) col) ]
  @
  if running m then
    (* the running indicator, turning *)
    let a = float_of_int (m.frames * 12) in
    [ caption (rgb 0 120 0) 690. (status_y +. 20.) 15. "running"; rectangle (rgb 0 150 0) 16. 4. |> rotate a |> at 960. (status_y +. 20.) ]
  else []

let view_world (w : world) : shape list =
  match w.picture with
  | None -> []
  | Some i ->
      let k = world_scale i in
      let pw = Scheme_image.width i *. k and ph = Scheme_image.height i *. k in
      let x = 500. -. (pw /. 2.) and y = 520. -. (ph /. 2.) in
      [ box black (x -. 6.) (y -. 36.) (pw +. 12.) (ph +. 42.); box title_blue (x -. 4.) (y -. 34.) (pw +. 8.) 30. ]
      @ [ caption white (x +. (pw /. 2.)) (y -. 19.) 15. "big-bang  (Esc closes)" ]
      @ [ box white x y pw ph; Bigbang.to_shape (picture i) |> scale k |> at 500. 520. ]

let view_stepper (s : stepper) : shape list =
  let pane_w = (sw -. 60.) /. 2. in
  let pcols = int_of_float ((pane_w -. 16.) /. cw) in
  (* the text broken into lines of [pcols], a character's offset kept *)
  let pane x (text : string) (mark : int * int) (shade : color) : shape list =
    let a, b = mark in
    let rows = List.init ((String.length text + pcols - 1) / max 1 pcols) (fun r -> (r * pcols, String.sub text (r * pcols) (min pcols (String.length text - (r * pcols))))) in
    [ box white x (sy +. 70.) pane_w (sh -. 150.) ]
    @ frame (rgb 160 160 150) x (sy +. 70.) pane_w (sh -. 150.)
    @ List.concat
        (List.mapi
           (fun r (start, l) ->
             let ly = sy +. 80. +. (float_of_int r *. lh) in
             let lo = max a start and hi = min b (start + String.length l) in
             (if lo < hi then [ box shade (x +. 8. +. (float_of_int (lo - start) *. cw)) ly (float_of_int (hi - lo) *. cw) lh ] else [])
             @ label black (x +. 8.) ly l)
           rows)
  in
  let n = Array.length s.steps in
  let body =
    if s.shown < n then
      let st = s.steps.(s.shown) in
      pane (sx +. 20.) st.before st.redex (rgb 193 255 193) @ pane (sx +. 40. +. pane_w) st.after st.contractum (rgb 225 200 255)
      @ [ caption dark (sx +. 20. +. (pane_w /. 2.)) (sy +. 55.) 15. "before"; caption dark (sx +. 40. +. (pane_w *. 1.5)) (sy +. 55.) 15. "after" ]
    else
      let msg = match s.error with Some e -> e | None -> "All of the definitions have been successfully evaluated." in
      [ box white (sx +. 20.) (sy +. 70.) (sw -. 40.) (sh -. 150.) ] @ label (if s.error = None then black else error_color) (sx +. 30.) (sy +. 90.) msg
  in
  [ box (rgb 80 80 80) (sx -. 3.) (sy -. 3.) (sw +. 6.) (sh +. 6.); box chrome sx sy sw sh; box title_blue sx sy sw 34. ]
  @ [ caption white (sx +. (sw /. 2.)) (sy +. 17.) 16. "Stepper  (Esc closes)" ]
  @ body
  @ button prev_button [ polygon black [ (6., 8.); (6., -8.); (-8., 0.) ] ] "Step" (s.shown > 0)
  @ button next_button [ polygon black [ (-6., 8.); (-6., -8.); (8., 0.) ] ] "Step" (s.shown < n)
  @ [ caption dark (sx +. (sw /. 2.)) (sy +. sh -. 40.) 15. (Printf.sprintf "step %d of %d" (min (s.shown + 1) n) n) ]

let view (_computer : computer) (m : model) : shape list =
  [ box chrome 0. 0. 1000. 1000. ]
  @ view_definitions m
  (* the divider, between the two windows *)
  @ [ box chrome 0. (defs_y +. defs_h) 1000. (inter_y -. defs_y -. defs_h); box (rgb 200 200 190) 470. (defs_y +. defs_h +. 2.) 60. 3. ]
  @ view_interactions m
  (* the chrome over the texts' overflow *)
  @ [ box title_blue 0. 0. 1000. title_h; caption white 500. 17. 17. "Untitled - DrScheme" ]
  @ view_toolbar m
  @ view_status m
  @ (match m.status with World w -> view_world w | Idle | Busy _ -> [])
  @ match m.stepper with Some s -> view_stepper s | None -> []

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
