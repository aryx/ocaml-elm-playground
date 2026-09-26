(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Smalltalk-80 (Alan Kay, Dan Ingalls, Adele
 * Goldberg and the Learning Research Group, Xerox PARC, 1972-1980;
 * the Byte issue of August 1981 and the Blue Book of 1983): the
 * language and its whole environment, on a black and white screen of
 * overlapping windows (plan_tiny_smalltalk.md, notes_smalltalk.md).
 *
 *   the left button selects (Smalltalk's red button); the right one
 *   opens the menu of what is under it (the yellow button's): in a
 *   text, do it, print it, inspect it, accept, cancel, copy, cut,
 *   paste; on a window's title, collapse and close (the blue
 *   button's); on the grey desktop, the screen menu (restore display,
 *   browser, workspace, system transcript, save, load); on a
 *   Browser's class list, definition, comment, file out; on its
 *   message list, remove.
 *   Keys: Control-D do it, Control-P print it, Control-I inspect it,
 *   Control-S accept, Control-A select all, Control-C copy -- and,
 *   while something runs, the user interrupt: a notifier on the
 *   running process. With no selection, do it runs the caret's line.
 *   Flags: image=saved starts from the image saved last (the screen
 *   menu's save), window=browser|workspace|debugger what is in front.
 *
 * Smalltalk's lesson is that the environment is the language's own
 * objects, live: the System Browser lists the classes and methods the
 * running system has, and "accept" compiles a method into it at once;
 * a Workspace runs any text you select; an Inspector shows an object's
 * fields; and an error opens a notifier, then a Debugger on the
 * stopped process, whose frames are the contexts -- objects -- of the
 * computation, where you can fix the method, restart it and carry on:
 * define the missing method in the debugger ("define", on a message
 * not understood), and the program goes on as if it had always been
 * there. Everything runs a budget of bytecodes a frame, so an endless
 * loop leaves the screen alive and Control-C stops it.
 *
 * The Smalltalk is libs/languages/smalltalk: the Blue Book's language,
 * its compiler to the Blue Book's bytecodes, its interpreter over an
 * object table, its kernel written in Smalltalk (kernel/*.st) and
 * bootstrapped when the program starts (St_boot.mli). The display is a
 * Smalltalk Form, drawn with BitBlt (St_bitblt.mli): what a Pen draws
 * ("Pen new dragon: 9", the Blue Book's) appears over the windows, as
 * it did in 1980, until the screen menu's "restore display".
 *
 * The trick of this app, the one departure from the original: the
 * windows are drawn by OCaml, reading the live object memory, where
 * Smalltalk-80's were Smalltalk objects (Model-View-Controller:
 * StandardSystemView, StringHolderController...), themselves open to
 * the Browser. Writing them in Smalltalk is the first exercise below.
 *
 * What it uses: the Playground, Caps (the store, for the image and the
 * file outs), graphics_rgba (the Display's pixels as a picture) and
 * libs/languages/smalltalk. Not libs/gui: the 1980 look, list panes,
 * text panes with their selections reversed and pop-up menus, is drawn
 * here, a character to a cell.
 *
 * What it deliberately does not do (exercises): MVC in Smalltalk, the
 * environment as the language's objects; the compiler in Smalltalk;
 * resizing windows (the blue menu's "frame"); scroll bars (the wheel
 * scrolls); text wrapped to its pane; senders and implementors, the
 * Browser's cross references; the changes file, every accept logged;
 * undo; processes and semaphores, the Smalltalk-level scheduler; the
 * Alto's fonts, proportional, drawn by BitBlt.
 *)
open Playground
module M = St_memory
module I = St_interp
module C = St_class
module B = St_bytecode
module D = St_debug

type oop = M.oop
type caps = < Cap.open_in ; Cap.open_out >

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

(* the Smalltalk screen: 800 by 600 pixels, y downwards, as a Form's
 * and a Point's are; scaled to the window *)
let screen_w = 800.
let screen_h = 600.

(* a character's cell, and a title tab's height *)
let cw = 7.
let lh = 13.
let title_h = 15.

type rect = { x : float; y : float; w : float; h : float }

let inside (r : rect) ((px, py) : float * float) : bool = px >= r.x && px < r.x +. r.w && py >= r.y && py < r.y +. r.h
let scale_of (computer : computer) : float = Float.min (computer.screen.width /. screen_w) (computer.screen.height /. screen_h)

(* the Smalltalk screen's point of the mouse *)
let mouse_point (computer : computer) : float * float =
  let s = scale_of computer in
  ((computer.mouse.mx /. s) +. (screen_w /. 2.), (screen_h /. 2.) -. (computer.mouse.my /. s))

(*****************************************************************************)
(* Texts *)
(*****************************************************************************)

(* a text being edited: its string, the caret and the other end of the
 * selection (equal: no selection), the first line shown *)
type text = { s : string; caret : int; anchor : int; top : int }

let text_of (s : string) : text = { s; caret = 0; anchor = 0; top = 0 }
let selection (t : text) : int * int = (min t.caret t.anchor, max t.caret t.anchor)

let selected (t : text) : string =
  let a, b = selection t in
  String.sub t.s a (b - a)

let replace (t : text) (str : string) : text =
  let a, b = selection t in
  let s = String.sub t.s 0 a ^ str ^ String.sub t.s b (String.length t.s - b) in
  let c = a + String.length str in
  { t with s; caret = c; anchor = c }

let line_start (s : string) (i : int) : int =
  match String.rindex_from_opt s (i - 1) '\n' with Some j when i > 0 -> j + 1 | _ -> 0

let line_end (s : string) (i : int) : int = match String.index_from_opt s i '\n' with Some j -> j | None -> String.length s

let line_of (s : string) (i : int) : int =
  let n = ref 0 in
  for j = 0 to min i (String.length s) - 1 do
    if s.[j] = '\n' then incr n
  done;
  !n

(* where line n starts *)
let nth_line (s : string) (n : int) : int option =
  let rec go i k = if k = n then Some i else match String.index_from_opt s i '\n' with Some j -> go (j + 1) (k + 1) | None -> None in
  go 0 0

(* the column a character is drawn at, tabs every 4 *)
let column (s : string) (i : int) : int =
  let c = ref 0 in
  for j = line_start s i to i - 1 do
    if s.[j] = '\t' then c := (!c / 4 * 4) + 4 else incr c
  done;
  !c

(* the character at a column of a line, or the line's end *)
let index_at (s : string) (ls : int) (col : int) : int =
  let le = line_end s ls in
  let rec go i c = if i >= le then le else let c' = if s.[i] = '\t' then (c / 4 * 4) + 4 else c + 1 in if col < c' then (if col - c < c' - col then i else i + 1) else go (i + 1) c' in
  go ls 0

let is_word_char (c : char) : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = ':'

(* a double click: the whole line at its start or end, else the word *)
let select_word (t : text) (i : int) : text =
  let ls = line_start t.s i and le = line_end t.s i in
  if i = ls || i = le then { t with anchor = ls; caret = le }
  else begin
    let a = ref i and b = ref i in
    while !a > 0 && is_word_char t.s.[!a - 1] do decr a done;
    while !b < String.length t.s && is_word_char t.s.[!b] do incr b done;
    { t with anchor = !a; caret = !b }
  end

(* where [sub] first is in [s], or 0 *)
let find_sub (s : string) (sub : string) : int =
  let n = String.length sub in
  let rec go i = if i + n > String.length s then 0 else if String.sub s i n = sub then i else go (i + 1) in
  go 0

(* with nothing selected, the caret's line, as for do it *)
let or_line (t : text) : text =
  if t.caret <> t.anchor then t else { t with anchor = line_start t.s t.caret; caret = line_end t.s t.caret }

(* the first line shown moved so that the caret is in the pane *)
let reveal (rows : int) (t : text) : text =
  let l = line_of t.s t.caret in
  if l < t.top then { t with top = l } else if l >= t.top + rows then { t with top = l - rows + 1 } else t

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* what the virtual machine's host writes into, from inside Smalltalk *)
type io = {
  mutable transcript : string;
  mutable inspect : oop list; (* anObject inspect *)
  mutable mouse : int * int * int; (* for Sensor *)
  mutable held : oop list; (* what the windows hold, for the collector *)
}

(* what to do with a do-it's answer *)
type origin = Do_it | Print_it of int * int (* window, text *) | Inspect_it

type browser = {
  cat : string option;
  cls : string option;
  meta : bool;
  proto : string option;
  sel : string option;
  code : text;
}

type inspector = { obj : oop; field : int option; value : text }

type debugger = {
  proc : I.process;
  dorigin : origin;
  frames : D.frame list;
  frame : int;
  dcode : text;
  rfield : int option;
  rvalue : text;
  cfield : int option;
  cvalue : text;
  defining : oop option; (* the class a missing method is being written for *)
}

type notifier = { nproc : I.process; norigin : origin; lines : string list }
type kind = Workspace of text | Transcript of text | Browser of browser | Inspector of inspector | Debugger of debugger | Notifier of notifier

type window = {
  id : int;
  frame : rect; (* the contents; the title tab is above *)
  title : string;
  collapsed : bool;
  kind : kind;
  tops : int list; (* the list panes' first rows shown *)
}

type action =
  | Do_it_in of int * int
  | Print_it_in of int * int
  | Inspect_it_in of int * int
  | Accept of int * int
  | Cancel of int * int
  | Copy of int * int
  | Cut of int * int
  | Paste of int * int
  | Close of int
  | Collapse of int
  | Open_browser
  | Open_workspace
  | Open_transcript
  | Restore_display
  | Save_image
  | Load_image
  | Proceed of int
  | Debug of int
  | Restart of int
  | Step of int
  | Send of int
  | Define of int
  | Show_definition of int
  | Show_comment of int
  | File_out of int
  | Remove_method of int
  | Instance_side of int * bool

type menu = { at : float * float; items : (string * action) list }
type drag = Moving of int * float * float | Selecting of int * int

type model = {
  vm : I.vm;
  io : io;
  windows : window list; (* back to front *)
  focus : (int * int) option; (* the text typed into: its window and number *)
  menu : menu option;
  drag : drag option;
  running : (I.process * origin) list;
  next_id : int;
  keys : string Set_.t; (* held at the last frame *)
  left : bool;
  right : bool;
  (* the Display's black, where it is, and St_bitblt.changes when it was
   * pictured *)
  display : (int * (rect * Rgba_image.t) option) option;
  clipboard : string;
  seen : int; (* the Transcript's length last shown *)
  started : bool;
}

let mem (m : model) : M.t = I.memory m.vm

(*****************************************************************************)
(* The first screen *)
(*****************************************************************************)

let workspace_text =
  {|"The caret on a line, then the right button's menu:
do it, print it, inspect it (or Control-D, P, I)."
3 + 4 * 2
100 factorial
(1/3) + (2/3)
#(5 3 8 1 2) asSortedCollection
(Rectangle origin: 0@0 corner: 4@6) center
(1 to: 10) collect: [:i | i * i]
Transcript show: 'Hello, world!'; cr
Metaclass class class == Metaclass
(3@4) inspect
Pen new dragon: 9
Pen new spiral: 150 angle: 89
Pen new scribble
10 fib
3 zork: 4
3 halt
[true] whileTrue|}

let transcript_text =
  "Smalltalk-80 of the Blue Book (Goldberg and Robson, 1983), in OCaml.\n\
   The kernel's classes compiled to its bytecodes, from their text.\n"

let browser_on (cat : string) (cls : string) (proto : string) (sel : string) (source : string) : browser =
  { cat = Some cat; cls = Some cls; meta = false; proto = Some proto; sel = Some sel; code = text_of source }

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* a pane of a window: a text (its number), a list (its number, items,
 * selection), a button *)
type pane =
  | P_text of int * rect
  | P_list of int * rect * string list * int option
  | P_button of rect * string * bool * action

let columns (r : rect) (n : int) : rect list =
  let w = r.w /. float_of_int n in
  List.init n (fun i -> { r with x = r.x +. (float_of_int i *. w); w })

let split_h (r : rect) (f : float) : rect * rect =
  let h = Float.round (r.h *. f) in
  ({ r with h }, { r with y = r.y +. h; h = r.h -. h })

let buttons (r : rect) (bs : (string * bool * action) list) : pane list =
  List.map2 (fun c (l, on, a) -> P_button ({ x = c.x +. 2.; w = c.w -. 4.; y = c.y +. 2.; h = c.h -. 4. }, l, on, a)) (columns r (List.length bs)) bs

let categories (m : M.t) : string list = List.sort_uniq compare (List.map (C.category m) (C.classes m))

let class_named (m : M.t) (n : string) : oop option =
  match C.global m n with Some a when C.is_meta m (M.class_of m (M.fetch m a 1)) -> Some (M.fetch m a 1) | _ -> None

let browser_class (m : M.t) (b : browser) : oop option =
  match Option.bind b.cls (class_named m) with Some c -> Some (if b.meta then M.class_of m c else c) | None -> None

let index_of (x : string option) (l : string list) : int option =
  match x with
  | None -> None
  | Some x ->
      let rec go i = function [] -> None | y :: rest -> if x = y then Some i else go (i + 1) rest in
      go 0 l

let fields_of (m : model) (o : oop) : (string * oop) list = ("self", o) :: D.fields m.vm o

let panes (m : model) (w : window) : pane list =
  let mm = mem m in
  let f = w.frame in
  match w.kind with
  | Workspace _ | Transcript _ -> [ P_text (0, f) ]
  | Browser b ->
      let top, code = split_h f 0.45 in
      let cols = columns top 4 in
      let c0, c1, c2, c3 = match cols with [ a; b; c; d ] -> (a, b, c, d) | _ -> assert false in
      let c1, switch = split_h c1 ((c1.h -. 16.) /. c1.h) in
      let cats = categories mm in
      let classes =
        match b.cat with None -> [] | Some cat -> List.filter (fun c -> C.category mm c = cat) (C.classes mm) |> List.map (C.name mm)
      in
      let protos = match browser_class mm b with Some c -> C.categories mm c | None -> [] in
      let sels = match (browser_class mm b, b.proto) with Some c, Some p -> C.category_selectors mm c p | _ -> [] in
      [
        P_list (0, c0, cats, index_of b.cat cats);
        P_list (1, c1, classes, index_of b.cls classes);
        P_list (2, c2, protos, index_of b.proto protos);
        P_list (3, c3, sels, index_of b.sel sels);
        P_text (0, code);
      ]
      @ buttons switch [ ("instance", not b.meta, Instance_side (w.id, true)); ("class", b.meta, Instance_side (w.id, false)) ]
  | Inspector i ->
      let names = List.map fst (fields_of m i.obj) in
      let l, r = ({ f with w = Float.round (f.w *. 0.35) }, { f with x = f.x +. Float.round (f.w *. 0.35); w = f.w -. Float.round (f.w *. 0.35) }) in
      [ P_list (0, l, names, i.field); P_text (0, r) ]
  | Debugger d ->
      let stack, rest = split_h f 0.25 in
      let row, rest = split_h rest (18. /. rest.h) in
      let code, bottom = split_h rest 0.62 in
      let half = columns bottom 2 in
      let rl, rv, cl, cv =
        match half with
        | [ a; b ] ->
            let split r = ({ r with w = Float.round (r.w *. 0.4) }, { r with x = r.x +. Float.round (r.w *. 0.4); w = r.w -. Float.round (r.w *. 0.4) }) in
            let a1, a2 = split a and b1, b2 = split b in
            (a1, a2, b1, b2)
        | _ -> assert false
      in
      let ctx = match List.nth_opt d.frames d.frame with Some fr -> fr.ctx | None -> M.nil in
      let receiver = if ctx = M.nil then M.nil else M.fetch mm (I.context_home m.vm ctx) I.c_receiver in
      let dnu = D.not_understood m.vm d.proc <> None in
      [
        P_list (0, stack, List.map (fun (fr : D.frame) -> fr.label) d.frames, Some d.frame);
        P_text (0, code);
        P_list (1, rl, List.map fst (fields_of m receiver), d.rfield);
        P_text (1, rv);
        P_list (2, cl, (if ctx = M.nil then [] else List.map fst (D.variables m.vm ctx)), d.cfield);
        P_text (2, cv);
      ]
      @ buttons row
          ([ ("proceed", false, Proceed w.id); ("restart", false, Restart w.id); ("step", false, Step w.id); ("send", false, Send w.id) ]
          @ if dnu then [ ("define", d.defining <> None, Define w.id) ] else [])
  | Notifier n ->
      let l, row = split_h f ((f.h -. 20.) /. f.h) in
      let dnu = D.not_understood m.vm n.nproc <> None in
      P_list (0, l, n.lines, None)
      :: buttons row ([ ("proceed", false, Proceed w.id); ("debug", false, Debug w.id) ] @ (if dnu then [ ("define", false, Define w.id) ] else []) @ [ ("close", false, Close w.id) ])

let text_panes (m : model) (w : window) : (int * rect) list = List.filter_map (function P_text (i, r) -> Some (i, r) | _ -> None) (panes m w)
let rows (r : rect) : int = max 1 (int_of_float ((r.h -. 4.) /. lh))

let get_text (w : window) (i : int) : text =
  match (w.kind, i) with
  | (Workspace t | Transcript t), _ -> t
  | Browser b, _ -> b.code
  | Inspector ins, _ -> ins.value
  | Debugger d, 0 -> d.dcode
  | Debugger d, 1 -> d.rvalue
  | Debugger d, _ -> d.cvalue
  | Notifier _, _ -> text_of ""

let set_text (w : window) (i : int) (t : text) : window =
  let kind =
    match (w.kind, i) with
    | Workspace _, _ -> Workspace t
    | Transcript _, _ -> Transcript t
    | Browser b, _ -> Browser { b with code = t }
    | Inspector ins, _ -> Inspector { ins with value = t }
    | Debugger d, 0 -> Debugger { d with dcode = t }
    | Debugger d, 1 -> Debugger { d with rvalue = t }
    | Debugger d, _ -> Debugger { d with cvalue = t }
    | (Notifier _ as k), _ -> k
  in
  { w with kind }

let list_top (w : window) (i : int) : int = Option.value (List.nth_opt w.tops i) ~default:0
let set_list_top (w : window) (i : int) (v : int) : window =
  let tops = List.init (max (i + 1) (List.length w.tops)) (fun j -> if j = i then max 0 v else list_top w j) in
  { w with tops }

(*****************************************************************************)
(* Windows *)
(*****************************************************************************)

let find (m : model) (id : int) : window option = List.find_opt (fun w -> w.id = id) m.windows
let update_window (m : model) (id : int) (f : window -> window) : model = { m with windows = List.map (fun w -> if w.id = id then f w else w) m.windows }

let to_front (m : model) (id : int) : model =
  match find m id with Some w -> { m with windows = List.filter (fun x -> x.id <> id) m.windows @ [ w ] } | None -> m

let add_window (m : model) ~(title : string) (frame : rect) (kind : kind) : model * int =
  let id = m.next_id in
  ({ m with windows = m.windows @ [ { id; frame; title; collapsed = false; kind; tops = [] } ]; next_id = id + 1 }, id)

let close (m : model) (id : int) : model =
  { m with windows = List.filter (fun w -> w.id <> id) m.windows; focus = (match m.focus with Some (w, _) when w = id -> None | f -> f) }

(* where the n-th window of its kind opens, a little lower each time *)
let cascade (m : model) (w : float) (h : float) : rect =
  let n = float_of_int (List.length m.windows mod 6) in
  { x = 140. +. (n *. 18.); y = 60. +. (n *. 18.); w; h }

let print_string (m : model) (o : oop) : string = I.print_string m.vm o

let open_inspector (m : model) (o : oop) : model =
  let title = C.name (mem m) (M.class_of (mem m) o) in
  fst (add_window m ~title (cascade m 360. 180.) (Inspector { obj = o; field = Some 0; value = text_of (print_string m o) }))

let frames_of (m : model) (p : I.process) ~(stepping : bool) : D.frame list = D.frames ~stepping m.vm p

(* the code pane of a debugger on its frame: the method, the send in
 * progress selected *)
let frame_code (d : debugger) : text =
  match List.nth_opt d.frames d.frame with
  | Some fr ->
      let t = text_of fr.source in
      (match fr.highlight with Some (a, b) -> reveal 8 { t with anchor = a; caret = b } | None -> t)
  | None -> text_of ""

let open_debugger (m : model) ?(at : rect option) ~(label : string) (p : I.process) (origin : origin) : model =
  let frames = frames_of m p ~stepping:(label = "Step") in
  let d =
    { proc = p; dorigin = origin; frames; frame = 0; dcode = text_of ""; rfield = None; rvalue = text_of ""; cfield = None; cvalue = text_of ""; defining = None }
  in
  let d = { d with dcode = frame_code d } in
  let frame = match at with Some r -> { r with w = 560.; h = 420. } | None -> { x = 120.; y = 40.; w = 560.; h = 420. } in
  fst (add_window m ~title:label frame (Debugger d))

let open_notifier (m : model) (label : string) (p : I.process) (origin : origin) : model =
  let lines = List.filteri (fun i _ -> i < 5) (List.map (fun (f : D.frame) -> f.label) (frames_of m p ~stepping:false)) in
  fst (add_window m ~title:label { x = 180.; y = 150.; w = 420.; h = 100. } (Notifier { nproc = p; norigin = origin; lines }))

(*****************************************************************************)
(* Running Smalltalk *)
(*****************************************************************************)

(* bytecodes a frame while a do-it runs: about 10 ms natively (9.5
 * million a second), 60 on the web (1.6 million, under node) *)
let budget = 100_000

let with_text (m : model) (wid : int) (tid : int) (f : text -> text) : model =
  update_window m wid (fun w -> set_text w tid (f (get_text w tid)))

(* a compiler's complaint, inserted where it is and selected, as the
 * Smalltalk-80 compiler did it: one Backspace takes it away *)
let complain (t : text) (at : int) (msg : string) : text =
  let t = { t with anchor = at; caret = at } in
  let msg = " " ^ msg ^ " ->" in
  let t = replace t msg in
  { t with anchor = at; caret = at + String.length msg }

(* what "self" is in a text: the inspected object, the frame's receiver *)
let receiver_of (m : model) (w : window) (tid : int) : oop =
  match w.kind with
  | Inspector i -> i.obj
  | Debugger d when tid = 0 -> (
      match List.nth_opt d.frames d.frame with Some fr -> M.fetch (mem m) (I.context_home m.vm fr.ctx) I.c_receiver | None -> M.nil)
  | _ -> M.nil

let evaluate (m : model) (wid : int) (tid : int) (origin : origin) : model =
  match find m wid with
  | None -> m
  | Some w ->
      let t = or_line (get_text w tid) in
      let a, _ = selection t in
      let receiver = receiver_of m w tid in
      let mm = mem m in
      (match St_compile.compile_doit mm ~receiver_class:(M.class_of mm receiver) (selected t) with
      | meth ->
          let p = I.spawn_method m.vm meth receiver in
          { (with_text m wid tid (fun _ -> t)) with running = m.running @ [ (p, origin) ] }
      | exception St_compile.Error (pos, msg) -> with_text m wid tid (fun _ -> complain t (a + pos) msg))

(* a finished do it's answer, where it goes *)
let answered (m : model) (origin : origin) (v : oop) : model =
  match origin with
  | Do_it -> m
  | Inspect_it -> open_inspector m v
  | Print_it (wid, tid) ->
      let str = print_string m v in
      with_text m wid tid (fun t ->
          let _, b = selection t in
          let t = replace { t with anchor = b; caret = b } (" " ^ str) in
          { t with anchor = b + 1 })

let stopped (m : model) (p : I.process) (origin : origin) : model =
  match p.state with
  | I.Finished v -> answered m origin v
  | I.Suspended label -> open_notifier m label p origin
  | I.Runnable -> { m with running = m.running @ [ (p, origin) ] }
  | I.Terminated -> m

(* a frame's worth of the processes, the first one first, round robin *)
let run_processes (m : model) : model =
  match m.running with
  | [] -> m
  | (p, origin) :: rest ->
      I.run m.vm p ~budget;
      stopped { m with running = rest } p origin

let interrupt (m : model) : model =
  List.fold_left
    (fun m (p, origin) ->
      I.suspend p "User Interrupt";
      open_notifier m "User Interrupt" p origin)
    { m with running = [] } m.running

(*****************************************************************************)
(* The Browser *)
(*****************************************************************************)

let method_template = "message selector and argument names\n\t\"comment stating purpose of message\"\n\n\t| temporary variable names |\n\tstatements"

let class_template (cat : string) =
  Printf.sprintf "Object subclass: #NameOfClass\n\tinstanceVariableNames: 'instVarName1 instVarName2'\n\tclassVariableNames: ''\n\tpoolDictionaries: ''\n\tcategory: '%s'" cat

(* the code pane of a Browser, from its selections *)
let browser_code (mm : M.t) (b : browser) : text =
  match (browser_class mm b, b.proto, b.sel) with
  | Some c, _, Some sel -> ( match C.local_method mm c (M.symbol mm sel) with Some meth -> text_of (B.source mm meth) | None -> text_of "")
  | Some _, Some _, None -> text_of method_template
  | Some c, None, _ -> text_of (C.definition mm c)
  | None, _, _ -> ( match b.cat with Some cat -> text_of (class_template cat) | None -> text_of "")

let browse (m : model) (wid : int) (f : browser -> browser) : model =
  let mm = mem m in
  update_window m wid (fun w -> match w.kind with Browser b -> let b = f b in { w with kind = Browser { b with code = browser_code mm b } } | _ -> w)

let accept_in_browser (m : model) (wid : int) (b : browser) : model =
  let mm = mem m in
  let t = b.code in
  match (browser_class mm b, b.proto) with
  | Some c, _ when b.proto <> None || b.sel <> None -> (
      let category = match b.proto with Some p -> p | None -> "as yet unclassified" in
      match St_compile.compile_and_install mm ~cls:c ~category t.s with
      | sel ->
          I.flush_cache m.vm;
          browse m wid (fun b -> { b with proto = Some category; sel = Some sel })
      | exception St_compile.Error (pos, msg) -> with_text m wid 0 (fun t -> complain t pos msg))
  | _ -> (
      (* a class's definition: evaluated, it defines the class *)
      match I.evaluate m.vm t.s with
      | Ok cls when (not (M.is_int cls)) && cls <> M.nil && C.is_meta mm (M.class_of mm cls) ->
          browse m wid (fun b -> { b with cat = Some (C.category mm cls); cls = Some (C.name mm cls); meta = false; proto = None; sel = None })
      | Ok _ -> m
      | Error msg -> with_text m wid 0 (fun t -> complain t (String.length t.s) msg))

let file_out (mm : M.t) (cls : oop) : string =
  let side c meta =
    List.map
      (fun cat ->
        St_chunk.methods_chunk ~class_name:(C.name mm (C.this_class mm cls)) ~meta ~category:cat
          (List.filter_map (fun sel -> Option.map (B.source mm) (C.local_method mm c (M.symbol mm sel))) (C.category_selectors mm c cat)))
      (C.categories mm c)
  in
  String.concat "\n" ((St_chunk.chunk (C.definition mm cls) ^ "\n") :: (side cls false @ side (M.class_of mm cls) true))

(*****************************************************************************)
(* The debugger *)
(*****************************************************************************)

let debugging (m : model) (wid : int) (f : debugger -> model) : model =
  match find m wid with Some { kind = Debugger d; _ } -> f d | _ -> m

(* the debugger's panes again, after its process moved *)
let refresh (m : model) (wid : int) ~(stepping : bool) : model =
  debugging m wid (fun d ->
      match d.proc.state with
      | I.Suspended _ ->
          let frames = frames_of m d.proc ~stepping in
          let d = { d with frames; frame = 0; rfield = None; cfield = None; rvalue = text_of ""; cvalue = text_of "" } in
          update_window m wid (fun w -> { w with kind = Debugger { d with dcode = frame_code d } })
      | _ -> stopped (close m wid) d.proc d.dorigin)

let select_frame (m : model) (wid : int) (i : int) : model =
  update_window m wid (fun w ->
      match w.kind with
      | Debugger d ->
          let d = { d with frame = i; rfield = None; cfield = None; rvalue = text_of ""; cvalue = text_of ""; defining = None } in
          { w with kind = Debugger { d with dcode = frame_code d } }
      | _ -> w)

let accept_in_debugger (m : model) (wid : int) (d : debugger) : model =
  let mm = mem m in
  match (d.defining, List.nth_opt d.frames d.frame) with
  | Some cls, _ -> (
      (* the missing method, into the receiver's class; then the frame
       * that sent it, again *)
      match St_compile.compile_and_install mm ~cls ~category:"as yet unclassified" d.dcode.s with
      | _ -> (
          I.flush_cache m.vm;
          match D.not_understood m.vm d.proc with
          | Some (_, _, sender) ->
              ignore (D.restart m.vm d.proc sender);
              refresh m wid ~stepping:true
          | None -> refresh m wid ~stepping:true)
      | exception St_compile.Error (pos, msg) -> with_text m wid 0 (fun t -> complain t pos msg))
  | None, Some fr -> (
      let meth = I.context_method m.vm fr.ctx in
      let cls = B.method_class mm meth in
      if cls = M.nil then m
      else
        match St_compile.compile_and_install mm ~cls ~category:(Option.value (C.category_of mm cls (M.string_of mm (B.selector mm meth))) ~default:"as yet unclassified") d.dcode.s with
        | _ ->
            I.flush_cache m.vm;
            ignore (D.restart m.vm d.proc fr.ctx);
            refresh m wid ~stepping:true
        | exception St_compile.Error (pos, msg) -> with_text m wid 0 (fun t -> complain t pos msg))
  | None, None -> m

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let image_name = "TinySmalltalk80.image"

let host (io : io) : I.host =
  {
    transcript =
      (fun s ->
        let s = String.map (fun c -> if c = '\r' then '\n' else c) s in
        let t = io.transcript ^ s in
        io.transcript <- (if String.length t > 20_000 then String.sub t (String.length t - 20_000) 20_000 else t));
    milliseconds = (fun () -> int_of_float (Sys.time () *. 1000.));
    inspect = (fun o -> io.inspect <- io.inspect @ [ o ]);
    mouse = (fun () -> io.mouse);
  }

let new_vm (io : io) (image : string option) : I.vm =
  let vm = match image with Some s -> St_image.load_vm ~host:(host io) s | None -> St_boot.boot ~host:(host io) () in
  I.set_extra_roots vm (fun () -> io.held);
  vm

let rec perform (caps : caps) (m : model) (a : action) : model =
  let mm = mem m in
  match a with
  | Do_it_in (w, t) -> evaluate m w t Do_it
  | Print_it_in (w, t) -> evaluate m w t (Print_it (w, t))
  | Inspect_it_in (w, t) -> evaluate m w t Inspect_it
  | Accept (wid, tid) -> (
      match find m wid with
      | Some { kind = Browser b; _ } -> accept_in_browser m wid b
      | Some { kind = Debugger d; _ } when tid = 0 -> accept_in_debugger m wid d
      | Some ({ kind = Inspector i; _ } as w) -> (
          (* the value, evaluated, put in the selected field *)
          match (I.evaluate m.vm ~receiver:i.obj (get_text w tid).s, i.field) with
          | Ok v, Some k when k > 0 && not (M.is_int i.obj) ->
              (match M.body mm i.obj with M.Pointers a when k - 1 < Array.length a -> a.(k - 1) <- v | _ -> ());
              m
          | _ -> m)
      | _ -> m)
  | Cancel (wid, tid) -> (
      match find m wid with
      | Some { kind = Browser b; _ } -> with_text m wid tid (fun _ -> browser_code mm b)
      | _ -> m)
  | Copy (wid, tid) -> ( match find m wid with Some w -> { m with clipboard = selected (get_text w tid) } | None -> m)
  | Cut (wid, tid) -> (
      match find m wid with
      | Some w ->
          let t = get_text w tid in
          { (with_text m wid tid (fun t -> replace t "")) with clipboard = selected t }
      | None -> m)
  | Paste (wid, tid) -> with_text m wid tid (fun t -> replace t m.clipboard)
  | Close wid -> (
      (* closing a notifier or a debugger ends its process *)
      (match find m wid with
      | Some { kind = Notifier n; _ } -> I.terminate m.vm n.nproc
      | Some { kind = Debugger d; _ } -> I.terminate m.vm d.proc
      | _ -> ());
      close m wid)
  | Collapse wid -> update_window m wid (fun w -> { w with collapsed = not w.collapsed })
  | Open_browser -> fst (add_window m ~title:"System Browser" (cascade m 560. 320.) (Browser { cat = None; cls = None; meta = false; proto = None; sel = None; code = text_of "" }))
  | Open_workspace -> fst (add_window m ~title:"Workspace" (cascade m 380. 200.) (Workspace (text_of "")))
  | Open_transcript -> fst (add_window m ~title:"System Transcript" (cascade m 380. 150.) (Transcript (text_of m.io.transcript)))
  | Restore_display ->
      ignore (I.evaluate m.vm "Display fillWhite");
      m
  | Save_image ->
      let image = St_image.save mm in
      Playground_platform.store caps image_name image;
      m.io.transcript <- m.io.transcript ^ Printf.sprintf "Image saved: %d objects, %d bytes.\n" (M.live mm) (String.length image);
      m
  | Load_image -> (
      match Playground_platform.fetch caps image_name with
      | Some image ->
          let vm = new_vm m.io (Some image) in
          m.io.transcript <- m.io.transcript ^ "Image loaded.\n";
          let keep = List.filter (fun w -> match w.kind with Workspace _ | Transcript _ | Browser _ -> true | _ -> false) m.windows in
          { m with vm; windows = keep; running = [] }
      | None ->
          m.io.transcript <- m.io.transcript ^ "No image saved yet: the screen menu's save first.\n";
          m)
  | Proceed wid -> (
      match find m wid with
      | Some { kind = Notifier n; _ } ->
          D.proceed n.nproc;
          { (close m wid) with running = m.running @ [ (n.nproc, n.norigin) ] }
      | Some { kind = Debugger d; _ } ->
          D.proceed d.proc;
          { (close m wid) with running = m.running @ [ (d.proc, d.dorigin) ] }
      | _ -> m)
  | Debug wid -> (
      match find m wid with
      | Some ({ kind = Notifier n; _ } as w) -> open_debugger (close m wid) ~at:w.frame ~label:w.title n.nproc n.norigin
      | _ -> m)
  | Define wid -> (
      match find m wid with
      | Some { kind = Notifier _; _ } ->
          (* the debugger first, the newest window *)
          let m = perform caps m (Debug wid) in
          perform caps m (Define (m.next_id - 1))
      | Some { kind = Debugger d; _ } -> (
          match D.not_understood m.vm d.proc with
          | Some (cls, sel, _) ->
              (* the template selected, so that typing replaces it *)
              let t = text_of (D.template sel) in
              let t = { t with caret = String.length t.s } in
              { (update_window m wid (fun w -> { w with kind = Debugger { d with defining = Some cls; dcode = t } })) with focus = Some (wid, 0) }
          | None -> m)
      | _ -> m)
  | Restart wid ->
      debugging m wid (fun d ->
          match List.nth_opt d.frames d.frame with
          | Some fr ->
              ignore (D.restart m.vm d.proc fr.ctx);
              refresh m wid ~stepping:true
          | None -> m)
  | Step wid ->
      debugging m wid (fun d ->
          match List.nth_opt d.frames d.frame with
          | Some fr ->
              D.step m.vm d.proc fr.ctx;
              refresh m wid ~stepping:true
          | None -> m)
  | Send wid ->
      debugging m wid (fun d ->
          match List.nth_opt d.frames d.frame with
          | Some fr ->
              D.step_into m.vm d.proc fr.ctx;
              refresh m wid ~stepping:true
          | None -> m)
  | Show_definition wid -> browse m wid (fun b -> { b with proto = None; sel = None })
  | Show_comment wid -> (
      match find m wid with
      | Some { kind = Browser b; _ } -> (
          match browser_class mm b with Some c -> with_text m wid 0 (fun _ -> text_of ("\"" ^ C.comment mm c ^ "\"")) | None -> m)
      | _ -> m)
  | File_out wid -> (
      match find m wid with
      | Some { kind = Browser b; _ } -> (
          match Option.bind b.cls (class_named mm) with
          | Some c ->
              let name = C.name mm c ^ ".st" in
              Playground_platform.export caps name (file_out mm c);
              m.io.transcript <- m.io.transcript ^ "Filed out " ^ name ^ "\n";
              m
          | None -> m)
      | _ -> m)
  | Remove_method wid -> (
      match find m wid with
      | Some { kind = Browser b; _ } -> (
          match (browser_class mm b, b.sel) with
          | Some c, Some sel ->
              C.remove mm c (M.symbol mm sel);
              I.flush_cache m.vm;
              browse m wid (fun b -> { b with sel = None })
          | _ -> m)
      | _ -> m)
  | Instance_side (wid, inst) -> browse m wid (fun b -> { b with meta = not inst; proto = None; sel = None })

(*****************************************************************************)
(* Lists *)
(*****************************************************************************)

let select_item (m : model) (w : window) (list : int) (i : int option) : model =
  let mm = mem m in
  match w.kind with
  | Browser b -> (
      let items = List.concat_map (function P_list (k, _, items, _) when k = list -> [ items ] | _ -> []) (panes m w) in
      let item = match (items, i) with [ items ], Some i -> List.nth_opt items i | _ -> None in
      match list with
      | 0 -> browse m w.id (fun _ -> { b with cat = item; cls = None; proto = None; sel = None })
      | 1 -> browse m w.id (fun _ -> { b with cls = item; proto = None; sel = None })
      | 2 -> browse m w.id (fun _ -> { b with proto = item; sel = None })
      | _ -> browse m w.id (fun _ -> { b with sel = item }))
  | Inspector ins ->
      let fs = fields_of m ins.obj in
      let value = match i with Some k -> ( match List.nth_opt fs k with Some (_, v) -> text_of (print_string m v) | None -> text_of "") | None -> text_of "" in
      update_window m w.id (fun w -> { w with kind = Inspector { ins with field = i; value } })
  | Debugger d -> (
      match (list, i) with
      | 0, Some k -> select_frame m w.id k
      | 1, _ ->
          let ctx = match List.nth_opt d.frames d.frame with Some fr -> fr.ctx | None -> M.nil in
          let receiver = if ctx = M.nil then M.nil else M.fetch mm (I.context_home m.vm ctx) I.c_receiver in
          let v = match i with Some k -> Option.map snd (List.nth_opt (fields_of m receiver) k) | None -> None in
          update_window m w.id (fun w -> { w with kind = Debugger { d with rfield = i; rvalue = text_of (match v with Some v -> print_string m v | None -> "") } })
      | 2, _ ->
          let ctx = match List.nth_opt d.frames d.frame with Some fr -> fr.ctx | None -> M.nil in
          let v = match i with Some k when ctx <> M.nil -> Option.map snd (List.nth_opt (D.variables m.vm ctx) k) | _ -> None in
          update_window m w.id (fun w -> { w with kind = Debugger { d with cfield = i; cvalue = text_of (match v with Some v -> print_string m v | None -> "") } })
      | _ -> m)
  | _ -> m

(*****************************************************************************)
(* The mouse *)
(*****************************************************************************)

(* the frontmost window under a point, and whether on its title tab *)
let title_rect (w : window) : rect = { x = w.frame.x; y = w.frame.y -. title_h; w = (float_of_int (String.length w.title) *. cw) +. 12.; h = title_h }

let window_at (m : model) (p : float * float) : (window * bool) option =
  List.fold_left
    (fun acc w -> if inside (title_rect w) p then Some (w, true) else if (not w.collapsed) && inside w.frame p then Some (w, false) else acc)
    None m.windows

let pane_at (m : model) (w : window) (p : float * float) : pane option =
  List.find_opt (function P_text (_, r) | P_list (_, r, _, _) | P_button (r, _, _, _) -> inside r p) (panes m w)

(* the character under a point of a text pane *)
let index_at_point (r : rect) (t : text) ((px, py) : float * float) : int =
  let row = int_of_float ((py -. r.y -. 2.) /. lh) in
  match nth_line t.s (t.top + max 0 row) with
  | Some ls -> index_at t.s ls (int_of_float (Float.round ((px -. r.x -. 3.) /. cw)))
  | None -> String.length t.s

let menu_rect (menu : menu) : rect =
  let x, y = menu.at in
  let w = (float_of_int (List.fold_left (fun a (l, _) -> max a (String.length l)) 0 menu.items) *. cw) +. 16. in
  let h = (float_of_int (List.length menu.items) *. lh) +. 6. in
  { x = Float.min x (screen_w -. w); y = Float.min y (screen_h -. h); w; h }

let menu_item (menu : menu) (p : float * float) : action option =
  let r = menu_rect menu in
  if not (inside r p) then None else List.nth_opt menu.items (int_of_float ((snd p -. r.y -. 3.) /. lh)) |> Option.map snd

let text_menu (wid : int) (tid : int) : (string * action) list =
  [
    ("do it", Do_it_in (wid, tid));
    ("print it", Print_it_in (wid, tid));
    ("inspect it", Inspect_it_in (wid, tid));
    ("accept", Accept (wid, tid));
    ("cancel", Cancel (wid, tid));
    ("copy", Copy (wid, tid));
    ("cut", Cut (wid, tid));
    ("paste", Paste (wid, tid));
  ]

let screen_menu =
  [
    ("restore display", Restore_display);
    ("browser", Open_browser);
    ("workspace", Open_workspace);
    ("system transcript", Open_transcript);
    ("save", Save_image);
    ("load", Load_image);
  ]

(* the right button: the menu of what is under the mouse *)
let open_menu (m : model) (p : float * float) : model =
  let items =
    match window_at m p with
    | Some (w, true) -> [ ((if w.collapsed then "expand" else "collapse"), Collapse w.id); ("close", Close w.id) ]
    | Some (w, false) -> (
        match (w.kind, pane_at m w p) with
        | _, Some (P_text (tid, _)) -> text_menu w.id tid
        | Browser _, Some (P_list (1, _, _, _)) -> [ ("definition", Show_definition w.id); ("comment", Show_comment w.id); ("file out", File_out w.id) ]
        | Browser _, Some (P_list (3, _, _, _)) -> [ ("remove", Remove_method w.id) ]
        | Debugger _, Some (P_list (0, _, _, _)) -> [ ("proceed", Proceed w.id); ("restart", Restart w.id); ("step", Step w.id); ("send", Send w.id) ]
        | _ -> [])
    | None -> screen_menu
  in
  if items = [] then m else { m with menu = Some { at = p; items } }

let left_down (caps : caps) (m : model) (computer : computer) (p : float * float) : model =
  match m.menu with
  | Some menu -> ( let m = { m with menu = None } in match menu_item menu p with Some a -> perform caps m a | None -> m)
  | None -> (
      match window_at m p with
      | None -> m
      | Some (w, true) -> { (to_front m w.id) with drag = Some (Moving (w.id, fst p -. w.frame.x, snd p -. w.frame.y)) }
      | Some (w, false) -> (
          let m = to_front m w.id in
          match pane_at m w p with
          | Some (P_button (_, _, _, a)) -> perform caps m a
          | Some (P_list (k, r, items, sel)) ->
              let row = int_of_float ((snd p -. r.y -. 2.) /. lh) + list_top w k in
              let i = if row >= 0 && row < List.length items then Some row else None in
              (* a second click on the selection deselects, as in 1980 *)
              let i = if i = sel then None else i in
              select_item m w k i
          | Some (P_text (tid, r)) ->
              let t = get_text w tid in
              let i = index_at_point r t p in
              let t = if computer.mouse.mdouble then select_word t i else { t with caret = i; anchor = i } in
              let m = with_text m w.id tid (fun _ -> t) in
              { m with focus = Some (w.id, tid); drag = (if computer.mouse.mdouble then None else Some (Selecting (w.id, tid))) }
          | None -> m))

let dragging (m : model) (p : float * float) : model =
  match m.drag with
  | Some (Moving (id, dx, dy)) -> update_window m id (fun w -> { w with frame = { w.frame with x = fst p -. dx; y = snd p -. dy } })
  | Some (Selecting (id, tid)) -> (
      match find m id with
      | Some w -> (
          match List.assoc_opt tid (text_panes m w) with
          | Some r -> with_text m id tid (fun t -> { t with caret = index_at_point r t p })
          | None -> m)
      | None -> m)
  | None -> m

let wheel (m : model) (p : float * float) (notches : float) : model =
  let d = if notches > 0. then -3 else 3 in
  match window_at m p with
  | Some (w, false) -> (
      match pane_at m w p with
      | Some (P_text (tid, _)) -> with_text m w.id tid (fun t -> { t with top = max 0 (min (line_of t.s (String.length t.s)) (t.top + d)) })
      | Some (P_list (k, _, items, _)) -> update_window m w.id (fun w -> set_list_top w k (min (List.length items - 1) (list_top w k + d)))
      | _ -> m)
  | _ -> m

(*****************************************************************************)
(* The keyboard *)
(*****************************************************************************)

(* Control and a letter: the text's menu without the menu *)
let shortcut (wid : int) (tid : int) (key : string) : action option =
  match String.lowercase_ascii key with
  | "d" -> Some (Do_it_in (wid, tid))
  | "p" -> Some (Print_it_in (wid, tid))
  | "i" -> Some (Inspect_it_in (wid, tid))
  | "s" -> Some (Accept (wid, tid))
  | "c" -> Some (Copy (wid, tid))
  | "x" -> Some (Cut (wid, tid))
  | "v" -> Some (Paste (wid, tid))
  | _ -> None

(* the keys into the focused text: the model, and the action a shortcut
 * asks for *)
let key_edit (m : model) (computer : computer) (went_down : string list) : model * action option =
  match m.focus with
  | None -> (m, None)
  | Some (wid, tid) when Set_.mem "Control" computer.keyboard.keys ->
      let m =
        if List.mem "a" (List.map String.lowercase_ascii went_down) then
          with_text m wid tid (fun t -> { t with anchor = 0; caret = String.length t.s })
        else m
      in
      (m, List.find_map (shortcut wid tid) went_down)
  | Some (wid, tid) ->
      let typed = String.of_seq (Seq.filter (fun c -> Char.code c >= 32 && Char.code c < 127) (String.to_seq computer.keyboard.typed)) in
      ( with_text m wid tid (fun t ->
          let t = if typed <> "" then replace t typed else t in
          List.fold_left
            (fun t k ->
              let len = String.length t.s in
              let a, b = selection t in
              match k with
              | "Enter" -> replace t "\n"
              | "Tab" -> replace t "\t"
              | "Backspace" -> if a < b then replace t "" else if a > 0 then replace { t with anchor = a - 1; caret = a } "" else t
              | "Delete" -> if a < b then replace t "" else if a < len then replace { t with anchor = a; caret = a + 1 } "" else t
              | "ArrowLeft" -> let c = if a < b then a else max 0 (t.caret - 1) in { t with caret = c; anchor = c }
              | "ArrowRight" -> let c = if a < b then b else min len (t.caret + 1) in { t with caret = c; anchor = c }
              | "ArrowUp" | "ArrowDown" ->
                  let ls = line_start t.s t.caret in
                  let col = column t.s t.caret in
                  let target = if k = "ArrowUp" then (if ls = 0 then None else Some (line_start t.s (ls - 1))) else (let le = line_end t.s t.caret in if le >= len then None else Some (le + 1)) in
                  (match target with Some l -> let c = index_at t.s l col in { t with caret = c; anchor = c } | None -> t)
              | "Home" -> let c = line_start t.s t.caret in { t with caret = c; anchor = c }
              | "End" -> let c = line_end t.s t.caret in { t with caret = c; anchor = c }
              | _ -> t)
            t went_down),
        None )

(* the focused text's caret kept in its pane *)
let keep_caret_visible (m : model) : model =
  match m.focus with
  | Some (wid, tid) -> (
      match find m wid with
      | Some w -> ( match List.assoc_opt tid (text_panes m w) with Some r -> with_text m wid tid (reveal (rows r)) | None -> m)
      | None -> m)
  | None -> m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let initial_windows (m : model) : model =
  let mm = mem m in
  let rect_src = match Option.bind (class_named mm "Rectangle") (fun c -> C.local_method mm c (M.symbol mm "center")) with Some meth -> B.source mm meth | None -> "" in
  let m, _ =
    add_window m ~title:"System Browser" { x = 8.; y = 20.; w = 560.; h = 300. }
      (Browser (browser_on "Graphics-Primitives" "Rectangle" "rectangle functions" "center" rect_src))
  in
  let m, _ = add_window m ~title:"System Transcript" { x = 480.; y = 350.; w = 312.; h = 240. } (Transcript (text_of m.io.transcript)) in
  let m, ws = add_window m ~title:"Workspace" { x = 8.; y = 350.; w = 462.; h = 240. } (Workspace (text_of workspace_text)) in
  { m with focus = Some (ws, 0) }

let initial : model =
  let io = { transcript = transcript_text; inspect = []; mouse = (0, 0, 0); held = [] } in
  let vm = new_vm io None in
  {
    vm;
    io;
    windows = [];
    focus = None;
    menu = None;
    drag = None;
    running = [];
    next_id = 1;
    keys = Set_.empty;
    left = false;
    right = false;
    display = None;
    clipboard = "";
    seen = 0;
    started = false;
  }

(* the flags, at the first frame *)
let start (caps : caps) (computer : computer) (m : model) : model =
  let m = { m with started = true } in
  let m = if List.assoc_opt "image" computer.flags = Some "saved" then perform caps m Load_image else m in
  let m = initial_windows m in
  match List.assoc_opt "window" computer.flags with
  | Some "browser" -> ( match List.find_opt (fun w -> match w.kind with Browser _ -> true | _ -> false) m.windows with Some w -> to_front m w.id | None -> m)
  | Some "debugger" -> (
      (* the classic session's start: a message not understood *)
      match List.find_opt (fun w -> match w.kind with Workspace _ -> true | _ -> false) m.windows with
      | Some w ->
          let m = with_text m w.id 0 (fun t -> let i = find_sub t.s "10 fib" in { t with caret = i; anchor = i }) in
          evaluate m w.id 0 (Print_it (w.id, 0))
      | None -> m)
  | _ -> m

(* the oops the windows hold, for the collector *)
let held (m : model) : oop list =
  List.concat_map (fun w -> match w.kind with Inspector i -> [ i.obj ] | _ -> []) m.windows

let update (caps : caps) (computer : computer) (m : model) : model =
  let m = if m.started then m else start caps computer m in
  let p = mouse_point computer in
  let went_down = Set_.elements (Set_.diff computer.keyboard.keys m.keys) in
  let busy = m.running <> [] in
  m.io.mouse <-
    (int_of_float (fst p), int_of_float (snd p), (if computer.mouse.mdown then 4 else 0) lor if computer.mouse.mrdown then 2 else 0);
  m.io.held <- held m;
  let pressed_left = computer.mouse.mdown && not m.left and pressed_right = computer.mouse.mrdown && not m.right in
  let m =
    if busy then
      (* the mouse is Smalltalk's while it runs (Sensor), as it was;
       * Control-C interrupts *)
      if Set_.mem "Control" computer.keyboard.keys && List.mem "c" went_down then interrupt m else m
    else begin
      let m = if pressed_right then open_menu m p else if pressed_left then left_down caps m computer p else m in
      let m = if computer.mouse.mdown && m.drag <> None then dragging m p else m in
      let m = if not computer.mouse.mdown then { m with drag = None } else m in
      let m = if computer.mouse.mwheel <> 0. then wheel m p computer.mouse.mwheel else m in
      let m, shortcut = key_edit m computer went_down in
      let m = match shortcut with Some a -> perform caps m a | None -> m in
      keep_caret_visible m
    end
  in
  let m = run_processes m in
  (* anObject inspect, from inside Smalltalk *)
  let requests = m.io.inspect in
  m.io.inspect <- [];
  let m = List.fold_left open_inspector m requests in
  (* the Transcript's new text *)
  let m =
    if String.length m.io.transcript = m.seen then m
    else
      let s = m.io.transcript in
      let m =
        { m with windows = List.map (fun w -> match w.kind with Transcript _ -> let n = String.length s in { w with kind = Transcript (reveal (rows w.frame) { (text_of s) with caret = n; anchor = n }) } | _ -> w) m.windows }
      in
      { m with seen = String.length s }
  in
  (* the Display's picture, made again when BitBlt drew *)
  let changes = St_bitblt.changes () in
  let m =
    match m.display with
    | Some (c, _) when c = changes -> m
    | _ ->
        let picture =
          match C.global (mem m) "Display" with
          | Some a -> (
              match St_bitblt.form (mem m) (M.fetch (mem m) a 1) with
              | Some (w, h, px) ->
                  (* only the rectangle that has black in it: a picture of
                   * the whole screen is slow to draw every frame *)
                  let x0 = ref w and y0 = ref h and x1 = ref (-1) and y1 = ref (-1) in
                  for y = 0 to h - 1 do
                    for x = 0 to w - 1 do
                      if px x y then begin
                        x0 := min !x0 x;
                        y0 := min !y0 y;
                        x1 := max !x1 x;
                        y1 := max !y1 y
                      end
                    done
                  done;
                  if !x1 < 0 then None
                  else begin
                    let iw = !x1 - !x0 + 1 and ih = !y1 - !y0 + 1 in
                    let img = Rgba_image.create ~width:iw ~height:ih in
                    for y = 0 to ih - 1 do
                      for x = 0 to iw - 1 do
                        if px (!x0 + x) (!y0 + y) then Bigarray.Array1.set img.rgba ((((y * iw) + x) * 4) + 3) 255
                      done
                    done;
                    Some ({ x = float_of_int !x0; y = float_of_int !y0; w = float_of_int iw; h = float_of_int ih }, img)
                  end
              | None -> None)
          | None -> None
        in
        { m with display = Some (changes, picture) }
  in
  { m with keys = computer.keyboard.keys; left = computer.mouse.mdown; right = computer.mouse.mrdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let desktop = rgb 150 150 150

(* a rectangle of the Smalltalk screen, as a shape *)
let box (s : float) (color : color) (r : rect) : shape =
  rectangle color (r.w *. s) (r.h *. s) |> move ((r.x +. (r.w /. 2.) -. (screen_w /. 2.)) *. s) (((screen_h /. 2.) -. r.y -. (r.h /. 2.)) *. s)

let border (s : float) (r : rect) : shape list =
  [
    box s black { r with h = 1. };
    box s black { r with y = r.y +. r.h -. 1.; h = 1. };
    box s black { r with w = 1. };
    box s black { r with x = r.x +. r.w -. 1.; w = 1. };
  ]

(* a character in its cell, whose top left is (x, y) *)
let glyph (s : float) (color : color) (x : float) (y : float) (c : char) : shape list =
  if c = ' ' || c = '\t' then []
  else [ words color (String.make 1 c) |> scale (s *. 1.1) |> move ((x +. (cw /. 2.) -. (screen_w /. 2.)) *. s) (((screen_h /. 2.) -. y -. (lh /. 2.)) *. s) ]

(* a string on one line, cut at [cols] characters *)
let label (s : float) (color : color) (x : float) (y : float) ?(cols = 1000) (str : string) : shape list =
  List.concat (List.init (min cols (String.length str)) (fun i -> glyph s color (x +. (float_of_int i *. cw)) y str.[i]))

let draw_text (s : float) (r : rect) (t : text) ~(focused : bool) : shape list =
  let cols = int_of_float ((r.w -. 6.) /. cw) in
  let a, b = selection t in
  let shapes = ref [] in
  let add l = shapes := l @ !shapes in
  let row = ref 0 and i = ref (match nth_line t.s t.top with Some i -> i | None -> String.length t.s) in
  let n = String.length t.s and maxrow = rows r in
  while !row < maxrow && !i <= n do
    let ls = !i in
    let le = line_end t.s ls in
    let y = r.y +. 2. +. (float_of_int !row *. lh) in
    let col = ref 0 in
    for j = ls to le - 1 do
      let c = t.s.[j] in
      let w = if c = '\t' then (!col / 4 * 4) + 4 - !col else 1 in
      if !col + w <= cols then begin
        let x = r.x +. 3. +. (float_of_int !col *. cw) in
        if j >= a && j < b then begin
          add [ box s black { x; y; w = float_of_int w *. cw; h = lh } ];
          add (glyph s white x y c)
        end
        else add (glyph s black x y c)
      end;
      col := !col + w
    done;
    (* a selected end of line, a cell of it *)
    if le >= a && le < b && le < n && !col < cols then add [ box s black { x = r.x +. 3. +. (float_of_int !col *. cw); y; w = cw; h = lh } ];
    if focused && a = b && t.caret >= ls && t.caret <= le then begin
      let x = r.x +. 3. +. (float_of_int (column t.s t.caret) *. cw) in
      add [ box s black { x = x -. 1.; y = y +. 1.; w = 1.5; h = lh -. 2. } ]
    end;
    incr row;
    i := le + 1
  done;
  (box s white r :: border s r) @ List.rev !shapes

let draw_list (s : float) (r : rect) (items : string list) (sel : int option) (top : int) : shape list =
  let cols = int_of_float ((r.w -. 6.) /. cw) in
  let n = rows r in
  (box s white r :: border s r)
  @ List.concat
      (List.mapi
         (fun k item ->
           let i = k + top in
           if k >= n then []
           else
             let y = r.y +. 2. +. (float_of_int k *. lh) in
             if sel = Some i then box s black { x = r.x +. 1.; y; w = r.w -. 2.; h = lh } :: label s white (r.x +. 3.) y ~cols item
             else label s black (r.x +. 3.) y ~cols item)
         (List.filteri (fun i _ -> i >= top) items))

let draw_button (s : float) (r : rect) (l : string) (on : bool) : shape list =
  let fg, bg = if on then (white, black) else (black, white) in
  let x = r.x +. ((r.w -. (float_of_int (String.length l) *. cw)) /. 2.) in
  (box s bg r :: border s r) @ label s fg x (r.y +. ((r.h -. lh) /. 2.)) l

let draw_window (s : float) (m : model) (w : window) ~(front : bool) : shape list =
  let tr = title_rect w in
  let fg, bg = if front then (white, black) else (black, white) in
  let title = (box s bg tr :: border s tr) @ label s fg (tr.x +. 6.) (tr.y +. 1.) w.title in
  if w.collapsed then title
  else
    let body =
      List.concat_map
        (function
          | P_text (tid, r) -> draw_text s r (get_text w tid) ~focused:(m.focus = Some (w.id, tid))
          | P_list (k, r, items, sel) -> draw_list s r items sel (list_top w k)
          | P_button (r, l, on, _) -> draw_button s r l on)
        (panes m w)
    in
    title @ (box s white w.frame :: body) @ border s { x = w.frame.x -. 1.; y = w.frame.y -. 1.; w = w.frame.w +. 2.; h = w.frame.h +. 2. }

let draw_menu (s : float) (menu : menu) (p : float * float) : shape list =
  let r = menu_rect menu in
  let hover = if inside r p then Some (int_of_float ((snd p -. r.y -. 3.) /. lh)) else None in
  (box s white r :: border s r)
  @ List.concat
      (List.mapi
         (fun i (l, _) ->
           let y = r.y +. 3. +. (float_of_int i *. lh) in
           if hover = Some i then box s black { x = r.x +. 1.; y; w = r.w -. 2.; h = lh } :: label s white (r.x +. 8.) y l
           else label s black (r.x +. 8.) y l)
         menu.items)

let view (computer : computer) (m : model) : shape list =
  let s = scale_of computer in
  let screen = { x = 0.; y = 0.; w = screen_w; h = screen_h } in
  let n = List.length m.windows in
  let windows = List.concat (List.mapi (fun i w -> draw_window s m w ~front:(i = n - 1)) m.windows) in
  let display =
    match m.display with
    | Some (_, Some (r, img)) ->
        [ bitmap (r.w *. s) (r.h *. s) img |> move ((r.x +. (r.w /. 2.) -. (screen_w /. 2.)) *. s) (((screen_h /. 2.) -. r.y -. (r.h /. 2.)) *. s) ]
    | _ -> []
  in
  let busy = if m.running <> [] then label s black 4. 2. "running... (Control-C interrupts)" else [] in
  let menu = match m.menu with Some menu -> draw_menu s menu (mouse_point computer) | None -> [] in
  (rectangle black computer.screen.width computer.screen.height :: box s desktop screen :: windows) @ display @ busy @ menu

let app (caps : caps) = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> caps)))
