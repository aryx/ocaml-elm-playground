(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tui_turbo.mli *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type action =
  | Open | New | Save | Save_as | Exit
  | Find | Find_again | Goto_line
  | Run | Go_to_cursor | Trace_into | Step_over | Reset | User_screen
  | Compile | Pcode_listing
  | Call_stack | Add_watch | Toggle_breakpoint | Clear_watches
  | Keys | About

(* a menu's item: its label, the letter that chooses it, its key *)
type item = { label : string; hot : char; shortcut : string; action : action }

let item label hot shortcut action = { label; hot; shortcut; action }

let menus : (string * item list) list =
  [ ( "File",
      [ item "Open..." 'O' "F3" Open; item "New" 'N' "" New; item "Save" 'S' "F2" Save; item "Save as..." 'a' "" Save_as;
        item "Exit" 'x' "Alt+X" Exit ] );
    ("Search", [ item "Find..." 'F' "" Find; item "Search again" 'S' "Ctrl+L" Find_again; item "Go to line number..." 'G' "" Goto_line ]);
    ( "Run",
      [ item "Run" 'R' "Ctrl+F9" Run; item "Step over" 'S' "F8" Step_over; item "Trace into" 'T' "F7" Trace_into;
        item "Go to cursor" 'G' "F4" Go_to_cursor; item "Program reset" 'P' "Ctrl+F2" Reset; item "User screen" 'U' "Alt+F5" User_screen ] );
    ("Compile", [ item "Compile" 'C' "Alt+F9" Compile; item "Make" 'M' "F9" Compile; item "P-code" 'P' "" Pcode_listing ]);
    ( "Debug",
      [ item "Call stack" 'C' "Ctrl+F3" Call_stack; item "Add watch..." 'W' "Ctrl+F7" Add_watch;
        item "Toggle breakpoint" 'B' "Ctrl+F8" Toggle_breakpoint; item "Remove all watches" 'R' "" Clear_watches ] );
    ("Help", [ item "Keys" 'K' "F1" Keys; item "About..." 'A' "" About ]) ]

type purpose = Saving_as | Finding | Going_to | Watching

type mode =
  | Editing
  | Menu of int * int (* the bar's menu open, the item selected *)
  | Open_dialog of int (* the file selected *)
  | Input of { title : string; label : string; text : string; purpose : purpose }
  | Info of string * string list (* a box: its title and lines; a key closes it *)
  | Executing (* the session's machine running towards its goal *)
  | Finished of Vt.t * (int * string) option (* the user screen, and a run-time error's line and message *)
  | Showing of Vt.t (* the user screen again: Alt-F5 *)
  | Listing of int (* the P-code, from this instruction *)
  | Stack (* the call stack's window *)

(* A program started: run, stepped, paused at its execution bar. Its
   machine is Pmachine's, driven a slice a frame towards its goal
   (Pdebug.step), its output on the user screen, which the IDE shows
   only when the program writes or reads (Turbo Pascal's "smart" swap:
   a step that prints nothing doesn't flash the screen) *)
type session = {
  program : Pcode.program;
  machine : Pmachine.machine;
  user : Vt.t;
  typing : string option; (* a line the program reads, being typed *)
  goal : Pdebug.step option; (* None: paused *)
  pause : Pmachine.machine -> bool; (* the goal's predicate, made when the step began *)
  seed : Lehmer.t; (* random(n)'s *)
  swapped : bool; (* the user screen shown *)
}
type model = {
  lines : string array; (* never changed in place *)
  row : int;
  col : int;
  top : int; (* the first line in the window, and the first column *)
  left : int;
  file : string;
  modified : bool;
  overwrite : bool;
  disk : (string * string) list;
  mode : mode;
  error : string option; (* the red bar *)
  compiled : Pcode.program option; (* the text's code, while the text is unchanged *)
  last_screen : Vt.t option;
  search : string;
  runs : int; (* each run's seed: another game *)
  session : session option;
  breakpoints : int list; (* lines, from 1 *)
  watches : string list;
  quit : bool;
}

(* the Watches window's height, when there are watches: at the bottom,
   the edit window above it *)
let watch_rows (m : model) : int = if m.watches = [] then 0 else min 8 (List.length m.watches + 2)

(* the window's text: 20 lines of 78 columns inside its frame, less
   the watches' *)
let text_rows (m : model) = 20 - watch_rows m
let text_cols = 78
let noname = "NONAME00.PAS"

let line (m : model) (r : int) : string = m.lines.(r)
let nlines (m : model) : int = Array.length m.lines
let text (m : model) : string = String.concat "\n" (Array.to_list m.lines) ^ "\n"

(* the window follows the cursor *)
let follow (m : model) : model =
  let row = max 0 (min (nlines m - 1) m.row) in
  let col = max 0 m.col in
  let top = if row < m.top then row else if row >= m.top + text_rows m then row - text_rows m + 1 else m.top in
  let left = if col < m.left then col else if col >= m.left + text_cols then col - text_cols + 1 else m.left in
  { m with row; col; top; left }

let load (m : model) (file : string) : model =
  let content = Option.value (List.assoc_opt file m.disk) ~default:"" in
  let content = if content <> "" && content.[String.length content - 1] = '\n' then String.sub content 0 (String.length content - 1) else content in
  { m with lines = Array.of_list (String.split_on_char '\n' content); file; row = 0; col = 0; top = 0; left = 0; modified = false;
    compiled = None; error = None; session = None; breakpoints = [] }

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

let set_line (m : model) (r : int) (s : string) : model =
  { m with lines = Array.mapi (fun i l -> if i = r then s else l) m.lines; modified = true; compiled = None }

let set_lines (m : model) (ls : string list) : model = { m with lines = Array.of_list (if ls = [] then [ "" ] else ls); modified = true; compiled = None }

(* the cursor may be past a line's end (Turbo's was): spaces fill the
   gap when something is typed there *)
let padded (m : model) : string =
  let s = line m m.row in
  if m.col > String.length s then s ^ String.make (m.col - String.length s) ' ' else s

let type_char (m : model) (c : string) : model =
  let s = padded m in
  let rest = String.sub s m.col (String.length s - m.col) in
  let rest = if m.overwrite && rest <> "" then String.sub rest 1 (String.length rest - 1) else rest in
  { (set_line m m.row (String.sub s 0 m.col ^ c ^ rest)) with col = m.col + 1 }

let indentation (s : string) : int =
  let rec go i = if i < String.length s && s.[i] = ' ' then go (i + 1) else i in
  go 0

(* Enter: the line cut in two, the new one indented as this one *)
let newline (m : model) : model =
  let s = padded m in
  let head = String.sub s 0 m.col and tail = String.sub s m.col (String.length s - m.col) in
  let indent = if String.trim head = "" then 0 else indentation head in
  let ls = Array.to_list m.lines in
  let before = List.filteri (fun i _ -> i < m.row) ls and after = List.filteri (fun i _ -> i > m.row) ls in
  { (set_lines m (before @ [ head; String.make indent ' ' ^ String.trim tail ] @ after)) with row = m.row + 1; col = indent }

let join_next (m : model) : model =
  if m.row + 1 >= nlines m then m
  else
    let ls = Array.to_list m.lines in
    let joined = padded m ^ line m (m.row + 1) in
    set_lines m (List.filteri (fun i _ -> i <> m.row + 1) (List.mapi (fun i l -> if i = m.row then joined else l) ls))

let backspace (m : model) : model =
  if m.col > 0 then
    let s = line m m.row in
    if m.col > String.length s then { m with col = m.col - 1 }
    else { (set_line m m.row (String.sub s 0 (m.col - 1) ^ String.sub s m.col (String.length s - m.col))) with col = m.col - 1 }
  else if m.row > 0 then
    let col = String.length (line m (m.row - 1)) in
    join_next { m with row = m.row - 1; col }
  else m

let delete (m : model) : model =
  let s = line m m.row in
  if m.col < String.length s then set_line m m.row (String.sub s 0 m.col ^ String.sub s (m.col + 1) (String.length s - m.col - 1)) else join_next m

let delete_line (m : model) : model =
  let ls = List.filteri (fun i _ -> i <> m.row) (Array.to_list m.lines) in
  { (set_lines m ls) with col = 0 }

let is_word (c : char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

(* Ctrl-F and Ctrl-A: the next word's start, the previous one's *)
let word_right (m : model) : model =
  let s = line m m.row in
  let rec skip i p = if i < String.length s && p s.[i] then skip (i + 1) p else i in
  if m.col >= String.length s then if m.row + 1 < nlines m then { m with row = m.row + 1; col = 0 } else m
  else { m with col = skip (skip m.col is_word) (fun c -> not (is_word c)) }

let word_left (m : model) : model =
  let s = line m m.row in
  let col = min m.col (String.length s) in
  let rec back i p = if i > 0 && p s.[i - 1] then back (i - 1) p else i in
  if col = 0 then if m.row > 0 then { m with row = m.row - 1; col = String.length (line m (m.row - 1)) } else m
  else { m with col = back (back col (fun c -> not (is_word c))) is_word }

let find (m : model) (pat : string) : model =
  let n = nlines m in
  let at r from =
    let s = line m r in
    let k = String.length pat in
    let rec go i = if i + k > String.length s then None else if String.lowercase_ascii (String.sub s i k) = String.lowercase_ascii pat then Some i else go (i + 1) in
    if pat = "" then None else go from
  in
  let rec scan k = if k >= n then None else let r = m.row + k in if r >= n then None else match at r (if k = 0 then m.col + 1 else 0) with Some c -> Some (r, c) | None -> scan (k + 1) in
  match scan 0 with
  | Some (r, c) -> { m with row = r; col = c; search = pat }
  | None -> { m with search = pat; mode = Info ("Information", [ "Search string not found." ]) }

let edit_key (m : model) (k : string) : model =
  match k with
  | "\x1b[A" | "\x05" -> { m with row = m.row - 1 }
  | "\x1b[B" | "\x18" -> { m with row = m.row + 1 }
  | "\x1b[D" | "\x13" -> if m.col > 0 then { m with col = m.col - 1 } else m
  | "\x1b[C" | "\x04" -> { m with col = m.col + 1 }
  | "\x01" -> word_left m
  | "\x06" -> word_right m
  | "\x1b[H" -> { m with col = 0 }
  | "\x1b[F" -> { m with col = String.length (line m m.row) }
  | "\x1b[5~" | "\x12" -> { m with row = m.row - text_rows m + 1; top = max 0 (m.top - text_rows m + 1) }
  | "\x1b[6~" | "\x03" ->
      { m with row = min (nlines m - 1) (m.row + text_rows m - 1); top = min (max 0 (nlines m - text_rows m)) (m.top + text_rows m - 1) }
  | "\x1b[2~" | "\x16" -> { m with overwrite = not m.overwrite }
  | "\r" -> newline m
  | "\x7f" | "\b" -> backspace m
  | "\x1b[3~" | "\x07" -> delete m
  | "\x19" -> delete_line m
  | "\t" -> List.fold_left type_char m [ " "; " " ]
  | "\x0c" -> find m m.search
  | _ when String.length k = 1 && k.[0] >= ' ' && k.[0] < '\x7f' -> type_char m k
  | _ -> m

(*****************************************************************************)
(* Compiling and running *)
(*****************************************************************************)

(* the text compiled: its code, or the red bar and the cursor on the
   error *)
let compile (m : model) : (Pcode.program * model, model) result =
  match Pascal_compile.compile (text m) with
  | Ok p -> Ok (p, { m with compiled = Some p; error = None })
  | Error e -> Error { m with error = Some ("Error: " ^ e.message ^ "."); row = e.line - 1; col = e.col - 1; mode = Editing }

let compiled_box (m : model) (p : Pcode.program) : model =
  { m with
    mode =
      Info
        ( "Compiling",
          [ "Main file: " ^ m.file; ""; "Done."; ""; Printf.sprintf "Lines compiled: %d" (nlines m);
            Printf.sprintf "P-code: %d instructions" (Array.length p.code) ] ) }

(*****************************************************************************)
(* The debugger *)
(*****************************************************************************)

let note = "\r\n\x1b[7m Press any key to return to Turbo Pascal \x1b[0m"

(* the program compiled and started, paused before its first
   instruction; its user screen blank *)
let start (m : model) : (session * model, model) result =
  match compile m with
  | Error m -> Error m
  | Ok (program, m) ->
      let machine = Pmachine.start program in
      Ok
        ( { program; machine; user = Vt.create ~rows:24 ~cols:80; typing = None; goal = None; pause = (fun _ -> false);
            seed = Lehmer.of_int (m.runs + 1); swapped = false },
          { m with runs = m.runs + 1 } )

(* the execution bar's line, and the cursor put on it *)
let paused_at (m : model) (s : session) : model =
  { m with session = Some { s with goal = None; swapped = false; typing = None }; mode = Editing; row = Pdebug.line s.program s.machine - 1; col = 0 }

(* the machine run towards its goal, a slice at most: paused, done,
   reading, or still going (the next frame goes on) *)
let rec advance (m : model) (s : session) : model =
  match s.goal with
  | None -> paused_at m s
  | Some _ when s.typing <> None -> { m with session = Some s; mode = Executing }
  | Some _ -> (
      (* a hundred thousand instructions a frame: some 4 ms natively,
         Wirth's queens (677,000) in seven frames *)
      let stop = Pmachine.resume ~pause:s.pause s.machine 100_000 in
      let out = Pmachine.output s.machine in
      let s = if out = "" then s else { s with user = Vt.feed s.user (Line_discipline.output out); swapped = true } in
      match stop with
      | Paused -> paused_at m s
      | Slice_over -> { m with session = Some s; mode = Executing }
      | Need_line -> { m with session = Some { s with typing = Some ""; swapped = true }; mode = Executing }
      | Need_random n ->
          let seed = Lehmer.next s.seed in
          Pmachine.give_random s.machine (int_of_float (Lehmer.to_unit seed *. float_of_int n));
          advance m { s with seed }
      | Halted -> { m with session = None; mode = Finished (Vt.feed s.user note, None); last_screen = Some s.user }
      | Failed (code, msg) ->
          let line = s.program.lines.(max 0 (Pmachine.pc s.machine - 1)) in
          let user = Vt.feed s.user (Printf.sprintf "\r\nRuntime error %d at line %d: %s" code line msg) in
          { m with session = None; mode = Finished (Vt.feed user note, Some (line, Printf.sprintf "Runtime error %d: %s." code msg)); last_screen = Some user })

(* a step taken (F7, F8, F4) or a run (Ctrl-F9), the program started
   first if it wasn't *)
let go (m : model) (step : Pdebug.step) : model =
  let started = match m.session with Some s -> Ok (s, m) | None -> start m in
  match started with
  | Error m -> m
  | Ok (s, m) -> advance m { s with goal = Some step; pause = Pdebug.pause_for s.program step s.machine; swapped = false }

(* a key while the program runs: the line it reads typed on the user
   screen, and Control-C, Turbo's Ctrl-Break, pausing it where it is *)
let executing_key (m : model) (s : session) (k : string) : model =
  match (k, s.typing) with
  | "\x03", _ -> paused_at m s
  | "\r", Some line ->
      Pmachine.give_line s.machine line;
      advance m { s with typing = None; user = Vt.feed s.user "\r\n" }
  | ("\x7f" | "\b"), Some line when line <> "" ->
      { m with session = Some { s with typing = Some (String.sub line 0 (String.length line - 1)); user = Vt.feed s.user "\b \b" } }
  | _, Some line when String.length k = 1 && k.[0] >= ' ' -> { m with session = Some { s with typing = Some (line ^ k); user = Vt.feed s.user k } }
  | _ -> m

(* the word under the cursor: what Ctrl-F7 offers to watch *)
let word_at (m : model) : string =
  let s = line m m.row in
  let rec back i = if i > 0 && is_word s.[i - 1] then back (i - 1) else i in
  let rec forth i = if i < String.length s && is_word s.[i] then forth (i + 1) else i in
  let c = min m.col (String.length s) in
  let a = back c and b = forth c in
  String.sub s a (b - a)

(*****************************************************************************)
(* Menus and dialogs *)
(*****************************************************************************)

let files (m : model) : string list = List.sort compare (List.map fst m.disk)

let write (m : model) (file : string) : model =
  { m with disk = (file, text m) :: List.remove_assoc file m.disk; file; modified = false }

let act (m : model) (a : action) : model =
  let m = { m with mode = Editing } in
  match a with
  | Open -> { m with mode = Open_dialog 0 }
  | New -> { (load m "") with lines = [| "" |]; file = noname }
  | Save -> if m.file = noname then { m with mode = Input { title = "Save File As"; label = "Save file as"; text = ""; purpose = Saving_as } } else write m m.file
  | Save_as -> { m with mode = Input { title = "Save File As"; label = "Save file as"; text = ""; purpose = Saving_as } }
  | Exit -> { m with quit = true }
  | Find -> { m with mode = Input { title = "Find"; label = "Text to find"; text = m.search; purpose = Finding } }
  | Find_again -> find m m.search
  | Goto_line -> { m with mode = Input { title = "Go to Line Number"; label = "Enter new line number"; text = ""; purpose = Going_to } }
  | Run -> go m (Continue m.breakpoints)
  | Trace_into -> go m Trace_into
  | Step_over -> go m Step_over
  | Go_to_cursor -> go m (To_line (m.row + 1))
  | Reset -> { m with session = None }
  | User_screen -> (
      match (m.session, m.last_screen) with
      | Some s, _ -> { m with mode = Showing s.user }
      | None, Some vt -> { m with mode = Showing vt }
      | None, None -> { m with mode = Showing (Vt.create ~rows:24 ~cols:80) })
  | Call_stack -> if m.session = None then { m with mode = Info ("Call Stack", [ "No program is running:"; "F7 or F8 starts one." ]) } else { m with mode = Stack }
  | Add_watch -> { m with mode = Input { title = "Add Watch"; label = "Watch expression"; text = word_at m; purpose = Watching } }
  | Toggle_breakpoint ->
      let l = m.row + 1 in
      { m with breakpoints = (if List.mem l m.breakpoints then List.filter (( <> ) l) m.breakpoints else l :: m.breakpoints) }
  | Clear_watches -> { m with watches = [] }
  | Compile -> ( match compile m with Ok (p, m) -> compiled_box m p | Error m -> m)
  | Pcode_listing -> (
      match compile m with
      | Error m -> m
      | Ok (p, m) ->
          (* from the first instruction of the cursor's line *)
          let first = ref 0 in
          (try Array.iteri (fun i l -> if l >= m.row + 1 then (first := i; raise Exit)) p.lines with Exit -> ());
          { m with mode = Listing (max 0 (!first - 3)) })
  | Keys ->
      { m with
        mode =
          Info
            ( "Keys",
              [ "F9 Make   Alt+F9 Compile   Ctrl+F9 Run"; "Alt+F5 User screen   F2 Save   F3 Open"; "F10 or Alt+letter: the menus   Alt+X Exit"; "";
                "F7 Trace into   F8 Step over   F4 Go to cursor"; "Ctrl+F8 Breakpoint   Ctrl+F7 Watch   Ctrl+F3 Calls"; "Ctrl+F2 Reset   Ctrl+C Break"; "";
                "Arrows, or Ctrl+E X S D    Ctrl+A F words"; "Ctrl+Y delete a line    Insert: overwrite"; "Ctrl+L search again" ] ) }
  | About ->
      { m with
        mode =
          Info
            ( "About",
              [ "Tiny Turbo Pascal"; ""; "after Turbo Pascal 7.0"; "Anders Hejlsberg, Borland, 1983-1992"; "";
                "compiled to P-code, as Wirth's Pascal-P"; "and UCSD Pascal did" ] ) }

(* Alt and a letter, as xterm sends it: Escape then the letter *)
let alt_letter (k : string) : char option = if String.length k = 2 && k.[0] = '\x1b' then Some (Char.lowercase_ascii k.[1]) else None

let menu_of_letter (c : char) : int option =
  let rec go i = function [] -> None | (title, _) :: rest -> if Char.lowercase_ascii title.[0] = c then Some i else go (i + 1) rest in
  go 0 menus

let menu_key (m : model) (bar : int) (sel : int) (k : string) : model =
  let items = snd (List.nth menus bar) in
  let n = List.length items and nbar = List.length menus in
  match k with
  | "\x1b" -> { m with mode = Editing }
  | "\x1b[D" -> { m with mode = Menu ((bar + nbar - 1) mod nbar, 0) }
  | "\x1b[C" -> { m with mode = Menu ((bar + 1) mod nbar, 0) }
  | "\x1b[A" -> { m with mode = Menu (bar, (sel + n - 1) mod n) }
  | "\x1b[B" -> { m with mode = Menu (bar, (sel + 1) mod n) }
  | "\r" -> act m (List.nth items sel).action
  | _ -> (
      match alt_letter k with
      | Some c -> ( match menu_of_letter c with Some b -> { m with mode = Menu (b, 0) } | None -> m)
      | None -> (
          let c = if String.length k = 1 then Some (Char.lowercase_ascii k.[0]) else None in
          match List.find_opt (fun it -> Some (Char.lowercase_ascii it.hot) = c) items with Some it -> act m it.action | None -> m))

let input_key (m : model) (title, label, text, purpose) (k : string) : model =
  let again text = { m with mode = Input { title; label; text; purpose } } in
  match k with
  | "\x1b" -> { m with mode = Editing }
  | "\r" -> (
      let m = { m with mode = Editing } in
      match purpose with
      | Saving_as -> if text = "" then m else write m (String.uppercase_ascii text)
      | Finding -> find m text
      | Going_to -> ( match int_of_string_opt (String.trim text) with Some n -> { m with row = n - 1; col = 0 } | None -> m)
      | Watching -> if String.trim text = "" then m else { m with watches = m.watches @ [ String.trim text ] })
  | "\x7f" | "\b" -> again (if text = "" then "" else String.sub text 0 (String.length text - 1))
  | _ when String.length k = 1 && k.[0] >= ' ' -> again (text ^ k)
  | _ -> m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let key (m : model) (k : string) : model =
  match m.mode with
  | Executing -> ( match m.session with Some s -> executing_key m s k | None -> { m with mode = Editing })
  | Finished (_, err) -> (
      match err with
      | Some (l, msg) -> { m with mode = Editing; error = Some msg; row = l - 1; col = 0 }
      | None -> { m with mode = Editing })
  | Showing _ | Info _ | Stack -> { m with mode = Editing }
  | Listing first -> (
      match k with
      | "\x1b[A" -> { m with mode = Listing (max 0 (first - 1)) }
      | "\x1b[B" -> { m with mode = Listing (first + 1) }
      | "\x1b[5~" -> { m with mode = Listing (max 0 (first - 15)) }
      | "\x1b[6~" -> { m with mode = Listing (first + 15) }
      | _ -> { m with mode = Editing })
  | Menu (bar, sel) -> menu_key m bar sel k
  | Open_dialog sel -> (
      let fs = files m in
      match k with
      | "\x1b[A" -> { m with mode = Open_dialog (max 0 (sel - 1)) }
      | "\x1b[B" -> { m with mode = Open_dialog (min (List.length fs - 1) (sel + 1)) }
      | "\r" -> load { m with mode = Editing } (List.nth fs sel)
      | _ -> { m with mode = Editing })
  | Input { title; label; text; purpose } -> input_key m (title, label, text, purpose) k
  | Editing -> (
      (* a key clears the red bar, as in Turbo Pascal *)
      let m = { m with error = None } in
      match k with
      | "\x1bOP" -> act m Keys
      | "\x1bOQ" -> act m Save
      | "\x1bOR" -> act m Open
      | "\x1b[20~" | "\x1b[20;3~" -> act m Compile
      | "\x1b[20;5~" -> act m Run
      | "\x1b[15;3~" -> act m User_screen
      | "\x1b[21~" -> { m with mode = Menu (0, 0) }
      (* the debugger's: F7 F8 F4, Ctrl-F2 Ctrl-F3 Ctrl-F7 Ctrl-F8 *)
      | "\x1b[18~" -> act m Trace_into
      | "\x1b[19~" -> act m Step_over
      | "\x1bOS" -> act m Go_to_cursor
      | "\x1b[1;5Q" -> act m Reset
      | "\x1b[1;5R" -> act m Call_stack
      | "\x1b[18;5~" -> act m Add_watch
      | "\x1b[19;5~" -> act m Toggle_breakpoint
      | _ -> (
          match alt_letter k with
          | Some 'x' -> act m Exit
          | Some c -> ( match menu_of_letter c with Some b -> { m with mode = Menu (b, 0) } | None -> m)
          | None ->
              (* the text changed: the program running is another's,
                 reset (Turbo Pascal asked first) *)
              let m' = edit_key m k in
              if m'.lines != m.lines then { m' with session = None } else m'))

let update (ev : Tui.event) (m : model) : model =
  match (ev, m.mode) with
  | Tick _, Executing -> ( match m.session with Some s -> advance m s | None -> { m with mode = Editing })
  | Tick _, _ -> m
  | Key k, _ -> follow (key m k)

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let attrs ?(bold = false) fg bg : Vt.attrs = { Vt.plain with fg; bg; bold }
let grey = attrs Vt.Black Vt.White
let grey_hot = attrs Vt.Red Vt.White
let chosen = attrs Vt.Black Vt.Green
let chosen_hot = attrs Vt.Red Vt.Green
let frame_attrs = attrs ~bold:true Vt.White Vt.Blue
let text_attrs = attrs ~bold:true Vt.Yellow Vt.Blue
let reserved_attrs = attrs ~bold:true Vt.White Vt.Blue
let comment_attrs = attrs Vt.White Vt.Blue
let literal_attrs = attrs ~bold:true Vt.Cyan Vt.Blue
let error_attrs = attrs ~bold:true Vt.White Vt.Red
let dialog_frame = attrs ~bold:true Vt.White Vt.White

let fill (top : int) (left : int) (h : int) (w : int) (a : Vt.attrs) (s : Curses.t) : Curses.t =
  let row = String.make w ' ' in
  let s = ref s in
  for r = top to top + h - 1 do
    s := Curses.put ~attrs:a r left row !s
  done;
  !s

(* a frame of the PC's box characters, its title centred in its top *)
let frame ?(double = true) ?(title = "") (top : int) (left : int) (h : int) (w : int) (a : Vt.attrs) (s : Curses.t) : Curses.t =
  let hz, vt, tl, tr, bl, br = if double then ("═", "║", "╔", "╗", "╚", "╝") else ("─", "│", "┌", "┐", "└", "┘") in
  let bar = String.concat "" (List.init (w - 2) (fun _ -> hz)) in
  let s = Curses.put ~attrs:a top left (tl ^ bar ^ tr) s in
  let s = Curses.put ~attrs:a (top + h - 1) left (bl ^ bar ^ br) s in
  let s = ref s in
  for r = top + 1 to top + h - 2 do
    s := Curses.put ~attrs:a r left vt !s;
    s := Curses.put ~attrs:a r (left + w - 1) vt !s
  done;
  if title = "" then !s else Curses.put ~attrs:a top (left + ((w - String.length title - 2) / 2)) (" " ^ title ^ " ") !s

(* the shadow a window throws, two columns right and a row down: what
   is under it, dark grey on black *)
let shadow (top : int) (left : int) (h : int) (w : int) (s : Curses.t) : Curses.t =
  let dark = attrs ~bold:true Vt.Black Vt.Black in
  let shade s r c = if r < Curses.rows s && c < Curses.cols s then Curses.put ~attrs:dark r c (Curses.cell s r c).glyph s else s in
  let s = ref s in
  for r = top + 1 to top + h do
    s := shade (shade !s r (left + w)) r (left + w + 1)
  done;
  for c = left + 2 to left + w - 1 do
    s := shade !s (top + h) c
  done;
  !s

(* a label with its hot letter in another colour *)
let hot_label (r : int) (c : int) (label : string) (hot : char) (a : Vt.attrs) (ha : Vt.attrs) (s : Curses.t) : Curses.t =
  let s = Curses.put ~attrs:a r c label s in
  match String.index_opt label hot with Some i -> Curses.put ~attrs:ha r (c + i) (String.make 1 hot) s | None -> s

(* the columns of the menu bar's titles *)
let bar_columns : int list =
  let rec go c = function [] -> [] | (t, _) :: rest -> c :: go (c + String.length t + 2) rest in
  go 2 menus

let menu_bar (open_ : int option) (s : Curses.t) : Curses.t =
  let s = fill 0 0 1 80 grey s in
  List.fold_left2
    (fun s (i, (title, _)) c ->
      let a, ha = if open_ = Some i then (chosen, chosen_hot) else (grey, grey_hot) in
      hot_label 0 (c - 1) (" " ^ title ^ " ") title.[0] a ha s)
    s
    (List.mapi (fun i menu -> (i, menu)) menus)
    bar_columns

let status_line (m : model) (s : Curses.t) : Curses.t =
  let s = fill 23 0 1 80 grey s in
  let keys =
    match (m.session, m.mode) with
    | Some _, Executing -> [ ("", "Running..."); ("Ctrl+C", "Break") ]
    | Some _, _ -> [ ("F7", "Trace"); ("F8", "Step"); ("F4", "Here"); ("Ctrl+F9", "Run"); ("Ctrl+F2", "Reset"); ("Ctrl+F7", "Watch") ]
    | None, _ -> [ ("F1", "Help"); ("F2", "Save"); ("F3", "Open"); ("Alt+F9", "Compile"); ("F9", "Make"); ("Ctrl+F9", "Run"); ("F10", "Menu") ]
  in
  fst
    (List.fold_left
       (fun (s, c) (k, what) ->
         let s = Curses.put ~attrs:grey_hot 23 c k s in
         let s = Curses.put ~attrs:grey 23 (c + String.length k + 1) what s in
         (s, c + String.length k + String.length what + 3))
       (s, 1) keys)

(* a line cut into its colours: reserved words, comments, strings and
   numbers. [comment] is the closing of a comment running on from the
   line before ('}', or ')' for a star-parenthesis), and the same is
   returned for the next line *)
let colour_line (l : string) (comment : char option) : (int * string * Vt.attrs) list * char option =
  let n = String.length l in
  let out = ref [] in
  let add start stop a = if stop > start then out := (start, String.sub l start (stop - start), a) :: !out in
  (* where a comment closed by [close] ends, searched from [from] *)
  let rec ending close from =
    if from >= n then None
    else if close = '}' && l.[from] = '}' then Some (from + 1)
    else if close = ')' && l.[from] = '*' && from + 1 < n && l.[from + 1] = ')' then Some (from + 2)
    else ending close (from + 1)
  in
  let rec go i comment =
    if i >= n then comment
    else
      match comment with
      | Some close -> in_comment i close i
      | None ->
          let c = l.[i] in
          if c = '{' then in_comment i '}' (i + 1)
          else if c = '(' && i + 1 < n && l.[i + 1] = '*' then in_comment i ')' (i + 2)
          else if c = '\'' then begin
            let rec close j = if j >= n then n else if l.[j] = '\'' then j + 1 else close (j + 1) in
            let stop = close (i + 1) in
            add i stop literal_attrs;
            go stop None
          end
          else if is_word c then begin
            let rec stop j = if j < n && is_word l.[j] then stop (j + 1) else j in
            let j = stop i in
            let w = String.lowercase_ascii (String.sub l i (j - i)) in
            add i j (if List.mem w Pascal_lexer.keywords then reserved_attrs else if c >= '0' && c <= '9' then literal_attrs else text_attrs);
            go j None
          end
          else begin
            add i (i + 1) text_attrs;
            go (i + 1) None
          end
  and in_comment start close from =
    match ending close from with
    | Some stop ->
        add start stop comment_attrs;
        go stop None
    | None ->
        add start n comment_attrs;
        Some close
  in
  let next = go 0 comment in
  (List.rev !out, next)

let exec_attrs = attrs Vt.Black Vt.Cyan
let break_attrs = attrs ~bold:true Vt.White Vt.Red

(* the line where a paused program is: the execution bar's *)
let execution_line (m : model) : int option =
  match m.session with Some s when s.goal = None -> Some (Pdebug.line s.program s.machine - 1) | _ -> None

let edit_window (m : model) (s : Curses.t) : Curses.t =
  let h = 22 - watch_rows m in
  let s = fill 1 0 h 80 text_attrs s in
  let s = frame ~title:m.file 1 0 h 80 frame_attrs s in
  let pos = Printf.sprintf " %s%d:%d " (if m.modified then "* " else "") (m.row + 1) (m.col + 1) in
  let s = Curses.put ~attrs:frame_attrs h 3 pos s in
  (* the comments running into the window from above it *)
  let comment = ref None in
  for r = 0 to m.top - 1 do
    comment := snd (colour_line (line m r) !comment)
  done;
  let s = ref s in
  let bar = execution_line m in
  for i = 0 to text_rows m - 1 do
    let r = m.top + i in
    if r < nlines m then begin
      let pieces, next = colour_line (line m r) !comment in
      comment := next;
      (* the execution bar, or a breakpoint: the whole line in its colour *)
      let whole = if bar = Some r then Some exec_attrs else if List.mem (r + 1) m.breakpoints then Some break_attrs else None in
      let pieces = match whole with Some a -> List.map (fun (st, piece, _) -> (st, piece, a)) pieces | None -> pieces in
      (match whole with Some a -> s := Curses.put ~attrs:a (2 + i) 1 (String.make text_cols ' ') !s | None -> ());
      List.iter
        (fun (start, piece, a) ->
          String.iteri
            (fun k ch ->
              let c = start + k - m.left in
              if c >= 0 && c < text_cols then s := Curses.put ~attrs:a (2 + i) (1 + c) (String.make 1 ch) !s)
            piece)
        pieces
    end
  done;
  (* the error, in a red bar over the window's first line *)
  match m.error with Some e -> Curses.put ~attrs:error_attrs 2 1 (Printf.sprintf " %-77s" e) !s | None -> !s

(* the watches, each with its value in the paused program *)
let watch_window (m : model) (s : Curses.t) : Curses.t =
  let h = watch_rows m in
  if h = 0 then s
  else
    let top = 23 - h in
    let window = attrs Vt.Black Vt.Cyan in
    let s = fill top 0 h 80 window s in
    let s = frame ~double:false ~title:"Watches" top 0 h 80 window s in
    let value w =
      match m.session with
      | Some sess when sess.goal = None -> Pdebug.watch sess.program sess.machine w
      | Some _ -> "(running)"
      | None -> "(no program running: F7 or F8 starts it)"
    in
    List.fold_left
      (fun s (i, w) -> if i < h - 2 then Curses.put ~attrs:window (top + 1 + i) 2 (let t = w ^ ": " ^ value w in if String.length t > 76 then String.sub t 0 76 else t) s else s)
      s
      (List.mapi (fun i w -> (i, w)) m.watches)

let dropdown (bar : int) (sel : int) (s : Curses.t) : Curses.t =
  let items = snd (List.nth menus bar) in
  let label_w = List.fold_left (fun w it -> max w (String.length it.label)) 0 items in
  let short_w = List.fold_left (fun w it -> max w (String.length it.shortcut)) 0 items in
  let w = label_w + short_w + 6 and h = List.length items + 2 in
  let left = List.nth bar_columns bar - 1 in
  let s = fill 1 left h w grey s in
  let s = frame ~double:false 1 left h w grey s in
  let s =
    List.fold_left
      (fun s (i, it) ->
        let a, ha = if i = sel then (chosen, chosen_hot) else (grey, grey_hot) in
        let text = Printf.sprintf " %-*s  %*s " label_w it.label short_w it.shortcut in
        hot_label (2 + i) (left + 1) text it.hot a ha s)
      s
      (List.mapi (fun i it -> (i, it)) items)
  in
  shadow 1 left h w s

let dialog (title : string) (h : int) (w : int) (s : Curses.t) : Curses.t * int * int =
  let top = (24 - h) / 2 and left = (80 - w) / 2 in
  let s = fill top left h w grey s in
  let s = frame ~title top left h w dialog_frame s in
  (shadow top left h w s, top, left)

let button (r : int) (c : int) (label : string) (s : Curses.t) : Curses.t = Curses.put ~attrs:chosen r c (" " ^ label ^ " ") s

let vt_screen (vt : Vt.t) : Curses.t =
  let s = ref (Curses.create ~rows:24 ~cols:80) in
  for r = 0 to min 23 (Vt.rows vt - 1) do
    for c = 0 to min 79 (Vt.cols vt - 1) do
      let cell = Vt.cell vt r c in
      if cell.glyph <> " " || cell.attrs <> Vt.plain then s := Curses.put ~attrs:cell.attrs r c cell.glyph !s
    done
  done;
  !s

let listing (m : model) (first : int) (s : Curses.t) : Curses.t =
  let p = match m.compiled with Some p -> p | None -> { Pcode.code = [||]; lines = [||]; statements = [||]; procedures = [||] } in
  let top = 3 and left = 8 and h = 18 and w = 64 in
  let window = attrs Vt.Black Vt.Cyan in
  let s = fill top left h w window s in
  let s = frame ~title:("P-code: " ^ m.file) top left h w (attrs ~bold:true Vt.White Vt.Cyan) s in
  let s = ref (shadow top left h w s) in
  for i = 0 to h - 3 do
    let a = first + i in
    if a < Array.length p.code then begin
      let here = p.lines.(a) = m.row + 1 in
      (* where a paused program is, marked *)
      let at_pc = match m.session with Some sess when sess.goal = None -> Pmachine.pc sess.machine = a | _ -> false in
      let text = Printf.sprintf "%s%4d  %-24s line %d" (if at_pc then ">" else " ") a (Pcode.show p.code.(a)) p.lines.(a) in
      s := Curses.put ~attrs:(if here then attrs ~bold:true Vt.Yellow Vt.Blue else window) (top + 1 + i) (left + 1) (Printf.sprintf "%-*s" (w - 2) text) !s
    end
  done;
  Curses.put ~attrs:(attrs ~bold:true Vt.White Vt.Cyan) (top + h - 1) (left + 2) " the cursor's line highlighted; Esc " !s

(* the call stack: each frame's call, and its links -- the static one
   to where its procedure was declared, the dynamic one to its caller *)
let stack_window (m : model) (s : Curses.t) : Curses.t =
  match m.session with
  | None -> s
  | Some sess ->
      let frames = Pdebug.frames sess.program sess.machine in
      let lines =
        List.map
          (fun (f : Pdebug.frame) ->
            if f.procedure = 0 then Printf.sprintf "%-16s frame %d" (Pdebug.call sess.program sess.machine f) f.base
            else Printf.sprintf "%-16s frame %-5d static link %-5d dynamic link %d" (Pdebug.call sess.program sess.machine f) f.base f.static_link f.dynamic_link)
          frames
      in
      let lines = List.filteri (fun i _ -> i < 12) lines @ [ ""; "static link: the frame of the procedure it is in"; "dynamic link: the frame of its caller" ] in
      let w = 66 in
      let s, top, left = dialog "Call Stack" (List.length lines + 4) w s in
      let s = List.fold_left (fun s (i, l) -> Curses.put ~attrs:grey (top + 2 + i) (left + 3) l s) s (List.mapi (fun i l -> (i, l)) lines) in
      Curses.cursor None s

let view (m : model) : Curses.t =
  let running_user = match (m.mode, m.session) with Executing, Some s when s.swapped -> Some s | _ -> None in
  match (m.mode, running_user) with
  | _, Some s -> Curses.cursor (if s.typing <> None then Some (Vt.cursor s.user) else None) (vt_screen s.user)
  | (Finished (vt, _) | Showing vt), _ -> Curses.cursor None (vt_screen vt)
  | _ -> (
      let s = Curses.create ~rows:24 ~cols:80 in
      let s = edit_window m s in
      let s = watch_window m s in
      let s = status_line m s in
      let menu_open = match m.mode with Menu (b, _) -> Some b | _ -> None in
      let s = menu_bar menu_open s in
      let editing_cursor = Some (2 + m.row - m.top, 1 + m.col - m.left) in
      match m.mode with
      | Menu (bar, sel) -> Curses.cursor None (dropdown bar sel s)
      | Open_dialog sel ->
          let fs = files m in
          let s, top, left = dialog "Open a File" (List.length fs + 5) 36 s in
          let s = Curses.put ~attrs:grey (top + 1) (left + 3) "Files" s in
          let s =
            List.fold_left
              (fun s (i, f) -> Curses.put ~attrs:(if i = sel then attrs ~bold:true Vt.White Vt.Cyan else attrs Vt.Black Vt.Cyan) (top + 2 + i) (left + 3) (Printf.sprintf " %-16s" f) s)
              s
              (List.mapi (fun i f -> (i, f)) fs)
          in
          let s = button (top + 2) (left + 24) " Open " s in
          Curses.cursor None (button (top + 4) (left + 24) "Cancel" s)
      | Input { title; label; text; _ } ->
          let s, top, left = dialog title 8 50 s in
          let s = Curses.put ~attrs:grey (top + 2) (left + 3) label s in
          let s = Curses.put ~attrs:(attrs ~bold:true Vt.White Vt.Blue) (top + 3) (left + 3) (Printf.sprintf " %-42s" text) s in
          let s = button (top + 5) (left + 14) "  OK  " s in
          Curses.cursor (Some (top + 3, left + 4 + String.length text)) (button (top + 5) (left + 26) "Cancel" s)
      | Info (title, lines) ->
          let w = max 32 (4 + List.fold_left (fun w l -> max w (String.length l)) 0 lines) in
          let s, top, left = dialog title (List.length lines + 5) w s in
          let s = List.fold_left (fun s (i, l) -> Curses.put ~attrs:grey (top + 2 + i) (left + ((w - String.length l) / 2)) l s) s (List.mapi (fun i l -> (i, l)) lines) in
          Curses.cursor None (button (top + List.length lines + 3) (left + (w / 2) - 4) "  OK  " s)
      | Listing first -> Curses.cursor None (listing m first s)
      | Stack -> stack_window m s
      | Executing -> Curses.cursor None s
      | _ -> Curses.cursor editing_cursor s)

(*****************************************************************************)
(* The start *)
(*****************************************************************************)

let init : model =
  load
    { lines = [| "" |]; row = 0; col = 0; top = 0; left = 0; file = noname; modified = false; overwrite = false; disk = Pascal_disk.files;
      mode = Editing; error = None; compiled = None; last_screen = None; search = ""; runs = 0; session = None; breakpoints = []; watches = [];
      quit = false }
    "QUEENS.PAS"

let program : model Tui.program = { init; update; view; over = (fun m -> m.quit) }
let lines (m : model) = Array.to_list m.lines
let cursor (m : model) = (m.row, m.col)
let error (m : model) = m.error

let screen (m : model) =
  match m.mode with
  | Editing -> "edit"
  | Menu _ -> "menu"
  | Open_dialog _ | Input _ | Info _ -> "dialog"
  | Executing -> "run"
  | Stack -> "dialog"
  | Finished _ | Showing _ -> "user"
  | Listing _ -> "p-code"

let file (m : model) (name : string) = List.assoc_opt name m.disk
let execution_line = execution_line
