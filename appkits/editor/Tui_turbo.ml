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

type action = Open | New | Save | Save_as | Exit | Find | Find_again | Goto_line | Run | User_screen | Compile | Pcode_listing | Keys | About

(* a menu's item: its label, the letter that chooses it, its key *)
type item = { label : string; hot : char; shortcut : string; action : action }

let item label hot shortcut action = { label; hot; shortcut; action }

let menus : (string * item list) list =
  [ ( "File",
      [ item "Open..." 'O' "F3" Open; item "New" 'N' "" New; item "Save" 'S' "F2" Save; item "Save as..." 'a' "" Save_as;
        item "Exit" 'x' "Alt+X" Exit ] );
    ("Search", [ item "Find..." 'F' "" Find; item "Search again" 'S' "Ctrl+L" Find_again; item "Go to line number..." 'G' "" Goto_line ]);
    ("Run", [ item "Run" 'R' "Ctrl+F9" Run; item "User screen" 'U' "Alt+F5" User_screen ]);
    ("Compile", [ item "Compile" 'C' "Alt+F9" Compile; item "Make" 'M' "F9" Compile; item "P-code" 'P' "" Pcode_listing ]);
    ("Help", [ item "Keys" 'K' "F1" Keys; item "About..." 'A' "" About ]) ]

type purpose = Saving_as | Finding | Going_to

type mode =
  | Editing
  | Menu of int * int (* the bar's menu open, the item selected *)
  | Open_dialog of int (* the file selected *)
  | Input of { title : string; label : string; text : string; purpose : purpose }
  | Info of string * string list (* a box: its title and lines; a key closes it *)
  | Running of Talk.machine
  | Finished of Vt.t * (int * string) option (* the user screen, and a run-time error's line and message *)
  | Showing of Vt.t (* the user screen again: Alt-F5 *)
  | Listing of int (* the P-code, from this instruction *)

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
  quit : bool;
}

(* the window's text: 20 lines of 78 columns, inside its frame *)
let text_rows = 20
let text_cols = 78
let noname = "NONAME00.PAS"

let line (m : model) (r : int) : string = m.lines.(r)
let nlines (m : model) : int = Array.length m.lines
let text (m : model) : string = String.concat "\n" (Array.to_list m.lines) ^ "\n"

(* the window follows the cursor *)
let follow (m : model) : model =
  let row = max 0 (min (nlines m - 1) m.row) in
  let col = max 0 m.col in
  let top = if row < m.top then row else if row >= m.top + text_rows then row - text_rows + 1 else m.top in
  let left = if col < m.left then col else if col >= m.left + text_cols then col - text_cols + 1 else m.left in
  { m with row; col; top; left }

let load (m : model) (file : string) : model =
  let content = Option.value (List.assoc_opt file m.disk) ~default:"" in
  let content = if content <> "" && content.[String.length content - 1] = '\n' then String.sub content 0 (String.length content - 1) else content in
  { m with lines = Array.of_list (String.split_on_char '\n' content); file; row = 0; col = 0; top = 0; left = 0; modified = false;
    compiled = None; error = None }

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
  | "\x1b[5~" | "\x12" -> { m with row = m.row - text_rows + 1; top = max 0 (m.top - text_rows + 1) }
  | "\x1b[6~" | "\x03" -> { m with row = min (nlines m - 1) (m.row + text_rows - 1); top = min (max 0 (nlines m - text_rows)) (m.top + text_rows - 1) }
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

let run (m : model) : model =
  match compile m with
  | Error m -> m
  | Ok (p, m) -> { m with mode = Running (Talk.start ~seed:(m.runs + 1) ~rows:24 ~cols:80 (Pmachine.run p)); runs = m.runs + 1 }

(* the machine's last words, when a run-time error stopped it:
   "Runtime error 201 at line 12: Range check error" (Turbo Pascal
   found the line from the address the program stopped at, the same) *)
let runtime_error (vt : Vt.t) : (int * string) option =
  List.find_map
    (fun l ->
      try Scanf.sscanf l "Runtime error %d at line %d: %[^\n]" (fun code line msg -> Some (line, Printf.sprintf "Runtime error %d: %s." code msg))
      with _ -> None)
    (Vt.text vt)

(* the machine still running, or the user screen with a note *)
let running (m : model) (machine : Talk.machine) : model =
  if Talk.finished machine then
    let vt = Talk.screen machine in
    let shown = Vt.feed vt "\r\n\x1b[7m Press any key to return to Turbo Pascal \x1b[0m" in
    { m with mode = Finished (shown, runtime_error vt); last_screen = Some vt }
  else { m with mode = Running machine }

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
  | Run -> run m
  | User_screen -> ( match m.last_screen with Some vt -> { m with mode = Showing vt } | None -> { m with mode = Showing (Vt.create ~rows:24 ~cols:80) })
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
      | Going_to -> ( match int_of_string_opt (String.trim text) with Some n -> { m with row = n - 1; col = 0 } | None -> m))
  | "\x7f" | "\b" -> again (if text = "" then "" else String.sub text 0 (String.length text - 1))
  | _ when String.length k = 1 && k.[0] >= ' ' -> again (text ^ k)
  | _ -> m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let key (m : model) (k : string) : model =
  match m.mode with
  | Running machine ->
      (* only what a program reads: characters, Enter, Backspace,
         Control-C to stop it *)
      if String.length k = 1 then running m (Talk.input machine k) else m
  | Finished (_, err) -> (
      match err with
      | Some (l, msg) -> { m with mode = Editing; error = Some msg; row = l - 1; col = 0 }
      | None -> { m with mode = Editing })
  | Showing _ | Info _ -> { m with mode = Editing }
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
      | _ -> (
          match alt_letter k with
          | Some 'x' -> act m Exit
          | Some c -> ( match menu_of_letter c with Some b -> { m with mode = Menu (b, 0) } | None -> m)
          | None -> edit_key m k))

let update (ev : Tui.event) (m : model) : model =
  match (ev, m.mode) with
  | Tick dt, Running machine -> running m (Talk.tick machine dt)
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

let status_line (s : Curses.t) : Curses.t =
  let s = fill 23 0 1 80 grey s in
  let keys = [ ("F1", "Help"); ("F2", "Save"); ("F3", "Open"); ("Alt+F9", "Compile"); ("F9", "Make"); ("Ctrl+F9", "Run"); ("F10", "Menu") ] in
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

let edit_window (m : model) (s : Curses.t) : Curses.t =
  let s = fill 1 0 22 80 text_attrs s in
  let s = frame ~title:m.file 1 0 22 80 frame_attrs s in
  let pos = Printf.sprintf " %s%d:%d " (if m.modified then "* " else "") (m.row + 1) (m.col + 1) in
  let s = Curses.put ~attrs:frame_attrs 22 3 pos s in
  (* the comments running into the window from above it *)
  let comment = ref None in
  for r = 0 to m.top - 1 do
    comment := snd (colour_line (line m r) !comment)
  done;
  let s = ref s in
  for i = 0 to text_rows - 1 do
    let r = m.top + i in
    if r < nlines m then begin
      let pieces, next = colour_line (line m r) !comment in
      comment := next;
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
  let p = match m.compiled with Some p -> p | None -> { Pcode.code = [||]; lines = [||] } in
  let top = 3 and left = 8 and h = 18 and w = 64 in
  let window = attrs Vt.Black Vt.Cyan in
  let s = fill top left h w window s in
  let s = frame ~title:("P-code: " ^ m.file) top left h w (attrs ~bold:true Vt.White Vt.Cyan) s in
  let s = ref (shadow top left h w s) in
  for i = 0 to h - 3 do
    let a = first + i in
    if a < Array.length p.code then begin
      let here = p.lines.(a) = m.row + 1 in
      let text = Printf.sprintf " %4d  %-24s line %d" a (Pcode.show p.code.(a)) p.lines.(a) in
      s := Curses.put ~attrs:(if here then attrs ~bold:true Vt.Yellow Vt.Blue else window) (top + 1 + i) (left + 1) (Printf.sprintf "%-*s" (w - 2) text) !s
    end
  done;
  Curses.put ~attrs:(attrs ~bold:true Vt.White Vt.Cyan) (top + h - 1) (left + 2) " the cursor's line highlighted; Esc " !s

let view (m : model) : Curses.t =
  match m.mode with
  | Running machine -> Curses.cursor (if Talk.reading machine then Some (Vt.cursor (Talk.screen machine)) else None) (vt_screen (Talk.screen machine))
  | Finished (vt, _) | Showing vt -> Curses.cursor None (vt_screen vt)
  | _ -> (
      let s = Curses.create ~rows:24 ~cols:80 in
      let s = edit_window m s in
      let s = status_line s in
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
      | _ -> Curses.cursor editing_cursor s)

(*****************************************************************************)
(* The start *)
(*****************************************************************************)

let init : model =
  load
    { lines = [| "" |]; row = 0; col = 0; top = 0; left = 0; file = noname; modified = false; overwrite = false; disk = Pascal_disk.files;
      mode = Editing; error = None; compiled = None; last_screen = None; search = ""; runs = 0; quit = false }
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
  | Running _ -> "run"
  | Finished _ | Showing _ -> "user"
  | Listing _ -> "p-code"

let file (m : model) (name : string) = List.assoc_opt name m.disk
