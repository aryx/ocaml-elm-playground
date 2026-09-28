(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Lisp

(* See Emacs_editor.mli *)

type undo = Inserted of int * int | Deleted of int * string | Boundary

type buffer = {
  name : string;
  text : Gap_buffer.t;
  point : int;
  mark : int option;
  file : string option;
  modified : bool;
  undo : undo list;
  undoing : undo list option;
  top : int;
  mode : string;
}

type request = Call_interactively of Lisp.t | Isearch of bool | Universal_argument

type t = {
  buffers : buffer list;
  disk : (string * string) list;
  kill_ring : string list;
  message : string;
  quit : bool;
  request : request option;
}
type lisp = t Lisp_eval.state

let window_height = 22

(*****************************************************************************)
(* Buffers *)
(*****************************************************************************)

let mode_of (name : string) : string =
  if name = "*scratch*" then "Lisp Interaction"
  else if Filename.check_suffix name ".el" || name = ".emacs" then "Emacs-Lisp"
  else if Filename.check_suffix name ".txt" || name = "TUTORIAL" then "Text"
  else "Fundamental"

let make_buffer ?file (name : string) (text : string) : buffer =
  { name; text = Gap_buffer.of_string text; point = 0; mark = None; file; modified = false; undo = []; undoing = None; top = 0;
    mode = mode_of name }

let current (e : t) : buffer = List.hd e.buffers
let set_current (b : buffer) (e : t) : t = { e with buffers = b :: List.filter (fun (x : buffer) -> x.name <> b.name) e.buffers }

(* the buffer [name] made current, the others keeping their order *)
let select (name : string) (e : t) : t option =
  match List.find_opt (fun (b : buffer) -> b.name = name) e.buffers with Some b -> Some (set_current b e) | None -> None

(*****************************************************************************)
(* Changes, and their undo *)
(*****************************************************************************)

(* a position after a change: past the insertion if it was after it
   (at it, it stays before, as a marker does), pulled back into a
   deletion's start *)
let after_insert (pos : int) (at : int) (n : int) : int = if pos > at then pos + n else pos
let after_delete (pos : int) (i : int) (j : int) : int = if pos >= j then pos - (j - i) else if pos > i then i else pos

let insert_at (b : buffer) (pos : int) (s : string) : buffer =
  if s = "" then b
  else
    let n = String.length s in
    (* typing merges into the insertion just before, one entry per run *)
    let undo = match b.undo with Inserted (i, j) :: rest when j = pos -> Inserted (i, j + n) :: rest | u -> Inserted (pos, pos + n) :: u in
    { b with text = Gap_buffer.insert b.text pos s; point = after_insert b.point pos n; mark = Option.map (fun m -> after_insert m pos n) b.mark;
      modified = true; undo }

let delete_range (b : buffer) (i : int) (j : int) : buffer =
  let i, j = (min i j, max i j) in
  if i = j then b
  else
    { b with text = Gap_buffer.delete b.text i j; point = after_delete b.point i j; mark = Option.map (fun m -> after_delete m i j) b.mark;
      modified = true; undo = Deleted (i, Gap_buffer.sub b.text i j) :: b.undo }

let boundary (e : t) : t =
  let b = current e in
  match b.undo with Boundary :: _ | [] -> e | u -> set_current { b with undo = Boundary :: u } e

(* one command's worth undone: the entries up to the next boundary,
   each undone by a change of its own, recorded *)
let undo_step (b : buffer) (entries : undo list) : buffer * undo list =
  let rec go (b : buffer) = function
    | Boundary :: rest -> (b, rest)
    | [] -> (b, [])
    | Inserted (i, j) :: rest -> go { (delete_range b i j) with point = i } rest
    | Deleted (i, s) :: rest -> go { (insert_at b i s) with point = i + String.length s } rest
  in
  go b entries

(*****************************************************************************)
(* Lines *)
(*****************************************************************************)

let line_start_at (b : buffer) (pos : int) : int = match Gap_buffer.rindex_before b.text pos '\n' with Some i -> i + 1 | None -> 0
let line_end_at (b : buffer) (pos : int) : int = match Gap_buffer.index_from b.text pos '\n' with Some i -> i | None -> Gap_buffer.length b.text

let line_of (b : buffer) (pos : int) : int =
  let n = ref 0 in
  for i = 0 to pos - 1 do
    if Gap_buffer.get b.text i = '\n' then incr n
  done;
  !n

let line_start (b : buffer) (line : int) : int =
  let rec go pos k = if k = 0 then pos else match Gap_buffer.index_from b.text pos '\n' with Some i -> go (i + 1) (k - 1) | None -> Gap_buffer.length b.text in
  go 0 line

(*****************************************************************************)
(* Keys *)
(*****************************************************************************)

let key_name (k : string) : string =
  let control c = "C-" ^ String.make 1 (Char.lowercase_ascii (Char.chr (Char.code c + 64))) in
  match k with
  | "\r" -> "RET"
  | "\t" -> "TAB"
  | "\x7f" -> "DEL"
  | " " -> "SPC"
  | "\x1b" -> "ESC"
  | "\x00" -> "C-@"
  | "\x1b[A" -> "<up>"
  | "\x1b[B" -> "<down>"
  | "\x1b[C" -> "<right>"
  | "\x1b[D" -> "<left>"
  | "\x1b[3~" -> "<deletechar>"
  | "\x1b[5~" -> "<prior>"
  | "\x1b[6~" -> "<next>"
  | _ when String.length k = 1 && Char.code k.[0] < 32 -> control k.[0]
  | _ when String.length k = 2 && k.[0] = '\x1b' ->
      let c = k.[1] in
      "M-" ^ if Char.code c < 32 then control c else if c = '\x7f' then "DEL" else String.make 1 c
  | _ -> k

let key_description (keys : string) : string = String.concat " " (List.map key_name (Line_discipline.split_keys keys))

(*****************************************************************************)
(* The primitives *)
(*****************************************************************************)

let signal = Lisp_eval.signal
let error = Lisp_eval.error
let wrong_type pred v = signal "wrong-type-argument" [ Sym pred; v ]
let int_of v = match v with Int n -> n | _ -> wrong_type "integer-or-marker-p" v
let string_of v = match v with Str s -> s | _ -> wrong_type "stringp" v

(* the current buffer, and the state with it changed *)
let buf (st : lisp) : buffer = current st.host
let with_buf (st : lisp) (b : buffer) : lisp = { st with host = set_current b st.host }
let with_host (st : lisp) (e : t) : lisp = { st with host = e }

(* a Lisp position (from 1) into the text's (from 0), inside it *)
let pos_of (b : buffer) (v : Lisp.t) : int = max 0 (min (Gap_buffer.length b.text) (int_of v - 1))
let lisp_pos (p : int) : Lisp.t = Int (p + 1)

(* an optional count: the first argument, 1 when missing or nil *)
let count (args : Lisp.t list) : int = match args with Int n :: _ -> n | _ -> 1

let message (st : lisp) (s : string) : lisp = with_host st { st.host with message = s }

let move (st : lisp) (n : int) : Lisp.t * lisp =
  let b = buf st in
  let p = b.point + n in
  if p < 0 then signal "beginning-of-buffer" []
  else if p > Gap_buffer.length b.text then signal "end-of-buffer" []
  else (nil, with_buf st { b with point = p })

(* forward-line: to the start of the line [n] lines down (up when
   negative), and how many lines were missing to get there *)
let forward_line (b : buffer) (n : int) : buffer * int =
  if n > 0 then
    let rec go p k =
      if k = 0 then (p, 0)
      else match Gap_buffer.index_from b.text p '\n' with Some i -> go (i + 1) (k - 1) | None -> (Gap_buffer.length b.text, k)
    in
    let p, short = go b.point n in
    ({ b with point = p }, short)
  else
    let rec go p k = if k = 0 then (p, 0) else if p = 0 then (0, k) else go (line_start_at b (p - 1)) (k - 1) in
    let p, short = go (line_start_at b b.point) (-n) in
    ({ b with point = p }, short)

let column (b : buffer) : int = b.point - line_start_at b b.point

let search (b : buffer) (s : string) ~(forward : bool) ~(bound : int option) : int option =
  let len = Gap_buffer.length b.text and n = String.length s in
  let at i = i >= 0 && i + n <= len && Gap_buffer.sub b.text i (i + n) = s in
  if forward then
    let limit = Option.value bound ~default:len in
    let rec go i = if i + n > limit then None else if at i then Some (i + n) else go (i + 1) in
    go b.point
  else
    let limit = Option.value bound ~default:0 in
    let rec go i = if i < limit then None else if at i then Some i else go (i - 1) in
    go (b.point - n)

(* the start of the balanced expression before [pos]: a list, a string,
   or a symbol, a quote before it included (lisp.el's backward-sexp,
   without syntax tables) *)
let sexp_start (b : buffer) (pos : int) : int =
  let get i = Gap_buffer.get b.text i in
  let rec skip_blank i = if i > 0 && (get (i - 1) = ' ' || get (i - 1) = '\n' || get (i - 1) = '\t') then skip_blank (i - 1) else i in
  let i = skip_blank pos in
  if i = 0 then signal "scan-error" [ Str "Containing expression ends prematurely" ]
  else
    let start =
      match get (i - 1) with
      | ')' ->
          let rec go j depth =
            if j < 0 then signal "scan-error" [ Str "Unbalanced parentheses" ]
            else match get j with ')' -> go (j - 1) (depth + 1) | '(' -> if depth = 1 then j else go (j - 1) (depth - 1) | _ -> go (j - 1) depth
          in
          go (i - 1) 0
      | '"' -> ( match Gap_buffer.rindex_before b.text (i - 1) '"' with Some j -> j | None -> signal "scan-error" [ Str "Unbalanced quotes" ])
      | _ ->
          let rec go j = if j > 0 && not (String.contains " \n\t()\"'" (get (j - 1))) then go (j - 1) else j in
          go (i - 1)
    in
    if start > 0 && get (start - 1) = '\'' then start - 1 else start

let switch_to (st : lisp) (name : string) : lisp =
  match select name st.host with
  | Some e -> with_host st e
  | None -> with_host st { st.host with buffers = make_buffer name "" :: st.host.buffers }

let visit (st : lisp) (file : string) : lisp =
  match List.find_opt (fun (b : buffer) -> b.file = Some file) st.host.buffers with
  | Some b -> with_host st (set_current b st.host)
  | None ->
      let text, msg = match List.assoc_opt file st.host.disk with Some text -> (text, "") | None -> ("", "(New file)") in
      let name = if List.exists (fun (b : buffer) -> b.name = file) st.host.buffers then file ^ "<2>" else file in
      message (with_host st { st.host with buffers = make_buffer ~file name text :: st.host.buffers }) msg

let write (st : lisp) (file : string) : lisp =
  let b = buf st in
  let e = { st.host with disk = (file, Gap_buffer.to_string b.text) :: List.remove_assoc file st.host.disk } in
  message (with_buf (with_host st e) { b with file = Some file; modified = false }) ("Wrote " ^ file)

let region (b : buffer) : int * int =
  match b.mark with Some m -> (min m b.point, max m b.point) | None -> error "The mark is not set now, so there is no region"

(* name, interactive spec when it is a command too, documentation, and
   the function *)
let primitives : (string * string option * string * t Lisp_eval.subr) list =
  [ (* positions *)
    ("point", None, "Return the value of point, an integer.", fun st _ -> (lisp_pos (buf st).point, st));
    ("point-min", None, "Return the minimum permissible value of point.", fun st _ -> (Int 1, st));
    ("point-max", None, "Return the maximum permissible value of point.", fun st _ -> (lisp_pos (Gap_buffer.length (buf st).text), st));
    ("buffer-size", None, "Return the number of characters in the buffer.", fun st _ -> (Int (Gap_buffer.length (buf st).text), st));
    ( "goto-char", None, "Set point to POSITION.",
      fun st args -> match args with [ p ] -> let b = buf st in (p, with_buf st { b with point = pos_of b p }) | _ -> error "goto-char takes a position" );
    ( "char-after", None, "Return the character at POS (point by default), or nil at the end.",
      fun st args ->
        let b = buf st in
        let p = match args with p :: _ when p <> nil -> int_of p - 1 | _ -> b.point in
        ((if p >= 0 && p < Gap_buffer.length b.text then Int (Char.code (Gap_buffer.get b.text p)) else nil), st) );
    ( "char-before", None, "Return the character before POS (point by default), or nil at the start.",
      fun st args ->
        let b = buf st in
        let p = match args with p :: _ when p <> nil -> int_of p - 1 | _ -> b.point in
        ((if p > 0 && p <= Gap_buffer.length b.text then Int (Char.code (Gap_buffer.get b.text (p - 1))) else nil), st) );
    ("bobp", None, "Return t if point is at the beginning of the buffer.", fun st _ -> (of_bool ((buf st).point = 0), st));
    ("eobp", None, "Return t if point is at the end of the buffer.", fun st _ -> let b = buf st in (of_bool (b.point = Gap_buffer.length b.text), st));
    ("bolp", None, "Return t if point is at the beginning of a line.", fun st _ -> let b = buf st in (of_bool (b.point = line_start_at b b.point), st));
    ("eolp", None, "Return t if point is at the end of a line.", fun st _ -> let b = buf st in (of_bool (b.point = line_end_at b b.point), st));
    ("line-beginning-position", None, "Return the position of the start of the current line.", fun st _ -> let b = buf st in (lisp_pos (line_start_at b b.point), st));
    ("line-end-position", None, "Return the position of the end of the current line.", fun st _ -> let b = buf st in (lisp_pos (line_end_at b b.point), st));
    ("current-column", None, "Return the horizontal position of point, from 0.", fun st _ -> (Int (column (buf st)), st));
    ( "move-to-column", None, "Move point to COLUMN in the current line, or its end if shorter.",
      fun st args ->
        let b = buf st in
        let start = line_start_at b b.point in
        let p = min (line_end_at b b.point) (start + int_of (List.hd args)) in
        (Int (p - start), with_buf st { b with point = p }) );
    (* moving: commands in C in Emacs too *)
    ("forward-char", Some "p", "Move point N characters forward (backward if N is negative).", fun st args -> move st (count args));
    ("backward-char", Some "p", "Move point N characters backward (forward if N is negative).", fun st args -> move st (-count args));
    ( "forward-line", Some "p", "Move N lines forward, to the start of a line; return how many lines could not be moved.",
      fun st args -> let b, short = forward_line (buf st) (count args) in (Int short, with_buf st b) );
    ("beginning-of-line", Some "", "Move point to the beginning of the current line.", fun st _ -> let b = buf st in (nil, with_buf st { b with point = line_start_at b b.point }));
    ("end-of-line", Some "", "Move point to the end of the current line.", fun st _ -> let b = buf st in (nil, with_buf st { b with point = line_end_at b b.point }));
    (* changing *)
    ( "insert", None, "Insert the strings or characters at point, point moving after them.",
      fun st args ->
        let s = String.concat "" (List.map (function Str s -> s | Int c -> String.make 1 (Char.chr (c land 255)) | v -> wrong_type "char-or-string-p" v) args) in
        let b = buf st in
        (nil, with_buf st { (insert_at b b.point s) with point = b.point + String.length s }) );
    ( "delete-region", None, "Delete the text between START and END.",
      fun st args -> match args with [ a; z ] -> let b = buf st in (nil, with_buf st (delete_range b (pos_of b a) (pos_of b z))) | _ -> error "delete-region takes two positions" );
    ( "delete-char", Some "p", "Delete the N characters after point (before it if N is negative).",
      fun st args ->
        let b = buf st in
        let n = count args in
        let p = b.point + n in
        if p < 0 then signal "beginning-of-buffer" [] else if p > Gap_buffer.length b.text then signal "end-of-buffer" []
        else (nil, with_buf st (delete_range b b.point p)) );
    ("erase-buffer", None, "Delete the entire contents of the current buffer.", fun st _ -> let b = buf st in (nil, with_buf st (delete_range b 0 (Gap_buffer.length b.text))));
    ( "buffer-substring", None, "Return the text between START and END as a string.",
      fun st args -> match args with [ a; z ] -> let b = buf st in let i = pos_of b a and j = pos_of b z in (Str (Gap_buffer.sub b.text (min i j) (max i j)), st) | _ -> error "buffer-substring takes two positions" );
    ("buffer-string", None, "Return the whole text of the buffer.", fun st _ -> (Str (Gap_buffer.to_string (buf st).text), st));
    ( "search-forward", Some "sSearch: ", "Search forward from point for STRING; point goes after it. BOUND limits the search, and NOERROR returns nil instead of failing.",
      fun st args ->
        let b = buf st in
        match args with
        | s :: rest -> (
            let bound = match rest with bd :: _ when bd <> nil -> Some (pos_of b bd) | _ -> None in
            match search b (string_of s) ~forward:true ~bound with
            | Some p -> (lisp_pos p, with_buf st { b with point = p })
            | None -> (match rest with [ _; noerror ] when noerror <> nil -> (nil, st) | _ -> signal "search-failed" [ s ]))
        | [] -> error "search-forward takes a string" );
    ( "search-backward", Some "sSearch backward: ", "Search backward from point for STRING; point goes to its start.",
      fun st args ->
        let b = buf st in
        match args with
        | s :: rest -> (
            let bound = match rest with bd :: _ when bd <> nil -> Some (pos_of b bd) | _ -> None in
            match search b (string_of s) ~forward:false ~bound with
            | Some p -> (lisp_pos p, with_buf st { b with point = p })
            | None -> (match rest with [ _; noerror ] when noerror <> nil -> (nil, st) | _ -> signal "search-failed" [ s ]))
        | [] -> error "search-backward takes a string" );
    ( "backward-sexp", Some "", "Move backward across one balanced expression.",
      fun st _ -> let b = buf st in (nil, with_buf st { b with point = sexp_start b b.point }) );
    (* the mark and the region *)
    ("mark", None, "Return the position of the mark, or nil.", fun st _ -> ((match (buf st).mark with Some m -> lisp_pos m | None -> nil), st));
    ( "set-mark", None, "Set the mark at POS (nil: no mark).",
      fun st args -> let b = buf st in (nil, with_buf st { b with mark = (match args with [ p ] when p <> nil -> Some (pos_of b p) | _ -> None) }) );
    ("region-beginning", None, "Return the start of the region.", fun st _ -> (lisp_pos (fst (region (buf st))), st));
    ("region-end", None, "Return the end of the region.", fun st _ -> (lisp_pos (snd (region (buf st))), st));
    (* the kill ring *)
    ("kill-new", None, "Make STRING the latest kill in the kill ring.", fun st args -> (nil, with_host st { st.host with kill_ring = string_of (List.hd args) :: st.host.kill_ring }));
    ( "kill-append", None, "Append STRING to the latest kill (prepend it if BEFORE-P).",
      fun st args ->
        let s = string_of (List.hd args) in
        let before = match args with [ _; b ] -> b <> nil | _ -> false in
        let ring = match st.host.kill_ring with k :: rest -> (if before then s ^ k else k ^ s) :: rest | [] -> [ s ] in
        (nil, with_host st { st.host with kill_ring = ring }) );
    ( "current-kill", None, "Rotate the kill ring N places, and return the latest kill.",
      fun st args ->
        match st.host.kill_ring with
        | [] -> error "Kill ring is empty"
        | ring ->
            let n = count args mod List.length ring in
            let ring = List.filteri (fun i _ -> i >= n) ring @ List.filteri (fun i _ -> i < n) ring in
            (Str (List.hd ring), with_host st { st.host with kill_ring = ring }) );
    (* undo *)
    ( "undo", Some "", "Undo some previous changes; repeat to undo more. Anything else in between makes the undos undoable in turn.",
      fun st _ ->
        let b = buf st in
        let entries = match (st.vars |> List.assoc_opt "last-command", b.undoing) with Some (Sym "undo"), Some rest -> rest | _ -> (match b.undo with Boundary :: rest -> rest | u -> u) in
        if entries = [] then error "No further undo information"
        else
          let b, rest = undo_step b entries in
          (nil, message (with_buf st { b with undoing = Some rest }) "Undo") );
    (* buffers and files *)
    ("buffer-name", None, "Return the name of the current buffer.", fun st _ -> (Str (buf st).name, st));
    ("buffer-list", None, "Return the names of the buffers, the current one first.", fun st _ -> (list (List.map (fun (b : buffer) -> Str b.name) st.host.buffers), st));
    ("buffer-file-name", None, "Return the name of the file the buffer visits, or nil.", fun st _ -> ((match (buf st).file with Some f -> Str f | None -> nil), st));
    ("buffer-modified-p", None, "Return t if the buffer was modified since it was read or saved.", fun st _ -> (of_bool (buf st).modified, st));
    ("set-buffer-modified-p", None, "Mark the buffer as modified or not, by FLAG.", fun st args -> let b = buf st in (nil, with_buf st { b with modified = List.hd args <> nil }));
    ( "switch-to-buffer", Some "BSwitch to buffer: ", "Make BUFFER-NAME current, creating it if there is none.",
      fun st args -> (nil, switch_to st (string_of (List.hd args))) );
    ( "kill-buffer", Some "bKill buffer: ", "Kill the buffer BUFFER-NAME.",
      fun st args ->
        let name = string_of (List.hd args) in
        match List.filter (fun (b : buffer) -> b.name <> name) st.host.buffers with
        | [] -> error "Can't kill the only buffer"
        | rest -> (nil, with_host st { st.host with buffers = rest }) );
    ("find-file", Some "FFind file: ", "Edit the file FILENAME, in a buffer of its own.", fun st args -> (nil, visit st (string_of (List.hd args))));
    ( "save-buffer", Some "", "Save the current buffer in its file.",
      fun st _ ->
        let b = buf st in
        match b.file with
        | None -> error "Buffer is not visiting a file (C-x C-w: write-file)"
        | Some f -> if b.modified then (nil, write st f) else (nil, message st "(No changes need to be saved)") );
    ("write-file", Some "FWrite file: ", "Write the buffer into the file FILENAME, which it visits from then on.", fun st args -> (nil, write st (string_of (List.hd args))));
    ("directory-files", None, "Return the names of the files on the disk.", fun st _ -> (list (List.map (fun (f, _) -> Str f) (List.sort compare st.host.disk)), st));
    ("kill-emacs", Some "", "Exit the Emacs job.", fun st _ -> (nil, with_host st { st.host with quit = true }));
    (* what only the command loop can do, asked of it *)
    ( "call-interactively", None, "Run COMMAND as if from a key: its arguments asked for as its interactive spec says.",
      fun st args -> (nil, with_host st { st.host with request = Some (Call_interactively (List.hd args)) }) );
    ( "isearch-forward", Some "", "Search as you type: each character typed extends the search string; C-s finds the next one, RET or a command key stops, C-g goes back.",
      fun st _ -> (nil, with_host st { st.host with request = Some (Isearch true) }) );
    ("isearch-backward", Some "", "Search backward as you type.", fun st _ -> (nil, with_host st { st.host with request = Some (Isearch false) }));
    ( "universal-argument", Some "", "Begin a numeric argument for the next command: 4, and 16 after C-u C-u.",
      fun st _ -> (nil, with_host st { st.host with request = Some Universal_argument }) );
    (* the window and the echo area *)
    ( "message", None, "Show the formatted string in the echo area, and return it.",
      fun st args ->
        let s = match (List.assoc "format" st.subrs) st args with Str s, _ -> s | _ -> "" in
        (Str s, message st s) );
    ( "recenter", Some "", "Scroll the window so that point's line is in its middle.",
      fun st _ -> let b = buf st in (nil, with_buf st { b with top = max 0 (line_of b b.point - (window_height / 2)) }) );
    ( "scroll-up-command", Some "", "Scroll the text up a windowful, less two lines.",
      fun st _ ->
        let b = buf st in
        let top = b.top + window_height - 2 in
        let last = line_of b (Gap_buffer.length b.text) in
        if b.top >= last then signal "end-of-buffer" []
        else (nil, with_buf st { b with top; point = max b.point (line_start b (min top last)) }) );
    ( "scroll-down-command", Some "", "Scroll the text down a windowful, less two lines.",
      fun st _ ->
        let b = buf st in
        if b.top = 0 then signal "beginning-of-buffer" []
        else
          let top = max 0 (b.top - window_height + 2) in
          let last_shown = line_start b (top + window_height - 1) in
          (nil, with_buf st { b with top; point = (if b.point >= last_shown then line_start b (top + window_height - 2) else b.point) }) );
    (* Lisp over text *)
    ( "read", None, "Read one Lisp expression from STRING.",
      fun st args ->
        match Lisp_read.read (string_of (List.hd args)) 0 with
        | v, _ -> (v, st)
        | exception Lisp_read.Error msg -> signal (if msg = "end of input" then "end-of-file" else "invalid-read-syntax") [ Str msg ] );
    ("key-description", None, "Return a pretty description of the key sequence KEYS: C-x C-f.", fun st args -> (Str (key_description (string_of (List.hd args))), st)) ]

(* (save-excursion body...): body, then point and the current buffer
   put back, even after an error *)
let save_excursion (st : lisp) (body : Lisp.t list) : Lisp.t * lisp =
  let b = buf st in
  let restore (st : lisp) : lisp =
    let st = match select b.name st.host with Some e -> with_host st e | None -> st in
    let now = buf st in
    if now.name = b.name then with_buf st { now with point = min b.point (Gap_buffer.length now.text) } else st
  in
  match Lisp_eval.protect st (fun st -> Lisp_eval.progn st body) with
  | Ok v, st -> (v, restore st)
  | Error e, st -> Lisp_eval.raise_error (restore st) e

let scratch_text = ";; This buffer is for text that is not saved, and for Lisp evaluation.\n;; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n"

let create ~(disk : (string * string) list) ~(simple : string) : lisp =
  let e = { buffers = [ make_buffer "*scratch*" scratch_text ]; disk; kill_ring = []; message = ""; quit = false; request = None } in
  let st = Lisp_eval.create e in
  let st = List.fold_left (fun st (name, interactive, doc, f) -> Lisp_eval.define_subr ?interactive ~doc name f st) st primitives in
  let st = Lisp_eval.define_special "save-excursion" save_excursion st in
  let st = Lisp_eval.load st simple in
  let b = buf st in
  with_buf st { b with point = Gap_buffer.length b.text }
