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

(* See Tui_emacs.mli *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a command waiting for its arguments, read in the minibuffer *)
type minibuffer = {
  prompt : string;
  input : string;
  command : Lisp.t;
  args : Lisp.t list; (* read so far, backwards *)
  codes : string list; (* the interactive codes left, the one being read first *)
}

type isearch = { forward : bool; text : string; origin : int; failing : bool }

type model = {
  lisp : Emacs_editor.lisp;
  keys : string; (* a prefix typed so far: C-x *)
  prefix : Lisp.t; (* C-u's argument for the next command, or nil *)
  mini : minibuffer option;
  isearch : isearch option;
  last_command : Lisp.t;
}

let editor (m : model) : Emacs_editor.t = m.lisp.host
let buf (m : model) : Emacs_editor.buffer = Emacs_editor.current (editor m)
let with_editor (m : model) (e : Emacs_editor.t) : model = { m with lisp = { m.lisp with host = e } }
let with_buf (m : model) (b : Emacs_editor.buffer) : model = with_editor m (Emacs_editor.set_current b (editor m))
let say (m : model) (s : string) : model = with_editor m { (editor m) with message = s }

(*****************************************************************************)
(* Running a command *)
(*****************************************************************************)

(* a Lisp evaluation's error, into the echo area *)
let run_lisp (m : model) (f : Emacs_editor.lisp -> Lisp.t * Emacs_editor.lisp) : Lisp.t option * model =
  match Lisp_eval.protect { m.lisp with fuel = 1_000_000 } f with
  | Ok v, st -> (Some v, { m with lisp = st })
  | Error e, st -> (None, say { m with lisp = st } (Lisp_eval.error_message e))

let self_insert = Sym "self-insert-command"

(* the command called on its arguments: the variables it reads set,
   an undo boundary first, and afterwards what it asked of the loop *)
let rec execute (m : model) (command : Lisp.t) (args : Lisp.t list) : model =
  let b = buf m in
  let prefix = m.prefix and last = m.last_command in
  let e = editor m in
  let typing = command = self_insert && m.last_command = self_insert in
  let m = with_editor m (if typing then e else Emacs_editor.boundary e) in
  let st = m.lisp in
  let st =
    st
    |> Lisp_eval.set_var "last-command" m.last_command
    |> Lisp_eval.set_var "this-command" command
    |> Lisp_eval.set_var "current-prefix-arg" m.prefix
    |> Lisp_eval.set_var "mode-name" (Str b.mode)
  in
  let _, m = run_lisp { m with lisp = st; prefix = nil } (fun st -> Lisp_eval.apply st command args) in
  let this = Option.value (Lisp_eval.get_var m.lisp "this-command") ~default:command in
  let m = { m with last_command = this } in
  let request = (editor m).request in
  let m = with_editor m { (editor m) with request = None } in
  match request with
  | None -> m
  | Some (Call_interactively c) -> call_interactively m c
  | Some (Isearch forward) -> say { m with isearch = Some { forward; text = ""; origin = (buf m).point; failing = false } } ""
  | Some Universal_argument ->
      (* C-u C-u: 16; the prefix survives this command, which isn't
         one for last-command *)
      { m with prefix = (match prefix with Int n -> Int (n * 4) | _ -> Int 4); last_command = last }

(* the codes of (interactive "p\nsName: "): what each argument is *)
and call_interactively (m : model) (command : Lisp.t) : model =
  match Lisp_eval.interactive_spec m.lisp command with
  | None -> say m (Lisp.princ command ^ " is not a command")
  | Some spec ->
      let codes = match spec with Str "" | Sym "nil" -> [] | Str s -> String.split_on_char '\n' s | _ -> [] in
      next_argument m command [] codes

and next_argument (m : model) (command : Lisp.t) (args : Lisp.t list) (codes : string list) : model =
  match codes with
  | [] -> execute m command (List.rev args)
  | code :: rest -> (
      let b = buf m in
      match code.[0] with
      | 'p' -> next_argument m command ((match m.prefix with Int n -> Int n | _ -> Int 1) :: args) rest
      | 'P' -> next_argument m command (m.prefix :: args) rest
      | 'd' -> next_argument m command (Int (b.point + 1) :: args) rest
      | 'r' -> (
          match b.mark with
          | Some mk -> next_argument m command (Int (max mk b.point + 1) :: Int (min mk b.point + 1) :: args) rest
          | None -> say m "The mark is not set now, so there is no region")
      | _ -> { m with mini = Some { prompt = String.sub code 1 (String.length code - 1); input = ""; command; args; codes } })

(*****************************************************************************)
(* The minibuffer *)
(*****************************************************************************)

(* what the code being read can complete to *)
let candidates (m : model) (mini : minibuffer) : string list =
  let st = m.lisp in
  let names l = List.sort_uniq compare (List.map fst l) in
  match (List.hd mini.codes).[0] with
  | 'C' -> List.filter (fun f -> Lisp_eval.interactive_spec st (Sym f) <> None) (names st.funs)
  | 'a' -> names st.funs
  | 'b' | 'B' -> List.map (fun (b : Emacs_editor.buffer) -> b.name) (editor m).buffers
  | 'f' | 'F' -> names (editor m).disk
  | _ -> []

let drop_last (s : string) : string = if s = "" then "" else String.sub s 0 (String.length s - 1)

let starts_with ~(prefix : string) (s : string) = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let matching (m : model) (mini : minibuffer) : string list = List.filter (starts_with ~prefix:mini.input) (candidates m mini)

(* the longest prefix the candidates share: what TAB completes to *)
let common_prefix (l : string list) : string =
  match l with
  | [] -> ""
  | first :: rest ->
      let n = List.fold_left (fun n s -> let k = ref 0 in while !k < n && !k < String.length s && s.[!k] = first.[!k] do incr k done; !k) (String.length first) rest in
      String.sub first 0 n

(* the text typed, made the argument its code says *)
let finish (m : model) (mini : minibuffer) : model =
  let code = (List.hd mini.codes).[0] and input = mini.input in
  let m = { m with mini = None } in
  let arg =
    match code with
    | 'n' -> ( match int_of_string_opt (String.trim input) with Some n -> Ok (Int n) | None -> Error "Please enter a number.")
    | 'x' -> ( match Lisp_read.read input 0 with v, _ -> Ok v | exception Lisp_read.Error msg -> Error ("Invalid read syntax: " ^ msg))
    | 'C' | 'a' -> if List.mem input (candidates m mini) then Ok (Sym input) else Error "[No match]"
    | 'b' | 'B' ->
        (* nothing typed: the buffer before this one *)
        if input = "" then match (editor m).buffers with _ :: other :: _ -> Ok (Str other.name) | _ -> Ok (Str (buf m).name) else Ok (Str input)
    | 'k' -> Ok (Str input)
    | _ -> if input = "" && (code = 'f' || code = 'F') then Error "No file name" else Ok (Str input)
  in
  match arg with
  | Ok v -> next_argument m mini.command (v :: mini.args) (List.tl mini.codes)
  | Error msg -> say m msg

(* global-map's bindings, the newest first *)
let bindings (m : model) : Lisp.t list =
  Option.value (Lisp.to_list (Option.value (Lisp_eval.get_var m.lisp "global-map") ~default:nil)) ~default:[]

let binding (m : model) (keys : string) : Lisp.t option =
  List.find_map (function Cons (Str k, command) when k = keys -> Some command | _ -> None) (bindings m)

(* whether some binding starts with [keys] and is longer: C-x *)
let is_prefix (m : model) (keys : string) : bool =
  List.exists (function Cons (Str k, _) -> String.length k > String.length keys && starts_with ~prefix:keys k | _ -> false) (bindings m)

let minibuffer_key (m : model) (mini : minibuffer) (k : string) : model =
  if (List.hd mini.codes).[0] = 'k' then
    (* a key sequence read whole, prefixes and all: C-h k *)
    let keys = mini.input ^ k in
    if is_prefix m keys && binding m keys = None then { m with mini = Some { mini with input = keys } } else finish m { mini with input = keys }
  else
    match k with
    | "\x07" -> say { m with mini = None } "Quit"
    | "\r" -> finish m mini
    | "\x7f" | "\b" -> { m with mini = Some { mini with input = drop_last mini.input } }
    | "\t" -> (
        match matching m mini with
        | [] -> say m "[No match]"
        | l -> { m with mini = Some { mini with input = common_prefix l } })
    | _ when String.length k = 1 && k.[0] >= ' ' -> { m with mini = Some { mini with input = mini.input ^ k } }
    | _ -> m

(*****************************************************************************)
(* Incremental search *)
(*****************************************************************************)

(* the search string found from [from], point at its end (forward) or
   start (backward); failing, point stays *)
let find (m : model) (is : isearch) (from : int) : model =
  let b = buf m in
  match Emacs_editor.search { b with point = from } is.text ~forward:is.forward ~bound:None with
  | Some p -> with_buf { m with isearch = Some { is with failing = false } } { b with point = p }
  | None -> { m with isearch = Some { is with failing = true } }

let rec isearch_key (m : model) (is : isearch) (k : string) : model =
  let b = buf m in
  let n = String.length is.text in
  match k with
  | "\x13" | "\x12" ->
      (* the next match, past the current one: forward from its end,
         backward from its start (point is at one or the other) *)
      let forward = k = "\x13" in
      let from = if forward then (if is.forward then b.point else b.point + 1) else if is.forward then b.point - 1 else b.point in
      if is.text = "" then m else find m { is with forward } from
  | "\x7f" | "\b" ->
      let is = { is with text = drop_last is.text } in
      find (with_buf m { b with point = is.origin }) is is.origin
  | "\x07" -> say (with_buf { m with isearch = None } { b with point = is.origin }) "Quit"
  | "\r" -> say (with_buf { m with isearch = None } { b with mark = Some is.origin }) "Mark saved where search started"
  | _ when String.length k = 1 && k.[0] >= ' ' ->
      (* a longer string, searched from the current match's start: it
         may still match there *)
      let is = { is with text = is.text ^ k } in
      find m is (if is.forward then (if is.failing then b.point else b.point - n) else b.point + n + 1)
  | _ ->
      (* any other key ends the search, then does what it does *)
      key (with_buf { m with isearch = None } { b with mark = Some is.origin }) k

(*****************************************************************************)
(* Keys *)
(*****************************************************************************)

(* C-g: what was half-typed forgotten, and a command for last-command,
   so that an undo after it undoes the undos *)
and quit (m : model) : model = say { m with prefix = nil; last_command = Sym "keyboard-quit" } "Quit"

and key (m : model) (k : string) : model =
  match (m.isearch, m.mini) with
  | Some is, _ -> isearch_key m is k
  | None, Some mini -> minibuffer_key m mini k
  | None, None -> (
      let keys = m.keys ^ k in
      let m = { m with keys = "" } in
      match binding m keys with
      | Some command -> if command = Sym "keyboard-quit" then quit m else call_interactively m command
      | None ->
          if k = "\x07" then quit m
          else if is_prefix m keys then { m with keys }
          else if String.length keys = 1 && keys.[0] >= ' ' && keys.[0] < '\x7f' then begin
            let st = Lisp_eval.set_var "last-command-event" (Int (Char.code keys.[0])) m.lisp in
            call_interactively { m with lisp = st } self_insert
          end
          else say m (Emacs_editor.key_description keys ^ " is undefined"))

(* the window follows point: recentred when point leaves it, as Emacs
   scrolls by default *)
let follow (m : model) : model =
  let b = buf m in
  let line = Emacs_editor.line_of b b.point in
  if line < b.top || line >= b.top + Emacs_editor.window_height then with_buf m { b with top = max 0 (line - (Emacs_editor.window_height / 2)) } else m

let update (ev : Tui.event) (m : model) : model =
  match ev with
  | Tick _ -> m
  | Key k ->
      (* the echo area cleared by the next key, as in Emacs *)
      let m = if m.mini = None && m.isearch = None then with_editor m { (editor m) with message = "" } else m in
      follow (key m k)

(*****************************************************************************)
(* The display *)
(*****************************************************************************)

let reverse = { Vt.plain with reverse = true }

(* a line as the window shows it: cut at the right edge with a $ *)
let shown (line : string) : string =
  let line = String.map (fun c -> if c < ' ' then '?' else c) line in
  if String.length line > 80 then String.sub line 0 79 ^ "$" else line

let mode_line (m : model) : string =
  let b = buf m in
  let lines = Emacs_editor.line_of b (Gap_buffer.length b.text) + 1 in
  let where =
    if b.top = 0 && lines <= Emacs_editor.window_height then "All"
    else if b.top = 0 then "Top"
    else if b.top + Emacs_editor.window_height >= lines then "Bot"
    else Printf.sprintf "%d%%" (100 * b.top / lines)
  in
  let s =
    Printf.sprintf "-UUU:%s-  %-18s %-4s L%-5d (%s) " (if b.modified then "**" else "--") b.name where (Emacs_editor.line_of b b.point + 1) b.mode
  in
  s ^ String.make (max 0 (80 - String.length s)) '-'

let echo (m : model) : string * int option =
  match (m.mini, m.isearch) with
  | Some mini, _ ->
      let prompt = mini.prompt ^ if (List.hd mini.codes).[0] = 'k' then Emacs_editor.key_description mini.input else mini.input in
      (* the completions, after what is typed: icomplete's way *)
      let more =
        match matching m mini with
        | [] | [ _ ] -> ""
        | l when mini.input <> "" || List.length l < 12 -> " {" ^ String.concat " | " (List.filteri (fun i _ -> i < 8) l) ^ if List.length l > 8 then " ...}" else "}"
        | _ -> ""
      in
      (prompt ^ more, Some (String.length prompt))
  | None, Some is ->
      let p = (if is.failing then "Failing " else "") ^ "I-search" ^ (if is.forward then "" else " backward") ^ ": " ^ is.text in
      (p, None)
  | None, None -> if m.keys <> "" then (Emacs_editor.key_description m.keys ^ "-", None) else ((editor m).message, None)

let view (m : model) : Curses.t =
  let b = buf m in
  let text = Gap_buffer.to_string b.text in
  let lines = String.split_on_char '\n' text in
  let screen = Curses.create ~rows:24 ~cols:80 in
  let screen = ref screen in
  List.iteri
    (fun i line -> if i >= b.top && i < b.top + Emacs_editor.window_height then screen := Curses.put (i - b.top) 0 (shown line) !screen)
    lines;
  let screen = Curses.put ~attrs:reverse Emacs_editor.window_height 0 (mode_line m) !screen in
  let echo_text, mini_col = echo m in
  let screen = Curses.put (Emacs_editor.window_height + 1) 0 (shown echo_text) screen in
  let line = Emacs_editor.line_of b b.point in
  let col = b.point - Emacs_editor.line_start b line in
  let cursor = match mini_col with Some c -> (Emacs_editor.window_height + 1, min 79 c) | None -> (line - b.top, min 79 col) in
  Curses.cursor (Some cursor) screen

(*****************************************************************************)
(* The disk, and the start *)
(*****************************************************************************)

let dot_emacs =
  {|;; .emacs: read by TinyEmacs when it starts, as Emacs reads yours.
;; Change something, then M-x eval-buffer, or C-x C-e after one
;; expression, and it takes effect at once: the editor is this Lisp.

;; a command of our own: M-x hello, or C-c h
(defun hello ()
  "Say hello in the echo area."
  (interactive)
  (message "Hello from your .emacs! %d buffers." (length (buffer-list))))
(global-set-key "\C-ch" 'hello)

;; C-x l: the line point is on
(global-set-key "\C-xl" 'what-line)
|}

let tutorial =
  {|TinyEmacs tutorial.

Emacs commands use the Control key (C-f: hold Control, type f) and the
Meta key (M-f: hold Alt, or type Escape then f).

  C-v   the next screen               M-v   the previous one
  C-f   forward a character           C-b   back a character
  C-n   next line                     C-p   previous line
  M-f   forward a word                M-b   back a word
  C-a   start of the line             C-e   end of the line
  M-<   start of the buffer           M->   end of the buffer

Type text anywhere: it is inserted. DEL deletes before the cursor,
C-d after it. C-k kills the rest of the line; C-y yanks it back, here
or anywhere. C-SPC sets the mark, move, then C-w kills the region
between; M-w copies it. C-/ undoes, again and again; after any other
command, it undoes the undos.

C-s searches as you type; C-s again for the next one, RET to stop.

C-x C-f visits a file, C-x C-s saves it, C-x b switches buffers,
C-x C-b lists them. C-g cancels whatever is half-typed.

Everything is Lisp. In *scratch*, type (+ 1 2) then C-j. M-x runs a
command by name (TAB completes). C-h k then a key says what it runs;
C-h f what a function does. Visit .emacs to see how a key is bound.

C-x C-c quits.
|}

let notes = "Things to do:\n- learn C-k and C-y\n- write a command in .emacs\n- try C-u C-u C-f\n"
let disk = [ (".emacs", dot_emacs); ("TUTORIAL", tutorial); ("notes.txt", notes) ]

let init : model =
  let lisp = Emacs_editor.create ~disk ~simple:Emacs_simple.text in
  let m = { lisp; keys = ""; prefix = nil; mini = None; isearch = None; last_command = nil } in
  (* the user's .emacs, as Emacs loads it when it starts *)
  let _, m = run_lisp m (fun st -> (nil, Lisp_eval.load st dot_emacs)) in
  if (editor m).message <> "" then m else say m "Welcome to TinyEmacs. C-h t: the tutorial; C-x C-c: quit."

let program : model Tui.program = { init; update; view; over = (fun m -> (editor m).quit) }
let buffer_name (m : model) = (buf m).name
let text (m : model) = Gap_buffer.to_string (buf m).text
let point (m : model) = (buf m).point + 1
let message (m : model) = (editor m).message
let file (m : model) (name : string) = List.assoc_opt name (editor m).disk
