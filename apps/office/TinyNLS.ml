(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyNLS: the oN-Line System
 * (Douglas Engelbart and the Augmentation Research Center, SRI, shown
 * in San Francisco on December 9, 1968: "the Mother of All Demos").
 *
 * The origin of this whole section of the catalogue, and of most of
 * what a computer is used for today. In ninety minutes, on a screen,
 * with a mouse: text edited on the screen rather than on paper,
 * documents structured and viewed in several ways, links jumped
 * along, a shared screen and video between two sites fifty km apart.
 * Bravo, the Alto, the Star, the Macintosh, the web -- each took a
 * piece of it. What this program shows:
 *
 *   - **the document is a tree** of statements, each numbered by its
 *     place, 1, 1a, 1a1 (Nls_doc.mli). A **branch**, a statement and
 *     all below it, is moved or copied or deleted as one: the
 *     structure is edited, not just displayed. And each statement has
 *     a permanent identifier as well as its number, which changes when
 *     it moves (the message says how);
 *   - **views** (viewspecs): V, then a digit to show that many levels
 *     (V 1 is the outline of the document), a for all of them, t for
 *     one line of each statement, n for the numbers, and OK. The
 *     document is not changed, only what is shown of it;
 *   - **links**: <shop> or <2c> in a statement names another, by the
 *     name it gives itself, "(shop) ...", or by its number. Jump Link,
 *     the bug on a link, brings it to the top of the screen; Jump
 *     Return goes back, as many times as you jumped. Hypertext, the
 *     word Ted Nelson had coined three years before;
 *   - **the commands**, a verb and a noun, typed as their first
 *     letters, then the target pointed at with the mouse -- the **bug**
 *     -- then OK (Enter): m b, bug a statement, bug another, OK moves
 *     the first's branch after the second (Up or Down before OK: up or
 *     down a level). The feedback line at the top spells the command
 *     out as it grows, and ends in OK? when it is complete;
 *   - **the mouse and the keyset**: Engelbart's hands were one on the
 *     mouse and one on a five-key chord keyset, letters typed as
 *     chords -- a letter's place in the alphabet in binary, a = 00001,
 *     c = 00011 -- so that neither hand left its device. The keyset
 *     under the screen lights the chord of each letter typed, and the
 *     mouse its button.
 *
 * What it uses: appkits/nls (Nls_doc, the tree, tested without a screen
 * in appkits/tests), libs/terminal's Curses and Vt and the Teletype
 * way's draw_screen for the character display. What it does not use:
 * gui/ -- no menus, no windows, no scroll bars, none of which existed
 * yet.
 *
 * What it deliberately does not do: the shared screen and the video
 * (the second half of the demo), files and several of them (a link
 * named a file too, <file, statement>), the rest of NLS's nouns
 * (Character, Text, Plex, Group, Visible) and verbs (Transpose,
 * Break, Append), content filters (a view showing only statements
 * that contain a word), and the keyset's command chords.
 *
 * Exercises: the shared screen over Multiplayer, two bugs on one
 * document, the demo's Menlo Park half; links into another file; a
 * content filter; Move Word, bug a word and a place; statements that
 * keep their SID across Save and Open, and a link by SID that no
 * edit can break.
 *)
open Playground

(*****************************************************************************)
(* The keys *)
(*****************************************************************************)

type key = Char of char | Enter | Escape | Backspace | Up | Down

let named_keys = [ ("Enter", Enter); ("Escape", Escape); ("Backspace", Backspace); ("ArrowUp", Up); ("ArrowDown", Down) ]

let keys_of (k : keyboard) ~(before : string list) : key list =
  let now = Set_.elements k.keys in
  let went_down = List.filter (fun n -> not (List.mem n before)) now in
  let chars = List.filter (fun c -> c >= ' ' && c < '\127') (List.init (String.length k.typed) (String.get k.typed)) in
  List.map (fun c -> Char c) chars @ List.filter_map (fun n -> List.assoc_opt n named_keys) went_down

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type verb = Insert | Delete | Replace | Move | Copy | Jump | View
type noun = Word | Statement | Branch | Link | Return

(* a command as it is being given: what was typed, what was bugged *)
type command = {
  verb : verb;
  noun : noun option;
  bugs : (int * int) list; (* each a statement's SID and a place in its text *)
  where : Nls_doc.where;
  text : string;
}

(* what is shown: how many levels, how many lines of each statement,
   the numbers or not *)
type view = { levels : int option; lines : int option; numbers : bool }

type model = {
  doc : Nls_doc.t;
  view : view;
  top : int; (* the SID at the top of the screen *)
  back : int list; (* where each Jump came from, for Jump Return *)
  command : command option;
  message : string;
  chord : char option; (* the last letter typed, lit on the keyset *)
  was : string list;
}

let verbs = [ ('i', Insert); ('d', Delete); ('r', Replace); ('m', Move); ('c', Copy); ('j', Jump); ('v', View) ]

let nouns = function
  | Insert | Replace -> [ ('w', Word); ('s', Statement) ]
  | Delete -> [ ('w', Word); ('b', Branch) ]
  | Move | Copy -> [ ('b', Branch) ]
  | Jump -> [ ('l', Link); ('r', Return) ]
  | View -> []

let verb_name = function
  | Insert -> "Insert" | Delete -> "Delete" | Replace -> "Replace" | Move -> "Move" | Copy -> "Copy" | Jump -> "Jump" | View -> "Viewspecs"

let noun_name = function Word -> "Word" | Statement -> "Statement" | Branch -> "Branch" | Link -> "Link" | Return -> "Return"

(* how many targets a command wants, whether it wants text typed, and
   whether its statement can go up or down a level *)
let bugs_wanted c = match (c.verb, c.noun) with (Move | Copy), _ -> 2 | Jump, Some Return | View, _ -> 0 | _ -> 1
let wants_text c = c.verb = Insert || c.verb = Replace || c.verb = View
let has_level c = match (c.verb, c.noun) with Insert, Some Statement | (Move | Copy), _ -> true | _ -> false
let ready c = c.verb = View || c.noun <> None
let complete c = ready c && List.length c.bugs = bugs_wanted c

(* the demo's own shopping list, and this program's manual *)
let opening =
  Nls_doc.of_outline
    [
      (0, "(nls) NLS, the oN-Line System: Engelbart's lab at SRI, shown on December 9, 1968");
      (1, "The document is a tree of statements, numbered by where they are: 1, 1a, 1a1.");
      (1, "(views) Viewspecs: v 1 then Enter shows the first level only; v a all of them; v t one line each; v n the numbers.");
      (1, "Links: <shop> by its name, <3b> by its number. Jump Link (j l), the bug on one, then Enter; Jump Return (j r) comes back.");
      (0, "(shop) Shopping list, the one planned in the demo");
      (1, "produce");
      (2, "apples");
      (2, "bananas");
      (2, "lettuce");
      (1, "bakery");
      (2, "bread");
      (1, "dairy");
      (2, "milk");
      (2, "cheese");
      (1, "drug store");
      (2, "aspirin");
      (0, "(commands) Commands: a verb and a noun by their first letters, the bug on the target, then OK (Enter).");
      (1, "Insert Statement (i s): the bug on a statement, Up or Down for a level up or down, the text, OK.");
      (1, "Move Branch (m b) and Copy Branch (c b): the bug on the branch, then on the statement it goes after, OK.");
      (1, "Delete Branch (d b), Delete Word (d w), Insert Word (i w), Replace Word (r w), Replace Statement (r s).");
      (1, "Escape cancels a command. Try: m b, the bug on dairy, the bug on produce, Enter -- and see <views>.");
    ]

let initial =
  {
    doc = opening;
    view = { levels = None; lines = None; numbers = true };
    top = 1;
    back = [];
    command = None;
    message = "";
    chord = None;
    was = [];
  }

(*****************************************************************************)
(* The display *)
(*****************************************************************************)

let cols = 72
let rows = 24
let first_row = 3
let text_rows = 19

(* a screen line of the document: the statement it shows, where in
   its text the line starts, and at which column *)
type line = { sid : int; start : int; col : int; shown : string }

(* a text cut into lines of at most [w] characters, at spaces: each
   line's start in the text, and the line *)
let wrap (text : string) (w : int) : (int * string) list =
  let n = String.length text in
  let rec go i =
    if i >= n then []
    else if n - i <= w then [ (i, String.sub text i (n - i)) ]
    else
      let cut = match String.rindex_from_opt text (i + w) ' ' with Some j when j > i -> j | _ -> i + w in
      (i, String.sub text i (cut - i)) :: go (if cut < n && text.[cut] = ' ' then cut + 1 else cut)
  in
  if n = 0 then [ (0, "") ] else go 0

let lines_of model : line list =
  let statements = Nls_doc.visible model.doc ~levels:model.view.levels in
  let rec from_top = function
    | [] -> statements
    | ((s : Nls_doc.statement), _) :: _ as l when s.sid = model.top -> l
    | _ :: rest -> from_top rest
  in
  let shown = from_top statements in
  let shown = if shown = [] then statements else shown in
  List.concat_map
    (fun ((s : Nls_doc.statement), depth) ->
      let number = if model.view.numbers then Option.value (Nls_doc.number model.doc s.sid) ~default:"" ^ " " else "" in
      let col = min (cols - 20) ((3 * depth) + String.length number) in
      let pieces = wrap s.text (cols - col) in
      let pieces = match model.view.lines with Some n -> List.filteri (fun i _ -> i < n) pieces | None -> pieces in
      let lead i = if i = 0 then String.make (3 * depth) ' ' ^ number else String.make col ' ' in
      List.mapi (fun i (start, piece) -> { sid = s.sid; start; col; shown = lead i ^ piece }) pieces)
    shown
  |> List.filteri (fun i _ -> i < text_rows)

(* the grid's size on the playground's screen, and the character cell
   under a point *)
let grid computer = Teletype.screen_size computer (Vt.create ~rows ~cols)

let cell_at computer (x, y) =
  let w, h = grid computer in
  (int_of_float (Float.floor (((h /. 2.) -. y) /. (h /. float_of_int rows))), int_of_float (Float.floor ((x +. (w /. 2.)) /. (w /. float_of_int cols))))

(* what the bug points at: a statement and a place in its text *)
let bugged computer model (x, y) : (int * int) option =
  let row, col = cell_at computer (x, y) in
  match List.nth_opt (lines_of model) (row - first_row) with
  | Some l when row >= first_row -> Some (l.sid, l.start + max 0 (col - l.col))
  | _ -> None

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

let text_of model sid = match Nls_doc.get model.doc sid with Some s -> s.text | None -> ""
let number_of model sid = Option.value (Nls_doc.number model.doc sid) ~default:"?"
let splice s a b by = String.sub s 0 a ^ by ^ String.sub s b (String.length s - b)

(* a word's span with the space after it, or before it at the end *)
let with_space s (a, b) = if b < String.length s then (a, b + 1) else if a > 0 then (a - 1, b) else (a, b)

let apply_view text view =
  String.fold_left
    (fun v c ->
      match c with
      | '1' .. '9' -> { v with levels = Some (Char.code c - Char.code '0') }
      | 'a' -> { v with levels = None }
      | 't' -> { v with lines = (if v.lines = None then Some 1 else None) }
      | 'n' -> { v with numbers = not v.numbers }
      | _ -> v)
    view text

let execute c model : model =
  let done_ model message = { model with command = None; message } in
  let word sid i f =
    let s = text_of model sid in
    match Nls_doc.word_at s i with
    | Some span -> done_ { model with doc = Nls_doc.set_text model.doc sid (f s span) } "OK"
    | None -> done_ { model with doc = Nls_doc.set_text model.doc sid c.text } "OK"
  in
  match (c.verb, c.noun, c.bugs) with
  | View, _, _ -> done_ { model with view = apply_view c.text model.view } "OK"
  | Insert, Some Word, [ (sid, i) ] -> word sid i (fun s (a, _) -> splice s a a (c.text ^ " "))
  | Replace, Some Word, [ (sid, i) ] -> word sid i (fun s (a, b) -> splice s a b c.text)
  | Delete, Some Word, [ (sid, i) ] -> word sid i (fun s span -> let a, b = with_space s span in splice s a b "")
  | Replace, Some Statement, [ (sid, _) ] -> done_ { model with doc = Nls_doc.set_text model.doc sid c.text } "OK"
  | Insert, Some Statement, [ (sid, _) ] ->
      let doc, made = Nls_doc.insert model.doc ~target:sid c.where c.text in
      done_ { model with doc } ("OK: statement " ^ Option.value (Nls_doc.number doc made) ~default:"")
  | Delete, Some Branch, [ (sid, _) ] ->
      let top = if sid = model.top then (List.hd (Nls_doc.visible model.doc ~levels:None) |> fst).sid else model.top in
      done_ { model with doc = Nls_doc.delete model.doc sid; top } ("OK: " ^ number_of model sid ^ " deleted, with its branch")
  | (Move | Copy), _, [ (sid, _); (target, _) ] -> (
      let f = if c.verb = Move then Nls_doc.move else Nls_doc.copy in
      match f model.doc sid ~target c.where with
      | Some doc ->
          (* the number changes, the SID does not: say so *)
          let now = if c.verb = Move then Option.value (Nls_doc.number doc sid) ~default:"" else "a copy" in
          done_ { model with doc } (Printf.sprintf "OK: %s is now %s" (number_of model sid) now)
      | None -> done_ model "A branch cannot go inside itself")
  | Jump, Some Link, [ (sid, i) ] -> (
      match List.find_opt (fun (a, b, _) -> a <= i && i < b) (Nls_doc.links (text_of model sid)) with
      | Some (_, _, name) -> (
          match Nls_doc.find model.doc name with
          | Some target -> done_ { model with top = target; back = model.top :: model.back } ("OK: jumped to " ^ number_of model target)
          | None -> done_ model ("No statement called " ^ name))
      | None -> done_ model "The bug is not on a link <...>")
  | Jump, Some Return, [] -> (
      match model.back with top :: back -> done_ { model with top; back } "OK: back" | [] -> done_ model "Nowhere to return to")
  | _ -> done_ model "?"

(* a key while a command is being given *)
let command_key key c model =
  let again c = { model with command = Some c; message = "" } in
  match key with
  | Escape -> { model with command = None; message = "Cancelled" }
  | Char ch when not (ready c) -> (
      match List.assoc_opt (Char.lowercase_ascii ch) (nouns c.verb) with
      | Some noun -> again { c with noun = Some noun }
      | None -> { model with message = verb_name c.verb ^ " what? " ^ String.concat ", " (List.map (fun (_, n) -> noun_name n) (nouns c.verb)) })
  | Enter when complete c -> execute c model
  | Enter -> { model with message = "BUG: point at the target with the mouse and click" }
  | Up when has_level c -> again { c with where = (if c.where = Nls_doc.Down then Nls_doc.After else Nls_doc.Up) }
  | Down when has_level c -> again { c with where = (if c.where = Nls_doc.Up then Nls_doc.After else Nls_doc.Down) }
  | Char ch when wants_text c && complete c -> again { c with text = c.text ^ String.make 1 ch }
  | Backspace when c.text <> "" -> again { c with text = String.sub c.text 0 (String.length c.text - 1) }
  | _ -> model

(* the next statement shown, up or down from the top *)
let scroll dir model =
  let shown = List.map (fun ((s : Nls_doc.statement), _) -> s.sid) (Nls_doc.visible model.doc ~levels:model.view.levels) in
  let rec index i = function [] -> 0 | s :: rest -> if s = model.top then i else index (i + 1) rest in
  match List.nth_opt shown (index 0 shown + dir) with Some top when dir <> 0 -> { model with top } | _ -> model

let press key model =
  let model = match key with Char c when Char.lowercase_ascii c >= 'a' && Char.lowercase_ascii c <= 'z' -> { model with chord = Some (Char.lowercase_ascii c) } | _ -> model in
  match model.command with
  | Some c -> command_key key c model
  | None -> (
      match key with
      | Char ch -> (
          match List.assoc_opt (Char.lowercase_ascii ch) verbs with
          | Some verb -> { model with command = Some { verb; noun = None; bugs = []; where = Nls_doc.After; text = "" }; message = "" }
          | None -> { model with message = "A verb: Insert, Delete, Replace, Move, Copy, Jump, Viewspecs" })
      | Up -> scroll (-1) model
      | Down -> scroll 1 model
      | _ -> model)

let update computer model =
  let keys = keys_of computer.keyboard ~before:model.was in
  let model = List.fold_left (fun m k -> press k m) model keys in
  let m = computer.mouse in
  (* the bug, when a command wants a target *)
  let model =
    match model.command with
    | Some c when m.mclick && ready c && List.length c.bugs < bugs_wanted c -> (
        match bugged computer model (m.mx, m.my) with
        | Some target -> { model with command = Some { c with bugs = c.bugs @ [ target ] } }
        | None -> model)
    | _ -> model
  in
  let model = if m.mwheel > 0. then scroll (-1) model else if m.mwheel < 0. then scroll 1 model else model in
  { model with was = Set_.elements computer.keyboard.keys }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let lit = { Vt.plain with reverse = true }
let bold = { Vt.plain with bold = true }

(* the command as NLS echoed it, growing as it is given *)
let feedback model =
  match model.command with
  | None -> ""
  | Some c ->
      let target (sid, i) =
        match c.noun with
        | Some Word -> ( match Nls_doc.word_at (text_of model sid) i with Some (a, b) -> String.sub (text_of model sid) a (b - a) | None -> number_of model sid)
        | _ -> number_of model sid
      in
      let bugs = List.mapi (fun i b -> (if bugs_wanted c = 2 then (if i = 0 then " (from) " else " (to) ") else " ") ^ target b) c.bugs in
      String.concat ""
        ([ verb_name c.verb; (match c.noun with Some n -> " " ^ noun_name n | None -> if c.verb = View then "" else " ?") ]
        @ bugs
        @ [
            (if ready c && List.length c.bugs < bugs_wanted c then " BUG" else "");
            (if has_level c && complete c then match c.where with Nls_doc.After -> " (same level)" | Nls_doc.Down -> " (down)" | Nls_doc.Up -> " (up)" else "");
            (if wants_text c && complete c then " T: " ^ c.text else "");
            (if complete c then "  OK?" else "");
          ])

let screen model : Curses.t =
  let t = Curses.create ~rows ~cols in
  let v = model.view in
  let viewspecs =
    Printf.sprintf "VIEWSPECS: %s, %s%s" (match v.levels with Some n -> Printf.sprintf "%d level%s" n (if n > 1 then "s" else "") | None -> "all levels")
      (match v.lines with Some _ -> "one line each" | None -> "all lines") (if v.numbers then ", numbers" else "")
  in
  let t = t |> Curses.put 0 0 (feedback model) |> Curses.put 1 0 ("<DEMO>  " ^ viewspecs) |> Curses.put 2 0 (String.make cols '_') in
  let lines = lines_of model in
  (* what the command has bugged, lit *)
  let bugged_sids = match model.command with Some c -> List.map fst c.bugs | None -> [] in
  let word_bug = match model.command with Some { noun = Some (Word | Link); bugs = [ (sid, i) ]; _ } -> Some (sid, i) | _ -> None in
  let t =
    List.fold_left
      (fun t (k, l) ->
        let row = first_row + k in
        let t = Curses.put row 0 l.shown t in
        (* the statement's text starts at l.col, on every line of it *)
        let text_col = l.col in
        let piece_len = String.length l.shown - text_col in
        (* links bold, as NLS showed them *)
        let t =
          List.fold_left
            (fun t (a, b, _) ->
              let a = max a l.start and b = min b (l.start + piece_len) in
              if a < b then Curses.put ~attrs:bold row (l.col + a - l.start) (String.sub l.shown (text_col + a - l.start) (b - a)) t else t)
            t
            (Nls_doc.links (text_of model l.sid))
        in
        match word_bug with
        | Some (sid, i) when sid = l.sid -> (
            match Nls_doc.word_at (text_of model sid) i with
            | Some (a, b) when a >= l.start && a < l.start + piece_len ->
                let b = min b (l.start + piece_len) in
                Curses.put ~attrs:lit row (l.col + a - l.start) (String.sub l.shown (text_col + a - l.start) (b - a)) t
            | _ -> t)
        | _ -> if List.mem l.sid bugged_sids && word_bug = None then Curses.put ~attrs:lit row l.col (String.sub l.shown text_col piece_len) t else t)
      t
      (List.mapi (fun k l -> (k, l)) lines)
  in
  let t = Curses.put 23 0 model.message t in
  let typing = match model.command with Some c -> wants_text c && complete c | None -> false in
  Curses.cursor (if typing then Some (0, min (cols - 1) (String.length (feedback model) - 5)) else None) t

let phosphor = rgb 205 230 255

(* the five-key keyset, the chord of the last letter typed lit, and
   the mouse, its button lit while held *)
let devices computer model =
  let _, h = grid computer in
  let y = -.(h /. 2.) -. 70. in
  let code = match model.chord with Some c -> Char.code c - Char.code 'a' + 1 | None -> 0 in
  let key i =
    let down = code land (1 lsl (4 - i)) <> 0 in
    rectangle (if down then phosphor else rgb 50 60 70) 34. 80. |> move (-300. +. (float_of_int i *. 42.)) y
  in
  let bits = String.init 5 (fun i -> if code land (1 lsl (4 - i)) <> 0 then '1' else '0') in
  let caption = match model.chord with Some c -> Printf.sprintf "keyset: %c = %s" c bits | None -> "keyset" in
  List.init 5 key
  @ [
      words (rgb 140 160 180) caption |> move (-215.) (y -. 55.);
      (* the mouse: a box on wheels, three buttons *)
      rectangle (rgb 70 60 50) 110. 90. |> move 240. y;
    ]
  @ List.init 3 (fun i ->
        rectangle (if i = 0 && computer.mouse.mdown then phosphor else rgb 110 100 90) 26. 16. |> move (206. +. (float_of_int i *. 34.)) (y +. 33.))
  @ [ words (rgb 140 160 180) "mouse" |> move 240. (y -. 55.) ]

(* the bug: NLS's pointer, a small arrow *)
let bug computer =
  let m = computer.mouse in
  [ polygon phosphor [ (0., 0.); (-7., -16.); (7., -16.) ] |> move m.mx m.my ]

let view computer model =
  let vt = Vt.feed (Vt.create ~rows ~cols) (Curses.redraw (screen model)) in
  (rectangle black computer.screen.width computer.screen.height :: Teletype.draw_screen ~phosphor ~cursor:true computer vt)
  @ devices computer model @ bug computer

let app = game view update initial
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
