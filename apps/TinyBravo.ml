(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyBravo: the first editor where the screen looked like the page
 * (Butler Lampson and Charles Simonyi, Xerox PARC, 1974;
 * plan_gui_teaching.md, phase 9c).
 *
 * Bravo ran on the Alto, whose screen was a sheet of paper standing
 * up -- 606 by 808 dots, black on white, portrait -- and it was the
 * first program to show text as it would print: in its fonts, bold and
 * italic, where the lines really broke. That is WYSIWYG, and it was
 * new. It is also where the **piece table** comes from, the structure
 * gui/Text_edit is; Simonyi took it to Microsoft and wrote Word with
 * it, which is why this program comes before apps/TinyWord, as
 * TinyVisiCalc comes before TinyExcel.
 *
 * What it uses: appkits/richtext (Rich, the text and its looks; Page,
 * the layout and the way back from a click), appkits/document/Undo,
 * and apps/Stroke_text to draw the looks from Hershey's strokes. No
 * widget at all: Bravo had none.
 *
 * And the thing it is remembered for besides WYSIWYG: it was
 * **modal**. The keyboard gives commands, until one of them says that
 * what comes next is text:
 *
 *   i   insert before the selection     (then type, then Escape)
 *   a   append after it                  (the same)
 *   d   delete it
 *   e   select everything
 *   l   looks: then b bold, i italic, u underline, s strike,
 *       + bigger, - smaller, p plain
 *   u   undo -- the last command, whole: everything typed between an
 *       i and its Escape is one edit
 *
 * The mouse selects: press, and drag. Which gives the famous trap,
 * reproducible here: in command mode, type the word "edit". The e
 * selects everything, the d deletes it, the i starts inserting, and
 * the t is all that is left of your document. (u gets it back.) Larry
 * Tesler told that story for the rest of his life, and his answer was
 * Gypsy (with Tim Mott, 1975): no modes, a caret you type at wherever
 * it is, and cut, copy and paste -- which Word, and everything since,
 * inherited. TinyWord is that answer.
 *
 * What it deliberately does not do: Bravo's own command letters and
 * mouse buttons exactly (the Alto's mouse had three, and Bravo gave
 * each its own kind of selection -- a character, a word, a line);
 * fonts (Hershey's one face, with looks drawn by the pen); paragraph
 * looks (Bravo had margins and tabs); and printing, which on the Alto
 * meant the laser printer PARC had also just invented.
 *
 * Exercises: Bravo's word selection (a double click, mouse.mdouble,
 * and Text's idea of a word); a replace command, r, which is delete
 * and insert as one edit; the three-button mouse, which the
 * playground's two buttons can nearly do; and the edit trap made
 * impossible without losing the modes -- which is the argument Tesler
 * lost to nobody.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type mode = Command | Insert | Looks

type model = {
  (* the text and its looks, and every version of them *)
  history : Rich.t Undo.t;
  mode : mode;
  (* the line at the top: what the last command did *)
  said : string;
  was_down : bool;
}

let opening =
  "Bravo, 1974\n\
   The first editor to show a page as it would print: its letters in \
   bold and italic where they would be, its lines broken where the \
   paper would break them. Select with the mouse, then give a command \
   from the keyboard: i to insert, d to delete, l then b for bold. \
   Escape ends typing.\n\
   And then try typing edit, in command mode."

(* where a word first appears in the opening, found rather than
   counted by hand -- which is how it went wrong the first time *)
let index_of word =
  let n = String.length word in
  let rec go i = if String.sub opening i n = word then i else go (i + 1) in
  go 0

(* the opening, with some looks on it: a title, a word in bold, a word
   in italic *)
let initial =
  let r = Rich.of_string opening in
  let look f word r =
    let a = index_of word in
    Rich.restyle f (Rich.select ~anchor:a ~caret:(a + String.length word) r)
  in
  let r = look (fun s -> { (Style.toggle_bold s) with size = 26. }) "Bravo, 1974" r in
  let r = look Style.toggle_bold "bold" r in
  let r = look Style.toggle_italic "italic" r in
  let r = Rich.at 0 r in
  { history = Undo.start r; mode = Command; said = "Ready"; was_down = false }

let doc model = Undo.now model.history

(* an edit, which undo can take back whole *)
let edit ~name f model = { model with history = Undo.record ~name (f (doc model)) model.history }

(* what changes the text without being an edit of its own: the
   selection moving, or the next letter of an insertion *)
let amend f model = { model with history = Undo.amend (f (doc model)) model.history }

(*****************************************************************************)
(* The page *)
(*****************************************************************************)

(* the Alto's screen was a sheet of paper standing up *)
let page_w = 606.
let page_h = 780.
let margin = 48.
let page_left = -.page_w /. 2.
let page_top = 380.

let laid_out model =
  Page.layout ~metrics:Stroke_text.metrics ~width:(page_w -. (2. *. margin)) (doc model)

(* the page's coordinates (y down from the top of the text) into the
   playground's *)
let on_screen (x, y) = (page_left +. margin +. x, page_top -. margin -. y)
let off_screen (x, y) = (x -. page_left -. margin, page_top -. margin -. y)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* one character of the keyboard, through the modes *)
let key c model =
  match model.mode with
  | Insert -> amend (Rich.insert (String.make 1 c)) model
  | Looks ->
      let look name f = edit ~name:"Looks" (Rich.restyle f) { model with mode = Command; said = name } in
      (match c with
      | 'b' -> look "Looks: bold" Style.toggle_bold
      | 'i' -> look "Looks: italic" Style.toggle_italic
      | 'u' -> look "Looks: underline" Style.toggle_underline
      | 's' -> look "Looks: strike" Style.toggle_strike
      | '+' -> look "Looks: bigger" (fun s -> { s with size = s.size *. 1.25 })
      | '-' -> look "Looks: smaller" (fun s -> { s with size = s.size /. 1.25 })
      | 'p' -> look "Looks: plain" (fun s -> { Style.plain with size = s.size })
      | _ -> { model with mode = Command; said = "Looks: b i u s + - p" })
  | Command -> (
      let r = doc model in
      let a, b = Rich.range r in
      match c with
      (* an insertion is one edit however long it is: recorded here,
         amended by every letter after *)
      | 'i' -> edit ~name:"Insert" (Rich.at a) { model with mode = Insert; said = "Insert" }
      | 'a' -> edit ~name:"Append" (Rich.at b) { model with mode = Insert; said = "Append" }
      | 'd' -> edit ~name:"Delete" Rich.delete_backward { model with said = "Delete" }
      | 'e' ->
          amend (fun r -> Rich.select ~anchor:0 ~caret:(Rich.length r) r) { model with said = "Everything" }
      | 'l' -> { model with mode = Looks; said = "Looks:" }
      | 'u' ->
          let said = match Undo.undo_name model.history with Some n -> "Undo " ^ n | None -> "Nothing to undo" in
          { model with history = Undo.undo model.history; said }
      | c -> { model with said = Printf.sprintf "%c? no such command" c })

let update computer model =
  let m = computer.mouse and k = computer.keyboard in
  let page = laid_out model in
  (* the mouse selects, in command mode: press, and drag *)
  let model =
    if model.mode <> Command then model
    else
      let at = Page.offset_at page (off_screen (m.mx, m.my)) in
      if m.mdown && not model.was_down then amend (Rich.at at) model
      else if m.mdown then amend (Rich.to_ at) model
      else model
  in
  (* the keyboard: every character typed this frame, in order, through
     the modes -- so that "edit" typed fast is four commands *)
  let model = String.fold_left (fun model c -> key c model) model k.typed in
  let model =
    if List.mem "Escape" (Set_.elements k.keys) && model.mode <> Command then
      { model with mode = Command; said = "Ready" }
    else if model.mode = Insert && k.kbackspace then amend Rich.delete_backward model
    else model
  in
  { model with was_down = m.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view _computer model =
  let r = doc model in
  let page = laid_out model in
  let a, b = Rich.range r in
  let ink = rgb 20 20 20 in
  let glyphs =
    List.concat_map
      (fun (g : Page.glyph) ->
        if g.text = "\n" || g.text = " " then []
        else
          let x, y = on_screen (g.x, g.baseline) in
          Stroke_text.glyph ink g.style g.text ~x ~baseline:y)
      (Page.glyphs page)
  in
  (* the selection, as the Alto showed it: the characters on grey *)
  let selected =
    List.filter_map
      (fun (g : Page.glyph) ->
        if g.offset >= a && g.offset < b && g.text <> "\n" then
          let x, y = on_screen (g.x, g.baseline) in
          Some (rectangle (rgb 200 200 200) g.advance (g.style.size *. 1.2)
                |> move (x +. (g.advance /. 2.)) (y +. (g.style.size *. 0.3)))
        else None)
      (Page.glyphs page)
  in
  let caret =
    if model.mode = Insert || a = b then
      let x, baseline, height = Page.caret_at page (Rich.caret r) in
      let x, y = on_screen (x, baseline) in
      [ rectangle ink 2. (height *. 0.8) |> move x (y +. (height *. 0.25)) ]
    else []
  in
  [
    rectangle (rgb 90 90 90) 1000. 1000.;
    (* the Alto's screen: a sheet of paper standing up *)
    rectangle white page_w page_h |> move 0. (page_top -. (page_h /. 2.));
  ]
  @ selected @ glyphs @ caret
  @ [
      (* Bravo's line at the top: the mode, and what the last command did *)
      words white
        (Printf.sprintf "%s     %s"
           (match model.mode with Command -> "COMMAND" | Insert -> "INSERT (Escape ends it)" | Looks -> "LOOKS")
           model.said)
      |> move 0. 440.;
      words (rgb 200 200 200) "i insert  a append  d delete  e everything  l looks  u undo"
      |> move 0. (-440.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
