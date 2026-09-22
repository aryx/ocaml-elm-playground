(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyWord: the same text as TinyBravo, eleven years later and with no
 * modes (Microsoft Word, 1983 on DOS and 1985 on the Macintosh;
 * plan_gui_teaching.md, phase 9d).
 *
 * The pair with TinyBravo.ml is the point, as TinyVisiCalc and
 * TinyExcel are, and what is shared comes first: appkits/richtext --
 * the piece table, the runs of looks over it, the page they are laid
 * out on and the way back from a click -- and Stroke_text, the
 * looks drawn with Hershey's pen. Not a line of it differs. Charles
 * Simonyi wrote both programs' ancestors: Bravo at Xerox, then Word at
 * Microsoft, with the same piece table.
 *
 * What differs is the thing Larry Tesler spent his career arguing for
 * (and put on his licence plate: NO MODES):
 *
 *                  TinyBravo (1974)          TinyWord (1985)
 *   typing         only after i, until       anywhere, at any time: a
 *                  Escape                    key is a letter unless
 *                                            Control is down
 *   the caret      where the selection       wherever you click
 *                  starts
 *   looks          l, then a letter you had  a menu you can read, a
 *                  to know                   button with the look drawn
 *                                            on it, Control-B
 *   size           a letter per step         a list of sizes
 *   undo           the last command          named ("Undo Typing"),
 *                                            both ways
 *   "edit"         deletes your document     types the word "edit"
 *
 * Gypsy (Tesler and Tim Mott, Xerox, 1975) was the modeless editor
 * that came between them, and cut, copy and paste are its; Word
 * inherited all of it, and so did everything since.
 *
 * What it uses: appkits/richtext (Rich, Page and its alignment),
 * appkits/document (Undo, Clipboard), Stroke_text, and the
 * playground's menus. The toolbar is drawn by the program rather than
 * made of widgets, because its icons are drawn with the pen -- a bold
 * B, an italic I -- which a widget's label cannot be; so it asks
 * Gui.modal before taking a click, as any drawn surface has to.
 *
 * Typing is one edit per run of keystrokes -- "Undo Typing" takes back
 * the whole word, not its last letter -- which is Undo.amend, and the
 * same question 7GUIs' Circle Drawer asks about a slider.
 *
 * What it deliberately does not do: alignment per paragraph (here it is
 * the document's; Word's ruler set it for each paragraph, which is a
 * second table of runs over paragraphs rather than characters) -- and,
 * being kept beside the text rather than in it, alignment is not in the
 * history either: undo does not take a centring back; fonts
 * -- Hershey's one face, the looks drawn by the pen; a clipboard that
 * keeps the looks (this one carries characters); styles with names
 * ("Normal", "Heading 1"); pages, and printing them; and a caret that
 * blinks, since nothing here depends on the clock.
 *
 * Exercises: alignment per paragraph, as a second run table in Rich,
 * which puts it in the history for free;
 * a second Hershey face (the Roman duplex is the same letters with
 * more strokes, which is what bold really was on a plotter) chosen from
 * a Font menu; a clipboard holding the runs too, so that bold pastes
 * bold; the ruler, with its little triangles for the margins.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  (* the text and its looks, and every version of them *)
  history : Rich.t Undo.t;
  clip : Clipboard.t;
  align : Page.align;
  (* whether the last thing done was typing, so that the next letter
     joins the same edit *)
  typing : bool;
  (* the document's name, and the File menu's dialog *)
  file : File_menu.t;
  (* the keys and the mouse button at the previous frame, for edges *)
  was : string list;
  was_down : bool;
}

let opening =
  "TinyWord, 1985\n\
   The same text as in Bravo, set on the same page -- and no modes. \
   Click anywhere and type. Select a word and press the bold button, or \
   Control-B, or choose Bold from the Format menu: three ways to one \
   look, and none of them needs remembering.\n\
   Try typing the word edit here. It only types the word edit."

let index_of word =
  let n = String.length word in
  let rec go i = if String.sub opening i n = word then i else go (i + 1) in
  go 0

let initial =
  let r = Rich.of_string opening in
  let look f word r =
    let a = index_of word in
    Rich.restyle f (Rich.select ~anchor:a ~caret:(a + String.length word) r)
  in
  let r = look (fun s -> { (Style.toggle_bold s) with size = 26. }) "TinyWord, 1985" r in
  let r = look Style.toggle_italic "no modes" r in
  let r = look Style.toggle_bold "bold button" r in
  let r = look Style.toggle_underline "three ways" r in
  {
    history = Undo.start (Rich.at (String.length opening) r);
    clip = Clipboard.empty;
    align = Page.Left;
    typing = false;
    file = File_menu.start;
    was = [];
    was_down = false;
  }

let doc model = Undo.now model.history
let edit ~name f model = { model with history = Undo.record ~name (f (doc model)) model.history; typing = false }
let amend f model = { model with history = Undo.amend (f (doc model)) model.history }

(* a run of typing is one edit: the first keystroke records it, the
   ones after amend it *)
let typed f model =
  if model.typing then amend f model
  else { (edit ~name:"Typing" f model) with typing = true }

(*****************************************************************************)
(* The page *)
(*****************************************************************************)

let page_w = 700.
let page_h = 800.
let margin = 50.
let page_left = -.page_w /. 2.
let page_top = 390.

let laid_out model =
  Page.layout ~align:model.align ~metrics:Stroke_text.metrics ~width:(page_w -. (2. *. margin))
    (doc model)

let on_screen (x, y) = (page_left +. margin +. x, page_top -. margin -. y)
let off_screen (x, y) = (x -. page_left -. margin, page_top -. margin -. y)

(*****************************************************************************)
(* The rules: looks on a selection, or on what is typed next *)
(*****************************************************************************)

(* is a look on? For a selection, on every character of it -- so that
   pressing bold over a half-bold selection makes it all bold, as Word
   does -- and with nothing selected, on what will be typed next *)
let is_on (get : Style.t -> bool) r =
  let a, b = Rich.range r in
  if a = b then get (Rich.typing_style r)
  else
    let rec all i = i >= b || (get (Rich.style_at r i) && all (i + 1)) in
    all a

let toggle ~name get set model =
  let on = is_on get (doc model) in
  edit ~name (Rich.restyle (fun s -> set s (not on))) model

let bold = toggle ~name:"Bold" (fun (s : Style.t) -> s.bold) (fun s v -> { s with bold = v })
let italic = toggle ~name:"Italic" (fun (s : Style.t) -> s.italic) (fun s v -> { s with italic = v })
let underline = toggle ~name:"Underline" (fun (s : Style.t) -> s.underline) (fun s v -> { s with underline = v })
let strike = toggle ~name:"Strike" (fun (s : Style.t) -> s.strike) (fun s v -> { s with strike = v })
let plain = edit ~name:"Plain" (Rich.restyle (fun s -> { Style.plain with size = s.Style.size }))
let sizes = [ 12.; 16.; 20.; 26.; 32. ]

let select_all model =
  amend (fun r -> Rich.select ~anchor:0 ~caret:(Rich.length r) r) { model with typing = false }

let copy model =
  let r = doc model in
  let a, b = Rich.range r in
  if a = b then model else { model with clip = Clipboard.put (String.sub (Rich.to_string r) a (b - a)) model.clip }

let cut model = edit ~name:"Cut" Rich.delete_backward (copy model)

let paste model =
  match Clipboard.get model.clip with Some s -> edit ~name:"Paste" (Rich.insert s) model | None -> model

let undo model = { model with history = Undo.undo model.history; typing = false }
let redo model = { model with history = Undo.redo model.history; typing = false }

(*****************************************************************************)
(* The toolbar: drawn, with its icons drawn by the pen *)
(*****************************************************************************)

type tool = Bold | Italic | Underline | Strike | Align of Page.align

let tools =
  [ Bold; Italic; Underline; Strike; Align Page.Left; Align Page.Center; Align Page.Right; Align Page.Justify ]

let tool_w = 34.
let tool_h = 30.
let toolbar_y = 428.

(* where each button sits: the looks, a gap, the alignments *)
let tool_x i = -300. +. (float_of_int i *. (tool_w +. 6.)) +. if i >= 4 then 18. else 0.
let tool_box i : Widget.box = { Widget.x = tool_x i; y = toolbar_y; w = tool_w; h = tool_h }

let tool_on model = function
  | Bold -> is_on (fun s -> s.Style.bold) (doc model)
  | Italic -> is_on (fun s -> s.Style.italic) (doc model)
  | Underline -> is_on (fun s -> s.Style.underline) (doc model)
  | Strike -> is_on (fun s -> s.Style.strike) (doc model)
  | Align a -> model.align = a

let use_tool model = function
  | Bold -> bold model
  | Italic -> italic model
  | Underline -> underline model
  | Strike -> strike model
  | Align a -> { model with align = a }

(* an icon: the look's own letter drawn in the look, or the four
   stacks of lines everyone knows from the ruler *)
let icon (b : Widget.box) tool =
  let ink = rgb 30 30 30 in
  let letter s look =
    let w = Stroke_text.metrics look s in
    Stroke_text.glyph ink look s ~x:(b.x -. (w /. 2.)) ~baseline:(b.y -. 6.)
  in
  let big = { Style.plain with size = 20. } in
  let bars widths place =
    List.mapi
      (fun i w ->
        let x = match place with `Left -> b.x -. 10. +. (w /. 2.) | `Center -> b.x | `Right -> b.x +. 10. -. (w /. 2.) in
        rectangle ink w 2. |> move x (b.y +. 7.5 -. (float_of_int i *. 5.)))
      widths
  in
  match tool with
  | Bold -> letter "B" { big with bold = true }
  | Italic -> letter "I" { big with italic = true }
  | Underline -> letter "U" { big with underline = true }
  | Strike -> letter "S" { big with strike = true }
  | Align Page.Left -> bars [ 20.; 13.; 20.; 9. ] `Left
  | Align Page.Center -> bars [ 20.; 12.; 17.; 8. ] `Center
  | Align Page.Right -> bars [ 20.; 13.; 20.; 9. ] `Right
  | Align Page.Justify -> bars [ 20.; 20.; 20.; 12. ] `Left

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type slot = File | Edit_ | Format | Size

let menu_edit = [ "Edit"; "Undo"; "Redo"; "Cut"; "Copy"; "Paste"; "Select All" ]
let menu_format = [ "Format"; "Bold"; "Italic"; "Underline"; "Strike"; "Plain" ]
let size_names = List.map (fun s -> Printf.sprintf "%.0f" s) sizes

let menu_box slot : Widget.box =
  match slot with
  | File -> { Widget.x = -400.; y = 470.; w = 100.; h = 30. }
  | Edit_ -> { Widget.x = -295.; y = 470.; w = 80.; h = 30. }
  | Format -> { Widget.x = -200.; y = 470.; w = 100.; h = 30. }
  | Size -> { Widget.x = 120.; y = toolbar_y; w = 80.; h = tool_h }

(* the keyboard, with no modes: a letter is a letter unless Control is
   down, and then it is a command *)
let keyboard computer model =
  let k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key model.was) in
  let control = List.mem "Control" now and shift = k.kshift in
  let r = doc model in
  let page = laid_out model in
  let caret = Rich.caret r in
  let move to_ model =
    amend (fun r -> if shift then Rich.to_ to_ r else Rich.at to_ r) { model with typing = false }
  in
  let vertical dy model =
    let x, baseline, height = Page.caret_at page caret in
    move (Page.offset_at page (x, baseline -. (height /. 2.) +. (dy *. height))) model
  in
  let model =
    if control then
      if pressed "b" then bold model
      else if pressed "i" then italic model
      else if pressed "u" then underline model
      else if pressed "z" then if shift then redo model else undo model
      else if pressed "y" then redo model
      else if pressed "c" then copy model
      else if pressed "x" then cut model
      else if pressed "v" then paste model
      else if pressed "a" then select_all model
      else model
    else if k.typed <> "" then typed (Rich.insert k.typed) model
    else if pressed "Enter" then typed (Rich.insert "\n") model
    else if pressed "Backspace" then typed Rich.delete_backward model
    else if pressed "Delete" then typed Rich.delete_forward model
    else if pressed "ArrowLeft" then move (Text.prev_char (Rich.to_string r) caret) model
    else if pressed "ArrowRight" then move (Text.next_char (Rich.to_string r) caret) model
    else if pressed "ArrowUp" then vertical (-1.) model
    else if pressed "ArrowDown" then vertical 1. model
    else model
  in
  { model with was = now }

(* a text, saved as its Rich.t -- the looks with it -- and its
   alignment *)
let kind = { File_menu.magic = "TinyWord 1"; extension = ".doc" }

let reopened (r : (Rich.t * Page.align) File_menu.result) model =
  match r with
  | File_menu.Nothing -> model
  | File_menu.New -> { initial with history = Undo.start (Rich.of_string ""); file = model.file }
  | File_menu.Opened (text, align) -> { initial with history = Undo.start (Rich.at 0 text); align; file = model.file }

let update caps computer model =
  let current () = (doc model, model.align) in
  if File_menu.busy model.file then
    let file, r = File_menu.dialog caps kind computer ~current model.file in
    reopened r { model with file; was_down = computer.mouse.mdown; was = Set_.elements computer.keyboard.keys }
  else
  let m = computer.mouse in
  let page = laid_out model in
  (* the menus: real widgets, asked for first, so that their items are
     what a click lands on while one is open *)
  let command items chosen model =
    match List.nth_opt items chosen with
    | Some "Undo" -> undo model
    | Some "Redo" -> redo model
    | Some "Cut" -> cut model
    | Some "Copy" -> copy model
    | Some "Paste" -> paste model
    | Some "Select All" -> select_all model
    | Some "Bold" -> bold model
    | Some "Italic" -> italic model
    | Some "Underline" -> underline model
    | Some "Strike" -> strike model
    | Some "Plain" -> plain model
    | _ -> model
  in
  let current_size = (Rich.typing_style (doc model)).size in
  let size_index =
    let rec go i = function [] -> 1 | s :: rest -> if s = current_size then i else go (i + 1) rest in
    go 0 sizes
  in
  let size_chosen = Gui.menu_in computer (menu_box Size) size_names size_index in
  let model =
    if size_chosen <> size_index then
      let size = List.nth sizes size_chosen in
      edit ~name:"Size" (Rich.restyle (fun s -> { s with size })) model
    else model
  in
  let model =
    let file, r = File_menu.menu_in caps kind computer (menu_box File) ~current model.file in
    reopened r { model with file }
  in
  let model = command menu_edit (Gui.menu_in computer (menu_box Edit_) menu_edit 0) model in
  let model = command menu_format (Gui.menu_in computer (menu_box Format) menu_format 0) model in
  (* the toolbar, and the page: drawn, so they ask first whether a menu
     has the mouse *)
  let model =
    if Gui.modal () then model
    else
      let clicked_tool =
        if m.mclick then
          List.find_opt (fun (i, _) -> Widget.contains (tool_box i) m.mx m.my) (List.mapi (fun i t -> (i, t)) tools)
        else None
      in
      match clicked_tool with
      | Some (_, tool) -> use_tool model tool
      | None ->
          let inside = m.my < page_top && m.my > page_top -. page_h && Float.abs m.mx < page_w /. 2. in
          if inside && m.mdown then
            let at = Page.offset_at page (off_screen (m.mx, m.my)) in
            if not model.was_down then amend (Rich.at at) { model with typing = false }
            else amend (Rich.to_ at) model
          else model
  in
  let model = keyboard computer model in
  { model with was_down = m.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view _computer model =
  let r = doc model in
  let page = laid_out model in
  let a, b = Rich.range r in
  let ink = rgb 20 20 20 in
  let th = Gui.theme () in
  let glyphs =
    List.concat_map
      (fun (g : Page.glyph) ->
        if g.text = "\n" || g.text = " " then []
        else
          let x, y = on_screen (g.x, g.baseline) in
          Stroke_text.glyph ink g.style g.text ~x ~baseline:y)
      (Page.glyphs page)
  in
  let selected =
    List.filter_map
      (fun (g : Page.glyph) ->
        if g.offset >= a && g.offset < b && g.text <> "\n" then
          let x, y = on_screen (g.x, g.baseline) in
          Some
            (rectangle (rgb 170 200 240) g.advance (g.style.size *. 1.2)
            |> move (x +. (g.advance /. 2.)) (y +. (g.style.size *. 0.3)))
        else None)
      (Page.glyphs page)
  in
  let caret =
    if a = b then
      let x, baseline, height = Page.caret_at page (Rich.caret r) in
      let x, y = on_screen (x, baseline) in
      [ rectangle ink 2. (height *. 0.8) |> move x (y +. (height *. 0.25)) ]
    else []
  in
  let toolbar =
    List.concat
      (List.mapi
         (fun i tool ->
           let bx = tool_box i in
           (* a button that is on is drawn pressed in *)
           [ rectangle (if tool_on model tool then th.face_down else th.face) bx.w bx.h |> move bx.x bx.y ]
           @ Gui.shapes (Widget.frame th.edge th.border bx)
           @ icon bx tool)
         tools)
  in
  let status =
    Printf.sprintf "%s   %d words   %s   %s"
      (if File_menu.said model.file <> "" then File_menu.said model.file else File_menu.title model.file)
      (List.length (List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.map (fun c -> if c = '\n' then ' ' else c) (Rich.to_string r)))))
      (match Undo.undo_name model.history with Some n -> "Undo " ^ n | None -> "")
      (match Clipboard.get model.clip with Some s -> Printf.sprintf "clipboard: %d" (String.length s) | None -> "")
  in
  [
    rectangle (rgb 205 205 200) 1000. 1000.;
    (* the menu bar and the toolbar *)
    rectangle (rgb 240 240 236) 1000. 100. |> move 0. 450.;
    rectangle white page_w page_h |> move 0. (page_top -. (page_h /. 2.));
  ]
  @ selected @ glyphs @ caret @ toolbar
  @ [ words (rgb 90 90 90) status |> move 0. (-440.) ]
  @ File_menu.view model.file
  (* last, so that an open menu's items are over everything *)
  @ Gui.draw ()

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> File_menu.caps)))
