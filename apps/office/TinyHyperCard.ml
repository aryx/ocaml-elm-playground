(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyHyperCard: programming for people who did not think of
 * themselves as programmers (Bill Atkinson, Apple, 1987;
 * plan_gui_teaching.md).
 *
 * HyperCard came free with every Macintosh. A **stack** is cards the
 * size of the Mac's screen (512 by 342 dots), each with buttons, fields
 * of text and a picture; you browse it by clicking, and you make one by
 * choosing a tool, drawing a button, and writing -- in HyperTalk, a
 * language meant to be read aloud (appkits/hypertalk) -- what it does
 * when clicked. The same thing is a document to read, a program to
 * run, and a program to change while it runs; there is no line
 * between using it and building it. Myst (1993) was made with it.
 *
 * Its two ideas worth taking away:
 *
 * - **Backgrounds**: the buttons and fields every card shares live on
 *   the background, once -- but each card keeps its *own* text in the
 *   background's fields. So a stack of recipes is one background with
 *   a Title and an Ingredients field, and a hundred cards that are
 *   really a hundred rows of a database; a card is a record, a
 *   background field a column. Here: the Title and Page fields, and the
 *   Prev and Next buttons.
 *
 * - **The message path**: a click sends "mouseUp" to the button, and
 *   whatever the button does not answer -- or answers and passes on --
 *   goes to the card, then the background, then the stack. So the
 *   stack's script can answer "openCard" for every card (here: it
 *   numbers the pages), and a card's script answers for all its
 *   buttons: shared behaviour by position, with no classes.
 *
 * What it uses: appkits/hypertalk (the language, run against this
 * stack through a record of functions), appkits/paint (the card's
 * picture and the pencil: Atkinson wrote MacPaint first, and
 * HyperCard's paint tools are MacPaint's), Stroke_text for the
 * fields' text, and the playground's menus, fields and text areas --
 * the fields, when browsing, are text areas of the toolkit, asked for
 * every frame, which is immediate mode taken at its word.
 *
 * The tools: Browse (click buttons, type in fields, arrows to turn the
 * page), Button and Field (click one to select it, drag it to move it;
 * Objects > Script to read or write its script), Pencil (draw on the
 * card). Objects > New Button / Field / Card to add them.
 *
 * What it deliberately does not do: HyperCard's five user levels
 * (browsing, typing, painting, authoring, scripting -- here the tools
 * are all there at once); more than one background; resizing parts;
 * button styles and icons; the message box, where a line of HyperTalk
 * could be typed and run at once; visual effects between cards; "find";
 * and most of HyperTalk (see its .mli). It saves as HyperCard did,
 * with no Save command: once the stack has a name (File > Save As...),
 * every change is written to it as it is made.
 *
 * Exercises: the message box -- a field at the bottom whose Enter
 * sends its line as a one-handler script, the quickest way to learn
 * the language; "visual effect dissolve", which is TinyPowerPoint's
 * push again; "find", searching every card's fields; a second
 * background; the user levels, as a preference that hides tools.
 *)
open Playground

(*****************************************************************************)
(* The stack *)
(*****************************************************************************)

type kind = Button | Field

(* a button or a field, where it is on the card (in the Mac's dots, y
   down), and its script *)
type part = { id : int; kind : kind; name : string; x : int; y : int; w : int; h : int; script : string }

type card = {
  cname : string;
  cparts : part list;
  (* the text of every field on it, the background's included, by id *)
  texts : (int * Text_edit.t) list;
  picture : Bitmap.t;
  cscript : string;
}

type stack = { bg : part list; bg_script : string; cards : card list; stack_script : string }

type tool = Browse | Button_tool | Field_tool | Pencil

(* whose script the editor is showing *)
type target = Of_part of int | Of_card | Of_background | Of_stack

type editor = { target : target; name : string; text : Text_edit.t; problem : string }

type model = {
  stack : stack;
  current : int;
  tool : tool;
  selected : int option;
  (* where on the selected part the mouse holds it *)
  grab : (int * int) option;
  pen : (int * int) option;
  editor : editor option;
  (* what "answer" said, waiting for OK *)
  dialog : string option;
  (* frames left of the screen flashing, HyperCard's beep with the
     sound off *)
  flash : int;
  next_id : int;
  said : string;
  (* the stack's name, and the File menu's dialog *)
  file : File_menu.t;
  was : string list;
  was_down : bool;
}

let card_w = 512
let card_h = 342

let card m = List.nth m.stack.cards m.current
let parts m = m.stack.bg @ (card m).cparts
let find_part m id = List.find_opt (fun p -> p.id = id) (parts m)

let text_of (c : card) id = match List.assoc_opt id c.texts with Some t -> Text_edit.to_string t | None -> ""

(* a change to the current card *)
let with_card m f = { m with stack = { m.stack with cards = List.mapi (fun i c -> if i = m.current then f c else c) m.stack.cards } }

let set_text m id v = with_card m (fun c -> { c with texts = (id, Text_edit.of_string v) :: List.remove_assoc id c.texts })

(* a change to a part, wherever it lives *)
let with_part m id f =
  let change ps = List.map (fun p -> if p.id = id then f p else p) ps in
  let m = { m with stack = { m.stack with bg = change m.stack.bg } } in
  with_card m (fun c -> { c with cparts = change c.cparts })

(*****************************************************************************)
(* The stack's world, as HyperTalk sees it *)
(*****************************************************************************)

let field_named m name =
  List.find_opt (fun p -> p.kind = Field && String.lowercase_ascii p.name = String.lowercase_ascii (String.trim name)) (parts m)

let go_to m (r : Hypertalk.card_ref) =
  let n = List.length m.stack.cards in
  let current =
    match r with
    (* HyperCard's stacks go round: next from the last is the first *)
    | Next -> (m.current + 1) mod n
    | Prev -> (m.current + n - 1) mod n
    | First -> 0
    | Last -> n - 1
    | Numbered k when k >= 1 && k <= n -> k - 1
    | Numbered k -> raise (Hypertalk.Error (Printf.sprintf "no card %d" k))
    | Named s -> (
        let rec find i = function
          | [] -> raise (Hypertalk.Error (Printf.sprintf "no card named %S" s))
          | c :: rest -> if String.lowercase_ascii c.cname = String.lowercase_ascii s then i else find (i + 1) rest
        in
        find 0 m.stack.cards)
  in
  { m with current }

let world : model Hypertalk.world =
  {
    get_field =
      (fun m name ->
        match field_named m name with
        | Some p -> text_of (card m) p.id
        | None -> raise (Hypertalk.Error (Printf.sprintf "no field %S on this card" name)));
    set_field =
      (fun m name v ->
        match field_named m name with
        | Some p -> set_text m p.id v
        | None -> raise (Hypertalk.Error (Printf.sprintf "no field %S on this card" name)));
    go = go_to;
    answer = (fun m s -> { m with dialog = Some s });
    beep = (fun m -> { m with flash = 8 });
    number_of_cards = (fun m -> List.length m.stack.cards);
    card_number = (fun m -> m.current + 1);
    card_name = (fun m -> (card m).cname);
  }

(* A message along the path from an object up: its script, the card's,
   the background's, the stack's. A mistake in any of them is shown as
   HyperCard did, in a dialog, and stops that message only. Arriving on
   another card sends it "openCard", along its own path. *)
let rec send ?(from = []) m msg =
  let before = m.current in
  let path = from @ [ (card m).cscript; m.stack.bg_script; m.stack.stack_script ] in
  let m =
    try Hypertalk.send world (List.map Hypertalk.parse path) msg m
    with Hypertalk.Error e -> { m with dialog = Some ("Script error: " ^ e) }
  in
  if m.current <> before && msg <> "openCard" then send m "openCard" else m

let go m r = try send (go_to m r) "openCard" with Hypertalk.Error e -> { m with dialog = Some e }

(*****************************************************************************)
(* The stack it opens on *)
(*****************************************************************************)

let bg_button id name x script = { id; kind = Button; name; x; y = 300; w = 90; h = 30; script }

let script lines = String.concat "\n" lines

let background =
  [
    { id = 1; kind = Field; name = "Title"; x = 24; y = 14; w = 464; h = 40; script = "" };
    bg_button 2 "Prev" 24 (script [ "on mouseUp"; "  go to prev card"; "end mouseUp" ]);
    bg_button 3 "Next" 398 (script [ "on mouseUp"; "  go to next card"; "end mouseUp" ]);
    { id = 4; kind = Field; name = "Page"; x = 196; y = 302; w = 120; h = 26; script = "" };
  ]

let new_card ?(texts = []) ?(parts = []) ?(cscript = "") ?(picture = Bitmap.create ~width:card_w ~height:card_h) cname =
  { cname; cparts = parts; texts = List.map (fun (id, s) -> (id, Text_edit.of_string s)) texts; picture; cscript }

let field id name x y w h = { id; kind = Field; name; x; y; w; h; script = "" }
let button id name x y w h script = { id; kind = Button; name; x; y; w; h; script }

let clicker =
  script [ "on mouseUp"; "  add 1 to field \"Count\""; "  if field \"Count\" = 10 then answer \"Ten clicks!\""; "end mouseUp" ]

let drawing =
  Bitmap.change (Bitmap.create ~width:card_w ~height:card_h) (fun b ->
      Paint.fill_oval b Pattern.grey (380, 150) (450, 220);
      Paint.frame_oval b Pattern.solid (380, 150) (450, 220);
      Paint.stroke b ~brush:Paint.pencil Pattern.solid (330, 280) (500, 280);
      Paint.stroke b ~brush:(Paint.round 1) Pattern.solid (340, 280) (360, 240);
      Paint.stroke b ~brush:(Paint.round 1) Pattern.solid (360, 240) (380, 280))

let opening =
  {
    bg = background;
    bg_script = "";
    stack_script = script [ "on openCard"; "  put the number of this card & \" of \" & the number of cards into field \"Page\""; "end openCard" ];
    cards =
      [
        new_card "Home"
          ~texts:
            [
              (1, "TinyHyperCard, 1987");
              ( 5,
                "Bill Atkinson's HyperCard came free with every Macintosh: a stack of cards the size of the \
                 screen, with buttons and fields, and a language to make them do things.\n\n\
                 Click Next. Later, choose the button tool, click a button, and choose Objects > Script to read \
                 what it does." );
            ]
          ~parts:[ field 5 "About" 24 66 464 220 ];
        new_card "Clicks"
          ~texts:[ (1, "A button, and its script"); (6, "0"); (8, clicker) ]
          ~parts:[ field 6 "Count" 196 70 120 36; button 7 "Click me" 186 120 140 36 clicker; field 8 "The script" 24 172 464 110 ];
        new_card "The message path"
          ~texts:
            [
              (1, "Where a click goes");
              (10, "");
              ( 12,
                "\"Pass it on\" answers the click and passes it on: the card's own script answers it next. \
                 \"Clear\" answers it and does not pass it." );
            ]
          ~parts:
            [
              button 9 "Pass it on" 24 76 170 36
                (script [ "on mouseUp"; "  put \"the button, which passes it\" & return after field \"Log\""; "  pass mouseUp"; "end mouseUp" ]);
              button 11 "Clear" 24 126 170 36 (script [ "on mouseUp"; "  put empty into field \"Log\""; "end mouseUp" ]);
              field 10 "Log" 214 66 274 150;
              field 12 "How" 24 226 464 60;
            ]
          ~cscript:(script [ "on mouseUp"; "  put \"then the card\" & return after field \"Log\""; "end mouseUp" ]);
        new_card "Your turn"
          ~texts:
            [
              (1, "Your turn");
              ( 13,
                "Objects > New Button, then the button tool to drag it where it goes, and Objects > Script to \
                 write what it does. The pencil draws on the card -- Atkinson wrote MacPaint first." );
            ]
          ~parts:[ field 13 "Help" 24 66 300 150 ]
          ~picture:drawing;
      ];
  }

let initial =
  let m =
    {
      stack = opening;
      current = 0;
      tool = Browse;
      selected = None;
      grab = None;
      pen = None;
      editor = None;
      dialog = None;
      flash = 0;
      next_id = 100;
      said = "";
      file = File_menu.start;
      was = [];
      was_down = false;
    }
  in
  send m "openCard"

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

(* the card shown 1.6 times the Mac's size, its top-left corner here *)
let k = 1.6
let card_left = -410.
let card_top = 300.

let box_of (p : part) : Widget.box =
  let w = float_of_int p.w *. k and h = float_of_int p.h *. k in
  { Widget.x = card_left +. (float_of_int p.x *. k) +. (w /. 2.); y = card_top -. (float_of_int p.y *. k) -. (h /. 2.); w; h }

let dot_at (x, y) = (int_of_float ((x -. card_left) /. k), int_of_float ((card_top -. y) /. k))
let on_card (x, y) = x >= 0 && y >= 0 && x < card_w && y < card_h

(* the part under a point, the one drawn last first *)
let part_at m (mx, my) = List.find_opt (fun p -> Widget.contains (box_of p) mx my) (List.rev (parts m))

let tools = [ (Browse, "Browse"); (Button_tool, "Button"); (Field_tool, "Field"); (Pencil, "Pencil") ]
let tool_box i : Widget.box = { Widget.x = 455.; y = 250. -. (float_of_int i *. 48.); w = 70.; h = 40. }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* HyperCard's File menu had no Save: a stack was saved as it changed
   (see [autosave] in update) *)
let file_items = [ "File"; "New"; "Open..."; "Save As..."; "Export" ]

let menus =
  [
    file_items;
    [ "Go"; "First"; "Prev"; "Next"; "Last" ];
    [ "Tools"; "Browse"; "Button"; "Field"; "Pencil" ];
    [ "Objects"; "Script..."; "Card Script..."; "Background Script..."; "Stack Script..."; "New Button"; "New Field"; "New Card"; "Delete Part" ];
  ]

let menu_box i : Widget.box = { Widget.x = -410. +. (float_of_int i *. 110.); y = 470.; w = 105.; h = 30. }

let open_editor m target =
  let name, text =
    match target with
    | Of_part id -> ( match find_part m id with Some p -> (p.name, p.script) | None -> ("", ""))
    | Of_card -> ((card m).cname, (card m).cscript)
    | Of_background -> ("", m.stack.bg_script)
    | Of_stack -> ("", m.stack.stack_script)
  in
  { m with editor = Some { target; name; text = Text_edit.of_string text; problem = "" } }

(* a new part on the card, in its middle, selected, with the tool to
   move it *)
let add_part m kind =
  let id = m.next_id in
  let p =
    match kind with
    | Button -> button id "New Button" 196 150 120 34 (script [ "on mouseUp"; "  beep"; "end mouseUp" ])
    | Field -> field id "New Field" 176 130 160 70
  in
  let m = with_card m (fun c -> { c with cparts = c.cparts @ [ p ] }) in
  { m with next_id = id + 1; selected = Some id; tool = (if kind = Button then Button_tool else Field_tool) }

let command c m =
  match c with
  | "First" -> go m First
  | "Prev" -> go m Prev
  | "Next" -> go m Next
  | "Last" -> go m Last
  | "Browse" -> { m with tool = Browse; selected = None }
  | "Button" -> { m with tool = Button_tool; selected = None }
  | "Field" -> { m with tool = Field_tool; selected = None }
  | "Pencil" -> { m with tool = Pencil; selected = None }
  | "Script..." -> ( match m.selected with Some id -> open_editor m (Of_part id) | None -> { m with said = "select a button or a field first" })
  | "Card Script..." -> open_editor m Of_card
  | "Background Script..." -> open_editor m Of_background
  | "Stack Script..." -> open_editor m Of_stack
  | "New Button" -> add_part m Button
  | "New Field" -> add_part m Field
  | "New Card" ->
      let c = new_card (Printf.sprintf "Card %d" (List.length m.stack.cards + 1)) in
      let cards = List.concat (List.mapi (fun i x -> if i = m.current then [ x; c ] else [ x ]) m.stack.cards) in
      go { m with stack = { m.stack with cards } } Next
  | "Delete Part" -> (
      match m.selected with
      | Some id ->
          let keep = List.filter (fun p -> p.id <> id) in
          let m = { m with stack = { m.stack with bg = keep m.stack.bg } } in
          { (with_card m (fun c -> { c with cparts = keep c.cparts })) with selected = None }
      | None -> m)
  | _ -> m

(* the script editor: a name, the script, and Close -- which checks it
   and keeps it, mistakes and all, saying where the mistake is *)
let edit_script computer m (e : editor) =
  let panel_top = 330. in
  let is_part = match e.target with Of_part _ | Of_card -> true | _ -> false in
  let name = if is_part then Gui.field_in computer { Widget.x = 30.; y = panel_top -. 40.; w = 380.; h = 32. } e.name else e.name in
  let text = Gui.text_area_in computer { Widget.x = 0.; y = panel_top -. 280.; w = 760.; h = 440. } e.text in
  let e = { e with name; text } in
  if Gui.button_in computer { Widget.x = 320.; y = panel_top -. 40.; w = 110.; h = 32. } "Close" then
    let s = Text_edit.to_string e.text in
    let problem = match Hypertalk.parse s with _ -> "" | exception Hypertalk.Error msg -> msg in
    let m =
      match e.target with
      | Of_part id -> with_part m id (fun p -> { p with name = e.name; script = s })
      | Of_card -> with_card m (fun c -> { c with cname = e.name; cscript = s })
      | Of_background -> { m with stack = { m.stack with bg_script = s } }
      | Of_stack -> { m with stack = { m.stack with stack_script = s } }
    in
    { m with editor = None; said = (if problem = "" then "script kept" else "script kept, but " ^ problem) }
  else { m with editor = Some e }

(* a stack is saved as the stack it is: backgrounds, cards, their
   fields' texts, pictures and scripts -- all data *)
let kind = { File_menu.magic = "TinyHyperCard 1"; extension = ".stack" }

let reopened (r : stack File_menu.result) model =
  match r with
  | File_menu.Nothing -> model
  | File_menu.New -> { initial with stack = { opening with cards = [ { (List.hd opening.cards) with cparts = []; texts = []; cscript = "" } ] }; file = model.file }
  | File_menu.Opened stack -> send { initial with stack; current = 0; file = model.file } "openStack"

let update caps computer model =
  let m = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  if File_menu.busy model.file then
    let file, r = File_menu.dialog caps kind computer ~current:(fun () -> model.stack) model.file in
    reopened r { model with file; was_down = m.mdown; was = now }
  else
  let before = model.stack in
  let pressed key = List.mem key now && not (List.mem key model.was) in
  let press = m.mdown && not model.was_down in
  let model =
    match (model.editor, model.dialog) with
    | Some e, _ -> edit_script computer model e
    | None, Some _ ->
        (* "answer": the message, and OK *)
        if Gui.button_in computer { Widget.x = 0.; y = -20.; w = 120.; h = 36. } "OK" || pressed "Enter" then { model with dialog = None } else model
    | None, None ->
        let model =
          List.fold_left
            (fun model (i, items) ->
              if i = 0 then
                let file, r = File_menu.menu_in ~items:file_items caps kind computer (menu_box i) ~current:(fun () -> model.stack) model.file in
                reopened r { model with file }
              else
              match List.nth_opt items (Gui.menu_in computer (menu_box i) items 0) with Some c when c <> List.hd items -> command c model | _ -> model)
            model
            (List.mapi (fun i items -> (i, items)) menus)
        in
        if Gui.modal () then model
        else
          (* the tool palette *)
          let model =
            match List.find_opt (fun (i, _) -> press && Widget.contains (tool_box i) m.mx m.my) (List.mapi (fun i t -> (i, t)) tools) with
            | Some (_, (tool, _)) -> { model with tool; selected = None }
            | None -> model
          in
          let at = dot_at (m.mx, m.my) in
          match model.tool with
          | Browse ->
              (* every field a live text area of the toolkit, holding
                 its text for this card *)
              let model =
                List.fold_left
                  (fun model (p : part) ->
                    if p.kind <> Field then model
                    else
                      let c = card model in
                      let edit = match List.assoc_opt p.id c.texts with Some t -> t | None -> Text_edit.of_string "" in
                      let edit' = Gui.text_area_in computer (box_of p) edit in
                      if edit' == edit then model else with_card model (fun c -> { c with texts = (p.id, edit') :: List.remove_assoc p.id c.texts }))
                  model (parts model)
              in
              let model =
                match part_at model (m.mx, m.my) with
                | Some ({ kind = Button; _ } as p) when press -> send ~from:[ p.script ] model "mouseUp"
                | _ -> model
              in
              if pressed "ArrowRight" then go model Next else if pressed "ArrowLeft" then go model Prev else model
          | Button_tool | Field_tool -> (
              let kind = if model.tool = Button_tool then Button else Field in
              let x, y = at in
              match (press, model.grab, model.selected) with
              (* only a press on the card: one on a menu's title is not
                 yet the menu's, and must not drop the selection *)
              | true, _, _ when on_card at -> (
                  match List.find_opt (fun p -> p.kind = kind) (Option.to_list (part_at model (m.mx, m.my))) with
                  | Some p -> { model with selected = Some p.id; grab = Some (x - p.x, y - p.y) }
                  | None -> { model with selected = None; grab = None })
              | false, Some (gx, gy), Some id when m.mdown ->
                  with_part model id (fun p ->
                      { p with x = max 0 (min (card_w - p.w) (x - gx)); y = max 0 (min (card_h - p.h) (y - gy)) })
              | false, _, _ when not m.mdown -> { model with grab = None }
              | _ -> model)
          | Pencil -> (
              match (m.mdown, model.pen) with
              | true, last when on_card at || last <> None ->
                  let from = Option.value last ~default:at in
                  let model = with_card model (fun c -> { c with picture = Bitmap.change c.picture (fun b -> Paint.stroke b ~brush:Paint.pencil Pattern.solid from at) }) in
                  { model with pen = Some at }
              | _ -> { model with pen = None })
  in
  (* HyperCard's saving: whenever the stack has changed, to its name --
     cheap to find out, a physical comparison first *)
  let model =
    if model.stack != before && model.stack <> before then
      { model with file = File_menu.autosave caps kind ~current:(fun () -> model.stack) model.file }
    else model
  in
  { model with flash = max 0 (model.flash - 1); was = now; was_down = m.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* The card's picture as rectangles, remembered for the bitmap it was
   made from (compared by ==: a picture is never changed in place) *)
let drawn : (Bitmap.t * shape list) list ref = ref []

let picture_shapes bits =
  match List.find_opt (fun (b, _) -> b == bits) !drawn with
  | Some (_, s) -> s
  | None ->
      let s =
        List.map
          (fun (x, y, w, h) ->
            rectangle black (float_of_int w *. k) (float_of_int h *. k)
            |> move (card_left +. ((float_of_int x +. (float_of_int w /. 2.)) *. k)) (card_top -. ((float_of_int y +. (float_of_int h /. 2.)) *. k)))
          (Bitmap.rectangles bits)
      in
      drawn := List.filteri (fun i _ -> i < 5) ((bits, s) :: !drawn);
      s

(* a field's text, when it is not a live text area: drawn *)
let text_shapes (b : Widget.box) text =
  let r = Rich.of_string ~style:{ Style.plain with size = 17. } text in
  let page = Page.layout ~metrics:Stroke_text.metrics ~width:(b.w -. 12.) r in
  let left = Widget.left b +. 6. and top = Widget.top b -. 4. in
  List.concat_map
    (fun (g : Page.glyph) ->
      let y = top -. g.baseline in
      if g.text = "\n" || g.text = " " || y < Widget.bottom b then []
      else Stroke_text.glyph black g.style g.text ~x:(left +. g.x) ~baseline:y)
    (Page.glyphs page)

let view _computer model =
  let c = card model in
  let cw = float_of_int card_w *. k and ch = float_of_int card_h *. k in
  let cx = card_left +. (cw /. 2.) and cy = card_top -. (ch /. 2.) in
  let authoring = model.tool = Button_tool || model.tool = Field_tool in
  let part_shapes (p : part) =
    let b = box_of p in
    match p.kind with
    | Button ->
        (* a round-cornered button with its shadow, as HyperCard drew
           them *)
        [
          rectangle black b.w b.h |> move (b.x +. 3.) (b.y -. 3.);
          rectangle black b.w b.h |> move b.x b.y;
          rectangle white (b.w -. 4.) (b.h -. 4.) |> move b.x b.y;
          words black p.name |> move b.x b.y;
        ]
    | Field ->
        (* browsing, a field is a text area and the toolkit draws it *)
        if model.tool = Browse && model.editor = None && model.dialog = None then []
        else [ rectangle black b.w b.h |> move b.x b.y; rectangle white (b.w -. 2.) (b.h -. 2.) |> move b.x b.y ] @ text_shapes b (text_of c p.id)
  in
  let outlines =
    if not authoring then []
    else
      List.concat_map
        (fun (p : part) ->
          let b = box_of p in
          let wanted = (model.tool = Button_tool && p.kind = Button) || (model.tool = Field_tool && p.kind = Field) in
          if Some p.id = model.selected then Gui.shapes (Widget.frame (rgb 40 90 200) 3. { b with w = b.w +. 8.; h = b.h +. 8. })
          else if wanted then Gui.shapes (Widget.frame (rgb 150 150 150) 1. { b with w = b.w +. 6.; h = b.h +. 6. })
          else [])
        (parts model)
  in
  let palette =
    List.concat
      (List.mapi
         (fun i (tool, name) ->
           let b = tool_box i in
           let on = tool = model.tool in
           [ rectangle (if on then black else white) b.w b.h |> move b.x b.y ]
           @ Gui.shapes (Widget.frame black 1. b)
           @ [ words (if on then white else black) name |> move b.x b.y ])
         tools)
  in
  let overlay =
    match (model.editor, model.dialog) with
    | Some e, _ ->
        let whose =
          match e.target with
          | Of_part id -> ( match find_part model id with Some { kind = Button; _ } -> "button" | _ -> "field")
          | Of_card -> "card"
          | Of_background -> "background"
          | Of_stack -> "stack"
        in
        [
          rectangle (rgb 120 120 120) 1000. 1000. |> fade 0.5;
          rectangle white 800. 560. |> move 0. 50.;
          words black (Printf.sprintf "Script of the %s" whose) |> move (-280.) 290.;
        ]
        @ Gui.shapes (Widget.frame black 2. { Widget.x = 0.; y = 50.; w = 800.; h = 560. })
    | None, Some text ->
        [ rectangle (rgb 120 120 120) 1000. 1000. |> fade 0.4; rectangle white 520. 200. |> move 0. 40. ]
        @ Gui.shapes (Widget.frame black 3. { Widget.x = 0.; y = 40.; w = 520.; h = 200. })
        @ [ words black text |> move 0. 80. ]
    | None, None -> []
  in
  let tool_name = List.assoc model.tool tools in
  [
    rectangle (rgb 150 150 150) 1000. 1000.;
    rectangle (Gui.theme ()).face 1000. 40. |> move 0. 470.;
    rectangle white cw ch |> move cx cy;
  ]
  @ picture_shapes c.picture
  @ List.concat_map part_shapes (parts model)
  @ outlines
  @ Gui.shapes (Widget.frame black 2. { Widget.x = cx; y = cy; w = cw +. 4.; h = ch +. 4. })
  @ (if model.flash > 0 then [ rectangle black cw ch |> move cx cy |> fade 0.5 ] else [])
  @ palette
  @ [
      words (rgb 30 30 30)
        (Printf.sprintf "%s     card %d of %d, %S -- %s%s"
           (if File_menu.said model.file <> "" then File_menu.said model.file else File_menu.title model.file)
           (model.current + 1) (List.length model.stack.cards) c.cname tool_name
           (if model.said = "" then "" else "     " ^ model.said))
      |> move 0. (-300.);
    ]
  @ overlay @ File_menu.view model.file @ Gui.draw ()

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> File_menu.caps)))
