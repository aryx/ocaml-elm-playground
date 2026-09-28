(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyLotus123: the program that sold the IBM PC
 * (Mitch Kapor and Jonathan Sachs, Lotus Development, 1983).
 *
 * The middle of three. TinyVisiCalc (1979) and TinyExcel (1985) are
 * the same engine, appkits/sheet, with two interfaces; this is the
 * third, and it is the one that sits between them in time and in
 * ideas. What 1-2-3 added to VisiCalc, in the order this file shows
 * it:
 *
 *   - **natural order**. VisiCalc recalculated row by row or column
 *     by column (TinyVisiCalc's /G O R and /G O C); 1-2-3
 *     recalculated in the order of the dependencies, the engine's
 *     own [Sheet.set]. /Worksheet Global Recalculation keeps the two
 *     old orders beside it, F9 being 1-2-3's recalculate key;
 *   - **the menu that explains itself**. / opens a line of words,
 *     the highlighted one explained on the line below -- or, for a
 *     word that opens another menu, that menu's words, so you can see
 *     one level ahead. The arrows and Enter, or the first letter: a
 *     beginner reads, an expert types /GTB without looking. Between
 *     VisiCalc's letters you had to know and Excel's menu bar, and the
 *     DOS programs of the next ten years copied it. (Copied enough for
 *     a lawsuit: Lotus v. Borland, 1990-96, over whether a menu's tree
 *     of words can be owned; the Supreme Court split 4 to 4.)
 *   - **pointing** without a mouse. A command asking for a range
 *     (POINT mode) lets you type it, B4..B7, or walk the cell
 *     pointer to one corner, press '.' to anchor it, and walk to the
 *     other, the range lighting up as it grows;
 *   - **1, 2, 3**: the worksheet, the graph and the database in one
 *     program, over the same cells. /Graph draws the ranges you give
 *     it, full screen, the PC switching from text to CGA graphics and
 *     back; /Data Sort treats the rows of a range as records. VisiCalc
 *     needed VisiPlot and VisiFile for those, separate programs and
 *     separate disks;
 *   - **keystroke macros**, and this is the idea worth the program.
 *     A macro is text in cells -- /GRTB, {DOWN}, ~ for Enter -- the
 *     very keys you would type, and a range named \G (/Range Name
 *     Create) is run by Alt-G: the keys are fed to the program as if
 *     typed, cell after cell down the column until a blank one. So the
 *     person who uses the program can program it, in the language they
 *     already speak, its keystrokes; and the program is in the sheet,
 *     saved with it, visible, editable like any other label. The
 *     opening sheet has two, in column F.
 *
 * The one mechanism under the last idea: every key, typed or read
 * from a cell, goes through the same function, [press]. The keyboard
 * is one source of keys and a macro another ([step_macro], a few keys
 * a frame so that you can watch it type), and nothing else in the
 * program knows which it was given. A macro language made this way
 * costs nothing more than a parser for {DOWN} -- and inherits every
 * command, because every command is already a sequence of keys.
 *
 * Also 1983's: formulas spelled +B4*C4 and @SUM(B4..B7), two dots for
 * a range (translated to the engine's =SUM(B4:B7) and back, as
 * TinyVisiCalc translates 1979's); labels with a prefix saying how to
 * align them -- ' left, a double quote right, ^ centred -- and a left one flowing
 * over the empty cells to its right, which is how a macro longer than
 * a column is still readable; ERR in a cell whose formula failed; an
 * entry that does not parse is not refused, it sends you to EDIT mode
 * (F2 edits the cell under the pointer, too); F5 goes to a cell or a
 * range name; the mode at the top right (READY, LABEL, VALUE, EDIT,
 * MENU, POINT) and CMD at the bottom while a macro runs. The whole of
 * it is drawn as the PC's 80 by 25 text screen, through a Vt as the
 * Textmode way does (TinyTurboPascal), except when a graph is shown.
 *
 * What it uses: appkits/sheet (Sheet and its Formula, the same engine
 * as TinyVisiCalc and TinyExcel), appkits/document's Saved (the file),
 * libs/terminal's Curses and Vt, and the Teletype way's draw_screen for
 * the IBM PC's colours. What it does not use: gui/ (no mouse, no
 * widget: a PC of 1983 had neither), and Part_chart, whose bars are a
 * part of a document, where these are a mode of the screen.
 *
 * What it deliberately does not do: $A$1 (Formula has no absolute
 * references yet, so /Copy moves every reference -- the one thing
 * 1-2-3 had that the engine lacks), @IF and comparisons (same
 * reason), range names inside formulas (@SUM(SALES)), /Move, /Print,
 * /Worksheet Insert and Delete, column widths, formats (Fixed,
 * Currency, Percent), /Data Query and /Data Table, and more than two
 * data ranges in a graph (1-2-3 had A to F) or any graph option.
 *
 * Exercises: $A$1 in Formula, then /Copy keeping what is fixed; the
 * /X commands, which made macros a programming language -- /XG
 * (goto a cell and read the macro from there), /XI (if; needs
 * comparisons), /XQ (quit), /XM (a menu of your own, in cells);
 * Learn mode (Release 2.2): keys typed recorded into a range, the
 * macro written for you; /Data Query, a criteria range written in
 * cells above the data; Release 3's worksheets in three dimensions
 * (A:B4, 1989), before Excel's workbooks.
 *)
open Playground

(*****************************************************************************)
(* The keys *)
(*****************************************************************************)

(* what the program reacts to, whether typed or read from a macro *)
type key =
  | Char of char
  | Enter
  | Escape
  | Backspace
  | Up
  | Down
  | Left
  | Right
  | Home
  | Edit (* F2 *)
  | Goto (* F5 *)
  | Calc (* F9 *)
  | Alt of char

let named_keys =
  [ ("Enter", Enter); ("Escape", Escape); ("Backspace", Backspace); ("ArrowUp", Up); ("ArrowDown", Down);
    ("ArrowLeft", Left); ("ArrowRight", Right); ("Home", Home); ("F2", Edit); ("F5", Goto); ("F9", Calc) ]

(* the keys of this frame: the characters typed, then the named keys
   that went down; with Alt held, a letter is Alt and the letter *)
let keys_of (k : keyboard) ~(before : string list) : key list =
  let now = Set_.elements k.keys in
  let went_down = List.filter (fun n -> not (List.mem n before)) now in
  let chars = List.filter (fun c -> c >= ' ' && c < '\127') (List.init (String.length k.typed) (String.get k.typed)) in
  if List.mem "Alt" now then
    let letters = if chars <> [] then chars else List.filter_map (fun n -> if String.length n = 1 then Some n.[0] else None) went_down in
    List.map (fun c -> Alt (Char.uppercase_ascii c)) letters
  else List.map (fun c -> Char c) chars @ List.filter_map (fun n -> List.assoc_opt n named_keys) went_down

(* the macro language: a character is itself, ~ is Enter, and the
   other keys have names in braces *)
let macro_names =
  [ ("DOWN", Down); ("UP", Up); ("LEFT", Left); ("RIGHT", Right); ("HOME", Home); ("ESC", Escape);
    ("BS", Backspace); ("EDIT", Edit); ("GOTO", Goto); ("CALC", Calc) ]

let macro_keys (s : string) : (key list, string) result =
  let n = String.length s in
  let rec go i acc =
    if i >= n then Ok (List.rev acc)
    else
      match s.[i] with
      | '~' -> go (i + 1) (Enter :: acc)
      | '{' -> (
          match String.index_from_opt s i '}' with
          | None -> Error "a { with no }"
          | Some j -> (
              let name = String.uppercase_ascii (String.sub s (i + 1) (j - i - 1)) in
              match List.assoc_opt name macro_names with
              | Some k -> go (j + 1) (k :: acc)
              | None -> Error ("no key called {" ^ name ^ "}")))
      | c -> go (i + 1) (Char c :: acc)
  in
  go 0 []

(*****************************************************************************)
(* 1983's spelling *)
(*****************************************************************************)

(* what starts a value rather than a label, as 1-2-3 decided it from
   the first character typed *)
let is_value_start c = String.contains "0123456789+-.(@#$" c

(* ' left, a double quote right, ^ centred: a label's first character *)
let is_prefix c = c = '\'' || c = '"' || c = '^'
let is_letter c = c >= 'A' && c <= 'Z'

(* +B4*C4 and @SUM(B4..B7) into the engine's =B4*C4 and =SUM(B4:B7) *)
let of_123 (s : string) : string =
  match float_of_string_opt s with
  | Some _ -> s
  | None ->
      let s = String.uppercase_ascii s in
      let n = String.length s in
      let buf = Buffer.create n in
      let i = ref (if n > 0 && s.[0] = '+' then 1 else 0) in
      while !i < n do
        if s.[!i] = '@' then incr i
        else if s.[!i] = '.' && !i + 1 < n && s.[!i + 1] = '.' then begin
          (* two dots or more: a range *)
          Buffer.add_char buf ':';
          while !i < n && s.[!i] = '.' do incr i done
        end
        else begin
          Buffer.add_char buf s.[!i];
          incr i
        end
      done;
      "=" ^ Buffer.contents buf

(* and back: an @ before each function, a + before a formula starting
   with a cell (A1 alone would be a label) *)
let to_123 (raw : string) : string =
  if String.length raw > 0 && raw.[0] = '=' then begin
    let body = String.sub raw 1 (String.length raw - 1) in
    let n = String.length body in
    let buf = Buffer.create n in
    String.iteri
      (fun i c ->
        if is_letter c && (i = 0 || not (is_letter body.[i - 1])) then begin
          let j = ref i in
          while !j < n && is_letter body.[!j] do incr j done;
          if !j < n && body.[!j] = '(' then Buffer.add_char buf '@'
        end;
        if c = ':' then Buffer.add_string buf ".." else Buffer.add_char buf c)
      body;
    let s = Buffer.contents buf in
    if n > 0 && is_letter body.[0] && s.[0] <> '@' then "+" ^ s else s
  end
  else raw

(* what an entry is stored as: a label keeps its prefix, so that
   '1983 stays text *)
let engine_text (typed : string) : string =
  let s = String.trim typed in
  if s = "" then ""
  else if is_prefix s.[0] then s
  else if is_value_start s.[0] then of_123 s
  else "'" ^ s

let strip_prefix (s : string) : string =
  if s <> "" && is_prefix s.[0] then String.sub s 1 (String.length s - 1) else s

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type range = Formula.cell * Formula.cell
type graph_kind = Line | Bar | Pie

(* 1-2-3 had six data ranges, A to F; two are enough to compare *)
type graph = { kind : graph_kind; x : range option; a : range option; b : range option }

(* what a file holds: the cells, their names and the graph -- a .WKS
   kept all three, so a macro and the graph it draws travel with the
   sheet *)
type worksheet = { sheet : Sheet.t; names : (string * range) list; graph : graph }

(* a menu: words, each explained, each doing something or opening
   another menu *)
type item = { name : string; help : string; does : does }
and does = Sub of item list | Act of (model -> model)

and mode =
  | Ready
  | Entry of { text : string; edit : bool }
  (* the menu shown, the word highlighted, and the menus above it *)
  | Menu of { items : item list; at : int; up : (item list * int) list }
  | Prompt of prompt
  | Viewing (* the graph, full screen *)

(* a question on the second line; [point] while the answer is being
   pointed at rather than typed, from [anchor] (after '.') to the
   cell pointer, which goes back [home] when it is answered *)
and prompt = {
  question : string;
  answer : string;
  point : bool;
  anchor : Formula.cell option;
  home : Formula.cell;
  ok : string -> model -> model;
}

and model = {
  ws : worksheet;
  cursor : Formula.cell;
  corner : Formula.cell; (* the top-left cell on the screen *)
  mode : mode;
  (* None is natural order, 1983's; Some is VisiCalc's *)
  order : Sheet.order option;
  (* a macro running: the cell to read next, and the keys left of the
     one read last *)
  macro : (Formula.cell * key list) option;
  message : string;
  was : string list;
}

(* the table, a total, and two macros in column F: \G draws the table,
   \S sorts its rows by region *)
let opening =
  [
    ((0, 0), "'Sales by region, in thousands");
    ((0, 2), "'Region"); ((1, 2), "\"1982"); ((2, 2), "\"1983");
    ((0, 3), "'North"); ((1, 3), "120"); ((2, 3), "150");
    ((0, 4), "'South"); ((1, 4), "80"); ((2, 4), "95");
    ((0, 5), "'East"); ((1, 5), "45"); ((2, 5), "70");
    ((0, 6), "'West"); ((1, 6), "60"); ((2, 6), "40");
    ((0, 8), "'Total"); ((1, 8), "=SUM(B4:B7)"); ((2, 8), "=SUM(C4:C7)");
    ((4, 2), "'\\G"); ((5, 2), "'/GRTB"); ((5, 3), "'XA4..A7~AB4..B7~BC4..C7~V");
    ((4, 5), "'\\S"); ((5, 5), "'/DSA4..C7~A4~A~");
    ((0, 11), "'Alt-G graphs the table, Alt-S sorts it: the macros are column F");
  ]

let initial =
  {
    ws =
      {
        sheet = List.fold_left (fun s (c, text) -> Sheet.set c text s) Sheet.empty opening;
        names = [ ("\\G", ((5, 2), (5, 2))); ("\\S", ((5, 5), (5, 5))) ];
        graph = { kind = Bar; x = None; a = None; b = None };
      };
    cursor = (0, 0);
    corner = (0, 0);
    mode = Ready;
    order = None;
    macro = None;
    message = "";
    was = [];
  }

(*****************************************************************************)
(* The sheet *)
(*****************************************************************************)

(* the screen: 80 by 25, the sheet under three lines of control panel *)
let width = 80
let height = 25
let col_chars = 9
let border = 4
let shown_cols = 8
let shown_rows = 20
let first_row = 4

(* the cell pointer moved, the screen following it; 256 columns and
   2048 rows, 1-2-3's sheet *)
let go_to (c, r) model =
  let c = max 0 (min 255 c) and r = max 0 (min 2047 r) in
  let follow x corner shown = if x < corner then x else if x >= corner + shown then x - shown + 1 else corner in
  let cc, cr = model.corner in
  { model with cursor = (c, r); corner = (follow c cc shown_cols, follow r cr shown_rows) }

let step key model =
  let c, r = model.cursor in
  match key with
  | Up -> go_to (c, r - 1) model
  | Down -> go_to (c, r + 1) model
  | Left -> go_to (c - 1, r) model
  | Right -> go_to (c + 1, r) model
  | Home -> go_to (0, 0) model
  | _ -> model

let with_sheet sheet model = { model with ws = { model.ws with sheet } }

(* one cell changed, and recalculated in the order chosen *)
let set_cell c text model =
  match model.order with
  | None -> with_sheet (Sheet.set c text model.ws.sheet) model
  | Some order -> with_sheet (Sheet.recalculate order (Sheet.store c text model.ws.sheet)) model

(* what a cell holds, moved by (dc, dr): a formula's references move
   with it (Formula.shift), which is all /Copy and /Data Sort need *)
let moved (dc, dr) raw =
  match Formula.content_of raw with
  | Formula.Formula e -> "=" ^ Formula.to_string (Formula.shift (dc, dr) e)
  | _ -> raw

let name_of = Formula.name_of_cell
let range_text (a, b) = if a = b then name_of a else name_of a ^ ".." ^ name_of b

(* B4..C7, B4, or a range's name *)
let range_of (ws : worksheet) (s : string) : range option =
  let s = String.uppercase_ascii (String.trim s) in
  match List.assoc_opt s ws.names with
  | Some r -> Some r
  | None -> (
      match List.filter (( <> ) "") (String.split_on_char '.' s) |> List.map Formula.cell_of_name with
      | [ Some a ] -> Some (a, a)
      | [ Some (c1, r1); Some (c2, r2) ] -> Some ((min c1 c2, min r1 r2), (max c1 c2, max r1 r2))
      | _ -> None)

let cells_of ((c1, r1), (c2, r2)) : Formula.cell list =
  List.concat (List.init (r2 - r1 + 1) (fun r -> List.init (c2 - c1 + 1) (fun c -> (c1 + c, r1 + r))))

(* the source copied into each place of the destination it fits:
   one cell copied to a column fills the column *)
let copy (((c1, r1), (c2, r2)) as from) ((tc1, tr1), (tc2, tr2)) model =
  let w = c2 - c1 + 1 and h = r2 - r1 + 1 in
  let raws = List.map (fun (c, r) -> ((c - c1, r - r1), Sheet.raw model.ws.sheet (c, r))) (cells_of from) in
  let across = max 1 ((tc2 - tc1 + 1) / w) and down = max 1 ((tr2 - tr1 + 1) / h) in
  let places = cells_of ((0, 0), (across - 1, down - 1)) in
  List.fold_left
    (fun model (i, j) ->
      let dc = tc1 + (i * w) - c1 and dr = tr1 + (j * h) - r1 in
      List.fold_left (fun model ((x, y), raw) -> set_cell (c1 + x + dc, r1 + y + dr) (moved (dc, dr) raw) model) model raws)
    model places

(* the rows of a range as records, sorted by the column of [key] *)
let sort (((c1, r1), (c2, r2)) : range) (key_col : int) (descending : bool) model =
  let sheet = model.ws.sheet in
  let compare_values a b =
    match (Sheet.value sheet (key_col, a), Sheet.value sheet (key_col, b)) with
    | Sheet.Number x, Sheet.Number y -> compare x y
    | Sheet.Number _, _ -> -1
    | _, Sheet.Number _ -> 1
    | x, y -> compare (Sheet.show x) (Sheet.show y)
  in
  let rows = List.init (r2 - r1 + 1) (fun i -> r1 + i) in
  let sorted = List.stable_sort (fun a b -> if descending then compare_values b a else compare_values a b) rows in
  let records = List.map (fun r -> (r, List.init (c2 - c1 + 1) (fun i -> Sheet.raw sheet (c1 + i, r)))) sorted in
  List.fold_left
    (fun model (i, (old_row, raws)) ->
      let row = r1 + i in
      List.fold_left (fun model (j, raw) -> set_cell (c1 + j, row) (moved (0, row - old_row) raw) model) model
        (List.mapi (fun j raw -> (j, raw)) raws))
    model
    (List.mapi (fun i record -> (i, record)) records)

(*****************************************************************************)
(* The menus *)
(*****************************************************************************)

let act name help f = { name; help; does = Act f }

(* a menu's explanation is the menu below it: one level ahead *)
let sub name items = { name; help = String.concat "  " (List.map (fun i -> i.name) items); does = Sub items }

let ask ?(point = false) question ok model =
  { model with mode = Prompt { question; answer = ""; point; anchor = None; home = model.cursor; ok } }

let ask_range question ok model =
  ask ~point:true question
    (fun s model -> match range_of model.ws s with Some r -> ok r model | None -> { model with message = "Invalid range: " ^ s })
    model

let with_graph f model = { model with ws = { model.ws with graph = f model.ws.graph } }

(* the Graph menu stays until Quit, as 1-2-3's did: settings are made
   one after the other, and View between them *)
let rec graph_items () : item list =
  let back model = { model with mode = Menu { items = graph_items (); at = 0; up = [] } } in
  let kind k name help = act name help (fun m -> back (with_graph (fun g -> { g with kind = k }) m)) in
  let data name help set = act name help (ask_range ("Enter " ^ name ^ " range:") (fun r m -> back (with_graph (set r) m))) in
  [
    sub "Type" [ kind Line "Line" "Line graph"; kind Bar "Bar" "Bar graph"; kind Pie "Pie" "Pie chart: the A range only" ];
    data "X" "Set X range: the labels along the bottom" (fun r g -> { g with x = Some r });
    data "A" "Set first data range" (fun r g -> { g with a = Some r });
    data "B" "Set second data range" (fun r g -> { g with b = Some r });
    act "Reset" "Cancel all graph settings" (fun m -> back (with_graph (fun _ -> initial.ws.graph) m));
    act "View" "View the current graph" (fun m ->
        if m.ws.graph.a = None then back { m with message = "No data range: set A first" } else { m with mode = Viewing });
    act "Quit" "Return to READY mode" (fun m -> m);
  ]

let graph_menu model = { model with mode = Menu { items = graph_items (); at = 0; up = [] } }

let magic = "TinyLotus123 1"

let top (caps : < Cap.open_in ; Cap.open_out ; .. >) : item list =
  let order o name help = act name help (fun m -> { m with order = o }) in
  [
    sub "Worksheet"
      [
        sub "Global"
          [
            sub "Recalculation"
              [
                order None "Natural" "Recalculate in the order of the dependencies (1983)";
                order (Some Sheet.Columns) "Columnwise" "Recalculate column by column, as VisiCalc did";
                order (Some Sheet.Rows) "Rowwise" "Recalculate row by row, as VisiCalc did";
              ];
          ];
        sub "Erase"
          [
            act "No" "Do not erase the worksheet" (fun m -> m);
            act "Yes" "Erase the entire worksheet from memory" (fun m ->
                { initial with ws = { initial.ws with sheet = Sheet.empty; names = [] }; order = m.order });
          ];
      ];
    sub "Range"
      [
        act "Erase" "Erase a cell or range"
          (ask_range "Enter range to erase:" (fun r m -> List.fold_left (fun m c -> set_cell c "" m) m (cells_of r)));
        sub "Name"
          [
            act "Create" "Create or modify a range name"
              (ask "Enter name:" (fun name ->
                   let name = String.uppercase_ascii (String.trim name) in
                   ask_range ("Enter range for " ^ name ^ ":") (fun r m ->
                       { m with ws = { m.ws with names = (name, r) :: List.remove_assoc name m.ws.names } })));
            act "Delete" "Delete a range name"
              (ask "Enter name to delete:" (fun name m ->
                   { m with ws = { m.ws with names = List.remove_assoc (String.uppercase_ascii name) m.ws.names } }));
          ];
      ];
    act "Copy" "Copy a cell or range of cells"
      (ask_range "Enter range to copy FROM:" (fun from -> ask_range "Enter range to copy TO:" (copy from)));
    sub "File"
      [
        act "Retrieve" "Erase the current worksheet and display the selected worksheet"
          (ask "Name of file to retrieve:" (fun name m ->
               match Option.bind (Playground_platform.fetch caps (name ^ ".wk1")) (Saved.of_string ~magic) with
               | Some ws -> { (go_to (0, 0) m) with ws; message = "retrieved " ^ name ^ ".wk1" }
               | None -> { m with message = "File not found: " ^ name ^ ".wk1" }));
        act "Save" "Store the entire worksheet in a worksheet file"
          (ask "Enter save file name:" (fun name m ->
               Playground_platform.store caps (name ^ ".wk1") (Saved.to_string ~magic m.ws);
               { m with message = "saved " ^ name ^ ".wk1" }));
      ];
    { (act "Graph" "" graph_menu) with help = String.concat "  " (List.map (fun i -> i.name) (graph_items ())) };
    sub "Data"
      [
        act "Sort" "Sort the rows of a range, each a record"
          (ask_range "Enter data range:" (fun data ->
               ask_range "Primary sort key (a cell in its column):" (fun ((key_col, _), _) ->
                   ask "Sort order (A or D):" (fun o -> sort data key_col (String.uppercase_ascii (String.trim o) = "D")))));
      ];
  ]

(*****************************************************************************)
(* The keys, whoever pressed them *)
(*****************************************************************************)

let run_macro letter model =
  let name = "\\" ^ String.make 1 letter in
  match List.assoc_opt name model.ws.names with
  | Some (start, _) -> { model with macro = Some (start, []); message = "" }
  | None -> { model with message = "No macro called " ^ name }

let commit text model =
  let raw = engine_text text in
  let wrong = String.length raw > 0 && raw.[0] = '=' && Result.is_error (Formula.parse (String.sub raw 1 (String.length raw - 1))) in
  (* a formula that does not parse is not refused: 1-2-3 beeped and
     put you in EDIT mode, the text kept *)
  if wrong then { model with mode = Entry { text; edit = true }; message = "Formula error" }
  else set_cell model.cursor raw { model with mode = Ready }

let ready caps key model =
  match key with
  | Up | Down | Left | Right | Home -> step key model
  | Char '/' -> { model with mode = Menu { items = top caps; at = 0; up = [] }; message = "" }
  | Char c -> { model with mode = Entry { text = String.make 1 c; edit = false }; message = "" }
  | Edit -> { model with mode = Entry { text = to_123 (Sheet.raw model.ws.sheet model.cursor); edit = true } }
  | Goto ->
      ask ~point:true "Enter address to go to:"
        (fun s m -> match range_of m.ws s with Some (c, _) -> go_to c m | None -> { m with message = "Invalid address: " ^ s })
        model
  | Calc -> (
      match model.order with
      | None -> model
      | Some order -> with_sheet (Sheet.recalculate order model.ws.sheet) model)
  | Alt c -> run_macro c model
  | Enter | Escape | Backspace -> model

let entry key text edit model =
  let drop s = if s = "" then s else String.sub s 0 (String.length s - 1) in
  match key with
  | Enter -> commit text model
  (* an arrow enters what was typed and moves: the fast way down a
     column of numbers *)
  | (Up | Down | Left | Right) when not edit ->
      let model = commit text model in
      (match model.mode with Ready -> step key model | _ -> model)
  | Escape -> { model with mode = Ready }
  | Backspace -> { model with mode = Entry { text = drop text; edit } }
  | Char c -> { model with mode = Entry { text = text ^ String.make 1 c; edit } }
  | _ -> model

let menu key items at up model =
  let n = List.length items in
  let choose i =
    match (List.nth items i).does with
    | Sub below -> { model with mode = Menu { items = below; at = 0; up = (items, i) :: up } }
    | Act f -> f { model with mode = Ready }
  in
  match key with
  | Left -> { model with mode = Menu { items; at = (at + n - 1) mod n; up } }
  | Right -> { model with mode = Menu { items; at = (at + 1) mod n; up } }
  | Home -> { model with mode = Menu { items; at = 0; up } }
  | Enter -> choose at
  | Escape -> (
      match up with
      | [] -> { model with mode = Ready }
      | (items, at) :: up -> { model with mode = Menu { items; at; up } })
  | Char c -> (
      let rec find i = function
        | [] -> model
        | it :: rest -> if Char.uppercase_ascii it.name.[0] = Char.uppercase_ascii c then choose i else find (i + 1) rest
      in
      find 0 items)
  | _ -> model

let answer (p : prompt) model =
  if p.point then range_text ((match p.anchor with Some a -> a | None -> model.cursor), model.cursor) else p.answer

let prompt key (p : prompt) model =
  let again p = { model with mode = Prompt p } in
  match key with
  | Enter -> p.ok (answer p model) { (go_to p.home model) with mode = Ready }
  | Escape when p.point && p.anchor <> None -> again { p with anchor = None }
  | Escape -> { (go_to p.home model) with mode = Ready }
  | (Up | Down | Left | Right | Home) when p.point -> step key model
  | Char '.' when p.point -> again { p with anchor = Some (match p.anchor with Some a -> a | None -> model.cursor) }
  | Char c -> again { p with point = false; answer = (if p.point then "" else p.answer) ^ String.make 1 c }
  | Backspace when not p.point && p.answer <> "" -> again { p with answer = String.sub p.answer 0 (String.length p.answer - 1) }
  | _ -> model

let press caps key model =
  match model.mode with
  | Viewing -> graph_menu model
  | Ready -> ready caps key model
  | Entry { text; edit } -> entry key text edit model
  | Menu { items; at; up } -> menu key items at up model
  | Prompt p -> prompt key p model

(* a few keys a frame, so that the macro is seen typing: the real one
   ran as fast as the 8088 could, and the CMD at the bottom was all you
   saw of it *)
let keys_per_frame = 3

let rec step_macro caps budget model =
  match model.macro with
  | None -> model
  | Some _ when budget = 0 -> model
  | Some (next, k :: rest) -> step_macro caps (budget - 1) (press caps k { model with macro = Some (next, rest) })
  | Some ((c, r), []) -> (
      (* the next cell down: a label is more keys, anything else the end *)
      match Sheet.value model.ws.sheet (c, r) with
      | Sheet.Text label when strip_prefix label <> "" -> (
          match macro_keys (strip_prefix label) with
          | Ok keys -> step_macro caps budget { model with macro = Some ((c, r + 1), keys) }
          | Error why -> { model with macro = None; message = "Macro error in " ^ name_of (c, r) ^ ": " ^ why })
      | _ -> { model with macro = None })

let update caps computer model =
  let keys = keys_of computer.keyboard ~before:model.was in
  (* while a macro runs, the keyboard waits *)
  let model = match model.macro with None -> List.fold_left (fun m k -> press caps k m) model keys | Some _ -> model in
  let model = step_macro caps keys_per_frame model in
  { model with was = Set_.elements computer.keyboard.keys }

(*****************************************************************************)
(* The text screen *)
(*****************************************************************************)

let reverse = { Vt.plain with reverse = true }

(* a row of the sheet as the screen shows it: numbers against the
   right of their column, labels as their prefix says, a left one
   flowing over the empty cells to its right *)
let row_line model r : string =
  let b = Bytes.make (shown_cols * col_chars) ' ' in
  let put x s = String.iteri (fun j ch -> if x + j >= 0 && x + j < Bytes.length b then Bytes.set b (x + j) ch) s in
  let cc, _ = model.corner in
  let filled c = Sheet.raw model.ws.sheet (c, r) <> "" in
  for i = 0 to shown_cols - 1 do
    let c = cc + i and x = i * col_chars in
    match Sheet.value model.ws.sheet (c, r) with
    | Sheet.Empty -> ()
    | Sheet.Number _ as v ->
        let s = Sheet.show v in
        let s = if String.length s > col_chars - 1 then String.make (col_chars - 1) '*' else s in
        put (x + col_chars - 1 - String.length s) s
    | Sheet.Error _ -> put (x + col_chars - 4) "ERR"
    | Sheet.Text label -> (
        let s = strip_prefix label in
        let cut n = if String.length s > n then String.sub s 0 n else s in
        match label.[0] with
        | '"' -> let s = cut (col_chars - 1) in put (x + col_chars - 1 - String.length s) s
        | '^' -> let s = cut col_chars in put (x + ((col_chars - String.length s) / 2)) s
        | _ ->
            let room = ref col_chars and next = ref (c + 1) in
            while !room < String.length s && !next < cc + shown_cols && not (filled !next) do
              room := !room + col_chars;
              incr next
            done;
            put x (cut !room))
  done;
  Bytes.to_string b

let mode_name model =
  match model.mode with
  | Ready -> "READY"
  | Entry { edit = true; _ } -> "EDIT"
  | Entry { text; _ } -> if text <> "" && is_value_start text.[0] then "VALUE" else "LABEL"
  | Menu _ -> "MENU"
  | Prompt { point = true; _ } -> "POINT"
  | Prompt _ -> "EDIT"
  | Viewing -> ""

let screen model : Curses.t =
  let t = Curses.create ~rows:height ~cols:width in
  let cc, cr = model.corner in
  (* the control panel: the cell and what it holds, the mode *)
  let t = Curses.put 0 0 (name_of model.cursor ^ ": " ^ to_123 (Sheet.raw model.ws.sheet model.cursor)) t in
  let t = Curses.put ~attrs:reverse 0 (width - 7) (Printf.sprintf " %-5s " (mode_name model)) t in
  let t, cursor =
    match model.mode with
    | Menu { items; at; _ } ->
        let t, _ =
          List.fold_left
            (fun (t, x) (i, it) -> (Curses.put ?attrs:(if i = at then Some reverse else None) 1 x it.name t, x + String.length it.name + 2))
            (t, 0)
            (List.mapi (fun i it -> (i, it)) items)
        in
        (Curses.put 2 0 (List.nth items at).help t, None)
    | Entry { text; _ } -> (Curses.put 1 0 text t, Some (1, String.length text))
    | Prompt p ->
        let line = p.question ^ " " ^ answer p model in
        (Curses.put 1 0 line t, Some (1, String.length line))
    | Ready | Viewing -> (t, None)
  in
  (* the borders: the column letters, the row numbers *)
  let letters c = let n = name_of (c, 0) in String.sub n 0 (String.length n - 1) in
  let t =
    Curses.put ~attrs:reverse 3 0
      (String.make border ' '
      ^ String.concat "" (List.init shown_cols (fun i -> let l = letters (cc + i) in Printf.sprintf "%*s%*s" ((col_chars + String.length l) / 2) l ((col_chars - String.length l + 1) / 2) "")))
      t
  in
  let t =
    List.fold_left
      (fun t i ->
        let r = cr + i in
        t |> Curses.put ~attrs:reverse (first_row + i) 0 (Printf.sprintf "%-*d" border (r + 1)) |> Curses.put (first_row + i) border (row_line model r))
      t
      (List.init shown_rows Fun.id)
  in
  (* the cell pointer, a bar as wide as the column -- or, pointing at a
     range, the whole range lit *)
  let (c1, r1), (c2, r2) =
    match model.mode with
    | Prompt { point = true; anchor = Some a; _ } -> ((min (fst a) (fst model.cursor), min (snd a) (snd model.cursor)), (max (fst a) (fst model.cursor), max (snd a) (snd model.cursor)))
    | _ -> (model.cursor, model.cursor)
  in
  let t =
    List.fold_left
      (fun t (c, r) ->
        if c < cc || c >= cc + shown_cols || r < cr || r >= cr + shown_rows then t
        else
          let x = (c - cc) * col_chars in
          Curses.put ~attrs:reverse (first_row + r - cr) (border + x) (String.sub (row_line model r) x col_chars) t)
      t
      (cells_of ((c1, r1), (c2, r2)))
  in
  let t = Curses.put 24 0 model.message t in
  let t = match model.macro with Some _ -> Curses.put ~attrs:reverse 24 (width - 5) " CMD " t | None -> t in
  Curses.cursor cursor t

(*****************************************************************************)
(* The graph screen *)
(*****************************************************************************)

(* the CGA's four colours in 320 by 200, palette 1: what a graph on a
   PC of 1983 had to say it with *)
let cyan = rgb 85 255 255
let magenta = rgb 255 85 255
let bright = rgb 255 255 255

let text ?(size = 18.) color s = words color s |> scale (size /. words_font_size)

let segment color (x1, y1) (x2, y2) =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.sqrt ((dx *. dx) +. (dy *. dy))) 3.
  |> rotate (Float.atan2 dy dx *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let graph_view (ws : worksheet) : shape list =
  let g = ws.graph in
  let numbers r = List.map (fun c -> match Sheet.value ws.sheet c with Sheet.Number f -> Float.max 0. f | _ -> 0.) (cells_of r) in
  let series = List.filter_map (fun (r, color) -> Option.map (fun r -> (numbers r, color)) r) [ (g.a, cyan); (g.b, magenta) ] in
  let labels =
    match g.x with
    | Some r -> List.map (fun c -> match Sheet.value ws.sheet c with Sheet.Text s -> strip_prefix s | v -> Sheet.show v) (cells_of r)
    | None -> []
  in
  let label i = match List.nth_opt labels i with Some s -> s | None -> "" in
  let drawn =
    match g.kind with
    | Pie -> (
        match series with
        | [] -> []
        | (values, _) :: _ ->
            let total = List.fold_left ( +. ) 0. values in
            let colors = [| cyan; magenta; bright |] and radius = 230. in
            let rec slices i start = function
              | [] -> []
              | v :: rest ->
                  let sweep = if total = 0. then 0. else v /. total *. 2. *. Float.pi in
                  let steps = max 2 (int_of_float (sweep *. 20.)) in
                  let at a = (radius *. Float.cos a, radius *. Float.sin a) in
                  let arc = List.init (steps + 1) (fun k -> at (start +. (sweep *. float_of_int k /. float_of_int steps))) in
                  let mid = start +. (sweep /. 2.) in
                  (* three colours: the last slice, meeting the first, skips the first's *)
                  let color = if rest = [] && i > 0 && i mod 3 = 0 then colors.(1) else colors.(i mod 3) in
                  polygon color ((0., 0.) :: arc)
                  :: segment black (0., 0.) (at start)
                  :: (text bright (Printf.sprintf "%s (%.1f%%)" (label i) (100. *. v /. total))
                     |> move ((radius +. 90.) *. Float.cos mid) ((radius +. 30.) *. Float.sin mid))
                  :: slices (i + 1) (start +. sweep) rest
            in
            if total = 0. then [] else slices 0 0. values)
    | Bar | Line ->
        let left = -380. and right = 380. and bottom = -250. and top = 280. in
        let n = List.fold_left (fun m (v, _) -> max m (List.length v)) 1 series in
        let hi = List.fold_left (fun m (v, _) -> List.fold_left Float.max m v) 0. series in
        let hi = if hi = 0. then 1. else hi in
        let y v = bottom +. (v /. hi *. (top -. bottom)) in
        let slot = (right -. left) /. float_of_int n in
        let x i = left +. (slot *. (float_of_int i +. 0.5)) in
        let ns = float_of_int (List.length series) in
        let axes =
          [ segment bright (left, bottom) (right, bottom); segment bright (left, bottom) (left, top) ]
          @ List.concat_map
              (fun f -> [ segment bright (left -. 8., y (hi *. f)) (left, y (hi *. f)); text bright (Sheet.show (Sheet.Number (hi *. f))) |> move (left -. 45.) (y (hi *. f)) ])
              [ 0.; 0.5; 1. ]
          @ List.init n (fun i -> text bright (label i) |> move (x i) (bottom -. 25.))
        in
        let plot k (values, color) =
          List.concat
            (List.mapi
               (fun i v ->
                 match g.kind with
                 | Bar ->
                     let w = slot *. 0.7 /. ns in
                     let h = y v -. bottom in
                     [ rectangle color w h |> move (x i -. (slot *. 0.35) +. (w *. (float_of_int k +. 0.5))) (bottom +. (h /. 2.)) ]
                 | _ ->
                     (rectangle color 12. 12. |> move (x i) (y v))
                     :: (match List.nth_opt values (i + 1) with Some w -> [ segment color (x i, y v) (x (i + 1), y w) ] | None -> []))
               values)
        in
        axes @ List.concat (List.mapi plot series)
  in
  (rectangle black 1000. 1000. :: drawn) @ [ text ~size:14. (rgb 120 120 120) "any key: back to the Graph menu" |> move_y (-400.) ]

let view computer model =
  match model.mode with
  | Viewing -> graph_view model.ws
  | _ ->
      let vt = Vt.feed (Vt.create ~rows:height ~cols:width) (Curses.redraw (screen model)) in
      rectangle black computer.screen.width computer.screen.height :: Teletype.draw_screen ~pc:true ~cursor:true computer vt

let app caps = game view (update caps) initial
let main = Program.main __MODULE__ (fun () -> Cap.main (fun caps -> Playground_platform.run_app (app (caps :> < Cap.open_in ; Cap.open_out >))))
