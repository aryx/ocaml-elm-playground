(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Computed.mli *)
open Css_syntax
module V = Css_values

type display =
  | Inline
  | Block
  | Inline_block
  | List_item
  | Flex
  | Inline_flex
  | Grid
  | Table
  | Table_row_group
  | Table_row
  | Table_cell
  | Table_caption
  | Display_none
  | Contents

type position = Static | Relative | Absolute | Fixed | Sticky
type size = Auto | Len of Css_values.length
type family = Serif | Sans_serif | Monospace
type white_space = Normal | Pre | Nowrap | Pre_wrap | Pre_line
type text_align = Align_left | Align_right | Align_center | Align_justify
type vertical_align = Baseline | Middle | Top | Bottom | Text_top | Text_bottom | Sub | Super
type line_height = Line_normal | Factor of float | Line_px of float
type side = Side_none | Side_left | Side_right | Side_both
type flex_direction = Row | Row_reverse | Column | Column_reverse
type align = Start | End | Center | Stretch | Space_between | Space_around | Space_evenly | Align_baseline

type t = {
  display : display;
  position : position;
  float : side;
  clear : side;
  top : size;
  right : size;
  bottom : size;
  left : size;
  width : size;
  height : size;
  min_width : Css_values.length;
  min_height : Css_values.length;
  max_width : size;
  max_height : size;
  margin : size * size * size * size;
  padding : Css_values.length * Css_values.length * Css_values.length * Css_values.length;
  border_width : float * float * float * float;
  border_color : Css_values.color * Css_values.color * Css_values.color * Css_values.color;
  border_box : bool;
  color : Css_values.color;
  background : Css_values.color;
  font_size : float;
  bold : bool;
  italic : bool;
  family : family;
  line_height : line_height;
  text_align : text_align;
  underline : bool;
  line_through : bool;
  uppercase : bool;
  white_space : white_space;
  vertical_align : vertical_align;
  list_style : string;
  visible : bool;
  overflow_hidden : bool;
  flex_direction : flex_direction;
  flex_wrap : bool;
  justify_content : align;
  align_items : align;
  align_self : align option;
  flex_grow : float;
  flex_shrink : float;
  flex_basis : size;
  row_gap : Css_values.length;
  column_gap : Css_values.length;
  custom : (string * Css_syntax.component list) list;
}

let zero = V.zero
let black = V.black

let initial : t =
  {
    display = Inline;
    position = Static;
    float = Side_none;
    clear = Side_none;
    top = Auto;
    right = Auto;
    bottom = Auto;
    left = Auto;
    width = Auto;
    height = Auto;
    min_width = zero;
    min_height = zero;
    max_width = Auto;
    max_height = Auto;
    margin = (Len zero, Len zero, Len zero, Len zero);
    padding = (zero, zero, zero, zero);
    border_width = (0., 0., 0., 0.);
    border_color = (black, black, black, black);
    border_box = false;
    color = black;
    background = V.transparent;
    font_size = 16.;
    bold = false;
    italic = false;
    family = Serif;
    line_height = Line_normal;
    text_align = Align_left;
    underline = false;
    line_through = false;
    uppercase = false;
    white_space = Normal;
    vertical_align = Baseline;
    list_style = "disc";
    visible = true;
    overflow_hidden = false;
    flex_direction = Row;
    flex_wrap = false;
    justify_content = Start;
    align_items = Stretch;
    align_self = None;
    flex_grow = 0.;
    flex_shrink = 1.;
    flex_basis = Auto;
    row_gap = zero;
    column_gap = zero;
    custom = [];
  }

(*****************************************************************************)
(* Shorthands *)
(*****************************************************************************)

let ident (c : component) : string option = match c with Token (Ident s) -> Some (String.lowercase_ascii s) | _ -> None
let sides = [ "top"; "right"; "bottom"; "left" ]

(* one to four values: top, right, bottom, left *)
let four (vs : component list) : component list list option =
  match V.parts vs with
  | [ a ] -> Some [ [ a ]; [ a ]; [ a ]; [ a ] ]
  | [ a; b ] -> Some [ [ a ]; [ b ]; [ a ]; [ b ] ]
  | [ a; b; c ] -> Some [ [ a ]; [ b ]; [ c ]; [ b ] ]
  | [ a; b; c; d ] -> Some [ [ a ]; [ b ]; [ c ]; [ d ] ]
  | _ -> None

let border_styles = [ "none"; "hidden"; "dotted"; "dashed"; "solid"; "double"; "groove"; "ridge"; "inset"; "outset" ]

let is_width (c : component) =
  match c with
  | Token (Dimension _) | Token (Number 0.) | Func _ -> true
  | Token (Ident s) -> List.mem (String.lowercase_ascii s) [ "thin"; "medium"; "thick" ]
  | _ -> false

(* "1px solid #ccc", in any order *)
let border_parts (vs : component list) : (string * component list) list =
  List.fold_left
    (fun acc c ->
      match ident c with
      | Some s when List.mem s border_styles -> ("style", [ c ]) :: acc
      | _ when is_width c -> ("width", [ c ]) :: acc
      | _ -> ("color", [ c ]) :: acc)
    [] (V.parts vs)

let expand ((name, value) : string * component list) : (string * component list) list =
  let per_side prefix suffix =
    match four value with
    | Some vs -> List.map2 (fun s v -> (prefix ^ s ^ suffix, v)) sides vs
    | None -> []
  in
  match name with
  | "margin" -> per_side "margin-" ""
  | "padding" -> per_side "padding-" ""
  | "inset" -> per_side "" ""
  | "border-width" -> per_side "border-" "-width"
  | "border-style" -> per_side "border-" "-style"
  | "border-color" -> per_side "border-" "-color"
  | "border" ->
      (* a border's style, if not said, is none: "border: 1px red" draws
       * nothing; its width, medium *)
      let ps = border_parts value in
      let with_defaults = ps @ (if List.mem_assoc "style" ps then [] else [ ("style", [ Token (Ident "none") ]) ]) in
      List.concat_map (fun s -> List.map (fun (k, v) -> ("border-" ^ s ^ "-" ^ k, v)) with_defaults) sides
  | "border-top" | "border-right" | "border-bottom" | "border-left" ->
      let ps = border_parts value in
      let ps = ps @ if List.mem_assoc "style" ps then [] else [ ("style", [ Token (Ident "none") ]) ] in
      List.map (fun (k, v) -> (name ^ "-" ^ k, v)) ps
  | "background" -> (
      (* its colour, where it has one; an image alone makes it transparent *)
      match List.find_opt (fun c -> V.color ~current:black [ c ] <> None) (V.parts value) with
      | Some c -> [ ("background-color", [ c ]) ]
      | None -> [ ("background-color", [ Token (Ident "transparent") ]) ])
  | "font" -> (
      (* [style] [variant] [weight] size[/line-height] family *)
      let rec go acc = function
        | [] -> acc
        | c :: rest -> (
            match ident c with
            | Some ("italic" | "oblique") -> go (("font-style", [ c ]) :: acc) rest
            | Some ("bold" | "bolder" | "lighter") -> go (("font-weight", [ c ]) :: acc) rest
            | Some ("normal" | "small-caps") -> go acc rest
            | _ -> (
                match c with
                | Token (Number n) when n >= 100. -> go (("font-weight", [ c ]) :: acc) rest
                | _ -> (
                    let acc = ("font-size", [ c ]) :: acc in
                    match rest with
                    | Token (Delim '/') :: lh :: family -> ("line-height", [ lh ]) :: ("font-family", family) :: acc
                    | family -> ("font-family", family) :: acc)))
      in
      go [] (V.parts value))
  | "flex" -> (
      let num c = match c with Token (Number n) -> Some n | _ -> None in
      let n f = [ Token (Number f) ] in
      match V.parts value with
      | [ c ] when ident c = Some "none" -> [ ("flex-grow", n 0.); ("flex-shrink", n 0.); ("flex-basis", [ Token (Ident "auto") ]) ]
      | [ c ] when ident c = Some "auto" -> [ ("flex-grow", n 1.); ("flex-shrink", n 1.); ("flex-basis", [ Token (Ident "auto") ]) ]
      | [ c ] when num c <> None -> [ ("flex-grow", [ c ]); ("flex-shrink", n 1.); ("flex-basis", [ Token (Percentage 0.) ]) ]
      | [ c ] -> [ ("flex-grow", n 1.); ("flex-shrink", n 1.); ("flex-basis", [ c ]) ]
      | [ g; s ] when num s <> None -> [ ("flex-grow", [ g ]); ("flex-shrink", [ s ]); ("flex-basis", [ Token (Percentage 0.) ]) ]
      | [ g; b ] -> [ ("flex-grow", [ g ]); ("flex-shrink", n 1.); ("flex-basis", [ b ]) ]
      | [ g; s; b ] -> [ ("flex-grow", [ g ]); ("flex-shrink", [ s ]); ("flex-basis", [ b ]) ]
      | _ -> [])
  | "gap" | "grid-gap" -> (
      match V.parts value with [ a ] -> [ ("row-gap", [ a ]); ("column-gap", [ a ]) ] | [ a; b ] -> [ ("row-gap", [ a ]); ("column-gap", [ b ]) ] | _ -> [])
  | "list-style" -> (
      match List.find_opt (fun c -> match ident c with Some s -> s <> "inside" && s <> "outside" | None -> false) (V.parts value) with
      | Some c -> [ ("list-style-type", [ c ]) ]
      | None -> [])
  | "overflow" | "overflow-x" | "overflow-y" -> [ ("overflow", value) ]
  | "text-decoration-line" -> [ ("text-decoration", value) ]
  | "flex-flow" ->
      List.map (fun c -> match ident c with Some ("wrap" | "nowrap" | "wrap-reverse") -> ("flex-wrap", [ c ]) | _ -> ("flex-direction", [ c ])) (V.parts value)
  | _ -> [ (name, value) ]

(*****************************************************************************)
(* The properties *)
(*****************************************************************************)

let inherited =
  [ "color"; "font-size"; "font-weight"; "font-style"; "font-family"; "line-height"; "text-align"; "text-transform";
    "white-space"; "visibility"; "list-style-type"; "text-decoration" ]

let display_of (s : string) : display option =
  match s with
  | "inline" -> Some Inline
  | "block" | "flow-root" | "block flow" -> Some Block
  | "inline-block" | "inline-grid" | "inline-table" -> Some Inline_block
  | "list-item" -> Some List_item
  | "flex" -> Some Flex
  | "inline-flex" -> Some Inline_flex
  | "grid" -> Some Grid
  | "table" -> Some Table
  | "table-row-group" | "table-header-group" | "table-footer-group" -> Some Table_row_group
  | "table-row" -> Some Table_row
  | "table-cell" -> Some Table_cell
  | "table-caption" -> Some Table_caption
  | "none" | "table-column" | "table-column-group" -> Some Display_none
  | "contents" -> Some Contents
  | _ -> None

let align_of (s : string) : align option =
  match s with
  | "flex-start" | "start" | "left" | "self-start" | "normal" -> Some Start
  | "flex-end" | "end" | "right" | "self-end" -> Some End
  | "center" -> Some Center
  | "stretch" -> Some Stretch
  | "space-between" -> Some Space_between
  | "space-around" -> Some Space_around
  | "space-evenly" -> Some Space_evenly
  | "baseline" | "first baseline" -> Some Align_baseline
  | _ -> None

let width_keyword (s : string) : float option = match s with "thin" -> Some 1. | "medium" -> Some 3. | "thick" -> Some 5. | _ -> None

let compute (m : Cascade.media) ~(root_font_size : float) ~(parent : t) (declared : (string * component list) list) : t =
  (* the custom properties first: inherited, overridden by this element's *)
  let custom =
    List.fold_left
      (fun acc (n, v) -> if String.length n > 2 && String.sub n 0 2 = "--" then (n, v) :: List.remove_assoc n acc else acc)
      parent.custom declared
  in
  let lookup n = List.assoc_opt n custom in
  let decls =
    declared
    |> List.filter (fun (n, _) -> not (String.length n > 2 && String.sub n 0 2 = "--"))
    |> List.filter_map (fun (n, v) -> Option.map (fun v -> (n, v)) (V.substitute lookup v))
    |> List.concat_map expand
  in
  (* a property's value: this element's, else (if inherited) its
   * parent's -- None, the caller's initial value *)
  let get (name : string) : component list option =
    match List.assoc_opt name (List.rev decls) with
    | Some v -> (
        match V.parts v with
        | [ Token (Ident k) ] when String.lowercase_ascii k = "inherit" -> Some [ Token (Ident "inherit") ]
        | [ Token (Ident k) ] when String.lowercase_ascii k = "initial" -> None
        | [ Token (Ident k) ] when List.mem (String.lowercase_ascii k) [ "unset"; "revert"; "revert-layer" ] ->
            if List.mem name inherited then Some [ Token (Ident "inherit") ] else None
        | _ -> Some v)
    | None -> if List.mem name inherited then Some [ Token (Ident "inherit") ] else None
  in
  let is_inherit v = match v with Some [ Token (Ident "inherit") ] -> true | _ -> false in
  let word name = match get name with Some v -> ( match V.parts v with [ c ] -> ident c | cs -> Some (String.lowercase_ascii (to_string cs))) | None -> None in
  (* font-size first: an em here is the parent's *)
  let font_size =
    let pctx : V.context = { em = parent.font_size; rem = root_font_size; viewport_width = m.width; viewport_height = m.height } in
    match get "font-size" with
    | v when is_inherit v -> parent.font_size
    | Some v -> (
        match V.parts v with
        | [ Token (Percentage p) ] -> parent.font_size *. p /. 100.
        | [ Token (Ident k) ] -> (
            match String.lowercase_ascii k with
            | "xx-small" -> 9.
            | "x-small" -> 10.
            | "small" -> 13.
            | "medium" -> 16.
            | "large" -> 18.
            | "x-large" -> 24.
            | "xx-large" -> 32.
            | "xxx-large" -> 48.
            | "larger" -> parent.font_size *. 1.2
            | "smaller" -> parent.font_size /. 1.2
            | _ -> parent.font_size)
        | [ c ] -> ( match V.length pctx c with Some l -> Float.max 0. (V.resolve l parent.font_size) | None -> parent.font_size)
        | _ -> parent.font_size)
    | None -> 16.
  in
  let ctx : V.context = { em = font_size; rem = root_font_size; viewport_width = m.width; viewport_height = m.height } in
  let len_of v = match V.parts v with [ c ] -> V.length ctx c | _ -> None in
  (* a property: [f] of its value, [inh] the parent's, [init] else *)
  let prop name ~inh ~init f =
    match get name with
    | v when is_inherit v -> inh
    | Some v -> ( match f v with Some x -> x | None -> if List.mem name inherited then inh else init)
    | None -> init
  in
  (* [inh] the parent's value: what inherit asks, even of a property
   * not inherited by default *)
  let length name ~inh ~init = prop name ~inh ~init len_of in
  let size name ~inh ~init =
    prop name ~inh ~init (fun v -> match V.parts v with [ c ] when ident c = Some "auto" || ident c = Some "none" -> Some Auto | _ -> Option.map (fun l -> Len l) (len_of v))
  in
  let (pmt, pmr, pmb, pml), (ppt, ppr, ppb, ppl) = (parent.margin, parent.padding) in
  let color = prop "color" ~inh:parent.color ~init:black (fun v -> V.color ~current:parent.color v) in
  let colour name = prop name ~inh:color ~init:color (fun v -> V.color ~current:color v) in
  let border s =
    let style = word ("border-" ^ s ^ "-style") in
    let visible = match style with Some ("none" | "hidden") | None -> false | Some _ -> true in
    let w =
      prop ("border-" ^ s ^ "-width") ~inh:3. ~init:3. (fun v ->
          match V.parts v with
          | [ Token (Ident k) ] -> width_keyword (String.lowercase_ascii k)
          | _ -> Option.map (fun l -> V.resolve l 0.) (len_of v))
    in
    ((if visible then w else 0.), colour ("border-" ^ s ^ "-color"))
  in
  let (bt, ct), (br, cr), (bb, cb), (bl, cl) = (border "top", border "right", border "bottom", border "left") in
  let decoration = word "text-decoration" in
  let has_decoration k = match decoration with Some d -> List.mem k (String.split_on_char ' ' d) | None -> false in
  {
    display =
      (match get "display" with
      | Some v -> ( match display_of (String.lowercase_ascii (to_string (V.parts v |> List.filteri (fun i _ -> i = 0)))) with Some d -> d | None -> Inline)
      | None -> Inline);
    position =
      (match word "position" with Some "relative" -> Relative | Some "absolute" -> Absolute | Some "fixed" -> Fixed | Some "sticky" -> Sticky | _ -> Static);
    float = (match word "float" with Some "left" -> Side_left | Some "right" -> Side_right | _ -> Side_none);
    clear = (match word "clear" with Some "left" -> Side_left | Some "right" -> Side_right | Some "both" -> Side_both | _ -> Side_none);
    top = size "top" ~inh:parent.top ~init:Auto;
    right = size "right" ~inh:parent.right ~init:Auto;
    bottom = size "bottom" ~inh:parent.bottom ~init:Auto;
    left = size "left" ~inh:parent.left ~init:Auto;
    width = size "width" ~inh:parent.width ~init:Auto;
    height = size "height" ~inh:parent.height ~init:Auto;
    min_width = length "min-width" ~inh:parent.min_width ~init:zero;
    min_height = length "min-height" ~inh:parent.min_height ~init:zero;
    max_width = size "max-width" ~inh:parent.max_width ~init:Auto;
    max_height = size "max-height" ~inh:parent.max_height ~init:Auto;
    margin =
      ( size "margin-top" ~inh:pmt ~init:(Len zero),
        size "margin-right" ~inh:pmr ~init:(Len zero),
        size "margin-bottom" ~inh:pmb ~init:(Len zero),
        size "margin-left" ~inh:pml ~init:(Len zero) );
    padding =
      ( length "padding-top" ~inh:ppt ~init:zero,
        length "padding-right" ~inh:ppr ~init:zero,
        length "padding-bottom" ~inh:ppb ~init:zero,
        length "padding-left" ~inh:ppl ~init:zero );
    border_width = (bt, br, bb, bl);
    border_color = (ct, cr, cb, cl);
    border_box = word "box-sizing" = Some "border-box";
    color;
    background = prop "background-color" ~inh:V.transparent ~init:V.transparent (fun v -> V.color ~current:color v);
    font_size;
    bold =
      prop "font-weight" ~inh:parent.bold ~init:false (fun v ->
          match V.parts v with
          | [ Token (Number n) ] -> Some (n >= 600.)
          | [ c ] -> ( match ident c with Some ("bold" | "bolder") -> Some true | Some ("normal" | "lighter") -> Some false | _ -> None)
          | _ -> None);
    italic = prop "font-style" ~inh:parent.italic ~init:false (fun v -> match V.parts v with [ c ] -> Option.map (fun s -> s = "italic" || s = "oblique") (ident c) | _ -> None);
    family =
      prop "font-family" ~inh:parent.family ~init:Serif (fun v ->
          (* the first family we can tell: our pens have three faces *)
          let names = List.map (fun f -> String.lowercase_ascii (to_string (trim f))) (split_on Comma v) in
          List.find_map
            (fun n ->
              let n = String.concat "" (String.split_on_char '"' n) in
              if List.mem n [ "monospace"; "courier"; "courier new"; "consolas"; "menlo"; "monaco"; "sfmono-regular"; "ui-monospace"; "dejavu sans mono" ] then Some Monospace
              else if List.mem n [ "serif"; "times"; "times new roman"; "georgia"; "linux libertine" ] then Some Serif
              else if List.mem n [ "sans-serif"; "arial"; "helvetica"; "verdana"; "system-ui"; "-apple-system"; "roboto"; "segoe ui"; "helvetica neue" ] then Some Sans_serif
              else None)
            names);
    line_height =
      prop "line-height" ~inh:parent.line_height ~init:Line_normal (fun v ->
          match V.parts v with
          | [ Token (Number n) ] -> Some (Factor n)
          | [ Token (Percentage p) ] -> Some (Line_px (font_size *. p /. 100.))
          | [ c ] when ident c = Some "normal" -> Some Line_normal
          | [ c ] -> Option.map (fun l -> Line_px (V.resolve l 0.)) (V.length ctx c)
          | _ -> None);
    text_align =
      prop "text-align" ~inh:parent.text_align ~init:Align_left (fun v ->
          match V.parts v with
          | [ c ] -> (
              match ident c with
              | Some ("left" | "start") -> Some Align_left
              | Some ("right" | "end") -> Some Align_right
              | Some ("center" | "-webkit-center" | "-moz-center") -> Some Align_center
              | Some "justify" -> Some Align_justify
              | _ -> None)
          | _ -> None);
    underline = (if decoration = None then parent.underline else has_decoration "underline");
    line_through = (if decoration = None then parent.line_through else has_decoration "line-through");
    uppercase = prop "text-transform" ~inh:parent.uppercase ~init:false (fun v -> match V.parts v with [ c ] -> Option.map (fun s -> s = "uppercase") (ident c) | _ -> None);
    white_space =
      prop "white-space" ~inh:parent.white_space ~init:Normal (fun v ->
          match V.parts v with
          | [ c ] -> (
              match ident c with
              | Some "pre" -> Some Pre
              | Some "nowrap" -> Some Nowrap
              | Some ("pre-wrap" | "break-spaces") -> Some Pre_wrap
              | Some "pre-line" -> Some Pre_line
              | Some "normal" -> Some Normal
              | _ -> None)
          | _ -> None);
    vertical_align =
      (match word "vertical-align" with
      | Some "middle" -> Middle
      | Some "top" -> Top
      | Some "bottom" -> Bottom
      | Some "text-top" -> Text_top
      | Some "text-bottom" -> Text_bottom
      | Some "sub" -> Sub
      | Some "super" -> Super
      | _ -> Baseline);
    list_style = prop "list-style-type" ~inh:parent.list_style ~init:"disc" (fun v -> match V.parts v with [ c ] -> ident c | _ -> None);
    visible =
      prop "visibility" ~inh:parent.visible ~init:true (fun v -> match V.parts v with [ c ] -> Option.map (fun s -> s = "visible") (ident c) | _ -> None)
      (* opacity: 0, what hides a checkbox that a label stands for: not
       * drawn either (and nor is what is in it) *)
      && (match get "opacity" with Some v -> ( match float_of_string_opt (String.trim (to_string v)) with Some o -> o > 0. | None -> true) | None -> true);
    (* auto and scroll: a box that scrolls, here clipped (no scrollbar) *)
    (* "hidden auto": x, then y; either clipping clips here *)
    overflow_hidden =
      (match get "overflow" with
      | Some v -> List.exists (fun c -> match ident c with Some ("hidden" | "clip" | "auto" | "scroll") -> true | _ -> false) (V.parts v)
      | None -> false);
    flex_direction =
      (match word "flex-direction" with Some "row-reverse" -> Row_reverse | Some "column" -> Column | Some "column-reverse" -> Column_reverse | _ -> Row);
    flex_wrap = (match word "flex-wrap" with Some ("wrap" | "wrap-reverse") -> true | _ -> false);
    justify_content = (match Option.bind (word "justify-content") align_of with Some a -> a | None -> Start);
    align_items = (match Option.bind (word "align-items") align_of with Some Start when word "align-items" = Some "normal" -> Stretch | Some a -> a | None -> Stretch);
    align_self = (match word "align-self" with Some "auto" | None -> None | Some s -> align_of s);
    flex_grow = prop "flex-grow" ~inh:0. ~init:0. (fun v -> match V.parts v with [ Token (Number n) ] -> Some n | _ -> None);
    flex_shrink = prop "flex-shrink" ~inh:1. ~init:1. (fun v -> match V.parts v with [ Token (Number n) ] -> Some n | _ -> None);
    flex_basis = size "flex-basis" ~inh:parent.flex_basis ~init:Auto;
    row_gap = length "row-gap" ~inh:parent.row_gap ~init:zero;
    column_gap = length "column-gap" ~inh:parent.column_gap ~init:zero;
    custom;
  }

(*****************************************************************************)
(* The browser's own sheet, and every element's style *)
(*****************************************************************************)

let user_agent_sheet : Cascade.sheet = { origin = User_agent; rules = parse_stylesheet Ua_sheet.text }

(* quirks mode's rules (WHATWG HTML, "Rendering", tables in quirks
 * mode): a table does not inherit its surroundings' fonts and
 * alignment -- pages of the 1990s <center>ed a table, not its text *)
let quirks_sheet : Cascade.sheet =
  {
    origin = User_agent;
    rules =
      parse_stylesheet
        "table { font-weight: initial; font-style: initial; font-size: initial; line-height: initial; white-space: initial; text-align: initial }";
  }

let styles ?visited ?(quirks = false) (m : Cascade.media) (sheets : Cascade.sheet list) (root : Dom.element) : Dom.element -> t =
  let ua = if quirks then [ user_agent_sheet; quirks_sheet ] else [ user_agent_sheet ] in
  let declared = Cascade.cascade ?visited m (ua @ sheets) root in
  let table : (int, Dom.element * t) Hashtbl.t = Hashtbl.create 1024 in
  let root_style = compute m ~root_font_size:16. ~parent:initial (declared root) in
  (* each element's from its parent's, down the tree *)
  let rec go (e : Dom.element) (style : t) =
    Hashtbl.add table (Hashtbl.hash e) (e, style);
    List.iter
      (fun (n : Dom.node) ->
        match n with
        | Element c -> go c (compute m ~root_font_size:root_style.font_size ~parent:style (declared c))
        | Text _ -> ())
      e.children
  in
  go root root_style;
  fun e -> match Cascade.find_element table e with Some s -> s | None -> initial
