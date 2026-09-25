(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Box_layout.mli *)

type box = {
  element : Dom.element option;
  style : Computed.t;
  x : float;
  y : float;
  width : float;
  height : float;
  border : float * float * float * float;
  children : box list;
  lines : Html_layout.line list;
  backdrops : box list;
  marker : Html_layout.marker option;
}

(*****************************************************************************)
(* Boxes *)
(*****************************************************************************)

let rec fragments (b : box) : Html_layout.fragment list =
  List.concat_map (fun (l : Html_layout.line) -> l.fragments) b.lines @ List.concat_map fragments b.children

(* a box and all it holds moved by (dx, dy): a float or an inline-block
 * is laid out where it will not stay *)
let rec moved (dx : float) (dy : float) (b : box) : box =
  if dx = 0. && dy = 0. then b
  else
    {
      b with
      x = b.x +. dx;
      y = b.y +. dy;
      children = List.map (moved dx dy) b.children;
      backdrops = List.map (moved dx dy) b.backdrops;
      lines =
        List.map
          (fun (l : Html_layout.line) ->
            {
              l with
              top = l.top +. dy;
              baseline = l.baseline +. dy;
              fragments = List.map (fun (f : Html_layout.fragment) -> { f with x = f.x +. dx; baseline = f.baseline +. dy }) l.fragments;
            })
          b.lines;
    }

(* how far right a box's content reaches, for shrink-to-fit: its lines'
 * words, its children's content -- a block of width auto is as wide as
 * its container, which is not what it needs *)
let rec inner_right (b : box) : float =
  let lines =
    List.fold_left
      (fun m (l : Html_layout.line) -> List.fold_left (fun m (f : Html_layout.fragment) -> Float.max m (f.x +. f.width)) m l.fragments)
      b.x b.lines
  in
  List.fold_left (fun m c -> Float.max m (right_edge c)) lines b.children

and right_edge (b : box) : float =
  match b.element with
  | Some _ when b.style.width <> Auto || b.style.display = Table -> b.x +. b.width
  | Some _ ->
      let _, pr, _, _ = b.style.padding and _, br, _, _ = b.border in
      inner_right b +. Css_values.resolve pr 0. +. br
  | None -> inner_right b

let rec last_baseline (b : box) : float option =
  match List.rev b.lines with
  | l :: _ -> Some l.baseline
  | [] -> List.fold_left (fun found c -> match last_baseline c with Some _ as s -> s | None -> found) None b.children

let rec as_html_layout (b : box) : Html_layout.box =
  {
    kind = (match b.element with Some e -> Block e | None -> Anonymous);
    x = b.x;
    y = b.y;
    width = b.width;
    height = b.height;
    children = List.map as_html_layout b.children;
    lines = b.lines;
    floats = [];
    marker = b.marker;
    background = None;
  }

let picture_src (e : Dom.element) : string option =
  match Dom.attribute "src" e with
  | Some s when String.trim s <> "" -> Some s
  | _ -> (
      (* "a.png 1x, b.png 2x": its first address *)
      match Dom.attribute "srcset" e with
      | Some set -> (
          match String.split_on_char ' ' (String.trim (List.hd (String.split_on_char ',' set))) with
          | url :: _ when url <> "" -> Some url
          | _ -> None)
      | None -> None)

(*****************************************************************************)
(* Styles *)
(*****************************************************************************)

let look_of (s : Computed.t) ~(link : string option) : Looks.t =
  let color = (s.color.r, s.color.g, s.color.b) in
  {
    size = s.font_size;
    bold = s.bold;
    italic = s.italic;
    underline = s.underline;
    strike = s.line_through;
    monospace = s.family = Monospace;
    color;
    link;
    pre = (match s.white_space with Pre | Pre_wrap -> true | _ -> false);
    align = (match s.text_align with Align_center -> Center | Align_right -> Right | Align_left | Align_justify -> Left);
    (* a link's colour is the style sheets' (:link, :visited), not the
     * look's *)
    link_color = color;
    visited_color = color;
    base = 16.;
    extensions = true;
  }

let line_height (s : Computed.t) : float =
  match s.line_height with Line_normal -> 1.2 *. s.font_size | Factor f -> f *. s.font_size | Line_px p -> p

(* a size in pixels, a percentage of [base]; auto None *)
let size (s : Computed.size) (base : float) : float option =
  match s with Auto -> None | Len l -> Some (Css_values.resolve l base)

let four (f : 'a -> 'b) ((t, r, b, l) : 'a * 'a * 'a * 'a) : 'b * 'b * 'b * 'b = (f t, f r, f b, f l)

(* a box that lays out its own content apart: its floats its own, its
 * margins not collapsed with its children's *)
let own_context (s : Computed.t) : bool =
  s.overflow_hidden || s.float <> Side_none
  || (match s.position with Absolute | Fixed -> true | _ -> false)
  || match s.display with Inline_block | Inline_flex | Flex | Grid | Table_cell | Table | Table_caption -> true | _ -> false

(* the larger margin, a negative one subtracted (CSS 2.1 section 8.3.1) *)
let collapse (a : float) (b : float) : float = if a >= 0. && b >= 0. then Float.max a b else if a <= 0. && b <= 0. then Float.min a b else a +. b

(*****************************************************************************)
(* Inline content *)
(*****************************************************************************)

(* a word's room above and below the baseline (half-leading), and its
 * baseline moved down by [shift] (sub; up for super) *)
type word_style = { look : Looks.t; above : float; below : float; shift : float; shown : bool }

let word_style (s : Computed.t) ~(link : string option) : word_style =
  let lh = line_height s and size = s.font_size in
  let shift = match s.vertical_align with Sub -> 0.2 *. size | Super -> -0.35 *. size | _ -> 0. in
  { look = look_of s ~link; above = (0.8 *. size) +. ((lh -. size) /. 2.); below = (0.2 *. size) +. ((lh -. size) /. 2.); shift; shown = s.visible }

type side = On_left | On_right

(* what a unit of a line may be besides text: a picture, a control, an
 * inline-block (laid out at its place's origin; its margins) *)
type boxed =
  | Pic of Html_layout.picture
  | Ctl of Html_layout.control
  | Inline of { ib : box; ml : float; mt : float; mb : float }

(* an inline element whose box is drawn (a background, a border): its
 * horizontal margins, which its box leaves out *)
type decoration = { de : Dom.element; ds : Computed.t; dml : float; dmr : float }

(* a spacer word's place at an inline element's edge: its margin,
 * border and padding, left or right *)
type edge = Body | Lead | Trail

type word = {
  text : string;
  ws : word_style;
  space_before : bool;
  glue : bool; (* no break before it: white-space: nowrap *)
  space : float; (* the width of the space before it *)
  width : float;
  boxed : boxed option;
  owner : Dom.element;
  decorations : decoration list; (* the inline elements it is in whose boxes are drawn *)
  edge : edge;
}

type item =
  | Word of word
  | Break
  | Anchor of string
  | Float of { fbox : box; fside : side; fmargin : float * float * float * float; mutable placed : bool }
  | Clear of side list

(* a float in its place: its margin box *)
type placed = { pside : side; left : float; right : float; ptop : float; pbottom : float }

(* the room for a line from [top], [height] high, beside the floats *)
let room (floats : placed list) ~(x : float) ~(width : float) ~(top : float) ~(height : float) : float * float =
  let l, r =
    List.fold_left
      (fun (l, r) p ->
        if p.ptop >= top +. height || p.pbottom <= top then (l, r)
        else match p.pside with On_left -> (Float.max l p.right, r) | On_right -> (l, Float.min r p.left))
      (x, x +. width) floats
  in
  (l, Float.max 0. (r -. l))

(* below the floats of [sides] *)
let cleared (floats : placed list) (sides : side list) (top : float) : float =
  List.fold_left (fun t p -> if List.mem p.pside sides then Float.max t p.pbottom else t) top floats

(* a word's extent above and below the baseline *)
let extent (ws : word_style) (boxed : boxed option) : float * float =
  match boxed with
  | Some (Pic { height; middle = false; _ }) -> (height, 0.)
  | Some (Pic { height; middle = true; _ }) -> (height /. 2., height /. 2.)
  | Some (Ctl { control_height = h; _ }) -> (0.75 *. h, 0.25 *. h)
  | Some (Inline { ib; mt; mb; _ }) ->
      let base = match last_baseline ib with Some b -> b -. ib.y | None -> ib.height in
      (mt +. base, ib.height -. base +. mb)
  | None -> (ws.above -. ws.shift, ws.below +. ws.shift)

(* a line's words placed, the line as tall as they need (at least the
 * block's strut); its inline-blocks moved into place *)
let set_line (strut : word_style) (align : Looks.align) ~(x : float) ~(width : float) ~(top : float) (words : item list) :
    Html_layout.line * box list * box list =
  let placed, line_width =
    List.fold_left
      (fun (placed, pen) item ->
        match item with
        | Word w ->
            let pen = if w.space_before then pen +. w.space else pen in
            (((w, pen) :: placed), pen +. w.width)
        | Break | Anchor _ | Float _ | Clear _ -> (placed, pen))
      ([], 0.) words
  in
  let placed = List.rev placed in
  let shift = match align with Left -> 0. | Center -> Float.max 0. ((width -. line_width) /. 2.) | Right -> Float.max 0. (width -. line_width) in
  let up, down =
    List.fold_left
      (fun (u, d) ((w : word), _) -> let a, b = extent w.ws w.boxed in (Float.max u a, Float.max d b))
      (strut.above, strut.below) placed
  in
  let baseline = top +. up in
  let fragments, boxes =
    List.fold_left
      (fun (frags, boxes) ((w : word), pen) ->
        let fx = x +. shift +. pen in
        let picture = match w.boxed with Some (Pic p) -> Some p | _ -> None in
        let control = match w.boxed with Some (Ctl c) -> Some c | _ -> None in
        let text = if w.ws.shown then w.text else "" in
        let frag : Html_layout.fragment =
          { text; look = w.ws.look; x = fx; width = w.width; baseline = baseline +. w.ws.shift; picture; control; element = w.owner }
        in
        let boxes =
          match w.boxed with
          | Some (Inline { ib; ml; mt; _ }) ->
              let above, _ = extent w.ws w.boxed in
              moved (fx +. ml -. ib.x) (baseline -. above +. mt -. ib.y) ib :: boxes
          | _ -> boxes
        in
        (frag :: frags, boxes))
      ([], []) placed
  in
  (* each decorated inline element's box on this line: from its first
   * word to its last, its margins out where its edges are here, as tall
   * as its font and its vertical padding and border (which the line's
   * height ignores) *)
  let decorations =
    List.fold_left
      (fun acc ((w : word), _) -> List.fold_left (fun acc d -> if List.exists (fun d' -> d'.de == d.de) acc then acc else acc @ [ d ]) acc w.decorations)
      [] placed
  in
  let backdrops =
    List.map
      (fun d ->
        let mine = List.filter (fun ((w : word), _) -> List.exists (fun d' -> d'.de == d.de) w.decorations) placed in
        let at_edge edge = List.exists (fun ((w : word), _) -> w.edge = edge && w.owner == d.de) mine in
        let left = List.fold_left (fun m (_, pen) -> Float.min m pen) infinity mine in
        let right = List.fold_left (fun m ((w : word), pen) -> Float.max m (pen +. w.width)) neg_infinity mine in
        let left = x +. shift +. left +. (if at_edge Lead then d.dml else 0.) in
        let right = x +. shift +. right -. (if at_edge Trail then d.dmr else 0.) in
        let pt, _, pb, _ = four (fun l -> Css_values.resolve l 0.) d.ds.padding and bt, br, bb, bl = d.ds.border_width in
        let size = d.ds.font_size in
        let y = baseline -. (0.8 *. size) -. pt -. bt in
        { element = Some d.de; style = d.ds; x = left; y; width = Float.max 0. (right -. left); height = size +. pt +. pb +. bt +. bb;
          border = (bt, (if at_edge Trail then br else 0.), bb, (if at_edge Lead then bl else 0.)); children = []; lines = []; backdrops = []; marker = None })
      decorations
  in
  ( { top; height = up +. down; baseline; fragments = List.rev fragments; anchors = List.filter_map (fun i -> match i with Anchor a -> Some a | _ -> None) words },
    List.rev boxes, backdrops )

(* the words stuck together: a unit starts at a word with a space
 * before it that may break there *)
let units_of (words : item list) : item list list =
  let rec go current acc words =
    match words with
    | [] -> List.rev (if current = [] then acc else List.rev current :: acc)
    | (Word { space_before = true; glue = false; _ } as w) :: rest when current <> [] -> go [ w ] (List.rev current :: acc) rest
    | w :: rest -> go (w :: current) acc rest
  in
  go [] [] words

let rec starting_line (unit : item list) : item list =
  match unit with
  | Word w :: rest -> Word { w with space_before = false } :: rest
  | ((Anchor _ | Float _) as i) :: rest -> i :: starting_line rest
  | _ -> unit

let unit_size (unit : item list) : float * float =
  List.fold_left
    (fun (space, width) item ->
      match item with
      | Word w -> if width = 0. && space = 0. && w.space_before then (w.space, w.width) else (space, width +. w.width)
      | Break | Anchor _ | Float _ | Clear _ -> (space, width))
    (0., 0.) unit

(* a float put at [top] against its edge of the room there (below the
 * floats beside it if it does not fit); its box moved there *)
let place (floats : placed list ref) ~(x : float) ~(width : float) ~(top : float) (placed_boxes : box list ref) (item : item) : unit =
  match item with
  | Float f when not f.placed ->
      f.placed <- true;
      let mt, mr, mb, ml = f.fmargin in
      let w = ml +. f.fbox.width +. mr and h = mt +. f.fbox.height +. mb in
      let rec find top =
        let l, rw = room !floats ~x ~width ~top ~height:(Float.max h 1.) in
        let below = List.fold_left (fun m p -> if p.pbottom > top && p.ptop <= top then Float.min m p.pbottom else m) infinity !floats in
        if rw >= w || below = infinity then (l, rw, top) else find below
      in
      let l, rw, top = find top in
      let fx = match f.fside with On_left -> l | On_right -> l +. rw -. w in
      floats := { pside = f.fside; left = fx; right = fx +. w; ptop = top; pbottom = top +. h } :: !floats;
      placed_boxes := moved (fx +. ml -. f.fbox.x) (top +. mt -. f.fbox.y) f.fbox :: !placed_boxes
  | _ -> ()

(* the items cut at the breaks, each run set on lines one at a time,
 * each as wide as the floats beside it leave (greedy); a float met at
 * a line's start put at its top, one inside it below it. The lines,
 * the boxes set in them (inline-blocks, floats), the bottom *)
let set_lines (floats : placed list ref) (strut : word_style) (align : Looks.align) ~(pre : bool) ~(x : float) ~(width : float) ~(top : float)
    (items : item list) : Html_layout.line list * box list * box list * float =
  let boxes = ref [] and backdrops = ref [] in
  let rec groups current acc items =
    match items with
    | [] -> List.rev (List.rev current :: acc)
    | Break :: rest -> groups [] (List.rev current :: acc) rest
    | w :: rest -> groups (w :: current) acc rest
  in
  let groups = groups [] [] items in
  let n = List.length groups in
  let set top acc words =
    let lx, lw = room !floats ~x ~width ~top ~height:1. in
    let line, inline_boxes, drops = set_line strut align ~x:lx ~width:lw ~top words in
    boxes := List.rev_append inline_boxes !boxes;
    backdrops := List.rev_append drops !backdrops;
    (top +. line.height, line :: acc)
  in
  let bottom, lines =
    List.fold_left
      (fun (top, acc) (i, group) ->
        let top = List.fold_left (fun top item -> match item with Clear sides -> cleared !floats sides top | _ -> top) top group in
        let group = List.filter (fun item -> match item with Clear _ -> false | _ -> true) group in
        let only_floats = List.for_all (fun item -> match item with Float _ | Anchor _ -> true | _ -> false) group in
        if group = [] && i = n - 1 then (top, acc)
        else if only_floats && group <> [] then (
          List.iter (place floats ~x ~width ~top boxes) group;
          (* anchors alone: a line of no height where they are *)
          if List.exists (fun item -> match item with Anchor _ -> true | _ -> false) group then
            let line, _, _ = set_line { strut with above = 0.; below = 0. } align ~x ~width ~top group in
            (top, line :: acc)
          else (top, acc))
        else if pre || group = [] then set top acc group
        else
          let units = Array.of_list (units_of group) in
          let sizes = Array.map unit_size units in
          let count = Array.length units in
          let strut_height = strut.above +. strut.below in
          let rec lines top start acc =
            if start >= count then (top, acc)
            else
              let rec leading items = match items with (Float _ as f) :: rest -> f :: leading rest | Anchor _ :: rest -> leading rest | _ -> [] in
              List.iter (place floats ~x ~width ~top boxes) (leading units.(start));
              let lx, lw = room !floats ~x ~width ~top ~height:strut_height in
              if snd sizes.(start) > lw && lw < width then
                (* no room beside the floats: below the first to end *)
                let below = List.fold_left (fun m p -> if p.ptop < top +. strut_height && p.pbottom > top then Float.min m p.pbottom else m) infinity !floats in
                if below = infinity then lines_from top start acc lx lw else lines below start acc
              else lines_from top start acc lx lw
          and lines_from top start acc lx lw =
            let rec extend j w =
              (* a hundredth of a pixel's slack: a box shrunk to fit its
               * line is exactly as wide as it, give or take rounding *)
              if j + 1 < count && w +. fst sizes.(j + 1) +. snd sizes.(j + 1) <= lw +. 0.01 then extend (j + 1) (w +. fst sizes.(j + 1) +. snd sizes.(j + 1)) else j
            in
            let j = extend start (snd sizes.(start)) in
            let words = List.concat (List.init (j - start + 1) (fun k -> if k = 0 then starting_line units.(start) else units.(start + k))) in
            let line, inline_boxes, drops = set_line strut align ~x:lx ~width:lw ~top words in
            boxes := List.rev_append inline_boxes !boxes;
            backdrops := List.rev_append drops !backdrops;
            let bottom = top +. line.height in
            List.iter (place floats ~x ~width ~top:bottom boxes) words;
            lines bottom (j + 1) (line :: acc)
          in
          lines top 0 acc)
      (top, [])
      (List.mapi (fun i g -> (i, g)) groups)
  in
  (List.rev lines, List.rev !boxes, List.rev !backdrops, bottom)

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

type env = {
  metrics : Html_layout.metrics;
  picture_size : string -> (float * float) option;
  style : Dom.element -> Computed.t;
  viewport : float * float;
  positioned : box list ref; (* absolute and fixed boxes, drawn last *)
  (* shrink-to-fit's measure: lines on the left (a centred line at an
   * unlimited width would be far to the right) *)
  measuring : bool;
  (* inside a <center> or an align=center: blocks centred (HTML's
   * "align descendants", -webkit-center) *)
  centring : bool;
  containing : float * float * float; (* the nearest positioned ancestor's padding box: x, y, width *)
}

(* a block being laid out: where its content goes, what is stacked
 * (its bottom [cursor], the margin [pending] below it), the inline
 * content not yet set *)
type ctx = {
  env : env;
  floats : placed list ref; (* its formatting context's *)
  block : Computed.t;
  x : float; (* the content box *)
  width : float;
  mutable cursor : float;
  mutable pending : float;
  mutable absorbed : bool; (* the next child's top margin is already the block's (collapsed through) *)
  mutable children : box list; (* the last first *)
  mutable items : item list; (* the last first *)
  mutable space : bool;
  mutable owner : Dom.element;
  mutable link : string option;
  mutable counter : int; (* its list items so far *)
  mutable decorations : decoration list; (* the inline elements the words now read are in, whose boxes are drawn *)
}

let is_space (c : char) : bool = c = ' ' || c = '\n' || c = '\t' || c = '\r' || c = '\012'

let add_word ?boxed ?owner ?(edge = Body) (ctx : ctx) (ws : word_style) ~(glue : bool) (text : string) (width : float) : unit =
  let rec after_word items = match items with Word _ :: _ -> true | (Anchor _ | Float _) :: rest -> after_word rest | _ -> false in
  let space_before = ctx.space && after_word ctx.items in
  let space = if space_before then ctx.env.metrics ws.look " " else 0. in
  let owner = Option.value owner ~default:ctx.owner in
  ctx.items <-
    Word { text; ws; space_before; glue = glue && space_before; space; width; boxed; owner; decorations = ctx.decorations; edge } :: ctx.items;
  ctx.space <- false

(* U+00A0 (UTF-8's two bytes C2 A0) as a space *)
let nbsp_to_space (text : string) : string =
  let b = Buffer.create (String.length text) in
  let n = String.length text in
  let i = ref 0 in
  while !i < n do
    if !i + 1 < n && text.[!i] = '\xc2' && text.[!i + 1] = '\xa0' then (Buffer.add_char b ' '; i := !i + 2)
    else (Buffer.add_char b text.[!i]; incr i)
  done;
  Buffer.contents b

let add_text (ctx : ctx) (s : Computed.t) (ws : word_style) (text : string) : unit =
  let text = if s.uppercase then String.uppercase_ascii text else text in
  (* a no-break space (U+00A0, &nbsp;) joins two words, and is a space *)
  let word text =
    let text = if String.contains text '\xa0' then nbsp_to_space text else text in
    add_word ctx ws ~glue:(s.white_space = Nowrap) text (ctx.env.metrics ws.look text)
  in
  match s.white_space with
  | Pre | Pre_wrap ->
      List.iteri
        (fun i part ->
          if i > 0 then ctx.items <- Break :: ctx.items;
          if part <> "" then (
            ctx.space <- false;
            word part))
        (String.split_on_char '\n' text)
  | Normal | Nowrap | Pre_line ->
      let b = Buffer.create 16 in
      let end_word () =
        if Buffer.length b > 0 then (
          word (Buffer.contents b);
          Buffer.clear b)
      in
      String.iter
        (fun c ->
          if c = '\n' && s.white_space = Pre_line then (
            end_word ();
            ctx.items <- Break :: ctx.items;
            ctx.space <- false)
          else if is_space c then (
            end_word ();
            ctx.space <- true)
          else Buffer.add_char b c)
        text;
      end_word ()

(* the block's inline content so far set on lines, in an anonymous box *)
let flush_inline (ctx : ctx) : unit =
  let items = List.rev ctx.items in
  ctx.items <- [];
  ctx.space <- false;
  if items <> [] then (
    let has_content = List.exists (fun i -> match i with Word _ | Break | Clear _ -> true | _ -> false) items in
    (* a float among blocks is placed below the margin pending *)
    let top = ctx.cursor +. if has_content then ctx.pending else 0. in
    let floats_top = ctx.cursor +. ctx.pending in
    let strut = word_style ctx.block ~link:None in
    let align = if ctx.env.measuring then Looks.Left else (look_of ctx.block ~link:None).align in
    let pre = match ctx.block.white_space with Pre | Pre_wrap -> true | _ -> false in
    let lines, boxes, backdrops, bottom =
      set_lines ctx.floats strut align ~pre ~x:ctx.x ~width:ctx.width ~top:(if has_content then top else floats_top) items
    in
    ctx.children <-
      { element = None; style = ctx.block; x = ctx.x; y = top; width = ctx.width; height = (if has_content then bottom -. top else 0.);
        border = (0., 0., 0., 0.); children = boxes; lines; backdrops; marker = None }
      :: ctx.children;
    if has_content then (
      ctx.cursor <- bottom;
      ctx.pending <- 0.;
      ctx.absorbed <- false))

(* the first thing in an element's flow, if it is a block: whose top
 * margin collapses with the element's *)
let rec first_block (env : env) (e : Dom.element) : (Dom.element * Computed.t) option =
  let rec go (nodes : Dom.node list) =
    match nodes with
    | [] -> None
    | Text t :: rest -> if String.for_all is_space t then go rest else None
    | Element c :: rest -> (
        let s = env.style c in
        match s.display with
        | Display_none -> go rest
        | _ when s.float <> Side_none || s.position = Absolute || s.position = Fixed -> go rest
        | Contents -> ( match first_block env c with Some _ as found -> found | None -> go rest)
        | Inline | Inline_block | Inline_flex -> None
        | _ -> Some (c, s))
  in
  go e.children

let top_padding_border (s : Computed.t) : float =
  let t, _, _, _ = s.padding and bt, _, _, _ = s.border_width in
  Css_values.resolve t 0. +. bt

(* the margin above an element: its own collapsed with its first
 * child's, through no border and no padding *)
let rec top_margin (env : env) (e : Dom.element) (s : Computed.t) ~(cb_width : float) : float =
  let mt, _, _, _ = s.margin in
  let own = Option.value (size mt cb_width) ~default:0. in
  if own_context s || top_padding_border s > 0. then own
  else match first_block env e with Some (c, cs) -> collapse own (top_margin env c cs ~cb_width) | None -> own

(* the horizontal equation (CSS 2.1 section 10.3.3): margin-left,
 * content width, margin-right; [content] a width already decided
 * (shrink-to-fit, a table's cell) *)
let horizontal (s : Computed.t) ~(cb_width : float) ?content () : float * float * float =
  let _, mr, _, ml = s.margin in
  let _, pr, _, pl = four (fun l -> Css_values.resolve l cb_width) s.padding in
  let _, br, _, bl = s.border_width in
  let chrome = pl +. pr +. bl +. br in
  let clamp w =
    let w = match size s.max_width cb_width with Some m -> Float.min w (if s.border_box then m -. chrome else m) | None -> w in
    Float.max w ((if s.border_box then -.chrome else 0.) +. Css_values.resolve s.min_width cb_width)
  in
  let given = match content with Some w -> Some w | None -> Option.map (fun w -> if s.border_box then Float.max 0. (w -. chrome) else w) (size s.width cb_width) in
  (* a width decided: the margins auto share the rest *)
  let solved w =
    let rest = cb_width -. w -. chrome in
    match (size ml cb_width, size mr cb_width) with
    | None, None -> (Float.max 0. (rest /. 2.), w, Float.max 0. (rest /. 2.))
    | None, Some mr -> (rest -. mr, w, mr)
    | Some ml, None -> (ml, w, rest -. ml)
    | Some ml, Some _ -> (ml, w, rest -. ml)
  in
  match given with
  | None ->
      let ml' = Option.value (size ml cb_width) ~default:0. and mr' = Option.value (size mr cb_width) ~default:0. in
      let natural = Float.max 0. (cb_width -. ml' -. mr' -. chrome) in
      let w = clamp natural in
      (* max-width met: solved again with it as the width (10.4), so
       * that margins auto centre a column *)
      if w <> natural then solved w else (ml', natural, mr')
  | Some w -> solved (clamp w)

let rec layout_block (env : env) (floats : placed list ref) (e : Dom.element) (s : Computed.t) ~(cb_x : float) ~(cb_width : float)
    ~(y : float) ~(marker : Html_layout.marker option) ?content () : box * float =
  (* measuring an intrinsic size: a percentage width is auto (it would
   * be of the size being measured: CSS Sizing, "cyclic percentages") *)
  let s = match s.width with Len l when env.measuring && l.pct <> 0. -> { s with width = Auto } | _ -> s in
  let ml, cw, _ = horizontal s ~cb_width ?content () in
  let pt, pr, pb, pl = four (fun l -> Css_values.resolve l cb_width) s.padding in
  let bt, br, bb, bl = s.border_width in
  let own = own_context s in
  let floats = if own then ref [] else floats in
  let x = cb_x +. ml in
  let env =
    match s.position with Static -> env | _ -> { env with containing = (x +. bl, y +. bt, pl +. cw +. pr) }
  in
  let centring =
    e.name = "center"
    || (e.name <> "table" && match Option.map String.lowercase_ascii (Dom.attribute ~extensions:true "align" e) with Some ("center" | "middle") -> true | _ -> false)
    || (env.centring && s.text_align = Align_center)
  in
  let env = if centring = env.centring then env else { env with centring } in
  let content_top = y +. bt +. pt in
  let ctx =
    { env; floats; block = s; x = x +. bl +. pl; width = cw; cursor = content_top; pending = 0.;
      absorbed = (not own) && pt +. bt = 0.; children = []; items = []; space = false; owner = e;
      link = (if e.name = "a" then Dom.attribute "href" e else None); counter = 0; decorations = [] }
  in
  (match s.display with
  | Flex | Inline_flex -> flex_children ctx e s
  | _ ->
      List.iter (walk ctx s (word_style s ~link:ctx.link)) e.children;
      flush_inline ctx);
  (* the last child's bottom margin goes through, if nothing holds it *)
  let through = (not own) && pb +. bb = 0. && s.height = Auto in
  let content_bottom = if through then ctx.cursor else ctx.cursor +. ctx.pending in
  let content_bottom = if own then List.fold_left (fun m p -> Float.max m p.pbottom) content_bottom !floats else content_bottom in
  let auto_height = content_bottom -. content_top in
  let ch =
    match s.height with
    | Len l when l.pct = 0. -> if s.border_box then Float.max 0. (l.px -. pt -. pb -. bt -. bb) else l.px
    | _ -> auto_height
  in
  (* min-height and max-height, of the border box with box-sizing:
   * border-box (Google's buttons) *)
  let vchrome = if s.border_box then pt +. pb +. bt +. bb else 0. in
  let ch = match s.max_height with Len l when l.pct = 0. -> Float.min ch (l.px -. vchrome) | _ -> ch in
  let ch = Float.max ch (Css_values.resolve s.min_height 0. -. vchrome) in
  ( { element = Some e; style = s; x; y; width = bl +. pl +. cw +. pr +. br; height = bt +. pt +. ch +. pb +. bb;
      border = (bt, br, bb, bl); children = List.rev ctx.children; lines = []; backdrops = []; marker },
    if through then ctx.pending else 0. )

(* a flex container's items laid out (Flex_layout's arithmetic): each
 * measured, the lines cut, the room shared, the items placed along and
 * across *)
and flex_children (ctx : ctx) (e : Dom.element) (s : Computed.t) : unit =
  let env = ctx.env in
  let row = match s.flex_direction with Row | Row_reverse -> true | Column | Column_reverse -> false in
  let no_margins = (Computed.Len Css_values.zero, Computed.Len Css_values.zero, Computed.Len Css_values.zero, Computed.Len Css_values.zero) in
  (* an item is a block (an inline one "blockified"); a run of text an
   * anonymous item in the container's style *)
  let blockify (cs : Computed.t) : Computed.t =
    { cs with display = (match cs.display with Inline | Inline_block | List_item -> Block | Inline_flex -> Flex | d -> d); float = Side_none }
  in
  let items =
    List.concat_map
      (fun (n : Dom.node) ->
        match n with
        | Text t when String.for_all is_space t -> []
        | Text t ->
            [ ( Dom.element "span" [ Text t ],
                { s with display = Block; margin = no_margins; padding = (Css_values.zero, Css_values.zero, Css_values.zero, Css_values.zero);
                  border_width = (0., 0., 0., 0.); width = Auto; height = Auto; min_width = Css_values.zero; max_width = Auto;
                  flex_grow = 0.; flex_shrink = 1.; flex_basis = Auto; background = Css_values.transparent; position = Static;
                  overflow_hidden = false; align_self = None } ) ]
        | Element c -> (
            let cs = env.style c in
            match cs.display with
            | Display_none -> []
            | _ when cs.position = Absolute || cs.position = Fixed ->
                add_absolute ctx c cs;
                []
            | _ -> [ (c, blockify cs) ]))
      e.children
  in
  let items = match s.flex_direction with Row_reverse | Column_reverse -> List.rev items | _ -> items in
  let items = Array.of_list items in
  let measuring = env.measuring in
  let width = ctx.width in
  let gap_main = Css_values.resolve (if row then s.column_gap else s.row_gap) width in
  let gap_cross = Css_values.resolve (if row then s.row_gap else s.column_gap) width in
  let justify = if measuring then Computed.Start else s.justify_content in
  (* an item's chrome across the main axis: margins (auto ones 0, and
   * said), borders and paddings *)
  let chrome (cs : Computed.t) =
    let mt, mr, mb, ml = cs.margin in
    let pt, pr, pb, pl = four (fun l -> Css_values.resolve l width) cs.padding and bt, br, bb, bl = cs.border_width in
    let m x = Option.value (size x width) ~default:0. in
    if row then ((m ml, m mr), pl +. pr +. bl +. br, (ml = Auto, mr = Auto))
    else ((m mt, m mb), pt +. pb +. bt +. bb, (mt = Auto, mb = Auto))
  in
  let align_of (cs : Computed.t) =
    let a = match cs.align_self with Some a -> a | None -> s.align_items in
    (* stretched only if its cross size is auto *)
    if a = Stretch && (if row then cs.height <> Auto else cs.width <> Auto) then Computed.Start else a
  in
  let content_top = ctx.cursor in
  let boxes =
    if row then (
      (* the base sizes: flex-basis, width, or the content's *)
      let bases =
        Array.map
          (fun (c, (cs : Computed.t)) ->
            let (ml, mr), ch, _ = chrome cs in
            let given w = if cs.border_box then Float.max 0. (w -. ch) else w in
            let base =
              match (cs.flex_basis, size cs.width width) with
              | Len l, _ -> given (Css_values.resolve l width)
              | Auto, Some w -> given w
              | Auto, None -> shrink env c cs ~available:infinity
            in
            ml +. base +. ch +. mr)
          items
      in
      let total = Array.fold_left ( +. ) 0. bases +. (gap_main *. float_of_int (max 0 (Array.length items - 1))) in
      let fitem i =
        let _, (cs : Computed.t) = items.(i) in
        let (ml, mr), ch, (ab, aa) = chrome cs in
        let outer w = ml +. w +. ch +. mr in
        (* an item does not shrink below its content (min-width: auto),
         * measured only when it has to shrink *)
        let min_size =
          if cs.min_width.px > 0. || cs.min_width.pct > 0. then outer (Css_values.resolve cs.min_width width)
          else if total > width && (not cs.overflow_hidden) && not measuring then
            outer (Float.min (shrink env (fst items.(i)) cs ~available:0.) (bases.(i) -. ml -. ch -. mr))
          else 0.
        in
        (* measuring: no growing, and no shrinking either -- a row that
         * does not wrap is as wide as its items, even at its narrowest *)
        { Flex_layout.base = bases.(i); grow = (if measuring then 0. else cs.flex_grow); shrink = (if measuring then 0. else cs.flex_shrink); min_size;
          max_size = (match size cs.max_width width with Some m -> outer m | None -> infinity);
          auto_before = ab && not measuring; auto_after = aa && not measuring }
      in
      let fitems = Array.init (Array.length items) fitem in
      let top = ref content_top and boxes = ref [] in
      List.iter
        (fun (first, last) ->
          let line = Array.sub fitems first (last - first + 1) in
          let sizes = Flex_layout.resolve ~room:width ~gap:gap_main line in
          let starts = Flex_layout.place ~justify ~room:width ~gap:gap_main line sizes in
          let laid =
            Array.mapi
              (fun k main ->
                let c, (cs : Computed.t) = items.(first + k) in
                let (ml, mr), ch, _ = chrome cs in
                let mt, _, mb, _ = cs.margin in
                let mt = Option.value (size mt width) ~default:0. and mb = Option.value (size mb width) ~default:0. in
                let cs_in = { cs with margin = no_margins } in
                let b, _ =
                  layout_block env (ref []) c cs_in ~cb_x:(ctx.x +. starts.(k) +. ml) ~cb_width:(main -. ml -. mr) ~y:(!top +. mt) ~marker:None
                    ~content:(Float.max 0. (main -. ml -. mr -. ch)) ()
                in
                (b, cs, mt, mb))
              sizes
          in
          let cross = Array.fold_left (fun m (b, _, mt, mb) -> Float.max m (mt +. b.height +. mb)) 0. laid in
          (* a single line is as tall as its container, if its height is
           * given (section 9.4) *)
          let cross =
            match s.height with
            | Len l when l.pct = 0. && (not s.flex_wrap) && not measuring ->
                let pt, _, pb, _ = four (fun l -> Css_values.resolve l width) s.padding and bt, _, bb, _ = s.border_width in
                Float.max cross (if s.border_box then l.px -. pt -. pb -. bt -. bb else l.px)
            | _ -> cross
          in
          Array.iter
            (fun ((b : box), cs, mt, mb) ->
              let offset, outer = Flex_layout.cross ~align:(align_of cs) ~line:cross ~size:(mt +. b.height +. mb) in
              boxes := relative ctx cs (moved 0. offset { b with height = outer -. mt -. mb }) :: !boxes)
            laid;
          (* measuring: the line's end, its last item's right margin
           * included, marked by an empty box -- a box's edge is inside
           * its margin, and the measure reads edges *)
          (if measuring then
             let last = Array.length sizes - 1 in
             let edge = ctx.x +. starts.(last) +. sizes.(last) in
             boxes :=
               { element = None; style = s; x = edge; y = !top; width = 0.; height = 0.; border = (0., 0., 0., 0.); children = [];
                 lines = []; backdrops = []; marker = None }
               :: !boxes);
          top := !top +. cross +. gap_cross)
        (Flex_layout.lines ~wrap:s.flex_wrap ~room:width ~gap:gap_main fitems);
      ctx.cursor <- (if !boxes = [] then content_top else !top -. gap_cross);
      List.rev !boxes)
    else (
      (* a column: each item laid out at its width first (stretched, or
       * shrunk to fit), its height its base *)
      let laid =
        Array.map
          (fun (c, (cs : Computed.t)) ->
            let _, _, _, ml = cs.margin and _, mr, _, _ = cs.margin in
            let ml' = Option.value (size ml width) ~default:0. and mr' = Option.value (size mr width) ~default:0. in
            let _, pr, _, pl = four (fun l -> Css_values.resolve l width) cs.padding and _, br, _, bl = cs.border_width in
            let hchrome = pl +. pr +. bl +. br in
            let content =
              match (align_of cs, size cs.width width) with
              | _, Some _ -> None
              | Stretch, None -> Some (Float.max 0. (width -. ml' -. mr' -. hchrome))
              | _, None -> Some (shrink env c cs ~available:(width -. ml' -. mr' -. hchrome))
            in
            let b, _ = layout_block env (ref []) c { cs with margin = no_margins } ~cb_x:0. ~cb_width:width ~y:0. ~marker:None ?content () in
            let x_offset =
              match align_of cs with
              | End -> width -. b.width -. mr'
              | Center -> (width -. b.width) /. 2.
              | _ -> ml'
            in
            (c, cs, b, x_offset))
          items
      in
      let fitems =
        Array.map
          (fun (_, (cs : Computed.t), (b : box), _) ->
            let (mt, mb), _, (ab, aa) = chrome cs in
            let base = match (cs.flex_basis, size cs.height 0.) with Len l, _ when l.pct = 0. -> l.px | _, Some h -> h | _ -> b.height in
            { Flex_layout.base = mt +. base +. mb; grow = cs.flex_grow; shrink = cs.flex_shrink;
              min_size = (if cs.overflow_hidden then 0. else mt +. b.height +. mb); max_size = infinity;
              auto_before = ab; auto_after = aa })
          laid
      in
      (* the room along: the container's height if it has one, else the
       * items' own *)
      let room =
        match s.height with
        | Len l when l.pct = 0. && not measuring ->
            let pt, _, pb, _ = four (fun l -> Css_values.resolve l width) s.padding and bt, _, bb, _ = s.border_width in
            if s.border_box then l.px -. pt -. pb -. bt -. bb else l.px
        | _ -> Array.fold_left (fun t (it : Flex_layout.item) -> t +. it.base) 0. fitems +. (gap_main *. float_of_int (max 0 (Array.length fitems - 1)))
      in
      let sizes = Flex_layout.resolve ~room ~gap:gap_main fitems in
      let starts = Flex_layout.place ~justify ~room ~gap:gap_main fitems sizes in
      ctx.cursor <- content_top +. (if Array.length sizes = 0 then 0. else room);
      Array.to_list
        (Array.mapi
           (fun k (_, cs, (b : box), x_offset) ->
             let (mt, mb), _, _ = chrome cs in
             relative ctx cs (moved (ctx.x +. x_offset -. b.x) (content_top +. starts.(k) +. mt -. b.y) { b with height = sizes.(k) -. mt -. mb }))
           laid))
  in
  ctx.children <- List.rev boxes;
  ctx.pending <- 0.

(* shrink-to-fit (CSS 2.1 section 10.3.5): the content's widest line,
 * at most [available], at least its widest word *)
and shrink (env : env) (e : Dom.element) (s : Computed.t) ~(available : float) : float =
  let env = { env with positioned = ref []; measuring = true } in
  let s' = { s with width = Auto; min_width = Css_values.zero; max_width = Auto; margin = (Len Css_values.zero, Len Css_values.zero, Len Css_values.zero, Len Css_values.zero) } in
  let _, _, _, pl = four (fun l -> Css_values.resolve l 0.) s.padding and _, _, _, bl = s.border_width in
  let measure w =
    let b, _ = layout_block env (ref []) e s' ~cb_x:0. ~cb_width:1e6 ~y:0. ~marker:None ~content:w () in
    Float.max 0. (inner_right b -. b.x -. pl -. bl)
  in
  let preferred = measure 1e6 in
  if preferred <= available then preferred else Float.max (measure 0.) available

(* a block placed in the flow, below what is stacked *)
and add_block (ctx : ctx) (e : Dom.element) (s : Computed.t) : unit =
  flush_inline ctx;
  let env = ctx.env in
  let mt = top_margin env e s ~cb_width:ctx.width in
  let _, _, mb, _ = s.margin in
  let mb = Option.value (size mb ctx.width) ~default:0. in
  let margin = if ctx.absorbed then 0. else collapse ctx.pending mt in
  let y = ctx.cursor +. margin in
  let y =
    match s.clear with
    | Side_none -> y
    | Side_left -> cleared !(ctx.floats) [ On_left ] y
    | Side_right -> cleared !(ctx.floats) [ On_right ] y
    | Side_both -> cleared !(ctx.floats) [ On_left; On_right ] y
  in
  let marker =
    if s.display <> List_item then None
    else (
      ctx.counter <- ctx.counter + 1;
      match s.list_style with
      | "none" -> None
      | "disc" | "circle" | "square" -> Some Html_layout.Bullet
      | _ -> Some (Number ctx.counter))
  in
  (* a formatting context of its own beside floats: narrowed *)
  let cb_x, cb_width = if own_context s then room !(ctx.floats) ~x:ctx.x ~width:ctx.width ~top:y ~height:1. else (ctx.x, ctx.width) in
  let box, through =
    match s.display with
    | Table -> (layout_table env e s ~cb_x ~cb_width ~y, 0.)
    | _ -> layout_block env ctx.floats e s ~cb_x ~cb_width ~y ~marker ()
  in
  (* in a <center>: a block narrower than the line, its margins not
   * auto, centred *)
  let box =
    let _, mr, _, ml = s.margin in
    if ctx.env.centring && (not ctx.env.measuring) && ml <> Auto && mr <> Auto && box.width < cb_width then moved (cb_x +. ((cb_width -. box.width) /. 2.) -. box.x) 0. box
    else box
  in
  let box = relative ctx s box in
  let empty = box.height = 0. && box.children = [] in
  ctx.children <- box :: ctx.children;
  if empty && not ctx.absorbed then ctx.pending <- collapse (collapse ctx.pending mt) (collapse mb through)
  else (
    ctx.cursor <- y +. box.height;
    ctx.pending <- collapse mb through;
    ctx.absorbed <- false)

(* position: relative, the box moved by its offsets *)
and relative (ctx : ctx) (s : Computed.t) (b : box) : box =
  if s.position <> Relative then b
  else
    let dx = match (size s.left ctx.width, size s.right ctx.width) with Some l, _ -> l | None, Some r -> -.r | None, None -> 0. in
    let dy = match (size s.top 0., size s.bottom 0.) with Some t, _ -> t | None, Some b -> -.b | None, None -> 0. in
    moved dx dy b

(* position: absolute or fixed, out of the flow *)
and add_absolute (ctx : ctx) (e : Dom.element) (s : Computed.t) : unit =
  let env = ctx.env in
  let cx, cy, cw = match s.position with Fixed -> (0., 0., fst env.viewport) | _ -> env.containing in
  let static_x = ctx.x and static_y = ctx.cursor +. ctx.pending in
  let left = size s.left cw and right = size s.right cw in
  let _, _, _, ml = s.margin in
  let ml = Option.value (size ml cw) ~default:0. in
  let content =
    match (s.width, left, right) with
    | Auto, Some l, Some r ->
        let _, pr, _, pl = four (fun l -> Css_values.resolve l cw) s.padding and _, br, _, bl = s.border_width in
        Some (Float.max 0. (cw -. l -. r -. pl -. pr -. bl -. br))
    | Auto, _, _ -> Some (shrink env e s ~available:cw)
    | _ -> None
  in
  let s_in = { s with margin = (let t, r, b, _ = s.margin in (t, r, b, Len Css_values.zero)) } in
  let box, _ = layout_block env (ref []) e s_in ~cb_x:0. ~cb_width:cw ~y:0. ~marker:None ?content () in
  let x = match (left, right) with Some l, _ -> cx +. l +. ml | None, Some r -> cx +. cw -. r -. box.width | None, None -> static_x +. ml in
  let y = match size s.top 0. with Some t -> cy +. t | None -> static_y in
  env.positioned := moved (x -. box.x) (y -. box.y) box :: !(env.positioned)

(* a float, laid out shrink-to-fit where it is, placed with the lines *)
and float_item (ctx : ctx) (e : Dom.element) (s : Computed.t) : item =
  let env = ctx.env in
  let margin = four (fun m -> Option.value (size m ctx.width) ~default:0.) s.margin in
  let box =
    if e.name = "img" then image_box ctx e s
    else
      let content = match s.width with Auto -> Some (shrink env e s ~available:ctx.width) | _ -> None in
      let s_in = { s with margin = (Len Css_values.zero, Len Css_values.zero, Len Css_values.zero, Len Css_values.zero) } in
      fst (layout_block env (ref []) e s_in ~cb_x:0. ~cb_width:ctx.width ~y:0. ~marker:None ?content ())
  in
  (* measuring: every float on the left -- only its width counts, and a
   * right one would be at the far end of an unlimited line *)
  let fside = if s.float = Side_right && not env.measuring then On_right else On_left in
  Float { fbox = box; fside; fmargin = margin; placed = false }

(* a picture's size: its style's width and height, one of them and its
 * ratio, or its own once it has come; max-width applied *)
and picture_size (ctx : ctx) (e : Dom.element) (s : Computed.t) : (float * float) option =
  let src = Option.value (picture_src e) ~default:"" in
  let w = size s.width ctx.width and h = size s.height 0. in
  let own = ctx.env.picture_size src in
  let wh =
    match (w, h, own) with
    | Some w, Some h, _ -> Some (w, h)
    | Some w, None, Some (iw, ih) when iw > 0. -> Some (w, w *. ih /. iw)
    | None, Some h, Some (iw, ih) when ih > 0. -> Some (h *. iw /. ih, h)
    | None, None, Some wh -> Some wh
    | _ -> None
  in
  match (wh, size s.max_width ctx.width) with
  | Some (w, h), Some m when w > m && w > 0. -> Some (m, h *. m /. w)
  | _ -> wh

(* a floated picture as a box of one line *)
and image_box (ctx : ctx) (e : Dom.element) (s : Computed.t) : box =
  let w, h = Option.value (picture_size ctx e s) ~default:(0., 0.) in
  let src = Option.value (picture_src e) ~default:"" in
  let ws = word_style s ~link:ctx.link in
  let frag : Html_layout.fragment =
    { text = ""; look = ws.look; x = 0.; width = w; baseline = h; picture = Some { src; height = h; middle = false }; control = None; element = e }
  in
  { element = Some e; style = s; x = 0.; y = 0.; width = w; height = h; border = (0., 0., 0., 0.); children = [];
    lines = [ { top = 0.; height = h; baseline = h; fragments = [ frag ]; anchors = [] } ]; backdrops = []; marker = None }

(* a node inside a block: inline content gathered, a block placed *)
and walk (ctx : ctx) (parent : Computed.t) (ws : word_style) (node : Dom.node) : unit =
  match node with
  | Text t -> add_text ctx parent ws t
  | Element e -> (
      let s = ctx.env.style e in
      match s.display with
      | Display_none -> ()
      | _ when s.position = Absolute || s.position = Fixed -> add_absolute ctx e s
      | _ when s.float <> Side_none -> ctx.items <- float_item ctx e s :: ctx.items
      | Contents -> List.iter (walk ctx s (word_style s ~link:ctx.link)) e.children
      | _ when e.name = "img" -> (
          match picture_size ctx e s with
          | Some (w, h) ->
              let src = Option.value (picture_src e) ~default:"" in
              add_word ctx (word_style s ~link:ctx.link) ~glue:false "" w ~owner:e
                ~boxed:(Pic { src; height = h; middle = s.vertical_align = Middle })
          | None -> (
              match Dom.attribute "alt" e with
              | Some alt when String.trim alt <> "" -> add_text ctx s (word_style s ~link:ctx.link) alt
              | _ -> ()))
      | _ when (e.name = "input" || e.name = "select" || e.name = "textarea") -> (
          let look = look_of s ~link:ctx.link in
          match Html_layout.control_size ctx.env.metrics look e with
          | Some (w, h) when s.visible -> add_word ctx (word_style s ~link:ctx.link) ~glue:false "" w ~owner:e ~boxed:(Ctl { element = e; control_height = h })
          (* hidden (opacity: 0, a styled checkbox's): its room only *)
          | Some (w, _) -> add_word ctx (word_style s ~link:ctx.link) ~glue:false "" w ~owner:e
          | None -> ())
      | Inline -> (
          (match (if e.name = "a" then Dom.attribute "name" e else None) with Some n -> ctx.items <- Anchor n :: ctx.items | None -> ());
          (match Dom.attribute "id" e with Some id -> ctx.items <- Anchor id :: ctx.items | None -> ());
          match e.name with
          | "br" ->
              ctx.items <- Break :: ctx.items;
              (match s.clear with
              | Side_left -> ctx.items <- Clear [ On_left ] :: ctx.items
              | Side_right -> ctx.items <- Clear [ On_right ] :: ctx.items
              | Side_both -> ctx.items <- Clear [ On_left; On_right ] :: ctx.items
              | Side_none -> ());
              ctx.space <- false
          | _ ->
              let outer_owner = ctx.owner and outer_link = ctx.link in
              ctx.owner <- e;
              (if e.name = "a" then match Dom.attribute "href" e with Some h -> ctx.link <- Some h | None -> ());
              let ws = word_style s ~link:ctx.link in
              (* its margin, border and padding: spacers at its two
               * ends, joined to its first and last words; its box drawn
               * under its words if it has a background or a border *)
              let _, pr, _, pl = four (fun l -> Css_values.resolve l ctx.width) s.padding and bt, br, bb, bl = s.border_width in
              let _, mr, _, ml = four (fun m -> Option.value (size m ctx.width) ~default:0.) s.margin in
              let outer_decorations = ctx.decorations in
              if s.background.a > 0. || bt +. br +. bb +. bl > 0. then ctx.decorations <- ctx.decorations @ [ { de = e; ds = s; dml = ml; dmr = mr } ];
              if ml +. bl +. pl > 0. then add_word ctx ws ~edge:Lead ~glue:false "" (ml +. bl +. pl);
              List.iter (walk ctx s ws) e.children;
              if mr +. br +. pr > 0. then (
                let space = ctx.space in
                ctx.space <- false;
                add_word ctx ws ~edge:Trail ~glue:false "" (mr +. br +. pr);
                ctx.space <- space);
              ctx.decorations <- outer_decorations;
              ctx.owner <- outer_owner;
              ctx.link <- outer_link)
      | Inline_block | Inline_flex ->
          let margin = four (fun m -> Option.value (size m ctx.width) ~default:0.) s.margin in
          let mt, mr, mb, ml = margin in
          let content = match s.width with Auto -> Some (shrink ctx.env e s ~available:(ctx.width -. ml -. mr)) | _ -> None in
          let s_in = { s with margin = (Len Css_values.zero, Len Css_values.zero, Len Css_values.zero, Len Css_values.zero) } in
          let ib, _ = layout_block ctx.env (ref []) e s_in ~cb_x:0. ~cb_width:ctx.width ~y:0. ~marker:None ?content () in
          let outer = ctx.link in
          (if e.name = "a" then match Dom.attribute "href" e with Some h -> ctx.link <- Some h | None -> ());
          add_word ctx (word_style s ~link:ctx.link) ~glue:false "" (ml +. ib.width +. mr) ~owner:e ~boxed:(Inline { ib; ml; mt; mb });
          ctx.link <- outer
      | Block | List_item | Flex | Grid | Table | Table_row_group | Table_row | Table_cell | Table_caption -> add_block ctx e s)

(* a table (CSS 2.1 chapter 17, the automatic layout): Table_layout's
 * grid and widths, each cell measured at width 0 and without limit,
 * each row as tall as its tallest cell *)
and layout_table (env : env) (table : Dom.element) (s : Computed.t) ~(cb_x : float) ~(cb_width : float) ~(y : float) : box =
  (* measuring: a percentage width is auto, as a block's (layout_block) *)
  let s = match s.width with Len l when env.measuring && l.pct <> 0. -> { s with width = Auto } | _ -> s in
  let cells, _ = Table_layout.grid table in
  (* the cells shown (not display: none: GitHub's small screens' cells),
   * their columns counted again without the others *)
  let cells = List.filter (fun (c : Table_layout.cell) -> (env.style c.element).display <> Display_none) cells in
  let cells =
    List.rev
      (snd
         (List.fold_left
            (fun ((row, column), acc) (c : Table_layout.cell) ->
              let column = if c.row = row then column else 0 in
              ((c.row, column + c.span), { c with column } :: acc))
            ((-1, 0), []) cells))
  in
  let n = List.fold_left (fun n (c : Table_layout.cell) -> max n (c.column + c.span)) 0 cells in
  if cells = [] then fst (layout_block env (ref []) table { s with display = Block } ~cb_x ~cb_width ~y ~marker:None ())
  else
    let spacing =
      match Option.bind (Dom.attribute "cellspacing" table) float_of_string_opt with Some sp when sp >= 0. -> sp | _ -> 2.
    in
    let bt, br, bb, bl = s.border_width in
    let _, pr, _, pl = four (fun l -> Css_values.resolve l cb_width) s.padding in
    let style_of (c : Table_layout.cell) = env.style c.element in
    (* a cell's chrome: its padding and border, across *)
    let chrome (cs : Computed.t) =
      let _, pr, _, pl = four (fun l -> Css_values.resolve l 0.) cs.padding and _, br, _, bl = cs.border_width in
      pl +. pr +. bl +. br
    in
    let cell_box (c : Table_layout.cell) ~x ~width ~y =
      let cs = style_of c in
      let cs = { cs with width = Auto; margin = (Len Css_values.zero, Len Css_values.zero, Len Css_values.zero, Len Css_values.zero) } in
      fst (layout_block env (ref []) c.element cs ~cb_x:x ~cb_width:width ~y ~marker:None ~content:(Float.max 0. (width -. chrome cs)) ())
    in
    let measured =
      List.map
        (fun (c : Table_layout.cell) ->
          let cs = style_of c in
          let lo = shrink env c.element cs ~available:0. +. chrome cs and hi = shrink env c.element cs ~available:1e6 +. chrome cs in
          (* a cell's width= (a hint) is its minimum *)
          let asked = match size cs.width cb_width with Some w -> w | None -> 0. in
          (c, (Float.max lo asked, Float.max hi (Float.max lo asked))))
        cells
    in
    let columns = Table_layout.columns n measured ~spacing in
    let frame = bl +. pl +. pr +. br +. (spacing *. float_of_int (n + 1)) in
    let asked = Option.map (fun w -> if s.border_box then w else w +. bl +. pl +. pr +. br) (size s.width cb_width) in
    let widths = Table_layout.widths ~room:(Option.value asked ~default:cb_width -. frame) ~fixed:(asked <> None) columns in
    let width = Array.fold_left ( +. ) frame widths in
    let _, mr, _, ml = s.margin in
    let x =
      match (size ml cb_width, size mr cb_width) with
      | None, None -> cb_x +. Float.max 0. ((cb_width -. width) /. 2.)
      | None, Some mr -> cb_x +. cb_width -. width -. mr
      | Some ml, _ -> cb_x +. ml
    in
    let sum i j = let t = ref 0. in for k = i to j - 1 do t := !t +. widths.(k) done; !t in
    let column_x i = x +. bl +. pl +. spacing +. sum 0 i +. (spacing *. float_of_int i) in
    let cell_width (c : Table_layout.cell) = sum c.column (c.column + c.span) +. (spacing *. float_of_int (c.span - 1)) in
    let caption =
      Option.map
        (fun e -> fst (layout_block env (ref []) e (env.style e) ~cb_x:x ~cb_width:width ~y ~marker:None ()))
        (Table_layout.caption table)
    in
    let top = match caption with Some c -> y +. c.height | None -> y in
    (* the rows, in Table_layout's order (an empty one too: Hacker News'
     * spacers), for their heights and backgrounds *)
    let trs =
      let rec trs (e : Dom.element) =
        List.concat_map
          (fun (n : Dom.node) ->
            match n with
            | Element ({ name = "tr"; _ } as tr) -> [ tr ]
            | Element ({ name = "thead" | "tbody" | "tfoot"; _ } as g) -> trs g
            | _ -> [])
          e.children
      in
      Array.of_list (trs table)
    in
    let rows = List.fold_left (fun m (c : Table_layout.cell) -> max m (c.row + 1)) (Array.length trs) cells in
    let tr_of (c : Table_layout.cell) = if c.row < Array.length trs then Some trs.(c.row) else None in
    let row_top = ref (top +. bt +. spacing) and boxes = ref [] in
    for r = 0 to rows - 1 do
      let row = List.filter (fun (c : Table_layout.cell) -> c.row = r) cells in
      let laid = List.map (fun c -> (c, cell_box c ~x:(column_x c.column) ~width:(cell_width c) ~y:0.)) row in
      let row_height = List.fold_left (fun m (_, (b : box)) -> Float.max m b.height) 0. laid in
      (* a cell's height *)
      let row_height =
        List.fold_left (fun m ((c : Table_layout.cell), _) -> match size (style_of c).height 0. with Some h -> Float.max m h | None -> m) row_height laid
      in
      (* and the row's own (Hacker News' spacer rows, height: 5px) *)
      let row_height =
        match (if r < Array.length trs then size (env.style trs.(r)).height 0. else None) with
        | Some h -> Float.max row_height h
        | None -> row_height
      in
      List.iter
        (fun ((c : Table_layout.cell), (b : box)) ->
          let cs = style_of c in
          let offset = match cs.vertical_align with Top | Baseline | Text_top -> 0. | Bottom | Text_bottom -> row_height -. b.height | _ -> (row_height -. b.height) /. 2. in
          let b = moved 0. (!row_top +. offset) b in
          (* the cell's box is its rectangle, its row's background if
           * it has none *)
          let style =
            match tr_of c with
            | Some tr when b.style.background.a = 0. -> { b.style with background = (env.style tr).background }
            | _ -> b.style
          in
          boxes := { b with y = !row_top; height = row_height; style } :: !boxes)
        laid;
      row_top := !row_top +. row_height +. spacing
    done;
    { element = Some table; style = s; x; y; width; height = !row_top +. bb -. y; border = (bt, br, bb, bl);
      children = Option.to_list caption @ List.rev !boxes; lines = []; backdrops = []; marker = None }

let layout (metrics : Html_layout.metrics) ?(picture_size = fun _ -> None) ~(viewport : float * float) (style : Dom.element -> Computed.t)
    (root : Dom.element) : box =
  let positioned = ref [] in
  let env = { metrics; picture_size; style; viewport; positioned; measuring = false; centring = false; containing = (0., 0., fst viewport) } in
  let s = style root in
  (* the root is its own formatting context: its floats inside it *)
  let s = { s with overflow_hidden = true } in
  let page, _ = layout_block env (ref []) root s ~cb_x:0. ~cb_width:(fst viewport) ~y:0. ~marker:None () in
  let bottom = List.fold_left (fun m (b : box) -> Float.max m (b.y +. b.height)) page.height !positioned in
  { page with height = bottom; children = page.children @ List.rev !positioned }
