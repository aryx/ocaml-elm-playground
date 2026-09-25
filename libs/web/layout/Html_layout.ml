(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Html_layout.mli *)

type metrics = Looks.t -> string -> float
type picture = { src : string; height : float; middle : bool }
type fragment = { text : string; look : Looks.t; x : float; width : float; baseline : float; picture : picture option }
type line = { top : float; height : float; baseline : float; fragments : fragment list; anchors : string list }
type kind = Block of Dom.element | Anonymous | Rule of Dom.element
type marker = Bullet | Number of int

type box = {
  kind : kind;
  x : float;
  y : float;
  width : float;
  height : float;
  children : box list;
  lines : line list;
  marker : marker option;
}

type unit_ = { space : float; width : float }
type breaker = measure:float -> unit_ array -> (int * int) list

(*****************************************************************************)
(* Breaking lines *)
(*****************************************************************************)

(* Linebreak.greedy's rule, with each unit's own space (a page's words
 * are in several looks, their spaces of several widths) *)
let greedy : breaker =
 fun ~measure units ->
  let n = Array.length units in
  let rec go start acc =
    if start >= n then List.rev acc
    else
      (* the line from [start], as long as the next unit still fits *)
      let rec extend j w =
        if j + 1 < n && w +. units.(j + 1).space +. units.(j + 1).width <= measure then
          extend (j + 1) (w +. units.(j + 1).space +. units.(j + 1).width)
        else j
      in
      let j = extend start units.(start).width in
      go (j + 1) ((start, j) :: acc)
  in
  go 0 []

(*****************************************************************************)
(* Inline content: words, then lines *)
(*****************************************************************************)

(* what inline content is cut into: words, the breaks the page asks
 * for (<br>, a newline in <pre>), and the places a #fragment can name
 * (<a name=...>, an id=), of no width; a word may be an image, its
 * width then the picture's *)
type item =
  | Word of { text : string; look : Looks.t; space_before : bool; picture : (picture * float) option }
  | Break
  | Anchor of string

(* a word's width: its text's in its look, or its picture's *)
let word_width (metrics : metrics) (look : Looks.t) (text : string) (picture : (picture * float) option) : float =
  match picture with Some (_, w) -> w | None -> metrics look text

(* a line's words placed: x from the line's start, then shifted by
 * the alignment; the line as tall as its tallest look needs *)
let set_line (metrics : metrics) (block : Looks.t) ~(x : float) ~(width : float) ~(top : float) (words : item list) : line =
  let placed, line_width =
    List.fold_left
      (fun (placed, pen) item ->
        match item with
        | Word { text; look; space_before; picture } ->
            let pen = if space_before then pen +. metrics look " " else pen in
            let w = word_width metrics look text picture in
            ((text, look, pen, w, Option.map fst picture) :: placed, pen +. w)
        | Break | Anchor _ -> (placed, pen))
      ([], 0.) words
  in
  let placed = List.rev placed in
  let shift =
    match block.align with
    | Left -> 0.
    | Center -> Float.max 0. ((width -. line_width) /. 2.)
    | Right -> Float.max 0. (width -. line_width)
  in
  (* half-leading: the line height's room beyond ascent and descent,
   * half above and half below *)
  let above (l : Looks.t) = (0.8 *. l.size) +. (((Looks.leading -. 1.) *. l.size) /. 2.) in
  let below (l : Looks.t) = (0.2 *. l.size) +. (((Looks.leading -. 1.) *. l.size) /. 2.) in
  (* each word's room above and below the baseline: a text's by its
   * look, a picture's by its height (no leading), its bottom or its
   * middle on the baseline *)
  let extent (look, picture) =
    match picture with
    | Some { height; middle = false; _ } -> (height, 0.)
    | Some { height; middle = true; _ } -> (height /. 2., height /. 2.)
    | None -> (above look, below look)
  in
  let extents =
    match placed with [] -> [ extent (block, None) ] | _ -> List.map (fun (_, l, _, _, p) -> extent (l, p)) placed
  in
  let up = List.fold_left (fun m (a, _) -> Float.max m a) 0. extents in
  let down = List.fold_left (fun m (_, b) -> Float.max m b) 0. extents in
  let baseline = top +. up in
  {
    top;
    height = up +. down;
    baseline;
    fragments =
      List.map (fun (text, look, pen, w, picture) -> { text; look; x = x +. shift +. pen; width = w; baseline; picture }) placed;
    anchors = List.filter_map (fun item -> match item with Anchor name -> Some name | _ -> None) words;
  }

(* a run of words between two breaks, as units -- the words stuck
 * together, a unit starting at each word with a space before it *)
let units_of (words : item list) : item list list =
  let rec go current acc words =
    match words with
    | [] -> List.rev (if current = [] then acc else List.rev current :: acc)
    | (Word { space_before = true; _ } as w) :: rest when current <> [] -> go [ w ] (List.rev current :: acc) rest
    | w :: rest -> go (w :: current) acc rest
  in
  go [] [] words

(* a unit's words, its first without the space before it: it starts
 * a line *)
let rec starting_line (unit : item list) : item list =
  match unit with
  | Word w :: rest -> Word { w with space_before = false } :: rest
  | Anchor a :: rest -> Anchor a :: starting_line rest
  | _ -> unit

(* the items cut at the breaks, each run of words broken into lines by
 * [breaker] (never in <pre>); an empty line where the page asked for
 * one, not after the last break *)
let lines_of (metrics : metrics) (breaker : breaker) (block : Looks.t) ~(x : float) ~(width : float) ~(top : float)
    (items : item list) : line list =
  let rec groups current acc items =
    match items with
    | [] -> List.rev (List.rev current :: acc)
    | Break :: rest -> groups [] (List.rev current :: acc) rest
    | w :: rest -> groups (w :: current) acc rest
  in
  let groups = groups [] [] items in
  let n = List.length groups in
  (* a group's lines: its words, broken into lines *)
  let broken (group : item list) : item list list =
    if block.pre || group = [] then [ group ]
    else
      let units = Array.of_list (units_of group) in
      let measure u =
        List.fold_left
          (fun (space, width) item ->
            match item with
            | Word { text; look; space_before; picture } ->
                let w = word_width metrics look text picture in
                if width = 0. && space = 0. && space_before then (metrics look " ", w) else (space, width +. w)
            | Break | Anchor _ -> (space, width))
          (0., 0.) u
      in
      let sizes = Array.map (fun u -> let space, width = measure u in { space; width }) units in
      breaker ~measure:width sizes
      |> List.map (fun (i, j) ->
             List.concat (List.mapi (fun k u -> if k = 0 then starting_line u else u) (Array.to_list (Array.sub units i (j - i + 1)))))
  in
  let _, lines =
    List.fold_left
      (fun (top, lines) (i, group) ->
        if group = [] && i = n - 1 then (top, lines)
        else
          List.fold_left
            (fun (top, lines) words ->
              let line = set_line metrics block ~x ~width ~top words in
              (top +. line.height, line :: lines))
            (top, lines) (broken group))
      (top, [])
      (List.mapi (fun i g -> (i, g)) groups)
  in
  List.rev lines

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

(* a block being laid out: where its content goes, what is stacked so
 * far (its bottom [cursor], and the margin [pending] below the last
 * child), and the inline content not yet set *)
type ctx = {
  metrics : metrics;
  breaker : breaker;
  picture_size : string -> (float * float) option;
  name : string; (* the block's element's: a list's items are numbered *)
  look : Looks.t; (* the block's: its alignment, its empty lines *)
  x : float;
  width : float;
  mutable cursor : float;
  mutable pending : float;
  mutable children : box list; (* the last first *)
  mutable items : item list; (* the last first *)
  mutable space : bool; (* a space read since the last word *)
  mutable items_seen : int; (* its <li>s so far *)
}

let is_space (c : char) : bool = c = ' ' || c = '\n' || c = '\t' || c = '\r'

let add_word ?picture (ctx : ctx) (look : Looks.t) (text : string) : unit =
  (* a word before this one on the line, anchors (of no width) skipped *)
  let rec after_word items = match items with Word _ :: _ -> true | Anchor _ :: rest -> after_word rest | _ -> false in
  let after_word = after_word ctx.items in
  ctx.items <- Word { text; look; space_before = ctx.space && after_word; picture } :: ctx.items;
  ctx.space <- false

(* text outside <pre>: its runs of spaces are one space, between words *)
let add_text (ctx : ctx) (look : Looks.t) (s : string) : unit =
  let word = Buffer.create 16 in
  let end_word () =
    if Buffer.length word > 0 then (
      add_word ctx look (Buffer.contents word);
      Buffer.clear word)
  in
  String.iter
    (fun c ->
      if is_space c then (
        end_word ();
        ctx.space <- true)
      else Buffer.add_char word c)
    s;
  end_word ()

(* text inside <pre>: a newline is a break, the rest is kept *)
let add_pre_text (ctx : ctx) (look : Looks.t) (s : string) : unit =
  List.iteri
    (fun i part ->
      if i > 0 then ctx.items <- Break :: ctx.items;
      if part <> "" then ctx.items <- Word { text = part; look; space_before = false; picture = None } :: ctx.items)
    (String.split_on_char '\n' s)

(* the inline content gathered, set on lines in an anonymous box *)
let flush_inline (ctx : ctx) : unit =
  let items = List.rev ctx.items in
  ctx.items <- [];
  ctx.space <- false;
  let anchors = List.filter_map (fun i -> match i with Anchor a -> Some a | _ -> None) items in
  if List.exists (fun i -> match i with Word _ -> true | Break | Anchor _ -> false) items then (
    let top = ctx.cursor +. ctx.pending in
    let lines = lines_of ctx.metrics ctx.breaker ctx.look ~x:ctx.x ~width:ctx.width ~top items in
    let height = List.fold_left (fun h (l : line) -> h +. l.height) 0. lines in
    ctx.children <-
      { kind = Anonymous; x = ctx.x; y = top; width = ctx.width; height; children = []; lines; marker = None }
      :: ctx.children;
    ctx.cursor <- top +. height;
    ctx.pending <- 0.)
  else if anchors <> [] then
    (* anchors with no text (<a name=top></a> before a heading): a line
     * of no height where they are, so that a #fragment finds them *)
    let top = ctx.cursor +. ctx.pending in
    ctx.children <-
      {
        kind = Anonymous;
        x = ctx.x;
        y = top;
        width = ctx.width;
        height = 0.;
        children = [];
        lines = [ { top; height = 0.; baseline = top; fragments = []; anchors } ];
        marker = None;
      }
      :: ctx.children

let rec layout_block (metrics : metrics) (breaker : breaker) (picture_size : string -> (float * float) option)
    (look : Looks.t) (e : Dom.element) ~(marker : marker option) ~(x : float) ~(width : float) ~(y : float) : box =
  let ctx =
    {
      metrics;
      breaker;
      picture_size;
      name = e.name;
      look;
      x;
      width;
      cursor = y;
      pending = 0.;
      children = [];
      items = [];
      space = false;
      items_seen = 0;
    }
  in
  List.iter (walk ctx look) e.children;
  flush_inline ctx;
  {
    kind = Block e;
    x;
    y;
    width;
    height = ctx.cursor +. ctx.pending -. y;
    children = List.rev ctx.children;
    lines = [];
    marker;
  }

(* a node inside a block, in the look of what it is in: inline content
 * gathered, a block placed below what is stacked (the inline content
 * before it set first) *)
and walk (ctx : ctx) (look : Looks.t) (node : Dom.node) : unit =
  match node with
  | Text s -> if look.pre then add_pre_text ctx look s else add_text ctx look s
  | Element e -> (
      let l = Looks.look look e in
      let b = Looks.box l e in
      match b.display with
      | Hidden -> ()
      | Inline -> (
          (* <a name=x> (HTML 2.0's way) or id=x (HTML 4's): a place *)
          (match (if e.name = "a" then Dom.attribute "name" e else None) with
          | Some name -> ctx.items <- Anchor name :: ctx.items
          | None -> ());
          (match Dom.attribute "id" e with Some id -> ctx.items <- Anchor id :: ctx.items | None -> ());
          match e.name with
          | "br" -> ctx.items <- Break :: ctx.items
          | "img" -> (
              (* its size: the page's width= and height=, else the
               * decoded picture's; else its alt text, until then *)
              let src = Option.value (Dom.attribute "src" e) ~default:"" in
              let number a = Option.bind (Dom.attribute a e) float_of_string_opt in
              let size = match (number "width", number "height") with Some w, Some h -> Some (w, h) | _ -> ctx.picture_size src in
              match size with
              | Some (w, h) ->
                  let middle = Option.map String.lowercase_ascii (Dom.attribute "align" e) = Some "middle" in
                  add_word ctx l "" ~picture:({ src; height = h; middle }, w)
              | None -> add_word ctx l (match Dom.attribute "alt" e with Some alt -> alt | None -> "[IMAGE]"))
          | _ -> List.iter (walk ctx l) e.children)
      | Block | Rule ->
          flush_inline ctx;
          let y = ctx.cursor +. Float.max ctx.pending b.margin_top in
          let marker =
            if e.name <> "li" then None
            else (
              ctx.items_seen <- ctx.items_seen + 1;
              Some (if ctx.name = "ol" then Number ctx.items_seen else Bullet))
          in
          let child =
            match b.display with
            | Rule ->
                { kind = Rule e; x = ctx.x; y; width = ctx.width; height = 2.; children = []; lines = []; marker = None }
            | _ ->
                layout_block ctx.metrics ctx.breaker ctx.picture_size l e ~marker ~x:(ctx.x +. b.indent)
                  ~width:(ctx.width -. b.indent -. b.right) ~y
          in
          ctx.children <- child :: ctx.children;
          ctx.cursor <- y +. child.height;
          ctx.pending <- b.margin_bottom)

let layout (metrics : metrics) ?(breaker = greedy) ?(picture_size = fun _ -> None) ~(root : Looks.t) ~(width : float)
    (html : Dom.element) : box =
  layout_block metrics breaker picture_size (Looks.look root html) html ~marker:None ~x:0. ~width ~y:0.

let rec fragments (b : box) : fragment list =
  List.concat_map (fun (l : line) -> l.fragments) b.lines @ List.concat_map fragments b.children

let rec first_baseline (b : box) : float option =
  match b.lines with
  | l :: _ -> Some l.baseline
  | [] -> List.fold_left (fun found c -> match found with Some _ -> found | None -> first_baseline c) None b.children
