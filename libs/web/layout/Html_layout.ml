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
type control = { element : Dom.element; control_height : float }

type fragment = {
  text : string;
  look : Looks.t;
  x : float;
  width : float;
  baseline : float;
  picture : picture option;
  control : control option;
}

(* what a word may be instead of text: a box of its own size *)
type boxed = Pic of picture | Ctl of control
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
  floats : fragment list;
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
 * (<a name=...>, an id=), of no width; a word may be an image or a
 * form's control, its width then the box's *)
type item =
  | Word of { text : string; look : Looks.t; space_before : bool; boxed : (boxed * float) option }
  | Break
  | Anchor of string
  | Float of floating (* <img align=left|right> *)
  | Clear of side list (* <br clear=...>: the next line below those floats *)

(* a picture the text flows around (Netscape 1.0): taken out of the
 * line, against the left or right edge, the lines beside it shortened
 * until its bottom *)
and side = On_left | On_right

and floating = { side : side; fw : float; fh : float; fpic : picture; flook : Looks.t; mutable placed : bool }

(* a word's width: its text's in its look, or its box's *)
let word_width (metrics : metrics) (look : Looks.t) (text : string) (boxed : (boxed * float) option) : float =
  match boxed with Some (_, w) -> w | None -> metrics look text

(* a line's words placed: x from the line's start, then shifted by
 * the alignment; the line as tall as its tallest look needs *)
let set_line (metrics : metrics) (block : Looks.t) ~(x : float) ~(width : float) ~(top : float) (words : item list) : line =
  let placed, line_width =
    List.fold_left
      (fun (placed, pen) item ->
        match item with
        | Word { text; look; space_before; boxed } ->
            let pen = if space_before then pen +. metrics look " " else pen in
            let w = word_width metrics look text boxed in
            ((text, look, pen, w, Option.map fst boxed) :: placed, pen +. w)
        | Break | Anchor _ | Float _ | Clear _ -> (placed, pen))
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
   * middle on the baseline; a control's, its bottom a quarter of it
   * below *)
  let extent (look, boxed) =
    match boxed with
    | Some (Pic { height; middle = false; _ }) -> (height, 0.)
    | Some (Pic { height; middle = true; _ }) -> (height /. 2., height /. 2.)
    | Some (Ctl { control_height = h; _ }) -> (0.75 *. h, 0.25 *. h)
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
      List.map
        (fun (text, look, pen, w, boxed) ->
          let picture = match boxed with Some (Pic p) -> Some p | _ -> None in
          let control = match boxed with Some (Ctl c) -> Some c | _ -> None in
          { text; look; x = x +. shift +. pen; width = w; baseline; picture; control })
        placed;
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
  | Float f :: rest -> Float f :: starting_line rest
  | _ -> unit

(*****************************************************************************)
(* Floats *)
(*****************************************************************************)

(* a float placed: the page's floats, shared by all its blocks -- a
 * picture floated in one paragraph shortens the lines of the next *)
type placed = { pside : side; frag : fragment; ptop : float; pbottom : float }

(* between a float and the text beside it *)
let gap = 6.

(* the room for a line from [top], [height] high, beside the floats:
 * its left edge and its width *)
let room (floats : placed list) ~(x : float) ~(width : float) ~(top : float) ~(height : float) : float * float =
  let left, right =
    List.fold_left
      (fun (l, r) p ->
        if p.ptop >= top +. height || p.pbottom <= top then (l, r)
        else
          match p.pside with
          | On_left -> (Float.max l (p.frag.x +. p.frag.width +. gap), r)
          | On_right -> (l, Float.min r (p.frag.x -. gap)))
      (x, x +. width) floats
  in
  (left, right -. left)

(* a float put at [top], against the edge of the room there *)
let place (floats : placed list ref) ~(x : float) ~(width : float) ~(top : float) (f : floating) : unit =
  f.placed <- true;
  let left, w = room !floats ~x ~width ~top ~height:f.fh in
  let fx = match f.side with On_left -> left | On_right -> left +. w -. f.fw in
  let frag = { text = ""; look = f.flook; x = fx; width = f.fw; baseline = top +. f.fh; picture = Some f.fpic; control = None } in
  floats := { pside = f.side; frag; ptop = top; pbottom = top +. f.fh } :: !floats

(* lines filled one at a time, each as wide as the floats beside it
 * leave (greedy: Knuth and Plass score a paragraph of one width); a
 * float met in a line is put below it, one before a line's first word
 * at its top; a unit too wide for the room goes below the float *)
let flow (metrics : metrics) (block : Looks.t) (floats : placed list ref) ~(x : float) ~(width : float) ~(top : float)
    (units : item list array) (sizes : unit_ array) : line list * float =
  let n = Array.length units in
  let line_height = Looks.leading *. block.size in
  let unplaced items = List.filter_map (fun i -> match i with Float f when not f.placed -> Some f | _ -> None) items in
  let rec leading items = match items with (Float _ as f) :: rest -> f :: leading rest | Anchor _ :: rest -> leading rest | _ -> [] in
  let rec go top start acc =
    if start >= n then (List.rev acc, top)
    else (
      List.iter (place floats ~x ~width ~top) (unplaced (leading units.(start)));
      let lx, lw = room !floats ~x ~width ~top ~height:line_height in
      if sizes.(start).width > lw && lw < width then
        (* no room beside the floats: below the first of them to end *)
        let below =
          List.fold_left
            (fun m p -> if p.ptop < top +. line_height && p.pbottom > top then Float.min m p.pbottom else m)
            infinity !floats
        in
        go below start acc
      else
        let rec extend j w =
          if j + 1 < n && w +. sizes.(j + 1).space +. sizes.(j + 1).width <= lw then
            extend (j + 1) (w +. sizes.(j + 1).space +. sizes.(j + 1).width)
          else j
        in
        let j = extend start sizes.(start).width in
        let words =
          List.concat (List.init (j - start + 1) (fun k -> if k = 0 then starting_line units.(start) else units.(start + k)))
        in
        let line = set_line metrics block ~x:lx ~width:lw ~top words in
        List.iter (place floats ~x ~width ~top:(top +. line.height)) (unplaced words);
        go (top +. line.height) (j + 1) (line :: acc))
  in
  go top 0 []

(* the items cut at the breaks, each run of words broken into lines by
 * [breaker] (never in <pre>); an empty line where the page asked for
 * one, not after the last break. Where there are floats (placed and
 * not yet ended, or among the words), the lines are [flow]ed around
 * them instead; the floats placed are returned too. *)
let lines_of (metrics : metrics) (breaker : breaker) (floats : placed list ref) (block : Looks.t) ~(x : float)
    ~(width : float) ~(top : float) (items : item list) : line list * fragment list * float =
  let before = List.length !floats in
  let rec groups current acc items =
    match items with
    | [] -> List.rev (List.rev current :: acc)
    | Break :: rest -> groups [] (List.rev current :: acc) rest
    | w :: rest -> groups (w :: current) acc rest
  in
  let groups = groups [] [] items in
  let n = List.length groups in
  (* a group's units, and their sizes *)
  let broken (group : item list) : item list array * unit_ array =
      let units = Array.of_list (units_of group) in
      let measure u =
        List.fold_left
          (fun (space, width) item ->
            match item with
            | Word { text; look; space_before; boxed } ->
                let w = word_width metrics look text boxed in
                if width = 0. && space = 0. && space_before then (metrics look " ", w) else (space, width +. w)
            | Break | Anchor _ | Float _ | Clear _ -> (space, width))
          (0., 0.) u
      in
      (units, Array.map (fun u -> let space, width = measure u in { space; width }) units)
  in
  let set (top, lines) words =
    let line = set_line metrics block ~x ~width ~top words in
    (top +. line.height, line :: lines)
  in
  let bottom, lines =
    List.fold_left
      (fun (top, lines) (i, group) ->
        (* <br clear=...>: below the floats of those sides *)
        let top =
          List.fold_left
            (fun top item ->
              match item with
              | Clear sides -> List.fold_left (fun t p -> if List.mem p.pside sides then Float.max t p.pbottom else t) top !floats
              | _ -> top)
            top group
        in
        let group = List.filter (fun item -> match item with Clear _ -> false | _ -> true) group in
        let beside =
          List.exists (fun p -> p.pbottom > top) !floats || List.exists (fun i -> match i with Float _ -> true | _ -> false) group
        in
        if group = [] && i = n - 1 then (top, lines)
        else if block.pre || group = [] then set (top, lines) group
        else
          let units, sizes = broken group in
          if beside then
            let flowed, top = flow metrics block floats ~x ~width ~top units sizes in
            (top, List.rev_append flowed lines)
          else
            breaker ~measure:width sizes
            |> List.map (fun (i, j) ->
                   List.concat
                     (List.mapi (fun k u -> if k = 0 then starting_line u else u) (Array.to_list (Array.sub units i (j - i + 1)))))
            |> List.fold_left set (top, lines))
      (top, [])
      (List.mapi (fun i g -> (i, g)) groups)
  in
  let placed = List.filteri (fun i _ -> i < List.length !floats - before) !floats in
  (List.rev lines, List.rev_map (fun p -> p.frag) placed, bottom)

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
  floats : placed list ref; (* the page's, shared *)
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

let add_word ?boxed (ctx : ctx) (look : Looks.t) (text : string) : unit =
  (* a word before this one on the line, anchors (of no width) skipped *)
  let rec after_word items =
    match items with Word _ :: _ -> true | (Anchor _ | Float _) :: rest -> after_word rest | _ -> false
  in
  let after_word = after_word ctx.items in
  ctx.items <- Word { text; look; space_before = ctx.space && after_word; boxed } :: ctx.items;
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
      if part <> "" then ctx.items <- Word { text = part; look; space_before = false; boxed = None } :: ctx.items)
    (String.split_on_char '\n' s)

(* a form's control's size, in the look it is in, by its kind; none for
 * a hidden one, or what is not a control *)
let control_size (metrics : metrics) (l : Looks.t) (e : Dom.element) : (float * float) option =
  let number name default =
    match Option.bind (Dom.attribute name e) int_of_string_opt with Some n when n > 0 -> float_of_int n | _ -> default
  in
  (* a fixed-width character's cell: a field's text is set on one *)
  let cell = 0.6 *. l.size in
  let button label = Some (metrics l label +. (1.4 *. l.size), 1.7 *. l.size) in
  match Forms.control e with
  | None -> None
  | Some c -> (
      match c.kind with
      | Hidden -> None
      | Text | Password -> Some ((number "size" 20. *. cell) +. 8., 1.6 *. l.size)
      | Checkbox | Radio -> Some (0.9 *. l.size, 0.9 *. l.size)
      | Submit | Reset -> button (Forms.label c)
      | Select opts ->
          let widest = List.fold_left (fun w (label, _) -> Float.max w (metrics l label)) 0. opts in
          Some (widest +. (2.2 *. l.size), 1.7 *. l.size)
      | Textarea -> Some ((number "cols" 20. *. cell) +. 8., (number "rows" 2. *. Looks.leading *. l.size) +. 8.))

(* the inline content gathered, set on lines in an anonymous box *)
let flush_inline (ctx : ctx) : unit =
  let items = List.rev ctx.items in
  ctx.items <- [];
  ctx.space <- false;
  let anchors = List.filter_map (fun i -> match i with Anchor a -> Some a | _ -> None) items in
  if List.exists (fun i -> match i with Word _ | Float _ | Clear _ -> true | Break | Anchor _ -> false) items then (
    let top = ctx.cursor +. ctx.pending in
    let lines, floats, bottom = lines_of ctx.metrics ctx.breaker ctx.floats ctx.look ~x:ctx.x ~width:ctx.width ~top items in
    let height = bottom -. top in
    ctx.children <-
      { kind = Anonymous; x = ctx.x; y = top; width = ctx.width; height; children = []; lines; floats; marker = None }
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
        floats = [];
        marker = None;
      }
      :: ctx.children

let rec layout_block (metrics : metrics) (breaker : breaker) (picture_size : string -> (float * float) option)
    (floats : placed list ref) (look : Looks.t) (e : Dom.element) ~(marker : marker option) ~(x : float) ~(width : float)
    ~(y : float) : box =
  let ctx =
    {
      metrics;
      breaker;
      picture_size;
      floats;
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
    floats = [];
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
          (* Netscape's attributes too, when the browser knows them *)
          let attribute name = Dom.attribute ~extensions:l.extensions name e in
          match e.name with
          | "br" -> (
              ctx.items <- Break :: ctx.items;
              match Option.map String.lowercase_ascii (attribute "clear") with
              | Some "left" -> ctx.items <- Clear [ On_left ] :: ctx.items
              | Some "right" -> ctx.items <- Clear [ On_right ] :: ctx.items
              | Some "all" -> ctx.items <- Clear [ On_left; On_right ] :: ctx.items
              | _ -> ())
          | "img" -> (
              (* its size: the page's width= and height= (Netscape's),
               * else the decoded picture's; else its alt text, until
               * then *)
              let src = Option.value (Dom.attribute "src" e) ~default:"" in
              let number a = Option.bind (attribute a) float_of_string_opt in
              let size = match (number "width", number "height") with Some w, Some h -> Some (w, h) | _ -> ctx.picture_size src in
              let align = Option.map String.lowercase_ascii (attribute "align") in
              match (size, align) with
              | Some (w, h), Some (("left" | "right") as side) ->
                  let side = if side = "left" then On_left else On_right in
                  ctx.items <-
                    Float { side; fw = w; fh = h; fpic = { src; height = h; middle = false }; flook = l; placed = false }
                    :: ctx.items
              | Some (w, h), _ ->
                  let middle = align = Some "middle" in
                  add_word ctx l "" ~boxed:(Pic { src; height = h; middle }, w)
              | None, _ -> add_word ctx l (match Dom.attribute "alt" e with Some alt -> alt | None -> "[IMAGE]"))
          | "input" | "select" | "textarea" -> (
              match control_size ctx.metrics l e with
              | Some (w, h) -> add_word ctx l "" ~boxed:(Ctl { element = e; control_height = h }, w)
              | None -> ())
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
                (* Netscape's size= (its thickness), width= (pixels or
                 * a percentage of the line), align= (centred) *)
                let attribute name = Dom.attribute ~extensions:l.extensions name e in
                let height = match Option.bind (attribute "size") float_of_string_opt with Some s when s > 0. -> s | _ -> 2. in
                let width =
                  match attribute "width" with
                  | Some w when String.ends_with ~suffix:"%" w -> (
                      match float_of_string_opt (String.sub w 0 (String.length w - 1)) with
                      | Some p -> Float.min ctx.width (ctx.width *. p /. 100.)
                      | None -> ctx.width)
                  | Some w -> ( match float_of_string_opt w with Some w -> Float.min ctx.width w | None -> ctx.width)
                  | None -> ctx.width
                in
                let x =
                  match Option.map String.lowercase_ascii (attribute "align") with
                  | Some "left" -> ctx.x
                  | Some "right" -> ctx.x +. ctx.width -. width
                  | _ -> ctx.x +. ((ctx.width -. width) /. 2.)
                in
                { kind = Rule e; x; y; width; height; children = []; lines = []; floats = []; marker = None }
            | _ ->
                layout_block ctx.metrics ctx.breaker ctx.picture_size ctx.floats l e ~marker ~x:(ctx.x +. b.indent)
                  ~width:(ctx.width -. b.indent -. b.right) ~y
          in
          ctx.children <- child :: ctx.children;
          ctx.cursor <- y +. child.height;
          ctx.pending <- b.margin_bottom)

let layout (metrics : metrics) ?(breaker = greedy) ?(picture_size = fun _ -> None) ~(root : Looks.t) ~(width : float)
    (html : Dom.element) : box =
  let floats = ref [] in
  let page = layout_block metrics breaker picture_size floats (Looks.look root html) html ~marker:None ~x:0. ~width ~y:0. in
  (* a float can hang below the last block: the page as long as it *)
  { page with height = List.fold_left (fun h p -> Float.max h p.pbottom) page.height !floats }

let rec fragments (b : box) : fragment list =
  List.concat_map (fun (l : line) -> l.fragments) b.lines @ b.floats @ List.concat_map fragments b.children

let rec first_baseline (b : box) : float option =
  match b.lines with
  | l :: _ -> Some l.baseline
  | [] -> List.fold_left (fun found c -> match found with Some _ -> found | None -> first_baseline c) None b.children
