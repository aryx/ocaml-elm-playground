(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Page.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type metrics = Style.t -> string -> float
type align = Left | Center | Right | Justify

type glyph = {
  offset : int;
  text : string;
  style : Style.t;
  x : float;
  baseline : float;
  advance : float;
}

type line = {
  top : float;
  height : float;
  baseline : float;
  cells : glyph list;
  (* the offsets it covers: [first, stop) *)
  first : int;
  stop : int;
}

type t = { lines : line list; length : int }

(* a line is as tall as its tallest look, with some air, and its
 * baseline sits one em of that look below its top *)
let line_height size = size *. 1.4

(*****************************************************************************)
(* From characters to words to lines *)
(*****************************************************************************)

(* a character, where it is in the text, its look and its width *)
type cell = { at : int; ch : string; look : Style.t; w : float }

(* a word is its letters and the spaces after them; a newline ends
 * the word it follows, and the line with it *)
type word = { parts : cell list; ink : float; total : float; hard : bool }

let words_of cells =
  let finish acc cur hard =
    if cur = [] then acc
    else
      let parts = List.rev cur in
      let is_space c = c.ch = " " || c.ch = "\n" in
      let ink =
        (* the width up to the last letter: the spaces after it may
           hang past the edge of the line *)
        let rec go w last = function
          | [] -> last
          | c :: rest -> let w = w +. c.w in go w (if is_space c then last else w) rest
        in
        go 0. 0. parts
      in
      { parts; ink; total = List.fold_left (fun a c -> a +. c.w) 0. parts; hard } :: acc
  in
  let rec go acc cur seen_space = function
    | [] -> List.rev (finish acc cur false)
    | c :: rest when c.ch = "\n" -> go (finish acc (c :: cur) true) [] false rest
    | c :: rest when c.ch = " " -> go acc (c :: cur) true rest
    | c :: rest ->
        (* a letter after spaces starts the next word *)
        if seen_space then go (finish acc cur false) [ c ] false rest else go acc (c :: cur) false rest
  in
  go [] [] false cells

(* Greedy, word by word: what Bravo did and what Word does. A word
 * that does not fit goes to the next line; a word longer than the
 * whole line is broken where it reaches the edge. *)
let lines_of ~width words =
  let lines = ref [] and cur = ref [] and x = ref 0. and last_hard = ref false in
  let close () =
    lines := List.rev !cur :: !lines;
    cur := [];
    x := 0.
  in
  List.iter
    (fun wd ->
      if !cur <> [] && !x +. wd.ink > width then close ();
      if wd.ink > width then
        (* too long for any line: as much as fits, then the rest *)
        List.iter
          (fun c ->
            if !cur <> [] && !x +. c.w > width && c.ch <> " " then close ();
            cur := c :: !cur;
            x := !x +. c.w)
          wd.parts
      else begin
        cur := List.rev_append wd.parts !cur;
        x := !x +. wd.total
      end;
      if wd.hard then close ();
      last_hard := wd.hard)
    words;
  (* the last line -- even an empty one, after a final newline or in
     an empty text, since the caret has to have somewhere to be *)
  if !cur <> [] || !lines = [] || !last_hard then close ();
  List.rev !lines

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* A line's slack, given to where the alignment says. [last] is whether
 * it ends its paragraph (a newline, or the end of the text), which is
 * set Left even when justifying. *)
let aligned align ~width ~last (glyphs : glyph list) =
  let ink =
    (* up to the end of the last glyph that is not a space: the spaces a
       line was broken after hang past the edge, and are not counted *)
    List.fold_left
      (fun acc (g : glyph) -> if g.text = " " || g.text = "\n" then acc else g.x +. g.advance)
      0. glyphs
  in
  let slack = max 0. (width -. ink) in
  let shift dx = List.map (fun (g : glyph) -> { g with x = g.x +. dx }) glyphs in
  match align with
  | Left -> glyphs
  | Center -> shift (slack /. 2.)
  | Right -> shift slack
  | Justify when last -> glyphs
  | Justify ->
      (* the spaces between words -- not the ones hanging at the end --
         each get an equal share *)
      let inner =
        List.filter (fun (g : glyph) -> g.text = " " && g.x +. g.advance <= ink) glyphs
      in
      let n = List.length inner in
      if n = 0 then glyphs
      else
        let extra = slack /. float_of_int n in
        let _, out =
          List.fold_left
            (fun (added, acc) (g : glyph) ->
              let g' = { g with x = g.x +. added } in
              let is_inner = g.text = " " && g.x +. g.advance <= ink in
              if is_inner then (added +. extra, { g' with advance = g.advance +. extra } :: acc)
              else (added, g' :: acc))
            (0., []) glyphs
        in
        List.rev out

(* The lines laid round boxes, one at a time (see the .mli): each as
 * its segments -- (cells, where the stretch starts, how wide it is) --
 * and its top. A line fills the widest stretch the boxes leave it, or,
 * [both], every stretch wide enough, left to right: a text on both
 * sides of a box. A line's height is guessed from its first letter's
 * look before it is filled, which is the look that decides it in all
 * but mixed lines. *)
let lines_around ~both ~width ~around ~size_of words =
  let min_w = 40. in
  (* the stretches of [0, width] the boxes reaching [y, y+h) leave, left
     to right, and those boxes *)
  let free y h =
    let blocking = List.filter (fun (_, y0, _, y1) -> y0 < y +. h && y1 > y) around in
    let cuts = List.sort compare (List.map (fun (x0, _, x1, _) -> (Float.max 0. x0, Float.min width x1)) blocking) in
    let stretches, last =
      List.fold_left (fun (acc, x) (a, b) -> ((if a > x then (x, a) :: acc else acc), Float.max x b)) ([], 0.) cuts
    in
    let stretches = List.rev (if width > last then (last, width) :: stretches else stretches) in
    let wide = List.filter (fun (a, b) -> b -. a >= min_w) stretches in
    let chosen =
      if both then wide
      else
        match
          List.fold_left (fun best (a, b) -> match best with Some (a', b') when b' -. a' >= b -. a -> best | _ -> Some (a, b)) None wide
        with
        | Some s -> [ s ]
        | None -> []
    in
    (chosen, blocking)
  in
  (* words into a stretch w wide, as many as fit -- and whether a
     newline ended the line *)
  let fill w words =
    let rec go x cur = function
      | wd :: rest when cur = [] || x +. wd.ink <= w ->
          let cur = List.rev_append wd.parts cur in
          if wd.hard then (List.rev cur, rest, true) else go (x +. wd.total) cur rest
      | rest -> (List.rev cur, rest, false)
    in
    go 0. [] words
  in
  let rec go y words acc last_hard =
    match words with
    | [] ->
        (* the caret's last line, after a final newline or in an empty
           text *)
        List.rev (if acc = [] || last_hard then ([ ([], 0., width) ], y) :: acc else acc)
    | first :: _ -> (
        let h = line_height (size_of (match first.parts with c :: _ -> [ c ] | [] -> [])) in
        match free y h with
        | [], blocking ->
            (* no room at this height: on below the nearest bottom of the
               boxes in the way *)
            let next = List.fold_left (fun m (_, _, _, y1) -> Float.min m y1) infinity blocking in
            go (if next = infinity || next <= y then y +. 1. else next) words acc last_hard
        | stretches, _ ->
            (* each stretch in turn, until the words or the line end *)
            let rec segments words hard acc = function
              | [] -> (List.rev acc, words, hard)
              | _ when words = [] || hard -> (List.rev acc, words, hard)
              | (a, b) :: more ->
                  let cells, rest, hard = fill (b -. a) words in
                  segments rest hard ((cells, a, b -. a) :: acc) more
            in
            let segs, rest, hard = segments words false [] stretches in
            let cells = List.concat_map (fun (c, _, _) -> c) segs in
            go (y +. line_height (size_of cells)) rest ((segs, y) :: acc) hard)
  in
  go 0. words [] false

let layout ?(align = Left) ?(around = []) ?(both = false) ~metrics ~width r =
  let s = Rich.to_string r in
  let rec cells_from i acc =
    if i >= String.length s then List.rev acc
    else
      let j = Text.next_char s i in
      let ch = String.sub s i (j - i) in
      let look = Rich.style_at r i in
      cells_from j ({ at = i; ch; look; w = (if ch = "\n" then 0. else metrics look ch) } :: acc)
  in
  let words = words_of (cells_from 0 []) in
  let length = String.length s in
  let base = Rich.typing_style (Rich.at length r) in
  let size_of cells =
    let size = List.fold_left (fun m c -> max m c.look.Style.size) 0. cells in
    if size = 0. then base.Style.size else size
  in
  (* each line: its segments -- cells, where the stretch starts, its
     width -- and its top *)
  let placed =
    if around = [] then
      List.rev
        (snd
           (List.fold_left
              (fun (top, acc) cells -> (top +. line_height (size_of cells), ([ (cells, 0., width) ], top) :: acc))
              (0., []) (lines_of ~width words)))
    else lines_around ~both ~width ~around ~size_of words
  in
  let lines =
    List.map
      (fun (segs, top) ->
        let cells = List.concat_map (fun (c, _, _) -> c) segs in
        let size = size_of cells in
        let baseline = top +. size in
        let first = match cells with c :: _ -> c.at | [] -> length in
        let stop = List.fold_left (fun _ c -> c.at + String.length c.ch) first cells in
        (* the last line of a paragraph: it ends with a newline, or the
           text ends with it *)
        let last = stop >= length || (match List.rev cells with c :: _ -> c.ch = "\n" | [] -> true) in
        let n = List.length segs in
        let glyphs =
          List.concat
            (List.mapi
               (fun i (cells, x0, w) ->
                 let _, glyphs =
                   List.fold_left
                     (fun (x, gs) c -> (x +. c.w, { offset = c.at; text = c.ch; style = c.look; x; baseline; advance = c.w } :: gs))
                     (0., []) cells
                 in
                 (* only the line's last segment can be a paragraph's last *)
                 let glyphs = aligned align ~width:w ~last:(last && i = n - 1) (List.rev glyphs) in
                 if x0 = 0. then glyphs else List.map (fun g -> { g with x = g.x +. x0 }) glyphs)
               segs)
        in
        { top; height = line_height size; baseline; cells = glyphs; first; stop })
      placed
  in
  { lines; length }

let glyphs t = List.concat_map (fun l -> l.cells) t.lines
let lines t = t.lines

let height t = List.fold_left (fun _ l -> l.top +. l.height) 0. t.lines

(*****************************************************************************)
(* Both ways between the text and the page *)
(*****************************************************************************)

let caret_at t offset =
  let offset = max 0 (min t.length offset) in
  match List.find_opt (fun l -> l.first <= offset && offset < l.stop) t.lines with
  | Some l ->
      let g = List.find (fun g -> g.offset = offset) l.cells in
      (g.x, l.baseline, l.height)
  | None -> (
      (* the very end of the text: after the last glyph of the last line *)
      match List.rev t.lines with
      | l :: _ -> (
          match List.rev l.cells with
          | g :: _ when g.text <> "\n" -> (g.x +. g.advance, l.baseline, l.height)
          | _ -> (0., l.baseline, l.height))
      | [] -> (0., 0., 0.))

let offset_at t (x, y) =
  let lines = t.lines in
  let line =
    match List.find_opt (fun l -> y < l.top +. l.height) lines with
    | Some l -> Some l
    | None -> ( match List.rev lines with l :: _ -> Some l | [] -> None)
  in
  match line with
  | None -> 0
  | Some l -> (
      let last_line = (match List.rev lines with z :: _ -> z == l | [] -> true) in
      match List.find_opt (fun g -> g.text <> "\n" && x < g.x +. (g.advance /. 2.)) l.cells with
      | Some g -> g.offset
      | None -> (
          (* past the end of the line: before its newline if it has one,
             at the end of the text on the last line, and otherwise
             before the space it was broken after -- so that the caret
             stays on the line that was clicked *)
          match List.rev l.cells with
          | g :: _ when g.text = "\n" -> g.offset
          | _ when last_line -> t.length
          | g :: _ -> g.offset
          | [] -> l.first))
