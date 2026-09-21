(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type slide = { title : string; points : (int * string) list }

(* how deep a line is indented, in columns, and what is after that *)
let indent line =
  let n = String.length line in
  let rec go i cols = if i < n && line.[i] = ' ' then go (i + 1) (cols + 1) else if i < n && line.[i] = '\t' then go (i + 1) (cols + 2) else (i, cols) in
  let i, cols = go 0 0 in
  (cols, String.sub line i (n - i))

(* a slide per title line; the lines are read in order, the slide
   being filled kept last-first until the next title closes it *)
let parse text =
  let close cur acc = match cur with Some s -> { s with points = List.rev s.points } :: acc | None -> acc in
  let cur, acc =
    List.fold_left
      (fun (cur, acc) line ->
        let cols, rest = indent line in
        if String.trim rest = "" then (cur, acc)
        else if cols = 0 then (Some { title = rest; points = [] }, close cur acc)
        else
          let level = max 1 (cols / 2) in
          let s = Option.value cur ~default:{ title = ""; points = [] } in
          (Some { s with points = (level, String.trim rest) :: s.points }, acc))
      (None, []) (String.split_on_char '\n' text)
  in
  List.rev (close cur acc)

let slide_at text offset =
  let before = String.sub text 0 (max 0 (min offset (String.length text))) in
  let lines = String.split_on_char '\n' before in
  (* the titles before the caret's line, and the caret's line itself if
     it is one; a point before any title is on slide 0 *)
  let titles = List.filter (fun l -> let cols, rest = indent l in cols = 0 && String.trim rest <> "") lines in
  let starts_untitled =
    match List.find_opt (fun l -> String.trim l <> "") (String.split_on_char '\n' text) with
    | Some l -> fst (indent l) > 0
    | None -> false
  in
  max 0 (List.length titles - 1 + if starts_untitled then 1 else 0)

let start_of text n =
  (* the offset of every title line, in order *)
  let rec titles pos acc =
    if pos > String.length text then List.rev acc
    else
      let e = match String.index_from_opt text pos '\n' with Some e -> e | None -> String.length text in
      let line = String.sub text pos (e - pos) in
      let cols, rest = indent line in
      let acc = if cols = 0 && String.trim rest <> "" then pos :: acc else acc in
      titles (e + 1) acc
  in
  match List.nth_opt (titles 0 []) n with Some pos -> pos | None -> String.length text

let to_text slides =
  String.concat "\n"
    (List.concat_map
       (fun s -> s.title :: List.map (fun (level, p) -> String.make (2 * level) ' ' ^ p) s.points)
       slides)
