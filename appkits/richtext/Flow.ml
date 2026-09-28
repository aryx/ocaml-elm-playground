(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type placed_line = { line : Page.line; column : int; top : float }
type placed_frame = { frame : int; in_column : int; at : float; height : float }
type t = { lines : placed_line list; frames : placed_frame list; columns : int }

let flow ~column_height ~anchors page =
  (* where the next thing goes: a column, and how far down it *)
  let col = ref 0 and y = ref 0. in
  (* something h high goes here, or at the top of the next column *)
  let place h =
    if !y > 0. && !y +. h > column_height then (
      incr col;
      y := 0.);
    let at = (!col, !y) in
    y := !y +. h;
    at
  in
  let lines = Page.lines page in
  let last = List.length lines - 1 in
  let anchors = List.mapi (fun i (offset, h) -> (i, offset, h)) anchors in
  let placed_lines = ref [] and frames = ref [] in
  List.iteri
    (fun k (l : Page.line) ->
      let column, top = place l.height in
      placed_lines := { line = l; column; top } :: !placed_lines;
      (* the frames anchored in this line -- the last line takes those
         anchored at the very end, or past it *)
      List.iter
        (fun (i, offset, h) ->
          if (offset >= l.first && offset < l.stop) || (k = last && offset >= l.stop) then
            let in_column, at = place h in
            frames := { frame = i; in_column; at; height = h } :: !frames)
        anchors)
    lines;
  { lines = List.rev !placed_lines; frames = List.rev !frames; columns = !col + 1 }
