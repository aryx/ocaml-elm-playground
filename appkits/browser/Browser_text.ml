(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_text.mli *)

let characters (s : string) : string list =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      go (i + n) (String.sub s i n :: acc)
  in
  go 0 []

let root_look = Looks.root ~size:16.

let style_of (l : Looks.t) : Style.t =
  { bold = l.bold; italic = l.italic; underline = l.underline; strike = l.strike; size = l.size }

let cell_of (l : Looks.t) : float = 0.6 *. l.size

let metrics (l : Looks.t) (s : string) : float =
  if l.monospace then cell_of l *. float_of_int (List.length (characters s))
  else List.fold_left (fun w c -> w +. Stroke_text.metrics (style_of l) c) 0. (characters s)

let tail (n : int) (s : string) : string =
  let cs = characters s in
  let k = List.length cs in
  if k <= n then s else String.concat "" (List.filteri (fun i _ -> i >= k - n) cs)

let escape_html (s : string) : string =
  String.concat "" (List.map (fun c -> match c with "&" -> "&amp;" | "<" -> "&lt;" | ">" -> "&gt;" | c -> c) (characters s))
