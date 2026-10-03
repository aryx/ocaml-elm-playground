(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Turbo_edit.mli *)

open Turbo_model

(* the Watches window's height, when there are watches: at the bottom,
   the edit window above it *)
let watch_rows (m : model) : int = if m.watches = [] then 0 else min 8 (List.length m.watches + 2)

(* the window's text: 20 lines of 78 columns inside its frame, less
   the watches' *)
let text_rows (m : model) = 20 - watch_rows m
let text_cols = 78
let noname = "NONAME00.PAS"

let line (m : model) (r : int) : string = m.lines.(r)
let nlines (m : model) : int = Array.length m.lines
let text (m : model) : string = String.concat "\n" (Array.to_list m.lines) ^ "\n"

(* the window follows the cursor *)
let follow (m : model) : model =
  let row = max 0 (min (nlines m - 1) m.row) in
  let col = max 0 m.col in
  let top = if row < m.top then row else if row >= m.top + text_rows m then row - text_rows m + 1 else m.top in
  let left = if col < m.left then col else if col >= m.left + text_cols then col - text_cols + 1 else m.left in
  { m with row; col; top; left }

let load (m : model) (file : string) : model =
  let content = Option.value (List.assoc_opt file m.disk) ~default:"" in
  let content = if content <> "" && content.[String.length content - 1] = '\n' then String.sub content 0 (String.length content - 1) else content in
  { m with lines = Array.of_list (String.split_on_char '\n' content); file; row = 0; col = 0; top = 0; left = 0; modified = false;
    compiled = None; error = None; session = None; breakpoints = [] }

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

let set_line (m : model) (r : int) (s : string) : model =
  { m with lines = Array.mapi (fun i l -> if i = r then s else l) m.lines; modified = true; compiled = None }

let set_lines (m : model) (ls : string list) : model = { m with lines = Array.of_list (if ls = [] then [ "" ] else ls); modified = true; compiled = None }

(* the cursor may be past a line's end (Turbo's was): spaces fill the
   gap when something is typed there *)
let padded (m : model) : string =
  let s = line m m.row in
  if m.col > String.length s then s ^ String.make (m.col - String.length s) ' ' else s

let type_char (m : model) (c : string) : model =
  let s = padded m in
  let rest = String.sub s m.col (String.length s - m.col) in
  let rest = if m.overwrite && rest <> "" then String.sub rest 1 (String.length rest - 1) else rest in
  { (set_line m m.row (String.sub s 0 m.col ^ c ^ rest)) with col = m.col + 1 }

let indentation (s : string) : int =
  let rec go i = if i < String.length s && s.[i] = ' ' then go (i + 1) else i in
  go 0

(* Enter: the line cut in two, the new one indented as this one *)
let newline (m : model) : model =
  let s = padded m in
  let head = String.sub s 0 m.col and tail = String.sub s m.col (String.length s - m.col) in
  let indent = if String.trim head = "" then 0 else indentation head in
  let ls = Array.to_list m.lines in
  let before = List.filteri (fun i _ -> i < m.row) ls and after = List.filteri (fun i _ -> i > m.row) ls in
  { (set_lines m (before @ [ head; String.make indent ' ' ^ String.trim tail ] @ after)) with row = m.row + 1; col = indent }

let join_next (m : model) : model =
  if m.row + 1 >= nlines m then m
  else
    let ls = Array.to_list m.lines in
    let joined = padded m ^ line m (m.row + 1) in
    set_lines m (List.filteri (fun i _ -> i <> m.row + 1) (List.mapi (fun i l -> if i = m.row then joined else l) ls))

let backspace (m : model) : model =
  if m.col > 0 then
    let s = line m m.row in
    if m.col > String.length s then { m with col = m.col - 1 }
    else { (set_line m m.row (String.sub s 0 (m.col - 1) ^ String.sub s m.col (String.length s - m.col))) with col = m.col - 1 }
  else if m.row > 0 then
    let col = String.length (line m (m.row - 1)) in
    join_next { m with row = m.row - 1; col }
  else m

let delete (m : model) : model =
  let s = line m m.row in
  if m.col < String.length s then set_line m m.row (String.sub s 0 m.col ^ String.sub s (m.col + 1) (String.length s - m.col - 1)) else join_next m

let delete_line (m : model) : model =
  let ls = List.filteri (fun i _ -> i <> m.row) (Array.to_list m.lines) in
  { (set_lines m ls) with col = 0 }

let is_word (c : char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

(* Ctrl-F and Ctrl-A: the next word's start, the previous one's *)
let word_right (m : model) : model =
  let s = line m m.row in
  let rec skip i p = if i < String.length s && p s.[i] then skip (i + 1) p else i in
  if m.col >= String.length s then if m.row + 1 < nlines m then { m with row = m.row + 1; col = 0 } else m
  else { m with col = skip (skip m.col is_word) (fun c -> not (is_word c)) }

let word_left (m : model) : model =
  let s = line m m.row in
  let col = min m.col (String.length s) in
  let rec back i p = if i > 0 && p s.[i - 1] then back (i - 1) p else i in
  if col = 0 then if m.row > 0 then { m with row = m.row - 1; col = String.length (line m (m.row - 1)) } else m
  else { m with col = back (back col (fun c -> not (is_word c))) is_word }

let find (m : model) (pat : string) : model =
  let n = nlines m in
  let at r from =
    let s = line m r in
    let k = String.length pat in
    let rec go i = if i + k > String.length s then None else if String.lowercase_ascii (String.sub s i k) = String.lowercase_ascii pat then Some i else go (i + 1) in
    if pat = "" then None else go from
  in
  let rec scan k = if k >= n then None else let r = m.row + k in if r >= n then None else match at r (if k = 0 then m.col + 1 else 0) with Some c -> Some (r, c) | None -> scan (k + 1) in
  match scan 0 with
  | Some (r, c) -> { m with row = r; col = c; search = pat }
  | None -> { m with search = pat; mode = Info ("Information", [ "Search string not found." ]) }

let edit_key (m : model) (k : string) : model =
  match k with
  | "\x1b[A" | "\x05" -> { m with row = m.row - 1 }
  | "\x1b[B" | "\x18" -> { m with row = m.row + 1 }
  | "\x1b[D" | "\x13" -> if m.col > 0 then { m with col = m.col - 1 } else m
  | "\x1b[C" | "\x04" -> { m with col = m.col + 1 }
  | "\x01" -> word_left m
  | "\x06" -> word_right m
  | "\x1b[H" -> { m with col = 0 }
  | "\x1b[F" -> { m with col = String.length (line m m.row) }
  | "\x1b[5~" | "\x12" -> { m with row = m.row - text_rows m + 1; top = max 0 (m.top - text_rows m + 1) }
  | "\x1b[6~" | "\x03" ->
      { m with row = min (nlines m - 1) (m.row + text_rows m - 1); top = min (max 0 (nlines m - text_rows m)) (m.top + text_rows m - 1) }
  | "\x1b[2~" | "\x16" -> { m with overwrite = not m.overwrite }
  | "\r" -> newline m
  | "\x7f" | "\b" -> backspace m
  | "\x1b[3~" | "\x07" -> delete m
  | "\x19" -> delete_line m
  | "\t" -> List.fold_left type_char m [ " "; " " ]
  | "\x0c" -> find m m.search
  | _ when String.length k = 1 && k.[0] >= ' ' && k.[0] < '\x7f' -> type_char m k
  | _ -> m
