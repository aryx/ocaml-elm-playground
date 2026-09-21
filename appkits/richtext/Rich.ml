(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Rich.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  (* the characters, the caret, the selection *)
  edit : Text_edit.t;
  (* the looks: (length in bytes, look), in order, covering the text *)
  runs : (int * Style.t) list;
  (* a look set with nothing selected, waiting for the next character *)
  pending : Style.t option;
  (* what an empty text looks like, so that its first character has a
     look to take *)
  base : Style.t;
}

let of_string ?(style = Style.plain) s =
  {
    edit = Text_edit.of_string s;
    runs = (if s = "" then [] else [ (String.length s, style) ]);
    pending = None;
    base = style;
  }

let to_string t = Text_edit.to_string t.edit
let length t = Text_edit.length t.edit
let edit t = t.edit

(*****************************************************************************)
(* The run table *)
(*****************************************************************************)

(* everything before [pos] and everything after, splitting the run it
 * falls inside -- the piece table's own surgery, on looks *)
let split runs pos =
  let rec go acc pos = function
    | [] -> (List.rev acc, [])
    | ((len, st) as r) :: rest ->
        if pos <= 0 then (List.rev acc, r :: rest)
        else if pos >= len then go (r :: acc) (pos - len) rest
        else (List.rev ((pos, st) :: acc), (len - pos, st) :: rest)
  in
  go [] pos runs

(* neighbours that have come to look the same are one run again: the
 * table stays as short as the text's looks really are *)
let merge runs =
  let rec go = function
    | (l1, s1) :: (l2, s2) :: rest when s1 = s2 -> go ((l1 + l2, s1) :: rest)
    | r :: rest -> r :: go rest
    | [] -> []
  in
  go (List.filter (fun (l, _) -> l > 0) runs)

let cut runs a b =
  let before, rest = split runs a in
  let _, after = split rest (b - a) in
  merge (before @ after)

let put runs a len st =
  let before, after = split runs a in
  merge (before @ [ (len, st) ] @ after)

let runs t =
  let _, out =
    List.fold_left (fun (pos, acc) (len, st) -> (pos + len, (pos, len, st) :: acc)) (0, []) t.runs
  in
  List.rev out

let style_at t i =
  let rec go pos = function
    | [] -> t.base
    | [ (_, st) ] -> st
    | (len, st) :: rest -> if i < pos + len then st else go (pos + len) rest
  in
  go 0 t.runs

(*****************************************************************************)
(* The caret and the selection *)
(*****************************************************************************)

let caret t = Text_edit.caret t.edit
let range t = Text_edit.range t.edit

(* moving the caret forgets a look that was waiting for it *)
let at pos t = { t with edit = Text_edit.at pos t.edit; pending = None }
let select ~anchor ~caret t = { t with edit = Text_edit.select ~anchor ~caret t.edit; pending = None }
let to_ pos t = { t with edit = Text_edit.to_ pos t.edit; pending = None }

let typing_style t =
  match t.pending with
  | Some st -> st
  | None ->
      let a, b = range t in
      if length t = 0 then t.base
      else if a <> b then style_at t a (* over a selection: its first character *)
      else if a > 0 then style_at t (a - 1) (* what is before the caret *)
      else style_at t 0 (* at the very start: what comes after *)

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

let insert s t =
  let style = typing_style t in
  let a, b = range t in
  let runs = put (cut t.runs a b) a (String.length s) style in
  { t with edit = Text_edit.insert s t.edit; runs; pending = None }

(* the same bytes Text_edit is about to delete, so both tables lose the
 * same stretch *)
let delete_backward t =
  let a, b = range t in
  let from, upto = if a <> b then (a, b) else if a = 0 then (0, 0) else (Text.prev_char (to_string t) a, a) in
  if from = upto then t
  else { t with edit = Text_edit.delete_backward t.edit; runs = cut t.runs from upto; pending = None }

let delete_forward t =
  let a, b = range t in
  let s = to_string t in
  let from, upto =
    if a <> b then (a, b) else if a >= String.length s then (a, a) else (a, Text.next_char s a)
  in
  if from = upto then t
  else { t with edit = Text_edit.delete_forward t.edit; runs = cut t.runs from upto; pending = None }

(*****************************************************************************)
(* Looks *)
(*****************************************************************************)

let restyle f t =
  let a, b = range t in
  if a = b then { t with pending = Some (f (typing_style t)) }
  else
    let before, rest = split t.runs a in
    let mid, after = split rest (b - a) in
    { t with runs = merge (before @ List.map (fun (l, st) -> (l, f st)) mid @ after) }
