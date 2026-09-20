(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Text_edit.mli *)

type source = Original | Added
type piece = { source : source; start : int; len : int }

type t = {
  (* never touched *)
  original : string;
  (* only ever appended to, and shared by every version: that one-way
     rule is what lets the rest be a value *)
  added : Buffer.t;
  pieces : piece list;
  caret : int;
  anchor : int;
  (* undo is the old list of pieces, kept. That is the whole of it. *)
  past : (piece list * int * int) list;
  future : (piece list * int * int) list;
}

let of_string s =
  {
    original = s;
    added = Buffer.create 64;
    pieces = (if s = "" then [] else [ { source = Original; start = 0; len = String.length s } ]);
    caret = 0;
    anchor = 0;
    past = [];
    future = [];
  }

let read t p =
  match p.source with
  | Original -> String.sub t.original p.start p.len
  | Added -> Buffer.sub t.added p.start p.len

let to_string t = String.concat "" (List.map (read t) t.pieces)
let length t = List.fold_left (fun n p -> n + p.len) 0 t.pieces
let pieces t = List.length t.pieces

(* everything before [pos], everything after -- splitting the piece
 * that [pos] falls inside, which is the only surgery a piece table
 * ever does *)
let split pieces pos =
  let rec go acc pos = function
    | [] -> (List.rev acc, [])
    | p :: rest ->
        if pos <= 0 then (List.rev acc, p :: rest)
        else if pos >= p.len then go (p :: acc) (pos - p.len) rest
        else
          ( List.rev ({ p with len = pos } :: acc),
            { p with start = p.start + pos; len = p.len - pos } :: rest )
  in
  go [] pos pieces

let caret t = t.caret
let anchor t = t.anchor
let range t = if t.caret <= t.anchor then (t.caret, t.anchor) else (t.anchor, t.caret)
let clamp t pos = max 0 (min (length t) pos)

let selected t =
  let a, b = range t in
  String.sub (to_string t) a (b - a)

let at pos t =
  let p = clamp t pos in
  { t with caret = p; anchor = p }

let select ~anchor ~caret t = { t with anchor = clamp t anchor; caret = clamp t caret }
let to_ pos t = { t with caret = clamp t pos }

(* the version about to be replaced, kept for undo; a new edit makes
 * the future unreachable, as it does in every editor *)
let remember t = { t with past = (t.pieces, t.caret, t.anchor) :: t.past; future = [] }

let raw_delete ~from ~len t =
  if len <= 0 then t
  else
    let before, rest = split t.pieces from in
    let _, after = split rest len in
    { t with pieces = before @ after }

let raw_insert ~at s t =
  if s = "" then t
  else
    let start = Buffer.length t.added in
    Buffer.add_string t.added s;
    let before, after = split t.pieces at in
    (* typing one letter after another extends the piece already
     * there, instead of adding one per keystroke: without this, a
     * paragraph typed by hand would be a thousand pieces *)
    match List.rev before with
    | last :: earlier when last.source = Added && last.start + last.len = start ->
        { t with pieces = List.rev ({ last with len = last.len + String.length s } :: earlier) @ after }
    | _ ->
        { t with pieces = before @ [ { source = Added; start; len = String.length s } ] @ after }

let insert s t =
  let a, b = range t in
  let t = remember t in
  let t = raw_delete ~from:a ~len:(b - a) t in
  let t = raw_insert ~at:a s t in
  let caret = a + String.length s in
  { t with caret; anchor = caret }

let delete ~from ~len t =
  let t = remember t in
  let t = raw_delete ~from ~len t in
  let p = clamp t from in
  { t with caret = p; anchor = p }

let delete_backward t =
  let a, b = range t in
  if a <> b then delete ~from:a ~len:(b - a) t
  else if a = 0 then t
  else
    let from = Text.prev_char (to_string t) a in
    delete ~from ~len:(a - from) t

let delete_forward t =
  let a, b = range t in
  if a <> b then delete ~from:a ~len:(b - a) t
  else
    let s = to_string t in
    if a >= String.length s then t else delete ~from:a ~len:(Text.next_char s a - a) t

let undo t =
  match t.past with
  | [] -> t
  | (pieces, caret, anchor) :: past ->
      { t with pieces; caret; anchor; past; future = (t.pieces, t.caret, t.anchor) :: t.future }

let redo t =
  match t.future with
  | [] -> t
  | (pieces, caret, anchor) :: future ->
      { t with pieces; caret; anchor; future; past = (t.pieces, t.caret, t.anchor) :: t.past }

let undos t = List.length t.past
let redos t = List.length t.future

(* Greedy word wrap: fill a line until the next character does not
 * fit, then break at the last space -- or inside the word, if the
 * word is longer than the line. *)
let lines ~width t =
  let s = to_string t in
  let n = String.length s in
  let width = max 1 width in
  let res = ref [] and start = ref 0 and i = ref 0 and count = ref 0 and last_space = ref (-1) in
  let emit stop next =
    res := (!start, String.sub s !start (stop - !start)) :: !res;
    start := next;
    i := next;
    count := 0;
    last_space := -1
  in
  while !i < n do
    if s.[!i] = '\n' then emit !i (!i + 1)
    else begin
      let next = Text.next_char s !i in
      if s.[!i] = ' ' then last_space := !i;
      incr count;
      if !count > width then
        if !last_space > !start then emit !last_space (!last_space + 1) else emit !i !i
      else i := next
    end
  done;
  res := (!start, String.sub s !start (n - !start)) :: !res;
  List.rev !res

let place ~width t pos =
  let ls = lines ~width t in
  let rec go n = function
    | [] -> (0, 0)
    | [ (start, text) ] -> (n, Text.column text (max 0 (pos - start)))
    | (start, text) :: ((next_start, _) :: _ as rest) ->
        if pos < next_start then (n, Text.column text (max 0 (pos - start))) else go (n + 1) rest
  in
  go 0 ls

let offset ~width t ~line ~column =
  let ls = lines ~width t in
  match List.nth_opt ls (max 0 (min (List.length ls - 1) line)) with
  | None -> 0
  | Some (start, text) -> start + Text.byte_of_column text (max 0 column)
