(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Gap_buffer.mli *)

(*****************************************************************************)
(* Stacks shared by their versions *)
(*****************************************************************************)

(* an array, and how much of it the newest version filled *)
type store = { mutable bytes : Bytes.t; mutable used : int }

(* a version: the first [len] bytes of [store] *)
type stack = { store : store; len : int }

let empty_stack () = { store = { bytes = Bytes.create 64; used = 0 }; len = 0 }

let push (s : stack) (text : string) : stack =
  let n = String.length text in
  if s.len = s.store.used && s.len + n <= Bytes.length s.store.bytes then begin
    (* the end of the used part: nobody reads past it, write in place *)
    Bytes.blit_string text 0 s.store.bytes s.len n;
    s.store.used <- s.len + n;
    { s with len = s.len + n }
  end
  else begin
    (* shared with a newer version, or full: a copy, twice as large *)
    let bytes = Bytes.create (max 64 (2 * (s.len + n))) in
    Bytes.blit s.store.bytes 0 bytes 0 s.len;
    Bytes.blit_string text 0 bytes s.len n;
    { store = { bytes; used = s.len + n }; len = s.len + n }
  end

let pop (s : stack) (n : int) : stack = { s with len = s.len - n }

(* the top [n] bytes, the topmost last *)
let top (s : stack) (n : int) : string = Bytes.sub_string s.store.bytes (s.len - n) n

let rev (s : string) : string = String.init (String.length s) (fun i -> s.[String.length s - 1 - i])

(*****************************************************************************)
(* The text *)
(*****************************************************************************)

(* after is reversed: its top is the byte just after the gap *)
type t = { before : stack; after : stack }

let length t = t.before.len + t.after.len
let gap t = t.before.len
let of_string s = { before = push (empty_stack ()) s; after = empty_stack () }

let get t i =
  if i < t.before.len then Bytes.get t.before.store.bytes i
  else Bytes.get t.after.store.bytes (t.after.len - 1 - (i - t.before.len))

let sub t i j = String.init (j - i) (fun k -> get t (i + k))
let to_string t = sub t 0 (length t)

(* the gap moved to [pos]: the bytes it passes over change stacks *)
let move_gap (t : t) (pos : int) : t =
  let g = gap t in
  if pos < g then
    let moved = top t.before (g - pos) in
    { before = pop t.before (g - pos); after = push t.after (rev moved) }
  else if pos > g then
    let moved = rev (top t.after (pos - g)) in
    { before = push t.before moved; after = pop t.after (pos - g) }
  else t

let insert t pos s =
  let t = move_gap t pos in
  { t with before = push t.before s }

let delete t i j =
  let t = move_gap t i in
  { t with after = pop t.after (j - i) }

let index_from t pos c =
  let n = length t in
  let rec go i = if i >= n then None else if get t i = c then Some i else go (i + 1) in
  go pos

let rindex_before t pos c =
  let rec go i = if i < 0 then None else if get t i = c then Some i else go (i - 1) in
  go (pos - 1)
