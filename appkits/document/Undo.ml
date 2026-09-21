(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Undo.mli *)

(* a version, and the name of the edit that made it *)
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'a step = { value : 'a; name : string option }

type 'a t = { now : 'a step; past : 'a step list; future : 'a step list; limit : int }

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

let start ?(limit = 100) value = { now = { value; name = None }; past = []; future = []; limit }
let now t = t.now.value

(* the oldest versions are forgotten: an editor's memory has to stop
 * somewhere, and it stops at the end furthest from the person *)
let rec keep n = function [] -> [] | x :: rest -> if n <= 0 then [] else x :: keep (n - 1) rest

let record ?name value t =
  { t with now = { value; name }; past = keep t.limit (t.now :: t.past); future = [] }

let undo t =
  match t.past with
  | [] -> t
  | step :: past -> { t with now = step; past; future = t.now :: t.future }

let redo t =
  match t.future with
  | [] -> t
  | step :: future -> { t with now = step; future; past = t.now :: t.past }

let can_undo t = t.past <> []
let can_redo t = t.future <> []

(* undo takes back the edit that made the state we are in, so its name
 * is the one to show *)
let undo_name t = if can_undo t then t.now.name else None
let redo_name t = match t.future with [] -> None | step :: _ -> step.name
let undos t = List.length t.past
let redos t = List.length t.future
