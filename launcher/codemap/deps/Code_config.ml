(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_config.mli *)

type rgb = int * int * int

(* a line of .codemapignore: its glob, whether it is from the root (a /
 * in it), a directory's only (a / at its end) *)
type pattern = { glob : string; anchored : bool; dir_only : bool }

type t = { patterns : pattern list }

let empty = { patterns = [] }

(*****************************************************************************)
(* .codemapignore *)
(*****************************************************************************)

let pattern (line : string) : pattern option =
  let s = String.trim line in
  if s = "" || s.[0] = '#' || s.[0] = '!' then None
  else begin
    let dir_only = s.[String.length s - 1] = '/' in
    let s = if dir_only then String.sub s 0 (String.length s - 1) else s in
    let anchored = String.contains s '/' in
    let s = if s <> "" && s.[0] = '/' then String.sub s 1 (String.length s - 1) else s in
    if s = "" then None else Some { glob = s; anchored; dir_only }
  end

(* [glob] matches all of [s]: * any characters but /, ? one *)
let rec matches (glob : string) (i : int) (s : string) (j : int) : bool =
  if i = String.length glob then j = String.length s
  else
    match glob.[i] with
    | '*' -> matches glob (i + 1) s j || (j < String.length s && s.[j] <> '/' && matches glob i s (j + 1))
    | '?' -> j < String.length s && s.[j] <> '/' && matches glob (i + 1) s (j + 1)
    | c -> j < String.length s && s.[j] = c && matches glob (i + 1) s (j + 1)

(* claude: a path is ignored when it is, or its name is; its directories
 * are asked first (the walk does not go into one ignored), so a pattern
 * need not match what is under what it names *)
let ignored (t : t) (path : string) ~(dir : bool) : bool =
  let name = Filename.basename path in
  List.exists
    (fun p -> ((not p.dir_only) || dir) && if p.anchored then matches p.glob 0 path 0 else matches p.glob 0 name 0)
    t.patterns

(*****************************************************************************)
(* Colours *)
(*****************************************************************************)

let hex (s : string) : rgb option =
  let digit c = match c with '0' .. '9' -> Some (Char.code c - 48) | 'a' .. 'f' -> Some (Char.code c - 87) | 'A' .. 'F' -> Some (Char.code c - 55) | _ -> None in
  let byte i = match (digit s.[i], digit s.[i + 1]) with Some a, Some b -> Some ((a * 16) + b) | _ -> None in
  if String.length s = 7 && s.[0] = '#' then
    match (byte 1, byte 3, byte 5) with Some r, Some g, Some b -> Some (r, g, b) | _ -> None
  else None

let make ~(ignore : string option) : t =
  { patterns = (match ignore with Some s -> List.filter_map pattern (String.split_on_char '\n' s) | None -> []) }
