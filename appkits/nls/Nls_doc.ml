(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Nls_doc.mli *)

type statement = { sid : int; text : string; children : statement list }
type t = { statements : statement list; next_sid : int }
type where = After | Down | Up

let of_outline (lines : (int * string) list) : t =
  let next = ref 0 in
  (* the statements at [depth], each with the deeper lines after it as
     its branch, and the lines left over; a line at least that deep is
     at this level, since the level above took the deeper ones *)
  let rec level depth lines =
    match lines with
    | (d, text) :: rest when d >= depth ->
        incr next;
        let sid = !next in
        let children, rest = level (depth + 1) rest in
        let siblings, rest = level depth rest in
        ({ sid; text; children } :: siblings, rest)
    | _ -> ([], lines)
  in
  let statements, _ = level 0 lines in
  { statements; next_sid = !next + 1 }

let rec find_in (l : statement list) sid : statement option =
  List.fold_left
    (fun found s -> match found with Some _ -> found | None -> if s.sid = sid then Some s else find_in s.children sid)
    None l

let get t sid = find_in t.statements sid

(* the path of indices down to a statement, from 0 *)
let rec path_in (l : statement list) sid : int list option =
  let rec go i = function
    | [] -> None
    | s :: rest -> (
        if s.sid = sid then Some [ i ] else match path_in s.children sid with Some p -> Some (i :: p) | None -> go (i + 1) rest)
  in
  go 0 l

(* levels alternate: 1-based numbers, then letters a to z, aa... as
   spreadsheet columns count *)
let rec letters n = if n < 26 then String.make 1 (Char.chr (97 + n)) else letters ((n / 26) - 1) ^ letters (n mod 26)

let number_of_path path = String.concat "" (List.mapi (fun depth i -> if depth mod 2 = 0 then string_of_int (i + 1) else letters i) path)
let number t sid = Option.map number_of_path (path_in t.statements sid)

let name_of (text : string) : string option =
  if String.length text > 2 && text.[0] = '(' then
    match String.index_opt text ')' with Some j when j > 1 -> Some (String.sub text 1 (j - 1)) | _ -> None
  else None

let rec all (l : statement list) = List.concat_map (fun s -> s :: all s.children) l

let find t (target : string) : int option =
  let target = String.lowercase_ascii (String.trim target) in
  let statements = all t.statements in
  match List.find_opt (fun s -> Option.map String.lowercase_ascii (name_of s.text) = Some target) statements with
  | Some s -> Some s.sid
  | None -> Option.map (fun s -> s.sid) (List.find_opt (fun s -> number t s.sid = Some target) statements)

(* [news] placed next to [target] in [l]; whether it found the place *)
let rec place (l : statement list) ~target where (news : statement) : statement list * bool =
  match l with
  | [] -> ([], false)
  | s :: rest ->
      if s.sid = target then
        match where with
        | After | Up -> (s :: news :: rest, true)
        | Down -> ({ s with children = news :: s.children } :: rest, true)
      else if where = Up && List.exists (fun c -> c.sid = target) s.children then (s :: news :: rest, true)
      else
        let children, found = place s.children ~target where news in
        if found then ({ s with children } :: rest, true)
        else
          let rest, found = place rest ~target where news in
          (s :: rest, found)

let rec map_statement (l : statement list) sid f : statement list =
  List.map (fun s -> if s.sid = sid then f s else { s with children = map_statement s.children sid f }) l

let rec remove (l : statement list) sid : statement list =
  List.filter_map (fun s -> if s.sid = sid then None else Some { s with children = remove s.children sid }) l

let insert t ~target where text =
  let news = { sid = t.next_sid; text; children = [] } in
  let statements, found = place t.statements ~target where news in
  if found then ({ statements; next_sid = t.next_sid + 1 }, news.sid) else (t, news.sid)

let set_text t sid text = { t with statements = map_statement t.statements sid (fun s -> { s with text }) }
let delete t sid = { t with statements = remove t.statements sid }

let inside (branch : statement) target = List.exists (fun s -> s.sid = target) (all [ branch ])

let move t sid ~target where =
  match get t sid with
  | Some branch when not (inside branch target) ->
      let statements, found = place (remove t.statements sid) ~target where branch in
      if found then Some { t with statements } else None
  | _ -> None

let copy t sid ~target where =
  match get t sid with
  | Some branch when not (inside branch target) ->
      let next = ref t.next_sid in
      let rec fresh s =
        let sid = !next in
        incr next;
        { s with sid; children = List.map fresh s.children }
      in
      let copied = fresh branch in
      let statements, found = place t.statements ~target where copied in
      if found then Some { statements; next_sid = !next } else None
  | _ -> None

let visible t ~levels : (statement * int) list =
  let rec go depth l =
    List.concat_map
      (fun s -> (s, depth) :: (match levels with Some n when depth + 1 >= n -> [] | _ -> go (depth + 1) s.children))
      l
  in
  go 0 t.statements

let links (text : string) : (int * int * string) list =
  let n = String.length text in
  let rec go i acc =
    match String.index_from_opt text i '<' with
    | None -> List.rev acc
    | Some a -> (
        match String.index_from_opt text a '>' with
        | Some b when b > a + 1 -> go (b + 1) ((a, b + 1, String.sub text (a + 1) (b - a - 1)) :: acc)
        | _ -> go (a + 1) acc)
  in
  if n = 0 then [] else go 0 []

let word_at (text : string) (i : int) : (int * int) option =
  let n = String.length text in
  let i = ref (max 0 (min i (n - 1))) in
  if n = 0 then None
  else begin
    while !i < n && text.[!i] = ' ' do incr i done;
    if !i >= n then None
    else begin
      let a = ref !i and b = ref !i in
      while !a > 0 && text.[!a - 1] <> ' ' do decr a done;
      while !b < n && text.[!b] <> ' ' do incr b done;
      Some (!a, !b)
    end
  end
