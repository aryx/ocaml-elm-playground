(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mail.mli *)

(*****************************************************************************)
(* A message *)
(*****************************************************************************)

type field = { name : string; raw : string }
type t = { fields : field list; body : string }

(* CR LF to LF; a lone CR is kept *)
let lf (s : string) : string =
  let b = Buffer.create (String.length s) in
  String.iteri (fun i c -> if not (c = '\r' && i + 1 < String.length s && s.[i + 1] = '\n') then Buffer.add_char b c) s;
  Buffer.contents b

let is_space c = c = ' ' || c = '\t'

let parse (text : string) : t =
  let text = lf text in
  (* the headers, line by line, from [pos]; the body is what is left *)
  let rec fields acc pos =
    if pos >= String.length text then (List.rev acc, "")
    else
      let eol = Option.value (String.index_from_opt text pos '\n') ~default:(String.length text) in
      let line = String.sub text pos (eol - pos) in
      let next = min (String.length text) (eol + 1) in
      if line = "" then (List.rev acc, String.sub text next (String.length text - next))
      else if is_space line.[0] && acc <> [] then
        (* a continuation: the field before goes on *)
        let f = List.hd acc in
        fields ({ f with raw = f.raw ^ "\n" ^ line } :: List.tl acc) next
      else
        match String.index_opt line ':' with
        | Some i when i > 0 && not (String.contains (String.sub line 0 i) ' ') ->
            fields ({ name = String.sub line 0 i; raw = String.sub line (i + 1) (String.length line - i - 1) } :: acc) next
        | _ -> (List.rev acc, String.sub text pos (String.length text - pos))
  in
  let fields, body = fields [] 0 in
  { fields; body }

let to_string (m : t) : string =
  String.concat "" (List.map (fun f -> f.name ^ ":" ^ f.raw ^ "\n") m.fields) ^ "\n" ^ m.body

let unfold (s : string) : string = String.concat "" (String.split_on_char '\n' s)
let same a b = String.lowercase_ascii a = String.lowercase_ascii b
let get_all (m : t) (name : string) : string list = List.filter_map (fun f -> if same f.name name then Some (String.trim (unfold f.raw)) else None) m.fields
let get (m : t) (name : string) : string option = match get_all m name with v :: _ -> Some v | [] -> None

let set (name : string) (value : string) (m : t) : t =
  let f = { name; raw = " " ^ value } in
  if List.exists (fun g -> same g.name name) m.fields then
    let rec go = function [] -> [] | g :: rest when same g.name name -> f :: List.filter (fun g -> not (same g.name name)) rest | g :: rest -> g :: go rest in
    { m with fields = go m.fields }
  else { m with fields = m.fields @ [ f ] }

let make (fields : (string * string) list) (body : string) : t = { fields = List.map (fun (name, v) -> { name; raw = " " ^ v }) fields; body }

(*****************************************************************************)
(* Addresses *)
(*****************************************************************************)

type address = { display : string; mailbox : string }

let unquote (s : string) : string =
  let s = String.trim s in
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

let address (s : string) : address option =
  let s = String.trim s in
  let between o c = match (String.index_opt s o, String.rindex_opt s c) with Some i, Some j when j > i -> Some (i, j) | _ -> None in
  let a =
    match between '<' '>' with
    | Some (i, j) -> { display = unquote (String.sub s 0 i); mailbox = String.trim (String.sub s (i + 1) (j - i - 1)) }
    | None -> (
        (* the old form: the name as a comment after the address *)
        match between '(' ')' with
        | Some (i, j) -> { display = String.trim (String.sub s (i + 1) (j - i - 1)); mailbox = String.trim (String.sub s 0 i) }
        | None -> { display = ""; mailbox = s })
  in
  if a.mailbox = "" then None else Some a

(* the commas at depth 0: not in "...", <...> or (...) *)
let addresses (s : string) : address list =
  let parts = ref [] and start = ref 0 and quoted = ref false and depth = ref 0 in
  String.iteri
    (fun i c ->
      match c with
      | '"' -> quoted := not !quoted
      | ('<' | '(') when not !quoted -> incr depth
      | ('>' | ')') when not !quoted -> decr depth
      | ',' when (not !quoted) && !depth = 0 ->
          parts := String.sub s !start (i - !start) :: !parts;
          start := i + 1
      | _ -> ())
    s;
  let parts = List.rev (String.sub s !start (String.length s - !start) :: !parts) in
  List.filter_map address parts

let address_to_string (a : address) : string =
  if a.display = "" then a.mailbox
  else
    (* RFC 5322's specials, which a bare display name cannot hold *)
    let special = String.exists (fun c -> String.contains "()<>[]:;@\\,.\"" c) a.display in
    (if special then "\"" ^ a.display ^ "\"" else a.display) ^ " <" ^ a.mailbox ^ ">"

let who (a : address) : string = if a.display <> "" then a.display else a.mailbox

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

type date = { day : Civil.date; time : Clock.time_of_day; offset : int }

let months = [ "jan"; "feb"; "mar"; "apr"; "may"; "jun"; "jul"; "aug"; "sep"; "oct"; "nov"; "dec" ]

(* the zones RFC 822 named, before numeric offsets won *)
let zones = [ ("gmt", 0); ("ut", 0); ("utc", 0); ("z", 0); ("est", -300); ("edt", -240); ("cst", -360); ("cdt", -300); ("mst", -420); ("mdt", -360); ("pst", -480); ("pdt", -420) ]

let index_of x l =
  let rec go i = function [] -> None | y :: _ when y = x -> Some i | _ :: r -> go (i + 1) r in
  go 0 l

let date (s : string) : date option =
  (* a comment "(CEST)" dropped; commas are spaces *)
  let s = match String.index_opt s '(' with Some i -> String.sub s 0 i | None -> s in
  let words = String.map (fun c -> if c = ',' then ' ' else c) s |> String.split_on_char ' ' |> List.filter (( <> ) "") in
  let words = match words with w :: rest when int_of_string_opt w = None -> rest | l -> l in
  match words with
  | d :: mon :: y :: hms :: zone -> (
      let mon = String.lowercase_ascii (if String.length mon >= 3 then String.sub mon 0 3 else mon) in
      let offset =
        match zone with
        | z :: _ when String.length z = 5 && (z.[0] = '+' || z.[0] = '-') -> (
            match int_of_string_opt (String.sub z 1 4) with
            | Some hhmm -> Some ((if z.[0] = '-' then -1 else 1) * (((hhmm / 100) * 60) + (hhmm mod 100)))
            | None -> None)
        | z :: _ -> List.assoc_opt (String.lowercase_ascii z) zones
        | [] -> Some 0
      in
      let hms = List.map int_of_string_opt (String.split_on_char ':' hms) in
      match (int_of_string_opt d, index_of mon months, int_of_string_opt y, hms, offset) with
      | Some day, Some m, Some y, (Some h :: Some mi :: sec), Some offset ->
          let year = if y < 50 then 2000 + y else if y < 100 then 1900 + y else y in
          let second = match sec with [ Some s ] -> float_of_int s | _ -> 0. in
          let day = { Civil.year; month = m + 1; day } in
          if Civil.is_valid day && h < 24 && mi < 60 then Some { day; time = { Clock.hour = h; minute = mi; second }; offset } else None
      | _ -> None)
  | _ -> None

let date_to_string (d : date) : string =
  let wd = Civil.weekday (Civil.days_from_civil d.day) in
  let a = abs d.offset in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d %c%02d%02d"
    (String.sub (Civil.weekday_name wd) 0 3)
    d.day.day
    (String.capitalize_ascii (List.nth months (d.day.month - 1)))
    d.day.year d.time.hour d.time.minute (int_of_float d.time.second)
    (if d.offset < 0 then '-' else '+')
    (a / 60) (a mod 60)

let seconds (d : date) : float = Clock.of_local ~offset:d.offset d.day d.time

(*****************************************************************************)
(* Message-IDs *)
(*****************************************************************************)

let message_ids (s : string) : string list =
  let rec go pos acc =
    match String.index_from_opt s pos '<' with
    | None -> List.rev acc
    | Some i -> (
        match String.index_from_opt s i '>' with Some j -> go (j + 1) (String.sub s (i + 1) (j - i - 1) :: acc) | None -> List.rev acc)
  in
  go 0 []
