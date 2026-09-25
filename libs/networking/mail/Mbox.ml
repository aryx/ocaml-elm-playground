(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mbox.mli *)

type entry = { envelope : string; mail : Mail.t }

(* a line ">>From x" is (2, "From x"): its ">"s counted *)
let quoted (line : string) : int option =
  let n = String.length line in
  let rec go i = if i < n && line.[i] = '>' then go (i + 1) else i in
  let k = go 0 in
  if n - k >= 5 && String.sub line k 5 = "From " then Some k else None

let map_lines f s = String.concat "\n" (List.map f (String.split_on_char '\n' s))
let escape = map_lines (fun l -> match quoted l with Some _ -> ">" ^ l | None -> l)
let unescape = map_lines (fun l -> match quoted l with Some k when k > 0 -> String.sub l 1 (String.length l - 1) | _ -> l)

let parse (text : string) : entry list =
  let text = Mail.lf text in
  let lines = String.split_on_char '\n' text in
  (* the split of a file ending in a line break has an empty last line *)
  let lines = match List.rev lines with "" :: rest -> List.rev rest | _ -> lines in
  let entry envelope body =
    (* the empty line that ended the message is the file's, not its *)
    let body = match body with "" :: rest -> List.rev rest | l -> List.rev l in
    { envelope; mail = Mail.parse (unescape (String.concat "\n" body ^ "\n")) }
  in
  (* body: the current message's lines, the latest first *)
  let rec go lines current acc =
    match (lines, current) with
    | [], None -> List.rev acc
    | [], Some (e, body) -> List.rev (entry e body :: acc)
    | l :: rest, _ when quoted l = Some 0 ->
        let acc = match current with Some (e, body) -> entry e body :: acc | None -> acc in
        go rest (Some (String.sub l 5 (String.length l - 5), [])) acc
    | l :: rest, Some (e, body) -> go rest (Some (e, l :: body)) acc
    | _ :: rest, None -> go rest None acc (* before the first message: not mail *)
  in
  go lines None []

let to_string (entries : entry list) : string =
  String.concat ""
    (List.map
       (fun e ->
         let text = Mail.to_string e.mail in
         let text = if text <> "" && text.[String.length text - 1] = '\n' then text else text ^ "\n" in
         "From " ^ e.envelope ^ "\n" ^ escape text ^ "\n")
       entries)

(* C's asctime: "Fri Sep 25 12:00:00 2026" *)
let envelope ~(sender : string) (d : Mail.date) : string =
  let wd = Civil.weekday (Civil.days_from_civil d.day) in
  Printf.sprintf "%s %s %s %2d %02d:%02d:%02d %d" sender
    (String.sub (Civil.weekday_name wd) 0 3)
    (String.sub (Civil.month_name d.day.month) 0 3)
    d.day.day d.time.hour d.time.minute (int_of_float d.time.second) d.day.year

let sender (e : entry) : string = List.hd (String.split_on_char ' ' e.envelope)
