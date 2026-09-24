(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ics.mli *)

type moment = { date : Civil.date; time : int option; utc : bool }

type event = {
  uid : string;
  summary : string;
  description : string;
  location : string;
  start : moment;
  end_ : moment option;
  rrule : Recur.rule option;
}

type todo = { uid : string; summary : string; due : moment option; priority : int; completed : bool }
type calendar = { events : event list; todos : todo list }

(*****************************************************************************)
(* Lines *)
(*****************************************************************************)

let unfold (s : string) : string list =
  let lines =
    String.split_on_char '\n' s
    |> List.map (fun l -> if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1) else l)
  in
  let continued l = String.length l > 0 && (l.[0] = ' ' || l.[0] = '\t') in
  List.fold_left
    (fun acc l ->
      match acc with
      | prev :: rest when continued l -> (prev ^ String.sub l 1 (String.length l - 1)) :: rest
      | _ -> l :: acc)
    [] lines
  |> List.rev |> List.filter (fun l -> l <> "")

let fold (line : string) : string =
  let n = String.length line in
  (* a byte continuing a UTF-8 character: 10xxxxxx *)
  let inside i = i < n && Char.code line.[i] land 0xC0 = 0x80 in
  let rec chunks from limit =
    if n - from <= limit then [ String.sub line from (n - from) ]
    else
      let cut = ref (from + limit) in
      while inside !cut do decr cut done;
      String.sub line from (!cut - from) :: chunks !cut 74 (* the space makes 75 *)
  in
  String.concat "\r\n " (chunks 0 75)

(*****************************************************************************)
(* Content lines *)
(*****************************************************************************)

let content_line (l : string) : (string * (string * string) list * string) option =
  let n = String.length l in
  let rec upto i stops = if i < n && not (List.mem l.[i] stops) then upto (i + 1) stops else i in
  let name_end = upto 0 [ ';'; ':' ] in
  (* the parameters, from [i] (at a ';' or the ':') *)
  let rec params i acc =
    if i >= n then None
    else if l.[i] = ':' then Some (List.rev acc, String.sub l (i + 1) (n - i - 1))
    else
      let eq = upto (i + 1) [ '='; ';'; ':' ] in
      if eq >= n then None
      else if l.[eq] <> '=' then
        (* vCard 2.1's bare parameter: TEL;HOME:... is TYPE=HOME *)
        params eq (("TYPE", String.uppercase_ascii (String.sub l (i + 1) (eq - i - 1))) :: acc)
      else
        let pname = String.uppercase_ascii (String.sub l (i + 1) (eq - i - 1)) in
        let v_end =
          if eq + 1 < n && l.[eq + 1] = '"' then
            match String.index_from_opt l (eq + 2) '"' with Some q -> upto q [ ';'; ':' ] | None -> n
          else upto (eq + 1) [ ';'; ':' ]
        in
        let v = String.sub l (eq + 1) (v_end - eq - 1) in
        let v = if String.length v >= 2 && v.[0] = '"' then String.sub v 1 (String.length v - 2) else v in
        params v_end ((pname, v) :: acc)
  in
  if name_end = 0 || name_end >= n then None
  else
    Option.map
      (fun (ps, value) -> (String.uppercase_ascii (String.sub l 0 name_end), ps, value))
      (params name_end [])

let escape (s : string) : string =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '\\' -> Buffer.add_string b "\\\\"
      | ';' -> Buffer.add_string b "\\;"
      | ',' -> Buffer.add_string b "\\,"
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let unescape (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      if s.[i] = '\\' && i + 1 < n then (
        Buffer.add_char b (match s.[i + 1] with 'n' | 'N' -> '\n' | c -> c);
        go (i + 2))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

(*****************************************************************************)
(* Values *)
(*****************************************************************************)

let digits (s : string) (from : int) (len : int) : int option =
  if from + len > String.length s then None
  else
    let sub = String.sub s from len in
    if String.for_all (fun c -> c >= '0' && c <= '9') sub then int_of_string_opt sub else None

let moment_of_string (s : string) : moment option =
  let ( let* ) = Option.bind in
  let* year = digits s 0 4 in
  let* month = digits s 4 2 in
  let* day = digits s 6 2 in
  let date : Civil.date = { year; month; day } in
  if not (Civil.is_valid date) then None
  else
    match String.length s with
    | 8 -> Some { date; time = None; utc = false }
    | (15 | 16) when s.[8] = 'T' && (String.length s = 15 || s.[15] = 'Z') ->
        let* h = digits s 9 2 in
        let* m = digits s 11 2 in
        let* sec = digits s 13 2 in
        (* 60: a leap second, which the format allows *)
        if h > 23 || m > 59 || sec > 60 then None
        else Some { date; time = Some ((h * 3600) + (m * 60) + sec); utc = String.length s = 16 }
    | _ -> None

let moment_to_string (m : moment) : string =
  let d = Printf.sprintf "%04d%02d%02d" m.date.year m.date.month m.date.day in
  match m.time with
  | None -> d
  | Some t -> Printf.sprintf "%sT%02d%02d%02d%s" d (t / 3600) (t / 60 mod 60) (t mod 60) (if m.utc then "Z" else "")

let weekdays = [ "SU"; "MO"; "TU"; "WE"; "TH"; "FR"; "SA" ]

exception Unsupported

let rule_of_string (s : string) : Recur.rule option =
  let int_of v = match int_of_string_opt v with Some i -> i | None -> raise Unsupported in
  (* a weekday's two letters; "1FR" or "-1SU" (an ordinal) is refused *)
  let weekday v =
    let rec index i = function [] -> raise Unsupported | w :: rest -> if w = v then i else index (i + 1) rest in
    index 0 weekdays
  in
  let part (r : Recur.rule option) (kv : string) : Recur.rule option =
    match String.index_opt kv '=' with
    | None -> raise Unsupported
    | Some i -> (
        let k = String.uppercase_ascii (String.sub kv 0 i) and v = String.sub kv (i + 1) (String.length kv - i - 1) in
        let r = match r with Some r -> r | None -> Recur.make Daily in
        let list f = String.split_on_char ',' v |> List.map f in
        match k with
        | "FREQ" ->
            let freq : Recur.freq =
              match v with
              | "DAILY" -> Daily
              | "WEEKLY" -> Weekly
              | "MONTHLY" -> Monthly
              | "YEARLY" -> Yearly
              | _ -> raise Unsupported
            in
            Some { r with freq }
        | "INTERVAL" -> Some { r with interval = max 1 (int_of v) }
        | "COUNT" -> Some { r with count = Some (int_of v) }
        | "UNTIL" -> (
            match moment_of_string v with
            | Some { date; time = None; _ } -> Some { r with until = Some (Until_date date) }
            | Some { date; time = Some t; _ } -> Some { r with until = Some (Until_time (date, t)) }
            | None -> raise Unsupported)
        | "BYDAY" -> Some { r with by_day = list weekday }
        | "BYMONTHDAY" ->
            Some
              { r with
                by_month_day = list (fun d -> let d = int_of d in if d = 0 || abs d > 31 then raise Unsupported else d) }
        | "WKST" -> Some { r with week_start = weekday v }
        | _ -> raise Unsupported)
  in
  let parts = String.split_on_char ';' s |> List.filter (fun p -> p <> "") in
  let has k = List.exists (fun p -> String.starts_with ~prefix:(k ^ "=") (String.uppercase_ascii p)) parts in
  match List.fold_left part None parts with
  | exception Unsupported -> None
  | None -> None
  | Some r ->
      (* BYDAY and BYMONTHDAY mean other things under the other
       * frequencies (every Monday of the month...): refused *)
      if (not (has "FREQ")) || (r.by_day <> [] && r.freq <> Weekly) || (r.by_month_day <> [] && r.freq <> Monthly)
      then None
      else Some r

let rule_to_string (r : Recur.rule) : string =
  let freq = match r.freq with Daily -> "DAILY" | Weekly -> "WEEKLY" | Monthly -> "MONTHLY" | Yearly -> "YEARLY" in
  let weekday i = List.nth weekdays i in
  [ Some ("FREQ=" ^ freq);
    (if r.interval > 1 then Some (Printf.sprintf "INTERVAL=%d" r.interval) else None);
    (if r.week_start <> 1 then Some ("WKST=" ^ weekday r.week_start) else None);
    (if r.by_day <> [] then Some ("BYDAY=" ^ String.concat "," (List.map weekday r.by_day)) else None);
    (if r.by_month_day <> [] then Some ("BYMONTHDAY=" ^ String.concat "," (List.map string_of_int r.by_month_day))
     else None);
    Option.map (Printf.sprintf "COUNT=%d") r.count;
    Option.map
      (function
        | Recur.Until_date d -> "UNTIL=" ^ moment_to_string { date = d; time = None; utc = false }
        | Until_time (d, t) -> "UNTIL=" ^ moment_to_string { date = d; time = Some t; utc = false })
      r.until ]
  |> List.filter_map Fun.id |> String.concat ";"

(*****************************************************************************)
(* Components *)
(*****************************************************************************)

type property = string * (string * string) list * string
type component = { name : string; props : property list; children : component list }

(* the lines up to [name]'s END (or the end of the text), nested
 * components included *)
let rec component (name : string) (lines : property list) : component * property list =
  let rec go lines props children =
    match lines with
    | [] -> ({ name; props = List.rev props; children = List.rev children }, [])
    | ("END", _, v) :: rest when String.uppercase_ascii v = name ->
        ({ name; props = List.rev props; children = List.rev children }, rest)
    | ("BEGIN", _, v) :: rest ->
        let child, rest = component (String.uppercase_ascii v) rest in
        go rest props (child :: children)
    | p :: rest -> go rest (p :: props) children
  in
  go lines [] []

let value (c : component) (name : string) : string option =
  List.find_map (fun (n, _, v) -> if n = name then Some v else None) c.props

let text (c : component) (name : string) : string = Option.fold ~none:"" ~some:unescape (value c name)
let moment (c : component) (name : string) : moment option = Option.bind (value c name) moment_of_string

let event_of (c : component) : event option =
  Option.map
    (fun start ->
      { uid = text c "UID";
        summary = text c "SUMMARY";
        description = text c "DESCRIPTION";
        location = text c "LOCATION";
        start;
        end_ = moment c "DTEND";
        (* a rule we can't follow: the first occurrence still shown *)
        rrule = Option.bind (value c "RRULE") rule_of_string })
    (moment c "DTSTART")

let todo_of (c : component) : todo =
  { uid = text c "UID";
    summary = text c "SUMMARY";
    due = moment c "DUE";
    priority = Option.value ~default:0 (Option.bind (value c "PRIORITY") int_of_string_opt);
    completed = value c "STATUS" = Some "COMPLETED" || value c "COMPLETED" <> None }

let of_string (s : string) : calendar =
  let root, _ = component "" (unfold s |> List.filter_map content_line) in
  (* the VCALENDAR's components, or the text's own if it has none *)
  let top = match List.find_opt (fun c -> c.name = "VCALENDAR") root.children with Some cal -> cal | None -> root in
  { events = List.filter_map (fun c -> if c.name = "VEVENT" then event_of c else None) top.children;
    todos = List.filter_map (fun c -> if c.name = "VTODO" then Some (todo_of c) else None) top.children }

let to_string ~(stamp : moment) (cal : calendar) : string =
  let line name value = [ name ^ ":" ^ value ] in
  let text name s = if s = "" then [] else line name (escape s) in
  let moment name (m : moment) =
    line (if m.time = None then name ^ ";VALUE=DATE" else name) (moment_to_string m)
  in
  let opt f = function Some x -> f x | None -> [] in
  let event (e : event) =
    List.concat
      [ line "BEGIN" "VEVENT"; line "UID" (escape e.uid); moment "DTSTAMP" stamp; moment "DTSTART" e.start;
        opt (moment "DTEND") e.end_; opt (fun r -> line "RRULE" (rule_to_string r)) e.rrule;
        text "SUMMARY" e.summary; text "DESCRIPTION" e.description; text "LOCATION" e.location;
        line "END" "VEVENT" ]
  in
  let todo (t : todo) =
    List.concat
      [ line "BEGIN" "VTODO"; line "UID" (escape t.uid); moment "DTSTAMP" stamp; text "SUMMARY" t.summary;
        opt (moment "DUE") t.due;
        (if t.priority > 0 then line "PRIORITY" (string_of_int t.priority) else []);
        (if t.completed then line "STATUS" "COMPLETED" else []);
        line "END" "VTODO" ]
  in
  List.concat
    [ line "BEGIN" "VCALENDAR"; line "VERSION" "2.0"; line "PRODID" "-//ocaml-elm-playground//Tiny//EN";
      List.concat_map event cal.events; List.concat_map todo cal.todos; line "END" "VCALENDAR" ]
  |> List.map fold |> List.map (fun l -> l ^ "\r\n") |> String.concat ""
