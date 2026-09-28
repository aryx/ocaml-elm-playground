(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Input_script.mli *)

(* what the script says, and the first and last frames it says it for *)
type entry =
  | Key of string * int * int
  | At of float * float * int * int
  | Button of bool (* the right one *) * int * int
  | Type of string * int

type t = entry list

let key_name (s : string) : string =
  match s with
  | "left" -> "ArrowLeft"
  | "right" -> "ArrowRight"
  | "up" -> "ArrowUp"
  | "down" -> "ArrowDown"
  | s -> s

let frames_of (frames : string) : (int * int) option =
  match String.split_on_char '-' frames |> List.map int_of_string_opt with
  | [ Some n ] -> Some (n, n)
  | [ Some a; Some b ] when a <= b -> Some (a, b)
  | _ -> None

let parse_entry (entry : string) : (entry, string) result =
  let entry = String.trim entry in
  let bad () =
    Error
      (Printf.sprintf
         "bad -script entry %S, expected key:n, key:a-b, at(x;y):n, click:n, rclick:n or type(text):n"
         entry)
  in
  (* claude: the last ':', not the first: the frames never hold one, and
   * a text typed may (a URL, "type(about:history):3", TinyNetscape) *)
  match String.rindex_opt entry ':' with
  | None -> bad ()
  | Some i -> (
      let what = String.sub entry 0 i in
      let frames = String.sub entry (i + 1) (String.length entry - i - 1) in
      match frames_of frames with
      | None -> bad ()
      | Some (a, b) -> (
          let n = String.length what in
          if n > 6 && String.sub what 0 5 = "type(" && what.[n - 1] = ')' then
            Ok (Type (String.sub what 5 (n - 6), a))
          else if n > 4 && String.sub what 0 3 = "at(" && what.[n - 1] = ')' then
            match String.split_on_char ';' (String.sub what 3 (n - 4)) |> List.map float_of_string_opt with
            | [ Some x; Some y ] -> Ok (At (x, y, a, b))
            | _ -> bad ()
          else
            match what with
            | "click" -> Ok (Button (false, a, b))
            | "rclick" -> Ok (Button (true, a, b))
            | "" -> bad ()
            | key -> Ok (Key (key_name key, a, b))))

let parse (s : string) : (t, string) result =
  String.split_on_char ',' s
  |> List.filter (fun e -> String.trim e <> "")
  |> List.fold_left
       (fun acc entry ->
         match (acc, parse_entry entry) with
         | Ok entries, Ok e -> Ok (e :: entries)
         | (Error _ as err), _ -> err
         | _, (Error _ as err) -> err)
       (Ok [])
  |> Result.map List.rev

let covers frame a b = a <= frame && frame <= b

let down (script : t) (frame : int) : string list =
  script
  |> List.filter_map (function Key (k, a, b) when covers frame a b -> Some k | _ -> None)
  |> List.sort_uniq compare

let changes (script : t) (frame : int) : (string * bool) list =
  let now = down script frame and before = down script (frame - 1) in
  List.map (fun k -> (k, true)) (List.filter (fun k -> not (List.mem k before)) now)
  @ List.map (fun k -> (k, false)) (List.filter (fun k -> not (List.mem k now)) before)

(* the last [at] covering this frame wins, so a later entry can move
 * the pointer over a stretch an earlier one also covers *)
let mouse (script : t) (frame : int) : (float * float) option =
  script
  |> List.fold_left
       (fun acc e -> match e with At (x, y, a, b) when covers frame a b -> Some (x, y) | _ -> acc)
       None

let buttons_down (script : t) (frame : int) : bool list =
  script
  |> List.filter_map (function Button (right, a, b) when covers frame a b -> Some right | _ -> None)
  |> List.sort_uniq compare

let typed (script : t) (frame : int) : string =
  script
  |> List.filter_map (function Type (s, a) when a = frame -> Some s | _ -> None)
  |> String.concat ""

let button_changes (script : t) (frame : int) : (bool * bool) list =
  let now = buttons_down script frame and before = buttons_down script (frame - 1) in
  List.map (fun r -> (r, true)) (List.filter (fun r -> not (List.mem r before)) now)
  @ List.map (fun r -> (r, false)) (List.filter (fun r -> not (List.mem r now)) before)
