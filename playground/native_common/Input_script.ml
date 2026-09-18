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

(* a key, by its playground name, and its first and last frames *)
type t = (string * int * int) list

let key_name (s : string) : string =
  match s with
  | "left" -> "ArrowLeft"
  | "right" -> "ArrowRight"
  | "up" -> "ArrowUp"
  | "down" -> "ArrowDown"
  | s -> s

let parse_entry (entry : string) : (string * int * int, string) result =
  let bad () = Error (Printf.sprintf "bad -script entry %S, expected key:n or key:a-b" entry) in
  match String.split_on_char ':' (String.trim entry) with
  | [ key; frames ] when key <> "" -> (
      match String.split_on_char '-' frames |> List.map int_of_string_opt with
      | [ Some n ] -> Ok (key_name key, n, n)
      | [ Some a; Some b ] when a <= b -> Ok (key_name key, a, b)
      | _ -> bad ())
  | _ -> bad ()

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

let down (script : t) (frame : int) : string list =
  script
  |> List.filter (fun (_, a, b) -> a <= frame && frame <= b)
  |> List.map (fun (key, _, _) -> key)
  |> List.sort_uniq compare

let changes (script : t) (frame : int) : (string * bool) list =
  let now = down script frame and before = down script (frame - 1) in
  List.map (fun k -> (k, true)) (List.filter (fun k -> not (List.mem k before)) now)
  @ List.map (fun k -> (k, false)) (List.filter (fun k -> not (List.mem k now)) before)
