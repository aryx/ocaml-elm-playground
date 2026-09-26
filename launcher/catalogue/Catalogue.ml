(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Catalogue.mli *)

type program = { name : string; source : string; look : string; after : string; one_line : string; brought : string }
type section = { title : string; games : bool; intro : string; programs : program list }

(*****************************************************************************)
(* Markdown's inline marks *)
(*****************************************************************************)

let plain (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      match s.[i] with
      | '`' -> go (i + 1)
      | '*' when i + 1 < n && s.[i + 1] = '*' -> go (i + 2)
      | '[' -> (
          (* [text](target): the text, if the link is whole *)
          match String.index_from_opt s i ']' with
          | Some j when j + 1 < n && s.[j + 1] = '(' && String.index_from_opt s j ')' <> None ->
              Buffer.add_string b (String.sub s (i + 1) (j - i - 1));
              go (String.index_from s j ')' + 1)
          | _ ->
              Buffer.add_char b '[';
              go (i + 1))
      | c ->
          Buffer.add_char b c;
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(*****************************************************************************)
(* The rows *)
(*****************************************************************************)

(* "| [Name](source) | look | after | one line | brought |" *)
let program_of_row (line : string) : program option =
  match List.map String.trim (String.split_on_char '|' line) with
  | [ ""; link; look; after; one_line; brought; "" ] -> (
      match (String.index_opt link ']', String.index_opt link '(', String.index_opt link ')') with
      | Some j, Some k, Some l when String.length link > 0 && link.[0] = '[' && k = j + 1 ->
          Some
            {
              name = String.sub link 1 (j - 1);
              source = String.sub link (k + 1) (l - k - 1);
              look;
              after = plain after;
              one_line = plain one_line;
              brought = plain brought;
            }
      | _ -> None)
  | _ -> None

(*****************************************************************************)
(* The sections *)
(*****************************************************************************)

let parse (text : string) : section list =
  (* claude: one pass over the lines, the section being read kept with
   * its paragraphs and rows in reverse *)
  let finish (cur : section option) (acc : section list) =
    match cur with
    | Some s when s.programs <> [] -> { s with intro = String.trim s.intro; programs = List.rev s.programs } :: acc
    | _ -> acc
  in
  let rec go games cur acc = function
    | [] -> List.rev (finish cur acc)
    | line :: rest ->
        if String.starts_with ~prefix:"## " line then
          let title = String.trim (String.sub line 3 (String.length line - 3)) in
          go games (Some { title; games; intro = ""; programs = [] }) (finish cur acc) rest
        else if String.starts_with ~prefix:"# " line then
          go (String.trim line = "# Games") None (finish cur acc) rest
        else
          match cur with
          | None -> go games cur acc rest
          | Some s when String.starts_with ~prefix:"|" line -> (
              match program_of_row line with
              | Some p -> go games (Some { s with programs = p :: s.programs }) acc rest
              | None -> go games cur acc rest (* the header and its |---| *))
          | Some s when s.programs = [] && String.trim line <> "" ->
              let sep = if s.intro = "" then "" else " " in
              go games (Some { s with intro = s.intro ^ sep ^ plain (String.trim line) }) acc rest
          | Some _ -> go games cur acc rest
  in
  go true None [] (String.split_on_char '\n' text)

let golden_frame (p : program) : string =
  Printf.sprintf "tests/%s/golden/%s.png" (if p.look = "3D" then "3d" else "2d") p.name
