(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Dom.mli *)

type node = Element of element | Text of string
and element = { name : string; attributes : (string * string) list; children : node list }

let attribute (name : string) (e : element) : string option = List.assoc_opt name e.attributes

let rec find_all (name : string) (e : element) : element list =
  (if e.name = name then [ e ] else [])
  @ List.concat_map (fun n -> match n with Element c -> find_all name c | Text _ -> []) e.children

let text_content (e : element) : string =
  let b = Buffer.create 64 in
  let rec go (e : element) =
    List.iter (fun n -> match n with Text s -> Buffer.add_string b s | Element c -> go c) e.children
  in
  go e;
  Buffer.contents b

let is_blank (s : string) : bool = String.for_all (fun c -> c = ' ' || c = '\n' || c = '\t') s

let rec without_blank_text (e : element) : element =
  {
    e with
    children =
      List.filter_map
        (fun n ->
          match n with Text s when is_blank s -> None | Text _ -> Some n | Element c -> Some (Element (without_blank_text c)))
        e.children;
  }

let to_lines (root : element) : string list =
  let lines = ref [] in
  let add depth s = lines := (String.make (2 * depth) ' ' ^ s) :: !lines in
  let rec go depth (e : element) =
    add depth
      (String.concat " " (e.name :: List.map (fun (n, v) -> Printf.sprintf "%s=\"%s\"" n v) e.attributes));
    List.iter
      (fun n ->
        match n with
        | Element c -> go (depth + 1) c
        | Text s ->
            let escaped = String.concat "\\n" (String.split_on_char '\n' s) in
            add (depth + 1) ("\"" ^ escaped ^ "\""))
      e.children
  in
  go 0 root;
  List.rev !lines
