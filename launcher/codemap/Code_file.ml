(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_file.mli *)

type t = {
  path : string;
  lines : Highlight_code.span list array;
  grid : Bytes.t;
  defs : (int * string * Highlight_code.category) list;
}

let cols = 80

let plain (src : string) : Highlight_code.span list array =
  String.split_on_char '\n' src |> List.map (fun l -> if l = "" then [] else [ { Highlight_code.col = 0; text = l; category = Normal } ]) |> Array.of_list

let make (path : string) (src : string) : t =
  let ocaml = Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli" in
  let lines = if ocaml then Highlight_ml.lines src else plain src in
  let n = Array.length lines in
  let grid = Bytes.make (n * cols) '\000' in
  let defs = ref [] in
  Array.iteri
    (fun y spans ->
      List.iter
        (fun (s : Highlight_code.span) ->
          let code = Char.chr (1 + Highlight_code.index s.category) in
          String.iteri (fun k c -> if s.col + k < cols && c <> ' ' && c <> '\t' then Bytes.set grid ((y * cols) + s.col + k) code) s.text;
          match s.category with
          | Def_function | Def_value | Def_type | Def_module -> defs := (y, s.text, s.category) :: !defs
          | Comment_section when String.length s.text > 4 && s.text.[3] <> '*' ->
              (* a section's title: (* Model *) *)
              let t = String.trim (String.sub s.text 2 (String.length s.text - 4)) in
              defs := (y, t, s.category) :: !defs
          | _ -> ())
        spans)
    lines;
  { path; lines; grid; defs = List.rev !defs }

let nlines (f : t) : int = Array.length f.lines

let at (f : t) (line : int) (col : int) : Highlight_code.category option =
  if line < 0 || line >= nlines f || col < 0 || col >= cols then None
  else
    let c = Char.code (Bytes.get f.grid ((line * cols) + col)) in
    if c = 0 then None else Some Highlight_code.all.(c - 1)

let modules_used (src : string) : string list =
  let rec go acc = function
    | (a : Token_ml.t) :: (b :: _ as rest) when a.kind = Uident && b.text = "." -> go (a.text :: acc) rest
    | (a : Token_ml.t) :: ((b : Token_ml.t) :: _ as rest) when (a.text = "open" || a.text = "include") && b.kind = Uident ->
        go (b.text :: acc) rest
    | _ :: rest -> go acc rest
    | [] -> acc
  in
  List.sort_uniq compare (go [] (Lexer_ml.tokens src))
