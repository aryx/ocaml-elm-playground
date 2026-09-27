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
  chars : Bytes.t;
  defs : (int * string * Highlight_code.category) list;
  marks : int list;
}

let cols = 80
let trick = "the trick of this game"

let plain (src : string) : Highlight_code.span list array =
  String.split_on_char '\n' src |> List.map (fun l -> if l = "" then [] else [ { Highlight_code.col = 0; text = l; category = Normal } ]) |> Array.of_list

let make (path : string) (src : string) : t =
  let ocaml = Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli" in
  let lines = if ocaml then Highlight_ml.lines src else plain src in
  let n = Array.length lines in
  let grid = Bytes.make (n * cols) '\000' in
  let chars = Bytes.make (n * cols) '\000' in
  let defs = ref [] in
  Array.iteri
    (fun y spans ->
      List.iter
        (fun (s : Highlight_code.span) ->
          let code = Char.chr (1 + Highlight_code.index s.category) in
          let k = ref 0 in
          while !k < String.length s.text do
            let c, len = Vga_font.decode s.text !k in
            let x = s.col + !k in
            if x < cols && s.text.[!k] <> ' ' && s.text.[!k] <> '\t' then begin
              Bytes.set grid ((y * cols) + x) code;
              Bytes.set chars ((y * cols) + x) (Char.chr c)
            end;
            k := !k + len
          done;
          match s.category with
          | Def_function | Def_value | Def_type | Def_module -> defs := (y, s.text, s.category) :: !defs
          | Comment_section when String.length s.text > 4 && s.text.[3] <> '*' ->
              (* a section's title: (* Model *) *)
              let t = String.trim (String.sub s.text 2 (String.length s.text - 4)) in
              defs := (y, t, s.category) :: !defs
          | _ -> ())
        spans)
    lines;
  (* claude: the lines saying "the trick of this game" *)
  let has s sub =
    let n = String.length sub in
    let rec at i = i + n <= String.length s && (String.sub s i n = sub || at (i + 1)) in
    at 0
  in
  let marks = String.split_on_char '\n' src |> List.mapi (fun i l -> (i, l)) |> List.filter_map (fun (i, l) -> if has l trick then Some i else None) in
  { path; lines; grid; chars; defs = List.rev !defs; marks }

let nlines (f : t) : int = Array.length f.lines

let at (f : t) (line : int) (col : int) : Highlight_code.category option =
  if line < 0 || line >= nlines f || col < 0 || col >= cols then None
  else
    let c = Char.code (Bytes.get f.grid ((line * cols) + col)) in
    if c = 0 then None else Some Highlight_code.all.(c - 1)

let modules_used = Code_deps.modules_used
