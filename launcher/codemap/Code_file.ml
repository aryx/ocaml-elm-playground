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
  names : Highlight_code.occurrence list array;
  uses : (int * int, Highlight_code.occurrence list) Hashtbl.t;
}

let cols = 80
let trick = "the trick of this game"

let plain (src : string) : Highlight_code.span list array =
  String.split_on_char '\n' src |> List.map (fun l -> if l = "" then [] else [ { Highlight_code.col = 0; text = l; category = Normal } ]) |> Array.of_list

let make (path : string) (src : string) : t =
  (* claude: an ocamllex or ocamlyacc file too, mostly OCaml; plain if
   * OCaml's lexer gives up on it *)
  let ocaml = List.exists (Filename.check_suffix path) [ ".ml"; ".mli"; ".mll"; ".mly" ] in
  let c = List.exists (Filename.check_suffix path) [ ".c"; ".h" ] in
  let lines, occurrences =
    if ocaml then (try Highlight_ml.analyze src with _ -> (plain src, []))
    else if c then (try Highlight_c.analyze src with _ -> (plain src, []))
    else (plain src, [])
  in
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
  (* claude: the lines saying the trick is here: [trick] in a comment,
   * not in quotes (a comment about the marker, or a string, is not one:
   * tinybox's own code has both) *)
  let says (text : string) : bool =
    let n = String.length trick in
    let rec at i =
      i + n <= String.length text && ((String.sub text i n = trick && (i = 0 || text.[i - 1] <> '"')) || at (i + 1))
    in
    at 0
  in
  let marks =
    List.filter_map
      (fun y ->
        if List.exists (fun (s : Highlight_code.span) -> (s.category = Comment || s.category = Comment_section) && says s.text) lines.(y)
        then Some y
        else None)
      (List.init n Fun.id)
  in
  (* claude: the names bound in a function, by line and by binding *)
  let names = Array.make n [] and uses = Hashtbl.create 64 in
  List.iter
    (fun (o : Highlight_code.occurrence) ->
      if o.line >= 0 && o.line < n then names.(o.line) <- o :: names.(o.line);
      Hashtbl.replace uses o.bound_at (o :: Option.value (Hashtbl.find_opt uses o.bound_at) ~default:[]))
    occurrences;
  { path; lines; grid; chars; defs = List.rev !defs; marks; names; uses }

let nlines (f : t) : int = Array.length f.lines

let at (f : t) (line : int) (col : int) : Highlight_code.category option =
  if line < 0 || line >= nlines f || col < 0 || col >= cols then None
  else
    let c = Char.code (Bytes.get f.grid ((line * cols) + col)) in
    if c = 0 then None else Some Highlight_code.all.(c - 1)

let modules_used = Code_deps.modules_used

let name_at (f : t) (line : int) (col : int) : Highlight_code.occurrence option =
  if line < 0 || line >= nlines f then None
  else List.find_opt (fun (o : Highlight_code.occurrence) -> col >= o.col && col < o.col + o.len) f.names.(line)

let uses (f : t) (o : Highlight_code.occurrence) : Highlight_code.occurrence list =
  Option.value (Hashtbl.find_opt f.uses o.bound_at) ~default:[]
