(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Highlight_asm.mli *)

open Highlight_code

let is_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '.' || c = '$'
let is_ident c = is_start c || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'

let strip (s : string) : string = if String.length s > 1 && s.[0] = '_' then String.sub s 1 (String.length s - 1) else s

let jumps = [ "jmp"; "ljmp"; "call"; "lcall"; "ret"; "lret"; "iret"; "int"; "loop"; "jz"; "jnz"; "je"; "jne"; "jl"; "jle"; "jg"; "jge"; "jb"; "jbe"; "ja"; "jae"; "jc"; "jnc"; "js"; "jns"; "jecxz"; "jcxz"; "jmpi"; "callf"; "jmpf" ]

type tok = { line : int; col : int; text : string; mutable cat : category }

let analyze (src : string) : analysis =
  let lines = String.split_on_char '\n' src in
  let toks = ref [] in
  let add line col text cat = toks := { line; col; text; cat } :: !toks in
  let in_comment = ref false in
  List.iteri
    (fun y0 l ->
      let y = y0 + 1 in
      let n = String.length l in
      let i = ref 0 in
      (* the statement's position: 0 its first word, 1 after its mnemonic *)
      let first = ref true in
      while !i < n do
        let c = l.[!i] in
        if !in_comment then begin
          let start = !i in
          while !i < n && not (!i + 1 < n && l.[!i] = '*' && l.[!i + 1] = '/') do incr i done;
          if !i < n then (i := !i + 2; in_comment := false);
          add y start (String.sub l start (!i - start)) Comment
        end
        else if c = '/' && !i + 1 < n && l.[!i + 1] = '*' then in_comment := true
        else if c = '|' || c = '!' || (c = '#' && !first) || (c = '/' && !i + 1 < n && l.[!i + 1] = '/') then begin
          add y !i (String.sub l !i (n - !i)) Comment;
          i := n
        end
        else if c = ';' then (first := true; incr i)
        else if c = '%' && !i + 1 < n && is_start l.[!i + 1] then begin
          let s = !i in
          incr i;
          while !i < n && is_ident l.[!i] do incr i done;
          add y s (String.sub l s (!i - s)) Parameter
        end
        else if c = '"' || c = '\'' then begin
          let s = !i in
          incr i;
          while !i < n && l.[!i] <> c do incr i done;
          if !i < n then incr i;
          add y s (String.sub l s (!i - s)) String
        end
        else if is_digit c then begin
          let s = !i in
          while !i < n && is_ident l.[!i] do incr i done;
          (* 1: a numeric label, 1f 1b its uses: not names *)
          add y s (String.sub l s (!i - s)) Number;
          if !i < n && l.[!i] = ':' then incr i
        end
        else if is_start c then begin
          let s = !i in
          while !i < n && is_ident l.[!i] do incr i done;
          let text = String.sub l s (!i - s) in
          let j = ref !i in
          while !j < n && (l.[!j] = ' ' || l.[!j] = '\t') do incr j done;
          let next = if !j < n then Some l.[!j] else None in
          if !first && next = Some ':' then (add y s text Def_function; i := !j + 1)
          else if !first && next = Some '=' then (add y s text Def_value; first := false)
          else if !first then begin
            add y s text (if text.[0] = '.' then Keyword_module else if List.mem (String.lowercase_ascii text) jumps then Keyword_control else Keyword);
            first := false
          end
          else add y s text Global
        end
        else incr i
      done)
    lines;
  let toks = Array.of_list (List.rev !toks) in
  let triples = Array.map (fun t -> (t.line, t.col, t.text)) toks in
  (* the file's labels and values, by their text *)
  let defs = Hashtbl.create 64 in
  Array.iteri (fun i t -> if (t.cat = Def_function || t.cat = Def_value) && not (Hashtbl.mem defs t.text) then Hashtbl.replace defs t.text i) toks;
  let binds = Hashtbl.create 64 in
  let definitions = ref [] and references = ref [] in
  Array.iteri
    (fun i t ->
      match t.cat with
      | Def_function | Def_value when Hashtbl.find_opt defs t.text = Some i ->
          Hashtbl.replace binds i i;
          definitions := definition ~name:(strip t.text) triples i Value 3 :: !definitions
      | Global -> (
          match Hashtbl.find_opt defs t.text with
          | Some d -> Hashtbl.replace binds i d; t.cat <- (if toks.(d).cat = Def_value then Constructor else Local)
          | None when t.text.[0] <> '.' ->
              references := { (reference triples i [] Value) with rname = strip t.text } :: !references
          | None -> ())
      | _ -> ())
    toks;
  {
    spans = Highlight_code.lines src (Array.to_list (Array.map (fun t -> (t.line, t.col, t.text, t.cat)) toks));
    occurrences = occurrences triples binds;
    definitions = List.rev !definitions;
    references = List.rev !references;
    opens = [];
    includes = [];
  }
