(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Line_mode.mli *)

type t = { lines : string list; links : string list }

(*****************************************************************************)
(* The printer's state *)
(*****************************************************************************)

(* the lines so far (the last first), the paragraph being gathered (its
 * words, the last first, and the word being read), where it goes *)
type printer = {
  width : int;
  mutable lines : string list;
  mutable words : string list;
  word : Buffer.t;
  mutable indent : int;
  mutable marker : string; (* before the paragraph's first line: "* ", "2. " *)
  mutable centred : bool;
  mutable links : string list; (* the last first *)
}

(* a UTF-8 string's characters, not its bytes: what a terminal counts *)
let length (s : string) : int =
  let n = ref 0 in
  String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr n) s;
  !n

let end_word (p : printer) : unit =
  if Buffer.length p.word > 0 then (
    p.words <- Buffer.contents p.word :: p.words;
    Buffer.clear p.word)

let add_line (p : printer) (line : string) : unit = p.lines <- line :: p.lines

(* the words gathered, broken into lines, greedily *)
let flush (p : printer) : unit =
  end_word p;
  if p.words <> [] then (
    let first = String.make p.indent ' ' ^ p.marker in
    let others = String.make (length first) ' ' in
    let room = max 1 (p.width - length first) in
    let lines = ref [] and current = ref "" in
    List.iter
      (fun w ->
        if !current = "" then current := w
        else if length !current + 1 + length w <= room then current := !current ^ " " ^ w
        else (
          lines := !current :: !lines;
          current := w))
      (List.rev p.words);
    lines := !current :: !lines;
    List.iteri
      (fun i line ->
        if p.centred then add_line p (String.make (max 0 ((p.width - length line) / 2)) ' ' ^ line)
        else add_line p ((if i = 0 then first else others) ^ line))
      (List.rev !lines);
    p.words <- [];
    p.marker <- "")

(* a blank line, unless at the top or after one already *)
let blank (p : printer) : unit =
  flush p;
  match p.lines with [] | "" :: _ -> () | _ -> add_line p ""

(* text outside <pre>: its spaces end words *)
let add_text (p : printer) (s : string) : unit =
  String.iter (fun c -> if c = ' ' || c = '\n' || c = '\t' then end_word p else Buffer.add_char p.word c) s

(* stuck to what is before it: "recipes[1]" *)
let glue (p : printer) (s : string) : unit =
  if Buffer.length p.word > 0 then Buffer.add_string p.word s
  else match p.words with w :: rest -> p.words <- (w ^ s) :: rest | [] -> Buffer.add_string p.word s

(*****************************************************************************)
(* The tree *)
(*****************************************************************************)

let rec element (p : printer) ~(pre : bool) (e : Dom.element) : unit =
  let children ?(pre = pre) () =
    List.iter
      (fun (n : Dom.node) ->
        match n with Element c -> element p ~pre c | Text s -> if pre then pre_text p s else add_text p s)
      e.children
  in
  let indented by f =
    p.indent <- p.indent + by;
    f ();
    flush p;
    p.indent <- p.indent - by
  in
  match e.name with
  | "head" | "script" | "style" | "title" -> ()
  | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" ->
      blank p;
      p.centred <- true;
      children ();
      flush p;
      p.centred <- false
  | "p" | "address" ->
      blank p;
      children ();
      flush p
  | "br" -> flush p
  | "hr" ->
      blank p;
      add_line p (String.make p.width '-')
  | "pre" ->
      blank p;
      children ~pre:true ();
      flush p
  | "ul" | "ol" | "dir" | "menu" ->
      blank p;
      indented 2 (fun () ->
          let n = ref 0 in
          List.iter
            (fun (c : Dom.node) ->
              match c with
              | Element ({ name = "li"; _ } as li) ->
                  incr n;
                  flush p;
                  p.marker <- (if e.name = "ol" then Printf.sprintf "%d. " !n else "* ");
                  element p ~pre li;
                  flush p
              | Element c -> element p ~pre c
              | Text s -> add_text p s)
            e.children)
  | "dl" ->
      blank p;
      children ()
  | "dt" ->
      flush p;
      children ();
      flush p
  | "dd" -> indented 4 children
  | "blockquote" ->
      blank p;
      indented 4 children
  | "a" when Dom.attribute "href" e <> None ->
      children ();
      p.links <- Option.get (Dom.attribute "href" e) :: p.links;
      glue p (Printf.sprintf "[%d]" (List.length p.links))
  | "img" -> add_text p (match Dom.attribute "alt" e with Some alt -> alt | None -> "[IMAGE]")
  | _ -> children ()

(* inside <pre>: its newlines end lines, its spaces are kept *)
and pre_text (p : printer) (s : string) : unit =
  let parts = String.split_on_char '\n' s in
  List.iteri
    (fun i part ->
      if i > 0 then (
        end_word p;
        add_line p (String.make p.indent ' ' ^ String.concat " " (List.rev p.words));
        p.words <- []);
      Buffer.add_string p.word part)
    parts

let render ?(width = 80) (root : Dom.element) : t =
  let p =
    { width; lines = []; words = []; word = Buffer.create 32; indent = 0; marker = ""; centred = false; links = [] }
  in
  element p ~pre:false root;
  flush p;
  (* no blank lines at the end *)
  let rec trim lines = match lines with "" :: rest -> trim rest | _ -> lines in
  { lines = List.rev (trim p.lines); links = List.rev p.links }
