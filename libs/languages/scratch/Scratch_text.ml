(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scratch_text.mli *)

open Scratch_blocks

exception Bad of string

(*****************************************************************************)
(* Printing *)
(*****************************************************************************)

(* the words and slots, spaced, but a ? kept against what it asks *)
let join pieces =
  List.fold_left (fun acc p -> if acc = "" then p else if p = "?" then acc ^ p else acc ^ " " ^ p) "" pieces

let rec slot_text part a =
  match (a, part) with
  | Block b, _ -> reporter b
  | Lit s, Num _ -> "(" ^ s ^ ")"
  | Lit s, Menu _ -> "[" ^ s ^ " v]"
  | Lit s, Bool -> "<" ^ s ^ ">"
  (* a number in a text slot, as it is usually written: round *)
  | Lit s, _ when s <> "" && float_of_string_opt s <> None -> "(" ^ s ^ ")"
  | Lit s, _ -> "[" ^ s ^ "]"

and reporter (b : block) =
  match (b.op, b.args) with
  | "data_variable", [ Lit name ] -> "(" ^ name ^ ")"
  | _ ->
      let inner = line b in
      if (spec b.op).shape = Predicate then "<" ^ inner ^ ">" else "(" ^ inner ^ ")"

(* the lines of a block's template, the arguments given out in turn *)
and lines (b : block) =
  let args = ref b.args in
  let next () = match !args with a :: rest -> args := rest; a | [] -> Lit "" in
  List.map (fun parts -> join (List.map (function Word w -> w | part -> slot_text part (next ())) parts)) (spec b.op).lines

and line b = List.hd (lines b)

let rec stack indent blocks = List.concat_map (block indent) blocks

and block indent (b : block) =
  let pad = String.make indent ' ' in
  match lines b with
  | first :: others when b.mouths <> [] ->
      let rec mouths ms ls =
        match (ms, ls) with
        | m :: ms, l :: ls -> stack (indent + 2) m @ [ pad ^ l ] @ mouths ms ls
        | [ m ], [] -> stack (indent + 2) m @ [ pad ^ "end" ]
        | m :: ms, [] -> stack (indent + 2) m @ mouths ms []
        | [], _ -> [ pad ^ "end" ]
      in
      (pad ^ first) :: mouths b.mouths others
  | l :: _ -> [ pad ^ l ]
  | [] -> []

let print scripts = String.concat "\n\n" (List.map (fun s -> String.concat "\n" (stack 0 s.blocks)) scripts) ^ "\n"

(*****************************************************************************)
(* Reading a line *)
(*****************************************************************************)

type tok = W of string | P of string | S of string | A of string

(* the index of what closes the bracket opened at i *)
let rec closing s i =
  let n = String.length s in
  let rec go j depth =
    if j >= n then raise (Bad ("no closing bracket in: " ^ s))
    else
      match (s.[i], s.[j]) with
      | _, '[' when s.[i] <> '[' -> go (String.index_from s j ']' + 1) depth
      | '[', ']' -> j
      | '(', '(' -> go (j + 1) (depth + 1)
      | '(', ')' -> if depth = 1 then j else go (j + 1) (depth - 1)
      | '<', '(' -> go (closing s j + 1) depth
      | '<', '<' when j + 1 < n && s.[j + 1] <> ' ' -> go (j + 1) (depth + 1)
      | '<', '>' when s.[j - 1] <> ' ' -> if depth = 1 then j else go (j + 1) (depth - 1)
      | _ -> go (j + 1) depth
  in
  go (i + 1) 1

let tokenize s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      match s.[i] with
      | ' ' -> go (i + 1) acc
      | ('(' | '[') as c ->
          let j = closing s i in
          let inner = String.sub s (i + 1) (j - i - 1) in
          go (j + 1) ((if c = '(' then P inner else S inner) :: acc)
      | '<' when i + 1 < n && s.[i + 1] <> ' ' ->
          let j = closing s i in
          go (j + 1) (A (String.sub s (i + 1) (j - i - 1)) :: acc)
      | _ ->
          let rec word j = if j < n && s.[j] <> ' ' && s.[j] <> '(' && s.[j] <> '[' then word (j + 1) else j in
          let j = word i in
          go j (W (String.sub s i (j - i)) :: acc)
  in
  go 0 []

let is_number s = s = "" || float_of_string_opt s <> None

(* the arguments a line's tokens give a template's parts, if they fit *)
let rec fit parts toks =
  match (parts, toks) with
  | [], [] -> Some []
  | Word w :: parts, W t :: toks when String.lowercase_ascii w = String.lowercase_ascii t -> fit parts toks
  | (Num _ | Text _ | Menu _) :: parts, ((P _ | S _ | A _) as t) :: toks -> Option.map (fun rest -> arg t :: rest) (fit parts toks)
  | Bool :: parts, (A _ as t) :: toks -> Option.map (fun rest -> arg t :: rest) (fit parts toks)
  | _ -> None

and arg = function
  | S s ->
      let l = String.length s in
      Lit (if l >= 2 && String.sub s (l - 2) 2 = " v" then String.sub s 0 (l - 2) else s)
  | P s -> if is_number (String.trim s) then Lit (String.trim s) else Block (inside s [ Reporter; Predicate ])
  | A s -> if String.trim s = "" then Lit "" else Block (inside s [ Predicate ])
  | W w -> Lit w

(* a bracket's inside: a reporter's words, else (round) a variable *)
and inside s shapes =
  match find (tokenize s) shapes with
  | Some b -> b
  | None -> if List.mem Reporter shapes then variable (String.trim s) else raise (Bad ("I don't know the block <" ^ s ^ ">"))

and find toks shapes =
  List.find_map
    (fun (sp : spec) ->
      if sp.op = "data_variable" || not (List.mem sp.shape shapes) then None
      else Option.map (fun args -> { (make sp.op) with args }) (fit (List.hd sp.lines) toks))
    specs

(*****************************************************************************)
(* Reading scripts *)
(*****************************************************************************)

let stack_shapes = [ Hat; Stack; Cap; C_block; C_cap ]

(* the blocks a line can be: "if <> then" is the if, and the if-else
   when an else comes before its end *)
let candidates l =
  let toks = tokenize l in
  match
    List.filter_map
      (fun (sp : spec) -> if List.mem sp.shape stack_shapes then Option.map (fun args -> { (make sp.op) with args }) (fit (List.hd sp.lines) toks) else None)
      specs
  with
  | [] -> raise (Bad ("I don't know the block: " ^ l))
  | bs -> bs

(* a stack, up to the "end" or "else" of the block it is in *)
let rec stack lines =
  match lines with
  | [] -> ([], [])
  | ("end" | "else") :: _ -> ([], lines)
  | l :: rest ->
      let rec first = function
        | [ b ] -> mouths b rest
        | b :: others -> ( try mouths b rest with Bad _ -> first others)
        | [] -> raise (Bad l)
      in
      let b, rest = first (candidates l) in
      let tail, rest = stack rest in
      (b :: tail, rest)

(* a C block's mouths, and the lines after its end *)
and mouths b rest =
  if b.mouths = [] then (b, rest)
  else
    let n = List.length b.mouths in
    let rec go k rest acc =
      let body, rest = stack rest in
      match rest with
      | "else" :: rest when k < n - 1 -> go (k + 1) rest (body :: acc)
      | "end" :: rest -> (List.rev (body :: acc), rest)
      (* the ends a script's last blocks leave out *)
      | [] -> (List.rev (body :: acc), [])
      | l :: _ -> raise (Bad ("an " ^ l ^ " that does not belong here"))
    in
    let ms, rest = go 0 rest [] in
    ({ b with mouths = ms @ List.init (n - List.length ms) (fun _ -> []) }, rest)

let parse text =
  let strip l = match String.index_opt l '/' with Some i when i + 1 < String.length l && l.[i + 1] = '/' -> String.sub l 0 i | _ -> l in
  let lines = List.map (fun l -> String.trim (strip l)) (String.split_on_char '\n' text) in
  (* the scripts: the runs of lines between blank ones *)
  let close group groups = if group = [] then groups else List.rev group :: groups in
  let group, groups = List.fold_left (fun (group, groups) l -> if l = "" then ([], close group groups) else (l :: group, groups)) ([], []) lines in
  let groups = List.rev (close group groups) in
  try
    Ok
      (List.map
         (fun g ->
           match stack g with
           | blocks, [] -> { x = 0.; y = 0.; blocks }
           | _, l :: _ -> raise (Bad ("an " ^ l ^ " without its block")))
         groups)
  with Bad msg -> Error msg | Not_found -> Error "a bracket not closed"
