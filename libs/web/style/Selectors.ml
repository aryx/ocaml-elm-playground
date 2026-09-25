(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Selectors.mli *)
open Css_syntax

type attr_op = Exists | Equals | Includes | Dash | Prefix | Suffix | Substring

type pseudo_class =
  | First_child
  | Last_child
  | Only_child
  | Nth_child of int * int
  | Not of complex list
  | Is of complex list * bool (* :is() (and :matches), counting as its most specific; :where(), counting nothing *)
  | Link
  | Visited
  | Hover
  | Active
  | Focus
  | Root
  | Empty
  | Checked
  | Disabled
  | Enabled

and simple =
  | Type of string
  | Universal
  | Id of string
  | Class of string
  | Attr of string * attr_op * string * bool
  | Pseudo of pseudo_class
  | Pseudo_element of string

and combinator = Descendant | Child | Next_sibling | Subsequent_sibling
and complex = (simple list * combinator option) list

exception Invalid

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* an+b: "odd", "even", "3", "2n+1", "-n+3" -- from the text of the
 * parenthesis, spaces removed *)
let nth (s : string) : int * int =
  let s = String.lowercase_ascii (String.concat "" (String.split_on_char ' ' s)) in
  let int_of s = match s with "" | "+" -> 1 | "-" -> -1 | s -> ( match int_of_string_opt s with Some n -> n | None -> raise Invalid) in
  match s with
  | "odd" -> (2, 1)
  | "even" -> (2, 0)
  | _ -> (
      match String.index_opt s 'n' with
      | None -> (0, match int_of_string_opt s with Some b -> b | None -> raise Invalid)
      | Some i ->
          let a = int_of (String.sub s 0 i) in
          let rest = String.sub s (i + 1) (String.length s - i - 1) in
          let b = if rest = "" then 0 else match int_of_string_opt (if rest.[0] = '+' then String.sub rest 1 (String.length rest - 1) else rest) with Some b -> b | None -> raise Invalid in
          (a, b))

let rec parse_list (cs : component list) : complex list =
  List.map parse_complex (split_on Comma cs)

and parse_complex (cs : component list) : complex =
  (* compounds, and the combinators between them: a space alone is a
   * descendant combinator, spaces around > + ~ are nothing *)
  let rec go (cs : component list) (current : simple list) (acc : complex) : complex =
    match cs with
    | [] -> if current = [] then raise Invalid else List.rev ((List.rev current, None) :: acc)
    | Token Whitespace :: rest -> (
        let rec skip = function Token Whitespace :: r -> skip r | l -> l in
        match skip rest with
        | [] -> go [] current acc
        | Token (Delim (('>' | '+' | '~') as c)) :: rest -> combine c rest current acc
        | rest -> if current = [] then go rest current acc else go rest [] ((List.rev current, Some Descendant) :: acc))
    | Token (Delim (('>' | '+' | '~') as c)) :: rest -> combine c rest current acc
    | _ ->
        let s, rest = simple cs in
        go rest (s :: current) acc
  and combine c rest current acc =
    if current = [] then raise Invalid;
    let comb = match c with '>' -> Child | '+' -> Next_sibling | _ -> Subsequent_sibling in
    let rec skip = function Token Whitespace :: r -> skip r | l -> l in
    go (skip rest) [] ((List.rev current, Some comb) :: acc)
  in
  go cs [] []

(* one simple selector at the head of [cs] *)
and simple (cs : component list) : simple * component list =
  match cs with
  | Token (Ident n) :: rest -> (Type (String.lowercase_ascii n), rest)
  | Token (Delim '*') :: rest -> (Universal, rest)
  | Token (Hash h) :: rest -> (Id h, rest)
  | Token (Delim '.') :: Token (Ident c) :: rest -> (Class c, rest)
  | Block ('[', inside) :: rest -> (attribute (trim inside), rest)
  | Token Colon :: Token Colon :: Token (Ident p) :: rest -> (Pseudo_element (String.lowercase_ascii p), rest)
  (* the old one-colon pseudo-elements *)
  | Token Colon :: Token (Ident (("before" | "after" | "first-line" | "first-letter") as p)) :: rest -> (Pseudo_element p, rest)
  | Token Colon :: Token (Ident p) :: rest ->
      let pc =
        match String.lowercase_ascii p with
        | "first-child" -> First_child
        | "last-child" -> Last_child
        | "only-child" -> Only_child
        | "link" | "any-link" -> Link
        | "visited" -> Visited
        | "hover" -> Hover
        | "active" -> Active
        | "focus" | "focus-visible" | "focus-within" -> Focus
        | "root" -> Root
        | "empty" -> Empty
        | "checked" -> Checked
        | "disabled" -> Disabled
        | "enabled" -> Enabled
        | "first-of-type" -> First_child
        | "last-of-type" -> Last_child
        | _ -> raise Invalid
      in
      (Pseudo pc, rest)
  | Token Colon :: Func (f, args) :: rest -> (
      match String.lowercase_ascii f with
      | "not" -> (Pseudo (Not (parse_list args)), rest)
      | "is" | "matches" | "-webkit-any" -> (Pseudo (Is (parse_list args, true)), rest)
      | "where" -> (Pseudo (Is (parse_list args, false)), rest)
      | "nth-child" | "nth-of-type" -> (
          let a, b = nth (to_string args) in
          (Pseudo (Nth_child (a, b)), rest))
      | _ -> raise Invalid)
  | _ -> raise Invalid

and attribute (cs : component list) : simple =
  let rec skip = function Token Whitespace :: r -> skip r | l -> l in
  match cs with
  | Token (Ident name) :: rest -> (
      let name = String.lowercase_ascii name in
      let value_and_flag (v : component list) : string * bool =
        match skip v with
        | Token (Ident s) :: r | Token (String s) :: r -> (
            match skip r with
            | [] -> (s, false)
            | Token (Ident ("i" | "I")) :: r when skip r = [] -> (s, true)
            | _ -> raise Invalid)
        | _ -> raise Invalid
      in
      let op o v = let s, i = value_and_flag v in Attr (name, o, s, i) in
      match skip rest with
      | [] -> Attr (name, Exists, "", false)
      | Token (Delim '=') :: v -> op Equals v
      | Token (Delim '~') :: Token (Delim '=') :: v -> op Includes v
      | Token (Delim '|') :: Token (Delim '=') :: v -> op Dash v
      | Token (Delim '^') :: Token (Delim '=') :: v -> op Prefix v
      | Token (Delim '$') :: Token (Delim '=') :: v -> op Suffix v
      | Token (Delim '*') :: Token (Delim '=') :: v -> op Substring v
      | _ -> raise Invalid)
  | _ -> raise Invalid

let parse (cs : component list) : complex list option = match parse_list cs with l -> Some l | exception Invalid -> None
let parse_string (s : string) : complex list option = parse (components_of s)

(*****************************************************************************)
(* Specificity *)
(*****************************************************************************)

let add (a, b, c) (x, y, z) = (a + x, b + y, c + z)

let rec specificity (sel : complex) : int * int * int =
  List.fold_left (fun acc (compound, _) -> List.fold_left (fun acc s -> add acc (of_simple s)) acc compound) (0, 0, 0) sel

and of_simple (s : simple) : int * int * int =
  match s with
  | Id _ -> (1, 0, 0)
  | Class _ | Attr _ -> (0, 1, 0)
  | Pseudo (Not l) | Pseudo (Is (l, true)) -> List.fold_left (fun m x -> max m (specificity x)) (0, 0, 0) l
  | Pseudo (Is (_, false)) -> (0, 0, 0)
  | Pseudo _ -> (0, 1, 0)
  | Type _ | Pseudo_element _ -> (0, 0, 1)
  | Universal -> (0, 0, 0)

let pseudo_element (sel : complex) : string option =
  match List.rev sel with
  | (compound, _) :: _ -> List.find_map (function Pseudo_element p -> Some p | _ -> None) compound
  | [] -> None

(*****************************************************************************)
(* Matching *)
(*****************************************************************************)

let words (s : string) : string list = List.filter (( <> ) "") (String.split_on_char ' ' (String.map (fun c -> if c = '\t' || c = '\n' then ' ' else c) s))

let element_children (e : Dom.element) : Dom.element list =
  List.filter_map (fun (n : Dom.node) -> match n with Element c -> Some c | Text _ -> None) e.children

(* the element's siblings before it and after it, in its parent *)
let siblings (ancestors : Dom.element list) (e : Dom.element) : Dom.element list * Dom.element list =
  match ancestors with
  | [] -> ([], [])
  | parent :: _ ->
      let rec split before = function [] -> (List.rev before, []) | x :: after -> if x == e then (List.rev before, after) else split (x :: before) after in
      split [] (element_children parent)

let attr_matches (e : Dom.element) (name : string) (op : attr_op) (v : string) (ci : bool) : bool =
  (* a Netscape attribute is an attribute to CSS too *)
  match Dom.attribute ~extensions:true name e with
  | None -> false
  | Some a -> (
      let a, v = if ci then (String.lowercase_ascii a, String.lowercase_ascii v) else (a, v) in
      match op with
      | Exists -> true
      | Equals -> a = v
      | Includes -> List.mem v (words a)
      | Dash -> a = v || String.starts_with ~prefix:(v ^ "-") a
      | Prefix -> v <> "" && String.starts_with ~prefix:v a
      | Suffix -> v <> "" && String.ends_with ~suffix:v a
      | Substring ->
          v <> ""
          &&
          let n = String.length a and m = String.length v in
          let rec at i = i + m <= n && (String.sub a i m = v || at (i + 1)) in
          at 0)

let rec matches_simple ~visited (ancestors : Dom.element list) (e : Dom.element) (s : simple) : bool =
  match s with
  | Type n -> e.name = n
  | Universal | Pseudo_element _ -> true
  | Id i -> Dom.attribute "id" e = Some i
  | Class c -> ( match Dom.attribute "class" e with Some cs -> List.mem c (words cs) | None -> false)
  | Attr (name, op, v, ci) -> attr_matches e name op v ci
  | Pseudo p -> (
      let before, after = siblings ancestors e in
      match p with
      | First_child -> ancestors <> [] && before = []
      | Last_child -> ancestors <> [] && after = []
      | Only_child -> ancestors <> [] && before = [] && after = []
      | Nth_child (a, b) ->
          let i = List.length before + 1 in
          if a = 0 then i = b else (i - b) mod a = 0 && (i - b) / a >= 0
      | Not l -> not (List.exists (fun sel -> matches_complex ~visited sel ancestors e) l)
      | Is (l, _) -> List.exists (fun sel -> matches_complex ~visited sel ancestors e) l
      | Link -> e.name = "a" && Dom.attribute "href" e <> None && not (match Dom.attribute "href" e with Some h -> visited h | None -> false)
      | Visited -> e.name = "a" && (match Dom.attribute "href" e with Some h -> visited h | None -> false)
      | Hover | Active | Focus -> false
      | Root -> ancestors = []
      | Empty -> List.for_all (fun (n : Dom.node) -> match n with Text "" -> true | _ -> false) e.children
      | Checked -> Dom.attribute "checked" e <> None || Dom.attribute "selected" e <> None
      | Disabled -> Dom.attribute "disabled" e <> None
      (* a form's control that is not disabled *)
      | Enabled -> List.mem e.name [ "input"; "button"; "select"; "textarea"; "option"; "fieldset" ] && Dom.attribute "disabled" e = None)

and matches_compound ~visited ancestors e (compound : simple list) : bool = List.for_all (matches_simple ~visited ancestors e) compound

(* right to left: [rest] the compounds before, the nearest first, each
 * with the combinator that joins it to what follows *)
and matches_complex ~visited (sel : complex) (ancestors : Dom.element list) (e : Dom.element) : bool =
  match List.rev sel with
  | [] -> false
  | (last, _) :: before -> matches_compound ~visited ancestors e last && left ~visited before ancestors e

and left ~visited (before : (simple list * combinator option) list) (ancestors : Dom.element list) (e : Dom.element) : bool =
  match before with
  | [] -> true
  | (compound, comb) :: rest -> (
      match comb with
      | Some Child | None -> (
          match ancestors with p :: up -> matches_compound ~visited up p compound && left ~visited rest up p | [] -> false)
      | Some Descendant ->
          let rec up = function
            | [] -> false
            | p :: above -> (matches_compound ~visited above p compound && left ~visited rest above p) || up above
          in
          up ancestors
      | Some Next_sibling -> (
          match List.rev (fst (siblings ancestors e)) with
          | s :: _ -> matches_compound ~visited ancestors s compound && left ~visited rest ancestors s
          | [] -> false)
      | Some Subsequent_sibling ->
          List.exists (fun s -> matches_compound ~visited ancestors s compound && left ~visited rest ancestors s) (fst (siblings ancestors e)))

let matches ?(visited = fun _ -> false) (sel : complex) (ancestors : Dom.element list) (e : Dom.element) : bool =
  matches_complex ~visited sel ancestors e

(*****************************************************************************)
(* Back to text *)
(*****************************************************************************)

let rec to_string (sel : complex) : string =
  String.concat ""
    (List.map
       (fun (compound, comb) ->
         String.concat "" (List.map simple_to_string compound)
         ^ match comb with Some Descendant -> " " | Some Child -> " > " | Some Next_sibling -> " + " | Some Subsequent_sibling -> " ~ " | None -> "")
       sel)

and simple_to_string (s : simple) : string =
  match s with
  | Type n -> n
  | Universal -> "*"
  | Id i -> "#" ^ i
  | Class c -> "." ^ c
  | Attr (n, op, v, ci) ->
      let o = match op with Exists -> "" | Equals -> "=" | Includes -> "~=" | Dash -> "|=" | Prefix -> "^=" | Suffix -> "$=" | Substring -> "*=" in
      "[" ^ n ^ (if op = Exists then "" else o ^ "\"" ^ v ^ "\"") ^ (if ci then " i" else "") ^ "]"
  | Pseudo_element p -> "::" ^ p
  | Pseudo p -> (
      ":"
      ^
      match p with
      | First_child -> "first-child"
      | Last_child -> "last-child"
      | Only_child -> "only-child"
      | Nth_child (a, b) -> Printf.sprintf "nth-child(%dn+%d)" a b
      | Not l -> "not(" ^ String.concat ", " (List.map to_string l) ^ ")"
      | Is (l, counts) -> (if counts then "is(" else "where(") ^ String.concat ", " (List.map to_string l) ^ ")"
      | Link -> "link"
      | Visited -> "visited"
      | Hover -> "hover"
      | Active -> "active"
      | Focus -> "focus"
      | Root -> "root"
      | Empty -> "empty"
      | Checked -> "checked"
      | Disabled -> "disabled"
      | Enabled -> "enabled")
