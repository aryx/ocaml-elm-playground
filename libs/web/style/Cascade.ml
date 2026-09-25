(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cascade.mli *)
open Css_syntax

type origin = User_agent | Author
type sheet = { origin : origin; rules : Css_syntax.rule list }
type media = { width : float; height : float }

(*****************************************************************************)
(* Media queries *)
(*****************************************************************************)

(* one feature, "(max-width: 800px)" *)
let feature (m : media) (inside : component list) : bool =
  let ctx : Css_values.context = { em = 16.; rem = 16.; viewport_width = m.width; viewport_height = m.height } in
  let len cs = match Css_values.parts cs with [ c ] -> Option.map (fun (l : Css_values.length) -> l.px) (Css_values.length ctx c) | _ -> None in
  match split_on Colon (trim inside) with
  | [ name; value ] -> (
      let name = String.lowercase_ascii (to_string (trim name)) and v = String.lowercase_ascii (to_string (trim value)) in
      match name with
      | "min-width" -> ( match len value with Some l -> m.width >= l | None -> false)
      | "max-width" -> ( match len value with Some l -> m.width <= l | None -> false)
      | "min-height" -> ( match len value with Some l -> m.height >= l | None -> false)
      | "max-height" -> ( match len value with Some l -> m.height <= l | None -> false)
      | "orientation" -> v = if m.width >= m.height then "landscape" else "portrait"
      | "prefers-color-scheme" -> v = "light"
      | "prefers-reduced-motion" -> v = "no-preference"
      | "hover" | "any-hover" -> v = "hover"
      | "pointer" | "any-pointer" -> v = "fine"
      | _ -> false)
  | [ name ] -> ( match String.lowercase_ascii (to_string (trim name)) with "color" | "hover" | "pointer" -> true | _ -> false)
  | _ -> false

(* "not screen and (max-width: 800px)": a type and features joined by
 * "and", "not" turning the whole around *)
let query (m : media) (cs : component list) : bool =
  let words = Css_values.parts cs in
  let negate, words =
    match words with
    | Token (Ident n) :: rest when String.lowercase_ascii n = "not" -> (true, rest)
    | Token (Ident n) :: rest when String.lowercase_ascii n = "only" -> (false, rest)
    | _ -> (false, words)
  in
  let holds =
    List.for_all
      (fun (c : component) ->
        match c with
        | Token (Ident n) -> ( match String.lowercase_ascii n with "and" | "screen" | "all" -> true | _ -> false)
        | Block ('(', inside) -> feature m inside
        | _ -> false)
      words
  in
  if negate then not holds else holds

let media_matches (m : media) (cs : component list) : bool =
  let cs = trim cs in
  cs = [] || List.exists (fun q -> query m q) (split_on Comma cs)

(*****************************************************************************)
(* The rules, flattened *)
(*****************************************************************************)

let rec flatten (m : media) (origin : origin) (rules : Css_syntax.rule list) : (origin * Selectors.complex * declaration list) list =
  List.concat_map
    (fun (r : Css_syntax.rule) ->
      match r with
      | Style_rule { prelude; declarations } -> (
          match Selectors.parse prelude with
          | Some sels -> List.map (fun s -> (origin, s, declarations)) sels
          | None -> [])
      | At_rule { name = "media"; prelude; block = Some b } -> if media_matches m prelude then flatten m origin (rules_of_block b) else []
      (* a browser that reads CSS3's syntax supports what it names:
       * close enough, and what a page puts there is usually its
       * modern layout, which it would rather have *)
      | At_rule { name = "supports"; block = Some b; _ } -> flatten m origin (rules_of_block b)
      | At_rule _ -> [])
    rules

let rules (m : media) (sheets : sheet list) = List.concat_map (fun s -> flatten m s.origin s.rules) sheets

(*****************************************************************************)
(* The index *)
(*****************************************************************************)

type entry = {
  layer_normal : int;
  layer_important : int;
  specificity : int * int * int;
  order : int;
  selector : Selectors.complex;
  declarations : declaration list;
  needs : string list; (* ids, classes and names some ancestor must have *)
}

(* the rule's key: its last compound's id, else its first class, else
 * its name, else "any" *)
let key (sel : Selectors.complex) : string =
  match List.rev sel with
  | (compound, _) :: _ -> (
      match List.find_map (function Selectors.Id i -> Some ("#" ^ i) | _ -> None) compound with
      | Some k -> k
      | None -> (
          match List.find_map (function Selectors.Class c -> Some ("." ^ c) | _ -> None) compound with
          | Some k -> k
          | None -> ( match List.find_map (function Selectors.Type n -> Some n | _ -> None) compound with Some k -> k | None -> "*")))
  | [] -> "*"

let words (s : string) : string list = List.filter (( <> ) "") (String.split_on_char ' ' s)

(* an element's keys: its name, #id, .classes *)
let keys_of (e : Dom.element) : string list =
  e.name
  :: (match Dom.attribute "id" e with Some i -> [ "#" ^ i ] | None -> [])
  @ List.map (fun c -> "." ^ c) (match Dom.attribute "class" e with Some c -> words c | None -> [])

(* the ancestor filter (WebKit's "selector filter", a Bloom filter
   there): what a selector asks of the element's ancestors -- the ids,
   classes and names of the compounds reached from its last by
   descendant and child combinators only (a sibling's are not an
   ancestor's) -- checked against the keys of the ancestors, kept as
   the tree is walked, before any matching: most of a big sheet's long
   selectors fail there at once *)
let needs (sel : Selectors.complex) : string list =
  let rec go (before : (Selectors.simple list * Selectors.combinator option) list) : string list =
    match before with
    | (compound, Some (Selectors.Descendant | Selectors.Child)) :: rest ->
        List.filter_map (function Selectors.Id i -> Some ("#" ^ i) | Selectors.Class c -> Some ("." ^ c) | Selectors.Type n -> Some n | _ -> None) compound
        @ go rest
    | _ -> []
  in
  match List.rev sel with _ :: before -> go before | [] -> []

(* a table from elements, by identity (two equal paragraphs are two):
 * filed under their hash, found among the bucket's by (==) -- what a
 * Hashtbl.Make with (==) as its equality would do, without a functor *)
let find_element (table : (int, Dom.element * 'a) Hashtbl.t) (e : Dom.element) : 'a option =
  List.assq_opt e (Hashtbl.find_all table (Hashtbl.hash e))

let cascade ?(visited = fun _ -> false) (m : media) (sheets : sheet list) (root : Dom.element) : Dom.element -> (string * component list) list =
  let index : (string, entry) Hashtbl.t = Hashtbl.create 1024 in
  List.iteri
    (fun order (origin, sel, declarations) ->
      if Selectors.pseudo_element sel = None then
        (* the page's normal declarations above the browser's, its
         * !important ones above both, the browser's !important last *)
        let layer_normal, layer_important = match origin with User_agent -> (0, 3) | Author -> (1, 2) in
        Hashtbl.add index (key sel)
          { layer_normal; layer_important; specificity = Selectors.specificity sel; order; selector = sel; declarations; needs = needs sel })
    (rules m sheets);
  let table : (int, Dom.element * (string * component list) list) Hashtbl.t = Hashtbl.create 1024 in
  (* the keys of the ancestors of the element being styled, counted *)
  let above : (string, int) Hashtbl.t = Hashtbl.create 256 in
  let rec go (ancestors : Dom.element list) (e : Dom.element) =
    let keys = keys_of e in
    let candidates = List.concat_map (fun k -> Hashtbl.find_all index k) (List.sort_uniq compare ("*" :: keys)) in
    (* before the ancestor filter (0.95 s on a Wikipedia article, 0.54
     * after; notes_opti_ocaml.md section 10):
     *   List.filter (fun en -> Selectors.matches ~visited en.selector ancestors e) candidates *)
    let matching =
      List.filter (fun en -> List.for_all (Hashtbl.mem above) en.needs && Selectors.matches ~visited en.selector ancestors e) candidates
    in
    (* each declaration with its sort key; style= an author's rule above
     * any selector *)
    let keyed =
      List.concat_map
        (fun en ->
          List.map
            (fun (d : declaration) -> (((if d.important then en.layer_important else en.layer_normal), en.specificity, en.order), d))
            en.declarations)
        matching
      @
      match Dom.attribute "style" e with
      | Some s -> List.map (fun (d : declaration) -> (((if d.important then 2 else 1), (max_int, 0, 0), max_int), d)) (parse_declarations s)
      | None -> []
    in
    let sorted = List.stable_sort (fun (a, _) (b, _) -> compare a b) keyed in
    let winning = List.fold_left (fun acc (_, (d : declaration)) -> (d.name, d.value) :: List.remove_assoc d.name acc) [] sorted in
    Hashtbl.add table (Hashtbl.hash e) (e, List.rev winning);
    List.iter (fun k -> Hashtbl.replace above k (1 + Option.value (Hashtbl.find_opt above k) ~default:0)) keys;
    List.iter (fun (n : Dom.node) -> match n with Element c -> go (e :: ancestors) c | Text _ -> ()) e.children;
    List.iter (fun k -> match Hashtbl.find_opt above k with Some 1 -> Hashtbl.remove above k | Some n -> Hashtbl.replace above k (n - 1) | None -> ()) keys
  in
  go [] root;
  fun e -> match find_element table e with Some ds -> ds | None -> []
