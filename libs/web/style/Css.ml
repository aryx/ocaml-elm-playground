(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Css.mli *)

type rule = { selector : Selectors.complex; declarations : (string * string) list; important : string list }
type sheet = rule list

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let text_of (ds : Css_syntax.declaration list) : (string * string) list =
  List.map (fun (d : Css_syntax.declaration) -> (d.name, Css_syntax.to_string d.value)) ds

(* a style rule's selectors, a rule each; an at-rule skipped (@media is
 * TinyChrome's cascade's); a selector list not understood, its rule
 * dropped whole, as CSS says *)
let parse (text : string) : sheet =
  Css_syntax.parse_stylesheet text
  |> List.concat_map (fun (r : Css_syntax.rule) ->
         match r with
         | Style_rule { prelude; declarations } -> (
             match Selectors.parse prelude with
             | Some selectors ->
                 let important = List.filter_map (fun (d : Css_syntax.declaration) -> if d.important then Some d.name else None) declarations in
                 List.map (fun selector -> { selector; declarations = text_of declarations; important }) selectors
             | None -> [])
         | At_rule _ -> [])

let declarations (text : string) : (string * string) list = text_of (Css_syntax.parse_declarations text)
let specificity = Selectors.specificity
let matches (sel : Selectors.complex) (ancestors : Dom.element list) (e : Dom.element) : bool = Selectors.matches sel ancestors e

(*****************************************************************************)
(* The cascade *)
(*****************************************************************************)

let page_sheet (root : Dom.element) : string = String.concat "\n" (List.map Dom.text_content (Dom.find_all "style" root))

(* a table from elements, by identity (two equal paragraphs are two) *)
module Elements = Hashtbl.Make (struct
  type t = Dom.element

  let equal = ( == )
  let hash = Hashtbl.hash
end)

let cascade (sheet : sheet) (root : Dom.element) : Dom.element -> (string * string) list =
  let table = Elements.create 64 in
  let rules = List.mapi (fun order r -> (specificity r.selector, order, r)) sheet in
  let rec go ancestors (e : Dom.element) =
    let matching = List.filter (fun (_, _, r) -> Selectors.pseudo_element r.selector = None && matches r.selector ancestors e) rules in
    (* the weakest first, so that a later one overrides *)
    let sorted = List.stable_sort (fun (s1, o1, _) (s2, o2, _) -> compare (s1, o1) (s2, o2)) matching in
    let inline = match Dom.attribute "style" e with Some s -> Css_syntax.parse_declarations s | None -> [] in
    let normal (r : rule) = List.filter (fun (p, _) -> not (List.mem p r.important)) r.declarations in
    let strong (r : rule) = List.filter (fun (p, _) -> List.mem p r.important) r.declarations in
    let all =
      List.concat_map (fun (_, _, r) -> normal r) sorted
      @ text_of (List.filter (fun (d : Css_syntax.declaration) -> not d.important) inline)
      (* then the !important ones, in the same order, over everything *)
      @ List.concat_map (fun (_, _, r) -> strong r) sorted
      @ text_of (List.filter (fun (d : Css_syntax.declaration) -> d.important) inline)
    in
    let winning = List.fold_left (fun acc (p, v) -> (p, v) :: List.remove_assoc p acc) [] all in
    if winning <> [] then Elements.replace table e (List.rev winning);
    List.iter (fun (n : Dom.node) -> match n with Element c -> go (e :: ancestors) c | Text _ -> ()) e.children
  in
  go [] root;
  fun e -> match Elements.find_opt table e with Some ds -> ds | None -> []
