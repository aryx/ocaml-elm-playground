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

type simple = { name : string option; id : string option; classes : string list }
type selector = simple list
type rule = { selector : selector; declarations : (string * string) list }
type sheet = rule list

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let without_comments (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else if i + 1 < n && s.[i] = '/' && s.[i + 1] = '*' then
      let rec close j = if j + 1 >= n then n else if s.[j] = '*' && s.[j + 1] = '/' then j + 2 else close (j + 1) in
      go (close (i + 2))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let words (s : string) : string list =
  String.split_on_char ' ' (String.map (fun c -> if c = '\n' || c = '\t' || c = '\r' then ' ' else c) s)
  |> List.filter (( <> ) "")

(* "p.note#top": its name, then .class and #id, in any order *)
let simple_of (s : string) : simple option =
  let n = String.length s in
  let rec pieces i acc =
    if i >= n then Some (List.rev acc)
    else
      let j = ref (i + 1) in
      while !j < n && s.[!j] <> '.' && s.[!j] <> '#' do incr j done;
      pieces !j (String.sub s i (!j - i) :: acc)
  in
  let first = ref 0 in
  while !first < n && s.[!first] <> '.' && s.[!first] <> '#' do incr first done;
  let name = String.lowercase_ascii (String.sub s 0 !first) in
  match pieces !first [] with
  | None -> None
  | Some parts ->
      let classes = List.filter_map (fun p -> if p.[0] = '.' then Some (String.sub p 1 (String.length p - 1)) else None) parts in
      let id = List.find_map (fun p -> if p.[0] = '#' then Some (String.sub p 1 (String.length p - 1)) else None) parts in
      let ok = String.for_all (fun c -> Char.code c > 127 || c = '-' || c = '_' || c = '*' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) name in
      if not ok then None else Some { name = (if name = "" || name = "*" then None else Some name); id; classes }

let declarations (s : string) : (string * string) list =
  String.split_on_char ';' s
  |> List.filter_map (fun d ->
         match String.index_opt d ':' with
         | Some i ->
             let property = String.lowercase_ascii (String.trim (String.sub d 0 i)) in
             let value = String.trim (String.sub d (i + 1) (String.length d - i - 1)) in
             if property = "" || value = "" then None else Some (property, value)
         | None -> None)

let parse (text : string) : sheet =
  let s = without_comments text in
  let n = String.length s in
  (* a selector's text up to '{', its declarations up to '}' *)
  let rec rules i acc =
    match String.index_from_opt s i '{' with
    | None -> List.rev acc
    | Some open_ -> (
        let close = match String.index_from_opt s open_ '}' with Some c -> c | None -> n in
        let head = String.trim (String.sub s i (open_ - i)) in
        let body = String.sub s (open_ + 1) (max 0 (close - open_ - 1)) in
        let next = min n (close + 1) in
        (* an @-rule (@media...): skipped, its block with it *)
        if String.length head > 0 && head.[0] = '@' then rules next acc
        else
          let selectors =
            String.split_on_char ',' head
            |> List.filter_map (fun sel ->
                   let simples = List.map simple_of (words sel) in
                   if simples = [] || List.mem None simples then None else Some (List.map Option.get simples))
          in
          let ds = declarations body in
          match selectors with
          | [] -> rules next acc
          | _ -> rules next (List.rev_append (List.map (fun selector -> { selector; declarations = ds }) selectors) acc))
  in
  rules 0 []

(*****************************************************************************)
(* Matching *)
(*****************************************************************************)

let specificity (sel : selector) : int * int * int =
  List.fold_left
    (fun (a, b, c) s ->
      ( (a + match s.id with Some _ -> 1 | None -> 0),
        b + List.length s.classes,
        c + match s.name with Some _ -> 1 | None -> 0 ))
    (0, 0, 0) sel

let matches_simple (s : simple) (e : Dom.element) : bool =
  let classes = match Dom.attribute "class" e with Some c -> words c | None -> [] in
  (match s.name with Some n -> n = e.name | None -> true)
  && (match s.id with Some id -> Dom.attribute "id" e = Some id | None -> true)
  && List.for_all (fun c -> List.mem c classes) s.classes

(* the element matches the last simple selector; each one before, some
 * ancestor further up (the nearest that matches: for descendants
 * only, the greedy search finds a match if there is one) *)
let matches (sel : selector) (ancestors : Dom.element list) (e : Dom.element) : bool =
  match List.rev sel with
  | [] -> false
  | subject :: outer ->
      matches_simple subject e
      &&
      let rec up outer ancestors =
        match (outer, ancestors) with
        | [], _ -> true
        | _, [] -> false
        | s :: rest, a :: above -> if matches_simple s a then up rest above else up outer above
      in
      up outer ancestors

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
    let matching = List.filter (fun (_, _, r) -> matches r.selector ancestors e) rules in
    (* the weakest first, so that a later one overrides *)
    let sorted = List.stable_sort (fun (s1, o1, _) (s2, o2, _) -> compare (s1, o1) (s2, o2)) matching in
    let inline = match Dom.attribute "style" e with Some s -> declarations s | None -> [] in
    let all = List.concat_map (fun (_, _, r) -> r.declarations) sorted @ inline in
    let winning = List.fold_left (fun acc (p, v) -> (p, v) :: List.remove_assoc p acc) [] all in
    if winning <> [] then Elements.replace table e (List.rev winning);
    List.iter (fun (n : Dom.node) -> match n with Element c -> go (e :: ancestors) c | Text _ -> ()) e.children
  in
  go [] root;
  fun e -> match Elements.find_opt table e with Some ds -> ds | None -> []
