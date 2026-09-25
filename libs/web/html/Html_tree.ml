(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Html_tree.mli *)

(*****************************************************************************)
(* Elements being built *)
(*****************************************************************************)

(* an element while its children still come: they are kept last first,
 * and the tree is frozen into Dom values at the end *)
type open_element = {
  name : string;
  origin : Dtd.origin;
  mutable attributes : (string * string) list;
  mutable extensions : (string * string) list; (* Netscape's, apart *)
  mutable children : child list; (* the last first *)
}

and child = E of open_element | T of string

(* a start tag's name and attributes, with their origins (Html_lexer) *)
type tag = { tag_name : string; origin : Dtd.origin; attributes : (string * string) list; extensions : (string * string) list }

let core (name : string) : tag = { tag_name = name; origin = Core; attributes = []; extensions = [] }

let make (tag : tag) : open_element =
  { name = tag.tag_name; origin = tag.origin; attributes = tag.attributes; extensions = tag.extensions; children = [] }

let rec freeze (e : open_element) : Dom.element =
  {
    name = e.name;
    attributes = e.attributes;
    extensions = e.extensions;
    origin = e.origin;
    children = List.rev_map (fun c -> match c with E e -> Dom.Element (freeze e) | T s -> Dom.Text s) e.children;
  }

(* attributes given again (a second <body bgcolor=...>): the new ones
 * added, the old ones kept *)
let add_attributes (e : open_element) (tag : tag) : unit =
  let add old (n, v) = if List.mem_assoc n old then old else old @ [ (n, v) ] in
  e.attributes <- List.fold_left add e.attributes tag.attributes;
  e.extensions <- List.fold_left add e.extensions tag.extensions

(*****************************************************************************)
(* The stack of open elements *)
(*****************************************************************************)

(* its top first; [body_started] tells the head from the body *)
type t = {
  html : open_element;
  head : open_element;
  body : open_element;
  mutable stack : open_element list;
  mutable body_started : bool;
  (* a newline right after <pre> is not content (so that the first line
   * can start on a line of its own in the source) *)
  mutable skip_newline : bool;
}

let top (t : t) : open_element = List.hd t.stack

let append_text (t : t) (s : string) : unit =
  let e = top t in
  match e.children with T before :: rest -> e.children <- T (before ^ s) :: rest | _ -> e.children <- T s :: e.children

let insert (t : t) (tag : tag) : unit =
  let e = make tag in
  (top t).children <- E e :: (top t).children;
  if not (Dtd.is_void tag.tag_name) then t.stack <- e :: t.stack

(* pop down to the first element satisfying [found], looking no further
 * than one satisfying [stop]; false if none was found *)
let pop_to (t : t) ~(found : string -> bool) ~(stop : string -> bool) : bool =
  let rec search (stack : open_element list) =
    match stack with
    | [] -> None
    | e :: rest -> if found e.name then Some rest else if stop e.name then None else search rest
  in
  match search t.stack with
  | Some rest ->
      t.stack <- rest;
      true
  | None -> false

let start_body (t : t) : unit =
  if not t.body_started then (
    t.body_started <- true;
    t.stack <- [ t.body; t.html ])

(*****************************************************************************)
(* The tokens *)
(*****************************************************************************)

let is_blank (s : string) : bool = String.for_all (fun c -> c = ' ' || c = '\n' || c = '\t') s

let start_tag ?(self_closing = false) (t : t) (tag : tag) : unit =
  let name = tag.tag_name in
  match name with
  (* inside <svg>, "foreign content": XML's rules, not HTML's -- nothing
   * closes what is open, and <path/> closes itself *)
  | _ when t.body_started && List.exists (fun (e : open_element) -> e.name = "svg") t.stack ->
      insert t tag;
      if self_closing then t.stack <- List.tl t.stack
  | "svg" when self_closing ->
      start_body t;
      insert t tag;
      t.stack <- List.tl t.stack
  | "html" -> add_attributes t.html tag
  | "head" -> ()
  | "body" ->
      start_body t;
      add_attributes t.body tag
  | _ when Dtd.is_head_element name && not t.body_started -> insert t tag
  | _ ->
      start_body t;
      (* 1. what x closes, nearest first, as long as some is found *)
      while pop_to t ~found:(Dtd.closes name) ~stop:(Dtd.stops name) do () done;
      (* 2. and 3. *)
      insert t tag;
      t.skip_newline <- List.mem name [ "pre"; "listing"; "textarea" ]

let end_tag (t : t) (name : string) : unit =
  match name with
  | "html" | "body" | "head" -> ()
  | "p" ->
      if not (pop_to t ~found:(( = ) "p") ~stop:(Dtd.stops "p")) then (
        (* </p> with no <p>: an empty one, as the spec says *)
        start_body t;
        insert t (core "p");
        ignore (pop_to t ~found:(( = ) "p") ~stop:(fun _ -> false)))
  | _ -> ignore (pop_to t ~found:(( = ) name) ~stop:(fun y -> List.mem y [ "html"; "table"; "td"; "th"; "caption" ]))

let parse (tokens : Html_lexer.token list) : Dom.element =
  let html = make (core "html") and head = make (core "head") and body = make (core "body") in
  html.children <- [ E body; E head ];
  let t = { html; head; body; stack = [ head; html ]; body_started = false; skip_newline = false } in
  List.iter
    (fun (token : Html_lexer.token) ->
      let token : Html_lexer.token =
        match token with
        | Text s when t.skip_newline && String.length s > 0 && s.[0] = '\n' -> Text (String.sub s 1 (String.length s - 1))
        | _ -> token
      in
      t.skip_newline <- false;
      match token with
      | Text "" -> ()
      | Doctype _ | Comment _ -> ()
      | Start_tag { name; attributes; extensions; origin; self_closing } ->
          start_tag ~self_closing t { tag_name = name; origin; attributes; extensions }
      | End_tag name -> end_tag t name
      | Text s ->
          (* in the head itself, only spaces are allowed: other text
           * starts the body; inside a <title>, it is the title *)
          if t.body_started || top t != t.head then append_text t s
          else if not (is_blank s) then (
            start_body t;
            append_text t s))
    tokens;
  freeze html

let of_string (s : string) : Dom.element = parse (Html_lexer.tokenize s)
