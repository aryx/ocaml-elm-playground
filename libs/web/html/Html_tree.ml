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
  mutable attributes : (string * string) list;
  mutable children : child list; (* the last first *)
}

and child = E of open_element | T of string

let make (name : string) (attributes : (string * string) list) : open_element = { name; attributes; children = [] }

let rec freeze (e : open_element) : Dom.element =
  {
    name = e.name;
    attributes = e.attributes;
    children = List.rev_map (fun c -> match c with E e -> Dom.Element (freeze e) | T s -> Dom.Text s) e.children;
  }

(* attributes given again (a second <body bgcolor=...>): the new ones
 * added, the old ones kept *)
let add_attributes (e : open_element) (attributes : (string * string) list) : unit =
  List.iter (fun (n, v) -> if not (List.mem_assoc n e.attributes) then e.attributes <- e.attributes @ [ (n, v) ]) attributes

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

let insert (t : t) (name : string) (attributes : (string * string) list) : unit =
  let e = make name attributes in
  (top t).children <- E e :: (top t).children;
  if not (Dtd.is_void name) then t.stack <- e :: t.stack

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

let start_tag (t : t) (name : string) (attributes : (string * string) list) : unit =
  match name with
  | "html" -> add_attributes t.html attributes
  | "head" -> ()
  | "body" ->
      start_body t;
      add_attributes t.body attributes
  | _ when Dtd.is_head_element name && not t.body_started -> insert t name attributes
  | _ ->
      start_body t;
      (* 1. what x closes, nearest first, as long as some is found *)
      while pop_to t ~found:(Dtd.closes name) ~stop:(Dtd.stops name) do () done;
      (* 2. and 3. *)
      insert t name attributes;
      t.skip_newline <- List.mem name [ "pre"; "listing"; "textarea" ]

let end_tag (t : t) (name : string) : unit =
  match name with
  | "html" | "body" | "head" -> ()
  | "p" ->
      if not (pop_to t ~found:(( = ) "p") ~stop:(Dtd.stops "p")) then (
        (* </p> with no <p>: an empty one, as the spec says *)
        start_body t;
        insert t "p" [];
        ignore (pop_to t ~found:(( = ) "p") ~stop:(fun _ -> false)))
  | _ -> ignore (pop_to t ~found:(( = ) name) ~stop:(fun y -> List.mem y [ "html"; "table"; "td"; "th"; "caption" ]))

let parse (tokens : Html_lexer.token list) : Dom.element =
  let html = make "html" [] and head = make "head" [] and body = make "body" [] in
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
      | Start_tag { name; attributes; _ } -> start_tag t name attributes
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
