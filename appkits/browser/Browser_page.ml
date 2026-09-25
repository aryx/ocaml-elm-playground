(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_page.mli *)

type t = {
  url : string;
  status : int;
  charset : Charset.t;
  bytes : int;
  lines : string list;
  tokens : Html_lexer.token list;
  tree : Dom.element;
  line_mode : Line_mode.t;
  title : string;
  layout : Html_layout.box;
  drawn : Browser_draw.drawn;
  forms : Forms.form list;
  values : (Dom.element * Forms.value) list;
}

type settings = {
  width : float;
  breaker : Html_layout.breaker;
  visited : string -> bool;
  picture : string -> Browser_picture.t option;
}

let pretty : Html_layout.breaker =
 fun ~measure units ->
  let space = Array.fold_left (fun s (u : Html_layout.unit_) -> if s = 0. then u.space else s) 0. units in
  let words = Array.map (fun (u : Html_layout.unit_) -> { Linebreak.text = ""; width = u.width }) units in
  Linebreak.optimal { measure; space; stretch = space; shrink = 0. } words
  |> List.map (fun (l : Linebreak.line) -> (l.first, l.last))

(*****************************************************************************)
(* Pages the browser writes *)
(*****************************************************************************)

let escape_html = Browser_text.escape_html

let media_type (content_type : string option) : string =
  match content_type with
  | None -> ""
  | Some ct -> String.lowercase_ascii (String.trim (List.hd (String.split_on_char ';' ct)))

(* what is not HTML made a page: text as Mosaic showed it, in <pre>;
 * anything else said what it is *)
let as_html (url : string) (content_type : string option) (text : string) (bytes : int) : string =
  let name = Filename.basename (fst (Browser_url.split_fragment url)) in
  match media_type content_type with
  | "" | "text/html" -> text
  | t when Browser_url.starts_with "text/" t -> Printf.sprintf "<title>%s</title><pre>\n%s</pre>" name (escape_html text)
  | t ->
      Printf.sprintf
        "<title>%s</title><h1>%s</h1><p>A document of type <code>%s</code>, %d bytes: not HTML nor text, not shown here."
        name name t bytes

let error_html (url : string) (why : string) : string =
  Printf.sprintf
    "<title>Failed</title><h1>Could not load the page</h1><p><code>%s</code><p>%s<p>Press <code>r</code> to try again, <code>b</code> to go back."
    (escape_html url) (escape_html why)

let echo_html (meth : string) (encoded : string) : string =
  let fields = Urlencoded.decode encoded in
  Printf.sprintf
    "<title>What the form sent</title><h1>What the form sent</h1><p>A %s, its fields encoded (%s):<pre>\n%s</pre><p>Decoded:<dl>%s</dl><p>Back to the form: <code>b</code>."
    meth
    (if meth = "GET" then "the URL's query" else "the request's body")
    (escape_html encoded)
    (String.concat "" (List.map (fun (n, v) -> Printf.sprintf "<dt><b>%s</b><dd>%s" (escape_html n) (escape_html v)) fields))

(*****************************************************************************)
(* The pipeline *)
(*****************************************************************************)

(* a tab to the next multiple of 8 columns, a CR dropped: the source as
 * a terminal would show it *)
let expand_tabs (line : string) : string =
  let b = Buffer.create (String.length line) in
  String.iter
    (fun c ->
      match c with
      | '\t' -> Buffer.add_string b (String.make (8 - (Buffer.length b mod 8)) ' ')
      | '\r' -> ()
      | c -> Buffer.add_char b c)
    line;
  Buffer.contents b

(* the tree laid out and drawn, the page's links and pictures resolved
 * against its URL *)
let lay_out (s : settings) (base : string) (tree : Dom.element) : Html_layout.box * Browser_draw.drawn =
  let picture src = s.picture (Browser_url.resolve base src) in
  let visited href = s.visited (fst (Browser_url.split_fragment (Browser_url.resolve base href))) in
  let picture_size src = Option.bind (picture src) Browser_picture.size in
  let layout =
    Html_layout.layout Browser_text.metrics ~breaker:s.breaker ~picture_size ~root:Browser_text.root_look ~width:s.width tree
  in
  (layout, Browser_draw.draw ~visited ~picture_of:picture layout)

let laid_out (s : settings) (p : t) : t =
  let layout, drawn = lay_out s p.url p.tree in
  { p with layout; drawn }

let read (s : settings) (url : string) (status : int) (content_type : string option) (bytes : string) : t =
  let charset = Charset.detect ?content_type bytes in
  let text = Charset.to_utf_8 charset bytes in
  let tokens = Html_lexer.tokenize (as_html url content_type text (String.length bytes)) in
  let tree = Html_tree.parse tokens in
  let title = match Dom.find_all "title" tree with t :: _ -> String.trim (Dom.text_content t) | [] -> "" in
  let layout, drawn = lay_out s url tree in
  {
    url;
    status;
    charset;
    bytes = String.length bytes;
    lines = List.map expand_tabs (String.split_on_char '\n' text);
    tokens;
    tree;
    line_mode = Line_mode.render tree;
    title;
    layout;
    drawn;
    forms = Forms.forms tree;
    values = [];
  }

(*****************************************************************************)
(* Form values *)
(*****************************************************************************)

let value_of (p : t) (e : Dom.element) : Forms.value =
  match List.find_opt (fun (e', _) -> e' == e) p.values with
  | Some (_, v) -> v
  | None -> ( match Forms.control e with Some c -> c.initial | None -> { text = ""; checked = false; selected = 0 })

let with_value (p : t) (e : Dom.element) (v : Forms.value) : t =
  { p with values = (e, v) :: List.filter (fun (e', _) -> e' != e) p.values }
