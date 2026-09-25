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
  background : Looks.color option;
  forms : Forms.form list;
  values : (Dom.element * Forms.value) list;
  quirks : bool;
}

type settings = {
  extensions : bool;
  css : bool;
  boxes : bool;
  width : float;
  breaker : Html_layout.breaker;
  visited : string -> bool;
  picture : string -> Browser_picture.t option;
  sheet : string -> string option;
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

(*****************************************************************************)
(* The page's style sheets *)
(*****************************************************************************)

(* an @import's address and the media it is for: url(x), url("x") or
 * "x", then a media query list *)
let import_of (prelude : Css_syntax.component list) : (string * Css_syntax.component list) option =
  match Css_syntax.trim prelude with
  | (Token (Url u) | Token (String u)) :: media -> Some (u, media)
  | Func (f, args) :: media when String.lowercase_ascii f = "url" -> (
      match Css_syntax.trim args with [ Token (String u) ] -> Some (u, media) | _ -> None)
  | _ -> None

(* a sheet's rules, its @imports' put in their place (four deep at
 * most: a sheet importing itself stops there); the addresses not had
 * yet added to [missing] *)
let rec expand (s : settings) (media : Cascade.media) (missing : string list ref) ~(depth : int) (url : string) (text : string) :
    Css_syntax.rule list =
  List.concat_map
    (fun (r : Css_syntax.rule) ->
      match r with
      | At_rule { name = "import"; prelude; _ } -> (
          match import_of prelude with
          | Some (u, m) when depth < 4 && Cascade.media_matches media m -> (
              let u = Browser_url.resolve url u in
              match s.sheet u with
              | Some t -> expand s media missing ~depth:(depth + 1) u t
              | None ->
                  missing := u :: !missing;
                  [])
          | _ -> [])
      | _ -> [ r ])
    (Css_syntax.parse_stylesheet text)

(* the page's sheets, in the order it gives them: each <link
 * rel=stylesheet> whose media= holds (its text, once it has come) and
 * each <style>; and the addresses still to fetch, the links' and their
 * @imports' *)
let page_sheets (s : settings) (media : Cascade.media) (base : string) (tree : Dom.element) : Cascade.sheet list * string list =
  let missing = ref [] in
  let holds (e : Dom.element) = match Dom.attribute "media" e with Some m -> Cascade.media_matches media (Css_syntax.components_of m) | None -> true in
  let rec go (e : Dom.element) : Cascade.sheet list =
    let own =
      match e.name with
      | "link" -> (
          let rel = List.map String.lowercase_ascii (String.split_on_char ' ' (Option.value (Dom.attribute "rel" e) ~default:"")) in
          match Dom.attribute "href" e with
          | Some href when List.mem "stylesheet" rel && (not (List.mem "alternate" rel)) && holds e -> (
              let url = Browser_url.resolve base href in
              match s.sheet url with
              | Some text -> [ { Cascade.origin = Author; rules = expand s media missing ~depth:0 url text } ]
              | None ->
                  missing := url :: !missing;
                  [])
          | _ -> [])
      | "style" when holds e -> [ { Cascade.origin = Author; rules = expand s media missing ~depth:0 base (Dom.text_content e) } ]
      | _ -> []
    in
    own @ List.concat_map (fun (n : Dom.node) -> match n with Element c -> go c | Text _ -> []) e.children
  in
  let sheets = go tree in
  (sheets, List.rev !missing)

(* the window's height, for media queries and vh: a laptop's screen *)
let viewport_height = 768.

(* the tree laid out and drawn, the page's links and pictures resolved
 * against its URL; with [boxes], by the box model (Cascade, Computed,
 * Box_layout, Browser_boxes), the page's colour its root's or its
 * body's (CSS 2.1 section 14.2: the canvas) *)
let lay_out ?(quirks = false) (s : settings) (base : string) (tree : Dom.element) : Html_layout.box * Browser_draw.drawn * Looks.color option option =
  let picture src = s.picture (Browser_url.resolve base src) in
  let visited href = s.visited (fst (Browser_url.split_fragment (Browser_url.resolve base href))) in
  let picture_size src = Option.bind (picture src) Browser_picture.size in
  if s.boxes then
    let media : Cascade.media = { width = s.width; height = viewport_height } in
    let sheets = if s.css then fst (page_sheets s media base tree) else [] in
    let styles = Computed.styles ~visited ~quirks media sheets tree in
    let boxes = Box_layout.layout Browser_text.metrics ~picture_size ~viewport:(s.width, viewport_height) styles tree in
    let canvas =
      List.find_map
        (fun e -> match (styles e).background with c when c.a > 0. -> Some (c.r, c.g, c.b) | _ -> None)
        (tree :: Dom.find_all "body" tree)
    in
    (Box_layout.as_html_layout boxes, Browser_boxes.draw ~visited ~picture_of:picture boxes, Some canvas)
  else
    let root = { Browser_text.root_look with extensions = s.extensions } in
    let style = if s.css then Css.cascade (Css.parse (Css.page_sheet tree)) tree else fun _ -> [] in
    let layout = Html_layout.layout Browser_text.metrics ~breaker:s.breaker ~picture_size ~style ~root ~width:s.width tree in
    (layout, Browser_draw.draw ~extensions:s.extensions ~visited ~picture_of:picture layout, None)

let sheets_wanted (s : settings) (p : t) : string list =
  if s.boxes && s.css then snd (page_sheets s { width = s.width; height = viewport_height } p.url p.tree) else []

(* the page's colour: the style sheets' for its <body> or <html>, else
 * Netscape's bgcolor= -- or the canvas's, by the box model *)
let background (s : settings) (tree : Dom.element) (canvas : Looks.color option option) : Looks.color option =
  match canvas with
  | Some c -> c
  | None ->
      let body = List.nth_opt (Dom.find_all "body" tree) 0 in
      let css =
        if not s.css then None
        else
          let style = Css.cascade (Css.parse (Css.page_sheet tree)) tree in
          List.find_map
            (fun e ->
              Option.bind (List.find_map (fun (p, v) -> if p = "background-color" || p = "background" then Some v else None) (style e)) Looks.color_of_string)
            (Option.to_list body @ [ tree ])
      in
      match css with
      | Some c -> Some c
      | None when s.extensions -> Option.bind (Option.bind body (Dom.attribute ~extensions:true "bgcolor")) Looks.color_of_string
      | None -> None

let laid_out (s : settings) (p : t) : t =
  let layout, drawn, canvas = lay_out ~quirks:p.quirks s p.url p.tree in
  { p with layout; drawn; background = background s p.tree canvas }

let title_of (tree : Dom.element) : string =
  match Dom.find_all "title" tree with t :: _ -> String.trim (Dom.text_content t) | [] -> ""

let with_tree (s : settings) (p : t) (tree : Dom.element) : t =
  let layout, drawn, canvas = lay_out ~quirks:p.quirks s p.url tree in
  {
    p with
    tree;
    line_mode = Line_mode.render tree;
    title = title_of tree;
    layout;
    drawn;
    background = background s tree canvas;
    forms = Forms.forms tree;
    (* the values were the old tree's elements'; a script's page keeps
     * a field's text in its value= (Browser_script.input) *)
    values = [];
  }

let read (s : settings) (url : string) (status : int) (content_type : string option) (bytes : string) : t =
  let charset = Charset.detect ?content_type bytes in
  let text = Charset.to_utf_8 charset bytes in
  let tokens = Html_lexer.tokenize (as_html url content_type text (String.length bytes)) in
  let tree = Html_tree.parse tokens in
  let title = title_of tree in
  (* no DOCTYPE: the page written for the browsers of the 1990s *)
  let quirks = not (List.exists (fun (t : Html_lexer.token) -> match t with Doctype _ -> true | _ -> false) tokens) in
  let layout, drawn, canvas = lay_out ~quirks s url tree in
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
    background = background s tree canvas;
    forms = Forms.forms tree;
    values = [];
    quirks;
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
