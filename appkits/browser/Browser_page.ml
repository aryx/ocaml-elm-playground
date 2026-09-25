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
  backgrounds : string list;
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

(* a sheet's url()s made absolute against it: a background's picture
 * is the sheet's neighbour, not the page's *)
let absolute_urls (base : string) (rules : Css_syntax.rule list) : Css_syntax.rule list =
  let rec fix (cs : Css_syntax.component list) : Css_syntax.component list =
    List.map
      (fun (c : Css_syntax.component) : Css_syntax.component ->
        match c with
        | Token (Url u) -> Token (Url (Browser_url.resolve base u))
        | Func (f, args) when String.lowercase_ascii f = "url" -> (
            match Css_syntax.trim args with [ Token (String u) ] -> Token (Url (Browser_url.resolve base u)) | _ -> c)
        | Func (f, args) -> Func (f, fix args)
        | Block (k, inside) -> Block (k, fix inside)
        | Token _ -> c)
      cs
  in
  List.map
    (fun (r : Css_syntax.rule) : Css_syntax.rule ->
      match r with
      | Style_rule { prelude; declarations } ->
          Style_rule { prelude; declarations = List.map (fun (d : Css_syntax.declaration) -> { d with value = fix d.value }) declarations }
      | At_rule { name = "import"; _ } -> r
      | At_rule { name; prelude; block } -> At_rule { name; prelude; block = Option.map fix block })
    rules

(* a sheet's rules, its url()s absolute: parsed once per text and
 * address, not again at each relayout -- a picture's arrival lays the
 * page out again, and GitHub's 41 sheets are 4.9 MB (notes_opti_ocaml.md
 * section 11) *)
let parsed_sheets : (string * int * int, string * Css_syntax.rule list) Hashtbl.t = Hashtbl.create 16

let parsed (url : string) (text : string) : Css_syntax.rule list =
  (* by address and text: a page's <style>s share its address *)
  let key = (url, String.length text, Hashtbl.hash text) in
  match Hashtbl.find_opt parsed_sheets key with
  | Some (t, rules) when t == text || t = text -> rules
  | _ ->
      (* before: absolute_urls url (Css_syntax.parse_stylesheet text), each time *)
      let rules = absolute_urls url (Css_syntax.parse_stylesheet text) in
      if Hashtbl.length parsed_sheets > 256 then Hashtbl.reset parsed_sheets;
      Hashtbl.replace parsed_sheets key (text, rules);
      rules

(* a sheet's rules, its @imports' put in their place (four deep at
 * most: a sheet importing itself stops there); the addresses not had
 * yet added to [missing] *)
let rec expand (s : settings) (media : Cascade.media) (missing : string list ref) ~(depth : int) (url : string) (text : string) :
    Css_syntax.rule list =
  let rules = parsed url text in
  (* a sheet without @import: its parsed rules as they are, the same list
   * at each relayout (styles_of's memo compares them by ==) *)
  if not (List.exists (fun (r : Css_syntax.rule) -> match r with At_rule { name = "import"; _ } -> true | _ -> false) rules) then rules
  else
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
    rules

(* the page's sheets, in the order it gives them, each with a name (the
 * link's address, or "<style> n"): each <link rel=stylesheet> whose
 * media= holds (its text, once it has come) and each <style>; and the
 * addresses still to fetch, the links' and their @imports' *)
let named_sheets (s : settings) (media : Cascade.media) (base : string) (tree : Dom.element) : (string * Cascade.sheet) list * string list =
  let missing = ref [] and styles = ref 0 in
  let holds (e : Dom.element) = match Dom.attribute "media" e with Some m -> Cascade.media_matches media (Css_syntax.components_of m) | None -> true in
  let rec go (e : Dom.element) : (string * Cascade.sheet) list =
    let own =
      match e.name with
      | "link" -> (
          let rel = List.map String.lowercase_ascii (String.split_on_char ' ' (Option.value (Dom.attribute "rel" e) ~default:"")) in
          match Dom.attribute "href" e with
          | Some href when List.mem "stylesheet" rel && (not (List.mem "alternate" rel)) && holds e -> (
              let url = Browser_url.resolve base href in
              match s.sheet url with
              | Some text -> [ (url, { Cascade.origin = Author; rules = expand s media missing ~depth:0 url text }) ]
              | None ->
                  missing := url :: !missing;
                  [])
          | _ -> [])
      | "style" when holds e ->
          incr styles;
          [ (Printf.sprintf "<style> %d" !styles, { Cascade.origin = Author; rules = expand s media missing ~depth:0 base (Dom.text_content e) }) ]
      | _ -> []
    in
    own @ List.concat_map (fun (n : Dom.node) -> match n with Element c -> go c | Text _ -> []) e.children
  in
  let sheets = go tree in
  (sheets, List.rev !missing)

let page_sheets s media base tree : Cascade.sheet list * string list =
  let named, missing = named_sheets s media base tree in
  (List.map snd named, missing)

(* the last page's computed styles, and what they were computed from:
 * its tree (==), its sheets' rules (==, Browser_page.parsed's), quirks,
 * the window -- the same when a relayout is for a picture that came,
 * the cascade then not run again (notes_opti_ocaml.md section 11) *)
let last_styles : (Dom.element * Cascade.sheet list * bool * Cascade.media * (Dom.element -> Computed.t)) option ref = ref None

let styles_of ~visited ~(quirks : bool) (media : Cascade.media) (sheets : Cascade.sheet list) (tree : Dom.element) : Dom.element -> Computed.t =
  let same_sheets a b = List.length a = List.length b && List.for_all2 (fun (x : Cascade.sheet) (y : Cascade.sheet) -> x.rules == y.rules && x.origin = y.origin) a b in
  match !last_styles with
  | Some (t, sh, q, m, styles) when t == tree && q = quirks && m = media && same_sheets sh sheets -> styles
  | _ ->
      let styles = Computed.styles ~visited ~quirks media sheets tree in
      last_styles := Some (tree, sheets, quirks, media, styles);
      styles

(* the window's height, for media queries and vh: a laptop's screen *)
let viewport_height = 768.

(* the tree laid out and drawn, the page's links and pictures resolved
 * against its URL; with [boxes], by the box model (Cascade, Computed,
 * Box_layout, Browser_boxes), the page's colour its root's or its
 * body's (CSS 2.1 section 14.2: the canvas) *)
let lay_out ?(quirks = false) (s : settings) (base : string) (tree : Dom.element) :
    Html_layout.box * Browser_draw.drawn * Looks.color option option * string list =
  let picture src = s.picture (Browser_url.resolve base src) in
  let visited href = s.visited (fst (Browser_url.split_fragment (Browser_url.resolve base href))) in
  let picture_size src = Option.bind (picture src) Browser_picture.size in
  if s.boxes then
    let media : Cascade.media = { width = s.width; height = viewport_height } in
    let sheets = if s.css then fst (page_sheets s media base tree) else [] in
    (* before: the cascade and the computed styles again at each relayout
     *   let styles = Computed.styles ~visited ~quirks media sheets tree in *)
    let styles = styles_of ~visited ~quirks media sheets tree in
    let boxes = Box_layout.layout Browser_text.metrics ~picture_size ~viewport:(s.width, viewport_height) styles tree in
    let canvas =
      List.find_map
        (fun e -> match (styles e).background with c when c.a > 0. -> Some (c.r, c.g, c.b) | _ -> None)
        (tree :: Dom.find_all "body" tree)
    in
    (* the backgrounds' pictures, to fetch: the boxes' that are shown *)
    let rec backgrounds (b : Box_layout.box) =
      List.filter_map
        (fun u -> match u with Some u when b.style.visible && b.element <> None -> Some (Browser_url.resolve base u) | _ -> None)
        [ b.style.background_image; b.style.mask_image ]
      @ List.concat_map backgrounds b.children @ List.concat_map backgrounds b.backdrops
    in
    (Box_layout.as_html_layout boxes, Browser_boxes.draw ~visited ~picture_of:picture boxes, Some canvas, List.sort_uniq compare (backgrounds boxes))
  else
    let root = { Browser_text.root_look with extensions = s.extensions } in
    let style = if s.css then Css.cascade (Css.parse (Css.page_sheet tree)) tree else fun _ -> [] in
    let layout = Html_layout.layout Browser_text.metrics ~breaker:s.breaker ~picture_size ~style ~root ~width:s.width tree in
    (layout, Browser_draw.draw ~extensions:s.extensions ~visited ~picture_of:picture layout, None, [])

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
  let layout, drawn, canvas, backgrounds = lay_out ~quirks:p.quirks s p.url p.tree in
  { p with layout; drawn; background = background s p.tree canvas; backgrounds }

let title_of (tree : Dom.element) : string =
  match Dom.find_all "title" tree with t :: _ -> String.trim (Dom.text_content t) | [] -> ""

let with_tree (s : settings) (p : t) (tree : Dom.element) : t =
  let layout, drawn, canvas, backgrounds = lay_out ~quirks:p.quirks s p.url tree in
  {
    p with
    tree;
    line_mode = Line_mode.render tree;
    title = title_of tree;
    layout;
    drawn;
    background = background s tree canvas;
    backgrounds;
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
  let layout, drawn, canvas, backgrounds = lay_out ~quirks s url tree in
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
    backgrounds;
  }

(*****************************************************************************)
(* Explaining a style *)
(*****************************************************************************)

let explain (s : settings) (p : t) (e : Dom.element) : (string * string * string) list =
  if not s.boxes then []
  else
    let media : Cascade.media = { width = s.width; height = viewport_height } in
    let visited href = s.visited (fst (Browser_url.split_fragment (Browser_url.resolve p.url href))) in
    let page = if s.css then fst (named_sheets s media p.url p.tree) else [] in
    let browser = List.mapi (fun i sh -> ((if i = 0 then "the browser's (ua.css)" else "quirks mode"), sh)) (Computed.browser_sheets ~quirks:p.quirks) in
    let named = browser @ page in
    Cascade.explain ~visited media (List.map snd named) p.tree e
    |> List.map (fun (prop, value, (src : Cascade.source), important) ->
           let where =
             match src with
             | Rule { sheet; selector } -> Printf.sprintf "%s  %s" (Selectors.to_string selector) (fst (List.nth named sheet))
             | Hint -> "an attribute"
             | Style_attribute -> "style="
           in
           (prop, Css_syntax.to_string value ^ (if important then " !important" else ""), where))

(*****************************************************************************)
(* Form values *)
(*****************************************************************************)

let value_of (p : t) (e : Dom.element) : Forms.value =
  match List.find_opt (fun (e', _) -> e' == e) p.values with
  | Some (_, v) -> v
  | None -> ( match Forms.control e with Some c -> c.initial | None -> { text = ""; checked = false; selected = 0 })

let with_value (p : t) (e : Dom.element) (v : Forms.value) : t =
  { p with values = (e, v) :: List.filter (fun (e', _) -> e' != e) p.values }
