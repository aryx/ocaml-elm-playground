(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of NCSA Mosaic (Marc Andreessen and Eric Bina, 1993),
 * the browser that made the World Wide Web popular: a grey window, the
 * page's title and URL at the top, the page in the middle, a row of
 * buttons at the bottom, and a globe that turns while something loads.
 *
 * Built in phases (plan_browser_teaching.md, notes_browser.md), each a
 * stage of every browser's pipeline:
 *
 *   bytes -> text -> tokens -> tree -> looks -> boxes -> shapes
 *
 * All of them now, and back: a click (phases 0 to 6). A page is
 * fetched (a built-in about: page, or http:// through the platform's
 * Http.get), its bytes decoded into text -- the encoding decided from
 * the header, a <meta>, or a guess (Charset) -- cut into tokens
 * (Html_lexer), built into a tree, its mistakes repaired (Html_tree),
 * given looks (Looks: Mosaic's table, a heading big and bold, a link
 * blue and underlined), laid out in blocks and lines (Html_layout),
 * and drawn, each letter by Hershey's pen (Stroke_text), a list's
 * bullets and numbers in its indent. Lines are broken at the width,
 * greedily as every browser does, or ("w", wrap=pretty) by Knuth and
 * Plass's breaker (appkits/typeset's Linebreak), which CSS adopted as
 * text-wrap: pretty; "[" and "]" narrow and widen the page, and the
 * same tree is laid out again -- a reflow.
 *
 * And the way back from the picture: the pointer over a link shows its
 * URL in the status line, a click follows it (Hit: from a point to the
 * fragment under it, and its link), a #fragment scrolls to its anchor.
 * The history is two stacks, pages behind and pages ahead: Back and
 * Forward (the buttons, or "b" and "f") give a page back as it was,
 * scrolled where it was, and a new visit empties what was ahead.
 * Visited links are purple.
 *
 * Each stage has its view, switched with a key:
 *
 *   p   the page (the default); "o" outlines its boxes, as an inspector
 *   s   the source, Mosaic's "View Source"
 *   t   the tokens, a line each, tags in blue
 *   d   the tree (the DOM), indented, elements in blue
 *   l   the page as the 1991 Line Mode Browser showed it (Line_mode):
 *       text, a number after each link; type the number, then Return,
 *       to follow it
 *
 * scrolled with the arrows, Page Up/Down and the wheel, "r" reloading
 * the page, "h" going home. The title at the top is the tree's <title>.
 *
 * The web, for real: http:// by our own client, stepped each frame
 * (the globe turns meanwhile, the status line says what is awaited),
 * its redirections followed, the page's links then relative to where
 * it ended; https:// by curl natively, blocking, until TLS is ours (in
 * a browser, the browser's). What is not HTML is made a page: text
 * shown as Mosaic did, in <pre>, anything else said what it is; what
 * could not be fetched is an error page, reloaded with "r". And a web
 * server of our own to browse with no Internet, tiny_httpd:
 *
 *   dune exec networking/httpd/tiny_httpd.exe
 *   dune exec apps/internet/TinyMosaic.exe -- url=http://localhost:8080/
 *
 *   dune exec apps/internet/TinyMosaic.exe
 *   dune exec apps/internet/TinyMosaic.exe -- url=http://info.cern.ch/
 *   dune exec apps/internet/TinyMosaic.exe -- url=https://example.com/
 *   http://localhost:8001/apps/internet/web/TinyMosaic.html
 *
 * flags url= (about:home), the first page;
 * view=page|source|tokens|tree|line (page); wrap=greedy|pretty
 * (greedy); width= (976, the page area's), the page's width.
 *
 * Uses: web's Charset, Html_lexer, Html_tree, Line_mode, Looks,
 * Html_layout and Hit (the pipeline, and back), networking's Url (a
 * link resolved against the page's URL), Playground.Http (the request,
 * its answer with the headers and the final URL), the appkits
 * Stroke_text (the letters, and their widths for the layout) and
 * Linebreak (wrap=pretty's lines, the layout taking its breaker from
 * the app, since libs/ must not depend on an appkit); the built-in
 * pages are site/*.html, embedded by dune (Site_pages).
 *
 * Exercises: the source coloured as an editor would, from the tokens
 * (each token's place in the text is what the lexer would have to
 * keep: a start and an end); in the line-mode view, the 1991 browser's
 * other commands (Back, Up, Help, Quit, typed as words); Mosaic's
 * "Window History", the list of the pages visited, one clicked to go
 * back to it.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a page fetched, its bytes decoded, read, laid out *)
type page = {
  url : string; (* where it came from, after the redirections *)
  status : int; (* 200; the about: pages too *)
  charset : Charset.t;
  bytes : int;
  lines : string list; (* UTF-8, tabs expanded *)
  tokens : Html_lexer.token list;
  tree : Dom.element;
  line_mode : Line_mode.t;
  title : string; (* the text of its <title>, or "" *)
  layout : Html_layout.box;
  (* the page drawn: each line's glyphs, made once, with where the line
   * is (its top and bottom), so that a frame shows the visible ones *)
  drawn : (float * float * shape) list;
}

(* a page that could not be fetched is shown too: an error page, of
 * status 0 (error_html) *)
type state = Loading of string | Shown of page

(* which stage of the pipeline is shown *)
type view = Page | Source | Tokens | Tree | Line

(* how lines are broken: every browser's way, or Knuth and Plass's *)
type wrap = Greedy | Pretty

(* a page in the history: kept whole, and where it was scrolled to *)
type entry = { at : string; kept : page option; scrolled_to : int }

type model = {
  state : state;
  view : view;
  width : float; (* the page's, narrowed with [ and widened with ] *)
  wrap : wrap;
  scroll : int; (* the first line shown; 14 of the page's units a line in the page view *)
  typed : string; (* in the line-mode view, the number being typed *)
  back : entry list; (* the pages behind, the last visited first *)
  forward : entry list; (* the pages ahead, after a Back *)
  visited : string list; (* the URLs followed: their links purple *)
  fragment : string option; (* where to scroll once the page is in *)
  mouse : float * float;
  outline : bool; (* the layout's boxes drawn over the page *)
  time : float; (* the globe's *)
}

type msg =
  | Got of string * (Http.response, Http.error) result
  | Tick of float
  | Key of string
  | Typed of string
  | Wheel of float
  | Mouse_move of float * float
  | Click

let home = "about:home"

(*****************************************************************************)
(* Text and URLs *)
(*****************************************************************************)

(* the characters of a UTF-8 string, each its own string *)
let characters (s : string) : string list =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      go (i + n) (String.sub s i n :: acc)
  in
  go 0 []

(* the root's look: text 16 high, as a browser's is 16 px *)
let root_look = Looks.root ~size:16.

(* a look as Stroke_text's pen knows it *)
let style_of (l : Looks.t) : Style.t =
  { bold = l.bold; italic = l.italic; underline = l.underline; strike = l.strike; size = l.size }

(* a fixed-width character's cell: 0.6 em, as a typewriter's *)
let cell_of (l : Looks.t) : float = 0.6 *. l.size

(* the layout's metrics: Hershey's widths, or the cells of a fixed-width
 * look (Hershey has one face; <tt> is it set on a grid) *)
let metrics (l : Looks.t) (s : string) : float =
  if l.monospace then cell_of l *. float_of_int (List.length (characters s))
  else List.fold_left (fun w c -> w +. Stroke_text.metrics (style_of l) c) 0. (characters s)

(* a URL and its #fragment, apart: the fragment is the browser's, never
 * sent to the server *)
let split_fragment (url : string) : string * string option =
  match String.index_opt url '#' with
  | Some i -> (String.sub url 0 i, Some (String.sub url (i + 1) (String.length url - i - 1)))
  | None -> (url, None)

(* a link's href, relative to the page it is on (RFC 3986) *)
let resolve (base : string) (href : string) : string =
  match (Url.parse base, Url.parse href) with
  | Ok base, Ok href -> Url.to_string (Url.resolve base href)
  | _ -> href

(*****************************************************************************)
(* Drawing a page *)
(*****************************************************************************)

(* the page's widest: the page area's, less a little air *)
let page_width = 976.

(* wrap=pretty: the paragraph's lines chosen whole, by Knuth and
 * Plass's breaker (Linebreak.optimal), ragged right as a browser's are
 * -- the spaces may stretch (a line may end short), not shrink (it may
 * not end past the edge) -- with the paragraph's first real space for
 * all (Linebreak's model has one; the words are set with their own) *)
let pretty : Html_layout.breaker =
 fun ~measure units ->
  let space = Array.fold_left (fun s (u : Html_layout.unit_) -> if s = 0. then u.space else s) 0. units in
  let words = Array.map (fun (u : Html_layout.unit_) -> { Linebreak.text = ""; width = u.width }) units in
  Linebreak.optimal { measure; space; stretch = space; shrink = 0. } words
  |> List.map (fun (l : Linebreak.line) -> (l.first, l.last))

let breaker (wrap : wrap) : Html_layout.breaker = match wrap with Greedy -> Html_layout.greedy | Pretty -> pretty

(* a visited link's colour: Mosaic's, and every browser's since *)
let visited_purple = (85, 26, 139)

(* a fragment's glyphs, in the page's coordinates turned over: x from
 * its left, y up from its top (so a line below it is negative) *)
let glyphs ?(visited = fun (_ : string) -> false) (f : Html_layout.fragment) : shape list =
  let style = style_of f.look in
  let (r, g, b) = match f.look.link with Some href when visited href -> visited_purple | _ -> f.look.color in
  let color = rgb r g b in
  let baseline = -.f.baseline in
  if f.look.monospace then
    let cell = cell_of f.look in
    characters f.text
    |> List.mapi (fun i c ->
           if c = " " then []
           else
             let w = Stroke_text.metrics style c in
             Stroke_text.glyph color style c ~x:(f.x +. (cell *. float_of_int i) +. ((cell -. w) /. 2.)) ~baseline)
    |> List.concat
  else
    let _, shapes =
      List.fold_left
        (fun (x, shapes) c -> (x +. Stroke_text.metrics style c, Stroke_text.glyph color style c ~x ~baseline :: shapes))
        (f.x, []) (characters f.text)
    in
    List.concat (List.rev shapes)

(* every line's glyphs, and every rule and marker, with where it is *)
let rec draw (visited : string -> bool) (b : Html_layout.box) : (float * float * shape) list =
  let lines =
    List.map
      (fun (l : Html_layout.line) -> (l.top, l.top +. l.height, group (List.concat_map (glyphs ~visited) l.fragments)))
      b.lines
  in
  let rule =
    match b.kind with
    | Rule _ ->
        (* an inset line, as Mosaic's Motif drew it: dark above, light below *)
        [ ( b.y,
            b.y +. 2.,
            group
              [ rectangle (rgb 130 130 130) b.width 1. |> move (b.x +. (b.width /. 2.)) (-.b.y -. 0.5);
                rectangle (rgb 235 235 235) b.width 1. |> move (b.x +. (b.width /. 2.)) (-.b.y -. 1.5) ] ) ]
    | _ -> []
  in
  (* a list item's marker, left of its first line, in its list's indent *)
  let marker =
    match (b.marker, Html_layout.first_baseline b) with
    | Some Bullet, Some baseline ->
        [ (baseline -. 12., baseline, circle (rgb 0 0 0) 3. |> move (b.x -. 12.) (-.(baseline -. 5.))) ]
    | Some (Number n), Some baseline ->
        let text = string_of_int n ^ "." in
        let look = root_look in
        let width = metrics look text in
        [ (baseline -. 12., baseline, group (glyphs { text; look; x = b.x -. 6. -. width; width; baseline })) ]
    | _ -> []
  in
  lines @ rule @ marker @ List.concat_map (draw visited) b.children

(* the layout's boxes outlined, as a browser's inspector does: blocks
 * blue, the anonymous boxes of inline content green, their lines grey *)
let rec outlines (b : Html_layout.box) : (float * float * shape) list =
  let frame color x y w h =
    let t = 1. in
    group
      [ rectangle color w t |> move (x +. (w /. 2.)) (-.y);
        rectangle color w t |> move (x +. (w /. 2.)) (-.(y +. h));
        rectangle color t h |> move x (-.(y +. (h /. 2.)));
        rectangle color t h |> move (x +. w) (-.(y +. (h /. 2.))) ]
  in
  let color = match b.kind with Anonymous -> rgb 0 150 0 | _ -> rgb 0 0 220 in
  ((b.y, b.y +. b.height, frame color b.x b.y b.width b.height)
  :: List.map (fun (l : Html_layout.line) -> (l.top, l.top +. l.height, frame (rgb 150 150 150) b.x l.top b.width l.height)) b.lines)
  @ List.concat_map outlines b.children

(*****************************************************************************)
(* Fetching and reading *)
(*****************************************************************************)

(* the built-in site: about:NAME is site/NAME.html *)
let about (name : string) : string option =
  match name with "home" -> Some Site_pages.home | "history" -> Some Site_pages.history | _ -> None

let starts_with (prefix : string) (s : string) : bool =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

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

(* whether a link on page [p] leads where the person has been *)
let is_visited (m : model) (base : string) (href : string) : bool =
  List.mem (fst (split_fragment (resolve base href))) m.visited

(* the tree laid out at the model's width and wrap, and drawn: done
 * again when either changes (a reflow), or the visited links do *)
let laid_out (m : model) (p : page) : page =
  let layout = Html_layout.layout metrics ~breaker:(breaker m.wrap) ~root:root_look ~width:m.width p.tree in
  { p with layout; drawn = draw (is_visited m p.url) layout }

(* a response's media type: "text/html; charset=utf-8" is "text/html" *)
let media_type (content_type : string option) : string =
  match content_type with
  | None -> ""
  | Some ct -> String.lowercase_ascii (String.trim (List.hd (String.split_on_char ';' ct)))

let escape_html (s : string) : string =
  String.concat "" (List.map (fun c -> match c with "&" -> "&amp;" | "<" -> "&lt;" | ">" -> "&gt;" | c -> c) (characters s))

(* what is not HTML made a page: text as Mosaic showed it, in <pre>;
 * anything else said what it is *)
let as_html (url : string) (content_type : string option) (text : string) (bytes : int) : string =
  let name = Filename.basename (fst (split_fragment url)) in
  match media_type content_type with
  | "" | "text/html" -> text
  | t when starts_with "text/" t -> Printf.sprintf "<title>%s</title><pre>\n%s</pre>" name (escape_html text)
  | t ->
      Printf.sprintf
        "<title>%s</title><h1>%s</h1><p>A document of type <code>%s</code>, %d bytes. TinyMosaic shows HTML and text; pictures, soon."
        name name t bytes

(* a page for what went wrong: laid out like any, reloaded with r *)
let error_html (url : string) (why : string) : string =
  Printf.sprintf
    "<title>Failed</title><h1>Could not load the page</h1><p><code>%s</code><p>%s<p>Press <code>r</code> to try again, <code>b</code> to go back."
    (escape_html url) (escape_html why)

(* the pipeline: bytes -> text -> tokens -> tree -> boxes -> shapes *)
let page_of (m : model) (url : string) (status : int) (content_type : string option) (bytes : string) : page =
  let charset = Charset.detect ?content_type bytes in
  let text = Charset.to_utf_8 charset bytes in
  let tokens = Html_lexer.tokenize (as_html url content_type text (String.length bytes)) in
  let tree = Html_tree.parse tokens in
  let title = match Dom.find_all "title" tree with t :: _ -> String.trim (Dom.text_content t) | [] -> "" in
  let layout = Html_layout.layout metrics ~breaker:(breaker m.wrap) ~root:root_look ~width:m.width tree in
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
    drawn = draw (is_visited m url) layout;
  }

(*****************************************************************************)
(* Scrolling *)
(*****************************************************************************)

let ink = rgb 0 0 0
let tag_blue = rgb 0 0 160

(* the colour of a token's line: tags blue, as an editor shows them *)
let token_color (token : Html_lexer.token) : color =
  match token with
  | Start_tag _ | End_tag _ -> tag_blue
  | Text _ -> ink
  | Comment _ -> rgb 100 100 100
  | Doctype _ -> rgb 0 110 0

(* the line-mode view's prompt, after the page, as in 1991 *)
let prompt (m : model) (p : page) : string =
  match List.length p.line_mode.links with
  | 0 -> "(no links) "
  | n -> Printf.sprintf "1-%d, type a link's number then <RETURN>: %s_" n m.typed

(* the lines of a text view, each with its colour *)
let view_lines (m : model) (p : page) : (color * string) list =
  match m.view with
  | Page -> []
  | Source -> List.map (fun line -> (ink, line)) p.lines
  | Tokens -> List.map (fun token -> (token_color token, Html_lexer.to_string token)) p.tokens
  | Tree ->
      (* a text's line starts with its quote, an element's with its name *)
      Dom.to_lines (Dom.without_blank_text p.tree)
      |> List.map (fun line ->
             let t = String.trim line in
             ((if t <> "" && t.[0] = '"' then ink else tag_blue), line))
  | Line -> List.map (fun line -> (ink, line)) p.line_mode.lines @ [ (ink, ""); (tag_blue, prompt m p) ]

(* the lines the page area holds, 14 high *)
let visible = 58
let line_height = 14.

let line_count (m : model) : int =
  match (m.state, m.view) with
  | Shown p, Page -> int_of_float (Float.ceil (p.layout.height /. line_height))
  | Shown p, _ -> List.length (view_lines m p)
  | _ -> 0

let scrolled (by : int) (m : model) : model =
  { m with scroll = max 0 (min (line_count m - visible) (m.scroll + by)) }

(* the page scrolled to its #fragment, if it was waiting for one and has
 * it (the page view's lines: 14 units each) *)
let to_fragment (m : model) : model =
  match (m.state, m.fragment) with
  | Shown p, Some name -> (
      let m = { m with fragment = None } in
      match Hit.anchor p.layout name with
      | Some y -> scrolled 0 { m with scroll = int_of_float (y /. line_height) }
      | None -> m)
  | _ -> m

(*****************************************************************************)
(* Update: going places *)
(*****************************************************************************)

let current_url (m : model) : string = match m.state with Loading url -> url | Shown p -> p.url

(* what went wrong fetching [url], shown as a page *)
let failed (m : model) (url : string) (why : string) : model =
  { m with state = Shown (page_of m url 0 None (error_html url why)) }

(* the page at [url] (no #fragment): at once for an about: page, else a
 * command; the history untouched (Reload, Back and Forward use it) *)
let load (network : < Cap.network ; .. >) (url : string) (m : model) : model * msg Cmd.t =
  let m = { m with scroll = 0; typed = "" } in
  if starts_with "about:" url then
    let name = String.sub url 6 (String.length url - 6) in
    match about name with
    | Some bytes ->
        (to_fragment { m with state = Shown (page_of m url 200 (Some "text/html; charset=utf-8") bytes) }, Cmd.none)
    | None -> (failed m url "There is no such page in the built-in site.", Cmd.none)
  else ({ m with state = Loading url }, Http.get network ~url ~expect:(Http.expect_response (fun r -> Got (url, r))))

(* where the person is now, as the history keeps it *)
let entry_of (m : model) : entry =
  match m.state with
  | Shown p -> { at = p.url; kept = Some p; scrolled_to = m.scroll }
  | Loading url -> { at = url; kept = None; scrolled_to = 0 }

(* a link followed: where the person was goes on the stack behind, what
 * was ahead is forgotten (a new branch); a #fragment of the page shown
 * only scrolls *)
let visit (network : < Cap.network ; .. >) (url : string) (m : model) : model * msg Cmd.t =
  let target, fragment = split_fragment url in
  let m =
    { m with back = entry_of m :: m.back; forward = []; visited = (if List.mem target m.visited then m.visited else target :: m.visited); fragment }
  in
  match m.state with
  | Shown p when fragment <> None && target = fst (split_fragment p.url) -> (to_fragment { m with state = Shown (laid_out m p) }, Cmd.none)
  | _ -> load network target m

(* back to a page of the history, as it was: kept whole (its links
 * redrawn, some may be purple since), or fetched again *)
let restore (network : < Cap.network ; .. >) (e : entry) (m : model) : model * msg Cmd.t =
  match e.kept with
  | Some p -> (scrolled 0 { m with state = Shown (laid_out m p); scroll = e.scrolled_to; typed = "" }, Cmd.none)
  | None -> load network e.at m

let go_back (network : < Cap.network ; .. >) (m : model) : model * msg Cmd.t =
  match m.back with
  | e :: rest -> restore network e { m with back = rest; forward = entry_of m :: m.forward }
  | [] -> (m, Cmd.none)

let go_forward (network : < Cap.network ; .. >) (m : model) : model * msg Cmd.t =
  match m.forward with
  | e :: rest -> restore network e { m with forward = rest; back = entry_of m :: m.back }
  | [] -> (m, Cmd.none)

(* the page laid out again at the model's width and wrap, the scroll
 * kept inside the new height *)
let relaid (m : model) : model =
  match m.state with Shown p -> scrolled 0 { m with state = Shown (laid_out m p) } | _ -> m

(* the line-mode view's Return: the link numbered so, if there is one *)
let follow (network : < Cap.network ; .. >) (m : model) : model * msg Cmd.t =
  match (m.state, int_of_string_opt m.typed) with
  | Shown p, Some n when n >= 1 && n <= List.length p.line_mode.links ->
      visit network (resolve p.url (List.nth p.line_mode.links (n - 1))) m
  | _ -> ({ m with typed = "" }, Cmd.none)

(*****************************************************************************)
(* Update: the pointer *)
(*****************************************************************************)

(* the page area: its top and left edges, its height *)
let area_top = 410.
let area_left = -488.
let area_height = 820.

(* the link under the pointer, in the page view *)
let hovered (m : model) : string option =
  let mx, my = m.mouse in
  match m.state with
  | Shown p when m.view = Page && my <= area_top && my >= area_top -. area_height ->
      let y = area_top -. my +. (float_of_int m.scroll *. line_height) in
      Hit.link_at p.layout ~x:(mx -. area_left) ~y
  | _ -> None

(* the buttons at the bottom: their names, and whether they do something
 * now; placed left to right *)
let buttons (m : model) : (string * bool) list =
  [ ("Back", m.back <> []); ("Forward", m.forward <> []); ("Home", true); ("Reload", true); ("Open...", false);
    ("Source", true) ]

let button_y = -470.
let button_x (i : int) : float = -490. +. (95. *. float_of_int i)
let button_width (text : string) : float = (7. *. float_of_int (String.length text)) +. 20.

(* the button under the pointer, if any *)
let button_at (m : model) : string option =
  let mx, my = m.mouse in
  List.mapi (fun i (text, active) -> (i, text, active)) (buttons m)
  |> List.find_map (fun (i, text, active) ->
         if active && mx >= button_x i && mx <= button_x i +. button_width text && Float.abs (my -. button_y) <= 13. then Some text
         else None)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let init (network : < Cap.network ; .. >) (flags : flags) : model * msg Cmd.t =
  let target, fragment = split_fragment (Option.value (List.assoc_opt "url" flags) ~default:home) in
  let view =
    match List.assoc_opt "view" flags with
    | Some "source" -> Source
    | Some "tokens" -> Tokens
    | Some "tree" -> Tree
    | Some "line" -> Line
    | _ -> Page
  in
  let width =
    match Option.bind (List.assoc_opt "width" flags) float_of_string_opt with
    | Some w -> Float.min page_width (Float.max 200. w)
    | None -> page_width
  in
  let wrap = if List.assoc_opt "wrap" flags = Some "pretty" then Pretty else Greedy in
  load network target
    {
      state = Loading target;
      view;
      width;
      wrap;
      scroll = 0;
      typed = "";
      back = [];
      forward = [];
      visited = [ target ];
      fragment;
      mouse = (0., 0.);
      outline = false;
      time = 0.;
    }

let update (network : < Cap.network ; .. >) (msg : msg) (m : model) : model * msg Cmd.t =
  let switch view = ({ m with view; scroll = 0; typed = "" }, Cmd.none) in
  match msg with
  | Got (_, Ok r) ->
      (* a header's name ignores case *)
      let content_type =
        List.find_map
          (fun (name, value) -> if String.lowercase_ascii name = "content-type" then Some value else None)
          r.headers
      in
      (to_fragment { m with state = Shown (page_of m r.url r.status content_type r.body) }, Cmd.none)
  | Got (url, Error e) -> (failed m url (String.capitalize_ascii (Http.error_to_string e) ^ "."), Cmd.none)
  | Tick time -> ({ m with time }, Cmd.none)
  | Wheel notches -> (scrolled (3 * int_of_float (Float.round notches)) m, Cmd.none)
  | Mouse_move (x, y) -> ({ m with mouse = (x, y) }, Cmd.none)
  | Click -> (
      match (button_at m, hovered m, m.state) with
      | Some "Back", _, _ -> go_back network m
      | Some "Forward", _, _ -> go_forward network m
      | Some "Home", _, _ -> visit network home m
      | Some "Reload", _, _ -> load network (current_url m) m
      | Some "Source", _, _ -> switch (if m.view = Source then Page else Source)
      | _, Some href, Shown p -> visit network (resolve p.url href) m
      | _ -> (m, Cmd.none))
  (* the digits of a link's number, in the line-mode view *)
  | Typed s when m.view = Line && String.for_all (fun c -> c >= '0' && c <= '9') s -> ({ m with typed = m.typed ^ s }, Cmd.none)
  | Typed _ -> (m, Cmd.none)
  | Key key -> (
      (* natively SDL's names, lowercased ("pagedown", "return"), in a
       * browser the DOM's ("PageDown", "Enter"): lowercased, both *)
      match String.lowercase_ascii key with
      | "arrowdown" | "down" -> (scrolled 1 m, Cmd.none)
      | "arrowup" | "up" -> (scrolled (-1) m, Cmd.none)
      | "pagedown" | " " | "space" -> (scrolled (visible - 2) m, Cmd.none)
      | "pageup" -> (scrolled (-(visible - 2)) m, Cmd.none)
      | ("enter" | "return") when m.view = Line -> follow network m
      | "backspace" when m.typed <> "" -> ({ m with typed = String.sub m.typed 0 (String.length m.typed - 1) }, Cmd.none)
      | "b" | "backspace" -> go_back network m
      | "f" -> go_forward network m
      | "r" -> load network (current_url m) m
      | "h" -> visit network home m
      | "p" -> switch Page
      | "s" -> switch Source
      | "t" -> switch Tokens
      | "d" -> switch Tree
      | "l" -> switch Line
      | "o" -> ({ m with outline = not m.outline }, Cmd.none)
      (* a reflow: the same tree laid out again *)
      | "w" -> (relaid { m with wrap = (if m.wrap = Greedy then Pretty else Greedy) }, Cmd.none)
      | "[" -> (relaid { m with width = Float.max 200. (m.width -. 100.) }, Cmd.none)
      | "]" -> (relaid { m with width = Float.min page_width (m.width +. 100.) }, Cmd.none)
      | _ -> (m, Cmd.none))

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* Mosaic's colours: Motif's grey, everywhere, the page included *)
let grey = rgb 191 191 191
let dark_grey = rgb 150 150 150
let light_grey = rgb 225 225 225

(* a line of text, a character per cell, 6 wide, from [x] rightwards
 * (words are centred: each character is moved to its cell's centre) *)
let cell = 6.

let monospace (x : number) (y : number) (color : color) (s : string) : shape list =
  characters s
  |> List.mapi (fun i c -> (i, c))
  |> List.filter (fun (i, c) -> c <> " " && i < 160)
  |> List.map (fun (i, c) -> words color c |> move (x +. (cell *. float_of_int i) +. (cell /. 2.)) y)

(* words starting at [x] rather than centred there (about 6 a character) *)
let label (x : number) (y : number) (color : color) (s : string) : shape =
  words color s |> move (x +. (3. *. float_of_int (String.length s))) y

(* a field: a sunken white box, its text from the left *)
let field (x : number) (y : number) (width : number) (text : string) : shape list =
  [ rectangle dark_grey (width +. 2.) 22. |> move (x +. (width /. 2.)) y;
    rectangle white width 20. |> move (x +. (width /. 2.)) y;
    label (x +. 4.) y ink text ]

(* a raised button, grey when it does nothing now *)
let button (x : number) (y : number) (text : string) (active : bool) : shape list =
  let width = button_width text in
  [ rectangle light_grey width 26. |> move (x +. (width /. 2.)) y;
    rectangle grey (width -. 3.) 23. |> move (x +. (width /. 2.) +. 1.5) (y -. 1.5);
    label (x +. 10.) y (if active then ink else dark_grey) text ]

(* the globe, turning while something loads: land passing over the sea *)
let globe (m : model) : shape list =
  let turn = match m.state with Loading _ -> m.time *. 2. | _ -> 0. in
  let continent = oval (rgb 60 140 70) 12. 22. |> move_x (10. *. sin turn) in
  [ circle (rgb 40 80 170) 18.; continent; oval (rgb 60 140 70) 8. 10. |> move (-.9. *. sin (turn +. 1.5)) 7. ]
  |> List.map (fun s -> s |> move 465. 455.)

(* the status line: the link under the pointer, as Mosaic showed it,
 * else what the view is *)
let status (m : model) : string =
  match (m.state, hovered m) with
  | Shown p, Some href -> resolve p.url href
  | Loading url, _ -> "Connecting to " ^ url ^ " ..."
  | Shown p, None when p.status = 0 -> "Failed: " ^ p.url
  | Shown p, None ->
      let n = line_count m in
      Printf.sprintf "%s: %d bytes, %s, status %d, lines %d-%d of %d"
        (match m.view with
        | Page ->
            Printf.sprintf "Page (%s lines, %.0f wide)" (match m.wrap with Greedy -> "greedy" | Pretty -> "pretty") m.width
        | Source -> "Document source"
        | Tokens -> "Tokens"
        | Tree -> "Tree"
        | Line -> "Line mode")
        p.bytes
        (match p.charset with Utf_8 -> "UTF-8" | Windows_1252 -> "Windows-1252")
        p.status
        (min n (m.scroll + 1))
        (min n (m.scroll + visible))
        n

(* the page view: the lines in the page area, the page moved up by the
 * scroll *)
let page_shapes (m : model) (p : page) : shape list =
  let scroll = float_of_int m.scroll *. line_height in
  (p.drawn @ if m.outline then outlines p.layout else [])
  |> List.filter (fun (top, bottom, _) -> bottom > scroll && top < scroll +. area_height)
  |> List.map (fun (_, _, s) -> s)
  |> group
  |> move area_left (area_top +. scroll)
  |> fun s -> [ s ]

let view (m : model) : shape list =
  let body =
    match m.state with
    | Shown p when m.view = Page -> page_shapes m p
    | Shown p ->
        view_lines m p
        |> List.filteri (fun i _ -> i >= m.scroll && i < m.scroll + visible)
        |> List.mapi (fun i (color, line) -> monospace (-480.) (400. -. (14. *. float_of_int i)) color line)
        |> List.concat
    | Loading _ -> []
  in
  let title = match m.state with Shown p -> p.title | _ -> "" in
  [ rectangle grey 1000. 1000. ]
  (* the page, sunken *)
  @ [ rectangle dark_grey 982. 832.; rectangle grey 980. 830. ]
  @ body
  (* the chrome over it: what overflows the page area is hidden *)
  @ [ rectangle grey 1000. 84. |> move_y 458.; rectangle grey 1000. 84. |> move_y (-458.) ]
  @ [ label (-490.) 470. ink "Title:" ] @ field (-440.) 470. 860. title
  @ [ label (-490.) 440. ink "URL:" ] @ field (-440.) 440. 860. (current_url m)
  @ globe m
  @ [ label (-490.) (-428.) ink (status m) ]
  @ List.concat (List.mapi (fun i (text, active) -> button (button_x i) button_y text active) (buttons m))

(*****************************************************************************)
(* The app *)
(*****************************************************************************)

(* the network granted: the pages' requests are all it does with it
 * (plan_caps.md) *)
let app (network : < Cap.network ; .. >) =
  {
    Playground.init = init network;
    update = update network;
    view;
    subscriptions =
      (fun _ ->
        Sub.batch
          [ Sub.on_animation_frame (fun t -> Tick t); Sub.on_key_down (fun key -> Key key);
            Sub.on_typed (fun s -> Typed s); Sub.on_mouse_wheel (fun n -> Wheel n);
            Sub.on_mouse_move (fun (x, y) -> Mouse_move (x, y)); Sub.on_mouse_down (fun () -> Click) ]);
  }

let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app caps))
