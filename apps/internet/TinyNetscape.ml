(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Netscape Navigator 1.0 (Netscape Communications,
 * December 1994), written by much of Mosaic's team a year after Mosaic:
 * the same web, and a browser that **does not wait**.
 *
 * Mosaic fetched a page, then its pictures one after the other, the
 * window frozen meanwhile in libwww's blocking reads. Netscape opened
 * four connections at once, filled the pictures in as they came, kept
 * the window alive -- the Stop button meant something -- and showed
 * how far it had got: the "N" with its meteors while loading, a
 * progress bar at the bottom, and the words "Document: Done." Here, the
 * pictures of a page are fetched four at a time (TinyMosaic: one), the
 * page laid out again as each arrives, the status bar counting them.
 *
 * And what could still freeze the window is done on threads, as
 * Netscape did on NSPR's: natively, the platform's commands resolve a
 * host's name and fetch an https:// page (curl) on a pool of four
 * threads (Worker, Commands), the "N" going on meanwhile; threads=off
 * for Mosaic's way, the window stopped while it waits (try an https://
 * page with both). In a browser, the web platform waits for us. The
 * rest of the lesson is in the plan (plan_browser_teaching.md,
 * "TinyNetscape"): Netscape 1.1's HTML (N3), tables (N4), style sheets
 * (N5).
 *
 * The chrome is Netscape's on X, in Motif: a toolbar of labelled
 * buttons (Back, Forward, Home, Reload, Images, Open, Print, Find,
 * Stop), the Location field, into which a URL is typed and Return goes
 * there (click it: what is there is selected, typing replaces it), the
 * "N" at the top right, and the status bar with its key -- broken for
 * http://, whole for https://, Netscape's sign of SSL (which it
 * invented, 1994), here curl's TLS.
 *
 * Keys: arrows, Page Up/Down and the wheel scroll; b and f go back and
 * forward, h home, r reloads, s shows the source (p the page again),
 * c switches the page's style sheets off and on (N5).
 *
 *   dune exec apps/internet/TinyNetscape.exe
 *   dune exec networking/httpd/tiny_httpd.exe      (then Location: http://localhost:8080/home.html)
 *   http://localhost:8001/apps/internet/web/TinyNetscape.html
 *
 * flags url= (about:netscape, Netscape's welcome page), the first page; images=off, pictures not
 * fetched until the Images button (Netscape's "Auto Load Images",
 * for a 14400 modem); threads=off, no threads (natively); css=off,
 * the pages' style sheets not honoured.
 *
 * And Netscape's HTML (N3): the same tree as TinyMosaic's, in which the
 * extensions are marked (Dtd.origin), honoured here and not there
 * (extensions = true in [settings]): colours, fonts, <center>, rules,
 * pictures the text flows around (floats). Its home page, about:netscape,
 * has them all; TinyMosaic shows the same page without them.
 *
 * Uses: the appkit appkits/browser (a page read, laid out, drawn; the
 * history; forms), shared with TinyMosaic, as the built-in site is
 * (Site); web's Hit; Playground.Http. Its own: the model, the chrome,
 * the fetching four at a time.
 *
 * Exercises: the page itself drawn as its bytes arrive (Http_request
 * giving its body in pieces, the tokenizer and the tree fed as they
 * come); Netscape's "What's New!" and "What's Cool!" directory buttons;
 * the Bookmarks menu.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type page = Browser_page.t
type state = Loading of string | Shown of page
type view = Page | Source
type entry = { at : string; kept : page option; scrolled_to : int }

type model = {
  state : state;
  view : view;
  scroll : int; (* the first line shown, 14 of the page's units *)
  history : entry Browser_history.t;
  visited : string list;
  fragment : string option;
  pictures : (string * Browser_picture.t) list; (* by URL, every page's: a cache *)
  queue : string list; (* the page's pictures still to fetch *)
  in_flight : string list; (* on their way: four at most *)
  total : int; (* the page's pictures to fetch, for the progress *)
  images : bool; (* Auto Load Images *)
  css : bool; (* the pages' style sheets honoured (N5) *)
  location : string; (* the Location field *)
  editing : bool; (* typing into it *)
  fresh : bool; (* just clicked: what is there is selected, typing replaces it *)
  mouse : float * float;
  focus : Dom.element option; (* a form's field typed into *)
  time : float; (* the meteors' *)
}

type msg =
  | Got of string * (Http.response, Http.error) result
  | Got_picture of string * (Http.response, Http.error) result
  | Tick of float
  | Key of string
  | Typed of string
  | Wheel of float
  | Mouse_move of float * float
  | Click

(* claude: Netscape's own welcome page, its extensions to HTML (N3) *)
let home = "about:netscape"
let characters = Browser_text.characters
let resolve = Browser_url.resolve
let split_fragment = Browser_url.split_fragment
let starts_with = Browser_url.starts_with

(* Netscape's four connections at once *)
let connections = 4

(*****************************************************************************)
(* Reading a page *)
(*****************************************************************************)

let page_width = 976.

let settings (m : model) : Browser_page.settings =
  {
    (* claude: Netscape's own extensions to HTML (N3) *)
    extensions = true;
    (* claude: CSS1, Netscape 4's (N5); c or css=off for the page
     * without its style sheets *)
    css = m.css;
    width = page_width;
    breaker = Html_layout.greedy;
    visited = (fun url -> List.mem url m.visited);
    picture = (fun url -> List.assoc_opt url m.pictures);
  }

let page_of (m : model) (url : string) (status : int) (content_type : string option) (bytes : string) : page =
  Browser_page.read (settings m) url status content_type bytes

let laid_out (m : model) (p : page) : page = Browser_page.laid_out (settings m) p

(*****************************************************************************)
(* Scrolling *)
(*****************************************************************************)

let visible = 56
let line_height = 14.

let line_count (m : model) : int =
  match (m.state, m.view) with
  | Shown p, Page -> int_of_float (Float.ceil (p.layout.height /. line_height))
  | Shown p, Source -> List.length p.lines
  | _ -> 0

let scrolled (by : int) (m : model) : model =
  { m with scroll = max 0 (min (line_count m - visible) (m.scroll + by)) }

let to_fragment (m : model) : model =
  match (m.state, m.fragment) with
  | Shown p, Some name -> (
      let m = { m with fragment = None } in
      match Hit.anchor p.layout name with
      | Some y -> scrolled 0 { m with scroll = int_of_float (y /. line_height) }
      | None -> m)
  | _ -> m

(*****************************************************************************)
(* Fetching: pictures four at a time *)
(*****************************************************************************)

let current_url (m : model) : string = match m.state with Loading url -> url | Shown p -> p.url

(* a page shown: the Location field says where it is *)
let shown (m : model) (p : page) : model =
  { m with state = Shown p; location = (if m.editing then m.location else p.url) }

let failed (m : model) (url : string) (why : string) : model =
  shown m (page_of m url 0 None (Browser_page.error_html url why))

(* a picture had (or not): the page laid out again with it *)
let with_arrived (m : model) (url : string) (pic : Browser_picture.t) : model =
  let m = { m with pictures = (url, pic) :: List.remove_assoc url m.pictures } in
  match m.state with Shown p -> { m with state = Shown (laid_out m p) } | Loading _ -> m

(* more pictures on their way, while fewer than four are: Netscape's
 * way, where Mosaic had one; a built-in one decoded at once *)
let rec fetch_more (network : < Cap.network ; .. >) ((m, cmd) : model * msg Cmd.t) : model * msg Cmd.t =
  match m.queue with
  | url :: rest when List.length m.in_flight < connections ->
      let m = { m with queue = rest } in
      if starts_with "about:" url then
        let pic =
          match Site.about (String.sub url 6 (String.length url - 6)) with
          | Some (bytes, _) -> Browser_picture.decode bytes
          | None -> Browser_picture.Broken
        in
        fetch_more network (with_arrived m url pic, cmd)
      else
        let get = Http.get network ~url ~expect:(Http.expect_response (fun r -> Got_picture (url, r))) in
        fetch_more network ({ m with in_flight = url :: m.in_flight }, Cmd.batch [ cmd; get ])
  | _ -> (m, cmd)

(* a page shown: its pictures not had yet queued (if Auto Load Images),
 * the ones of the page before dropped *)
let with_pictures (network : < Cap.network ; .. >) ((m, cmd) : model * msg Cmd.t) : model * msg Cmd.t =
  match m.state with
  | Loading _ -> (m, cmd)
  | Shown p ->
      let had url = match List.assoc_opt url m.pictures with Some (Arrived _ | Broken) -> true | _ -> false in
      let urls =
        Dom.find_all "img" p.tree
        |> List.filter_map (fun e -> Option.map (resolve p.url) (Dom.attribute "src" e))
        |> List.fold_left (fun acc u -> if List.mem u acc || had u || List.mem u m.in_flight then acc else acc @ [ u ]) []
      in
      if not m.images then ({ m with queue = []; total = 0 }, cmd)
      else fetch_more network ({ m with queue = urls; total = List.length urls + List.length m.in_flight }, cmd)

(*****************************************************************************)
(* Going places *)
(*****************************************************************************)

let load ?post (network : < Cap.network ; .. >) (url : string) (m : model) : model * msg Cmd.t =
  let m = { m with scroll = 0; focus = None; queue = []; total = 0 } in
  if starts_with "about:" url then
    let name, query = Browser_url.split_query (String.sub url 6 (String.length url - 6)) in
    let show bytes content_type =
      with_pictures network (to_fragment (shown m (page_of m url 200 (Some content_type) bytes)), Cmd.none)
    in
    match (name, post) with
    | "echo", Some (_, body) -> show (Browser_page.echo_html "POST" body) "text/html; charset=utf-8"
    | "echo", None -> show (Browser_page.echo_html "GET" (Option.value query ~default:"")) "text/html; charset=utf-8"
    | _ -> (
        match Site.about name with
        | Some (bytes, content_type) -> show bytes content_type
        | None -> (failed m url "There is no such page in the built-in site.", Cmd.none))
  else
    let expect = Http.expect_response (fun r -> Got (url, r)) in
    let m = { m with state = Loading url; location = (if m.editing then m.location else url) } in
    match post with
    | None -> (m, Http.get network ~url ~expect)
    | Some (content_type, body) -> (m, Http.post network ~url ~content_type ~body ~expect)

let entry_of (m : model) : entry =
  match m.state with
  | Shown p -> { at = p.url; kept = Some p; scrolled_to = m.scroll }
  | Loading url -> { at = url; kept = None; scrolled_to = 0 }

let visit ?post (network : < Cap.network ; .. >) (url : string) (m : model) : model * msg Cmd.t =
  let target, fragment = split_fragment url in
  let m =
    {
      m with
      history = Browser_history.visit (entry_of m) m.history;
      visited = (if List.mem target m.visited then m.visited else target :: m.visited);
      fragment;
      editing = false;
    }
  in
  match m.state with
  | Shown p when post = None && fragment <> None && target = fst (split_fragment p.url) ->
      (to_fragment { m with state = Shown (laid_out m p) }, Cmd.none)
  | _ -> load ?post network target m

let restore (network : < Cap.network ; .. >) (e : entry) (m : model) : model * msg Cmd.t =
  match e.kept with
  | Some p -> with_pictures network (scrolled 0 { (shown m (laid_out m p)) with scroll = e.scrolled_to }, Cmd.none)
  | None -> load network e.at m

let go_back network m =
  match Browser_history.back (entry_of m) m.history with Some (e, history) -> restore network e { m with history } | None -> (m, Cmd.none)

let go_forward network m =
  match Browser_history.forward (entry_of m) m.history with
  | Some (e, history) -> restore network e { m with history }
  | None -> (m, Cmd.none)

(* what was typed into the Location field, as a URL: one with no
 * scheme is taken for a site's name, as Netscape did ("info.cern.ch") *)
let typed_url (s : string) : string =
  let s = String.trim s in
  if String.contains s ':' then s else "http://" ^ s

let form_effect (network : < Cap.network ; .. >) ~(keep_focus : bool) (effect : Browser_forms.effect) (m : model) :
    model * msg Cmd.t =
  match effect with
  | Nothing -> (m, Cmd.none)
  | Focus e -> ({ m with focus = Some e; editing = false }, Cmd.none)
  | Unfocus -> ({ m with focus = None }, Cmd.none)
  | Changed p -> ({ m with state = Shown p; focus = (if keep_focus then m.focus else None) }, Cmd.none)
  | Submit { url; post; page } -> visit ?post network url { m with state = Shown page; focus = None }

(*****************************************************************************)
(* The pointer *)
(*****************************************************************************)

(* the page area: its top and left edges, its height *)
let area_top = 352.
let area_left = -488.
let area_height = 792.

let page_point (m : model) : (float * float) option =
  let mx, my = m.mouse in
  if m.view = Page && my <= area_top && my >= area_top -. area_height then
    Some (mx -. area_left, area_top -. my +. (float_of_int m.scroll *. line_height))
  else None

let hovered (m : model) : string option =
  match (m.state, page_point m) with Shown p, Some (x, y) -> Hit.link_at p.layout ~x ~y | _ -> None

let pointed_control (m : model) : Dom.element option =
  match (m.state, page_point m) with
  | Shown p, Some (x, y) -> (
      match Hit.fragment_at p.layout ~x ~y with Some { control = Some c; _ } -> Some c.element | _ -> None)
  | _ -> None

(* the toolbar: its buttons, whether each does something now *)
let buttons (m : model) : (string * bool) list =
  let loading = (match m.state with Loading _ -> true | Shown _ -> false) || m.in_flight <> [] || m.queue <> [] in
  [ ("Back", m.history.behind <> []); ("Forward", m.history.ahead <> []); ("Home", true); ("Reload", true);
    ("Images", not m.images); ("Open", true); ("Print", false); ("Find", false); ("Stop", loading) ]

let toolbar_y = 416.
let button_w = 70.
let button_x (i : int) : float = -490. +. (74. *. float_of_int i)

(* the Location field *)
let location_y = 372.
let location_x = -400.
let location_w = 790.

let button_at (m : model) : string option =
  let mx, my = m.mouse in
  List.mapi (fun i (text, active) -> (i, text, active)) (buttons m)
  |> List.find_map (fun (i, text, active) ->
         if active && mx >= button_x i && mx <= button_x i +. button_w && Float.abs (my -. toolbar_y) <= 26. then Some text else None)

let on_location (m : model) : bool =
  let mx, my = m.mouse in
  mx >= location_x && mx <= location_x +. location_w && Float.abs (my -. location_y) <= 12.

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let init (network : < Cap.network ; .. >) (flags : flags) : model * msg Cmd.t =
  let target, fragment = split_fragment (Option.value (List.assoc_opt "url" flags) ~default:home) in
  load network target
    {
      state = Loading target;
      view = Page;
      scroll = 0;
      history = Browser_history.empty;
      visited = [ target ];
      fragment;
      pictures = [];
      queue = [];
      in_flight = [];
      total = 0;
      images = List.assoc_opt "images" flags <> Some "off";
      css = List.assoc_opt "css" flags <> Some "off";
      location = target;
      editing = false;
      fresh = false;
      mouse = (0., 0.);
      focus = None;
      time = 0.;
    }

(* a key while the Location field is typed into *)
let edit_location (network : < Cap.network ; .. >) (key : string) (m : model) : model * msg Cmd.t =
  match key with
  | "enter" | "return" -> visit network (typed_url m.location) { m with editing = false }
  | "escape" -> ({ m with editing = false; location = current_url m }, Cmd.none)
  | "backspace" ->
      let cs = characters m.location in
      let location = if m.fresh then "" else String.concat "" (List.filteri (fun i _ -> i < List.length cs - 1) cs) in
      ({ m with location; fresh = false }, Cmd.none)
  | _ -> (m, Cmd.none)

let update (network : < Cap.network ; .. >) (msg : msg) (m : model) : model * msg Cmd.t =
  match msg with
  | Got (_, Ok r) ->
      let content_type =
        List.find_map (fun (name, value) -> if String.lowercase_ascii name = "content-type" then Some value else None) r.headers
      in
      with_pictures network (to_fragment (shown m (page_of m r.url r.status content_type r.body)), Cmd.none)
  | Got (url, Error e) -> (failed m url (String.capitalize_ascii (Http.error_to_string e) ^ "."), Cmd.none)
  | Got_picture (url, result) when List.mem url m.in_flight ->
      let pic = match result with Ok r when r.status / 100 = 2 -> Browser_picture.decode r.body | _ -> Browser_picture.Broken in
      fetch_more network (with_arrived { m with in_flight = List.filter (( <> ) url) m.in_flight } url pic, Cmd.none)
  (* one Stop said not to wait for *)
  | Got_picture _ -> (m, Cmd.none)
  | Tick time -> ({ m with time }, Cmd.none)
  | Wheel notches -> (scrolled (3 * int_of_float (Float.round notches)) m, Cmd.none)
  | Mouse_move (x, y) -> ({ m with mouse = (x, y) }, Cmd.none)
  | Click -> (
      if on_location m then ({ m with editing = true; fresh = true; focus = None }, Cmd.none)
      else
        let m = { m with editing = false; location = (if m.editing then current_url m else m.location) } in
        match (button_at m, pointed_control m, hovered m, m.state) with
        | Some "Back", _, _, _ -> go_back network m
        | Some "Forward", _, _, _ -> go_forward network m
        | Some "Home", _, _, _ -> visit network home m
        | Some "Reload", _, _, _ -> load network (current_url m) m
        | Some "Images", _, _, _ -> with_pictures network ({ m with images = true }, Cmd.none)
        | Some "Open", _, _, _ -> ({ m with editing = true; fresh = true }, Cmd.none)
        | Some "Stop", _, _, _ ->
            (* not waiting any more: the pictures on their way forgotten *)
            let m = match m.state with Loading _ -> { m with state = Shown (page_of m (current_url m) 0 None (Browser_page.error_html (current_url m) "Stopped.")) } | Shown _ -> m in
            ({ m with queue = []; in_flight = []; total = 0 }, Cmd.none)
        | _, Some e, _, Shown p -> form_effect network ~keep_focus:false (Browser_forms.click p e) m
        | _, _, Some href, Shown p -> visit network (resolve p.url href) m
        | _ -> ({ m with focus = None }, Cmd.none))
  (* the Location field typed into *)
  | Typed s when m.editing -> ({ m with location = (if m.fresh then s else m.location ^ s); fresh = false }, Cmd.none)
  | Key key when m.editing -> edit_location network (String.lowercase_ascii key) m
  (* a form's field *)
  | Typed s when m.focus <> None -> (
      match (m.state, m.focus) with
      | Shown p, Some e -> ({ m with state = Shown (Browser_forms.typed p e s) }, Cmd.none)
      | _ -> (m, Cmd.none))
  | Key key when m.focus <> None && not (List.mem (String.lowercase_ascii key) [ "arrowdown"; "arrowup"; "pagedown"; "pageup" ]) -> (
      match (m.state, m.focus) with
      | Shown p, Some e -> form_effect network ~keep_focus:true (Browser_forms.key p e (String.lowercase_ascii key)) m
      | _ -> (m, Cmd.none))
  | Typed _ -> (m, Cmd.none)
  | Key key -> (
      match String.lowercase_ascii key with
      | "arrowdown" | "down" -> (scrolled 1 m, Cmd.none)
      | "arrowup" | "up" -> (scrolled (-1) m, Cmd.none)
      | "pagedown" | " " | "space" -> (scrolled (visible - 2) m, Cmd.none)
      | "pageup" -> (scrolled (-(visible - 2)) m, Cmd.none)
      | "b" | "backspace" -> go_back network m
      | "f" -> go_forward network m
      | "r" -> load network (current_url m) m
      | "h" -> visit network home m
      | "s" -> ({ m with view = Source; scroll = 0 }, Cmd.none)
      | "p" -> ({ m with view = Page; scroll = 0 }, Cmd.none)
      | "c" -> (
          (* claude: the style sheets off, or on again: the same tree
           * laid out again *)
          let m = { m with css = not m.css } in
          match m.state with Shown p -> ({ m with state = Shown (laid_out m p) }, Cmd.none) | Loading _ -> (m, Cmd.none))
      | _ -> (m, Cmd.none))

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* Netscape 1.0's colours: Motif's grey, the page's too (1.1 let a page
 * choose: <body bgcolor>, N3) *)
let grey = rgb 191 191 191
let dark_grey = rgb 120 120 120
let light_grey = rgb 235 235 235
let ink = rgb 0 0 0
let netscape_blue = rgb 0 32 128

(* text a character per cell, 6 wide, from [x] *)
let cell = 6.

let monospace (x : number) (y : number) (color : color) (s : string) : shape list =
  characters s
  |> List.mapi (fun i c -> (i, c))
  |> List.filter (fun (i, c) -> c <> " " && i < 160)
  |> List.map (fun (i, c) -> words color c |> move (x +. (cell *. float_of_int i) +. (cell /. 2.)) y)

(* a raised Motif panel, its centre at (x, y) *)
let panel (x : number) (y : number) (w : number) (h : number) : shape list =
  [ rectangle dark_grey w h |> move (x +. 1.) (y -. 1.); rectangle light_grey w h |> move (x -. 1.) (y +. 1.); rectangle grey (w -. 2.) (h -. 2.) |> move x y ]

(* a toolbar button's picture, centred at (x, y), in [c] *)
let icon (name : string) (c : color) (x : number) (y : number) : shape list =
  let at shapes = List.map (fun s -> s |> move x y) shapes in
  match name with
  | "Back" -> at [ polygon c [ (-9., 0.); (6., 8.); (6., -8.) ] ]
  | "Forward" -> at [ polygon c [ (9., 0.); (-6., 8.); (-6., -8.) ] ]
  | "Home" -> at [ polygon c [ (-9., 1.); (0., 9.); (9., 1.) ]; rectangle c 12. 9. |> move 0. (-4.) ]
  | "Reload" -> at [ circle c 8.; circle grey 5.; polygon c [ (5., 2.); (12., 2.); (8., 8.) ] ]
  | "Images" -> at [ rectangle c 18. 13.; polygon grey [ (-7., -5.); (-1., 3.); (4., -2.); (7., -5.) ] ]
  | "Open" -> at [ rectangle c 18. 12. |> move 0. (-1.); rectangle c 8. 3. |> move (-5.) 6. ]
  | "Print" -> at [ rectangle c 18. 8.; rectangle grey 10. 4. |> move 0. 6.; rectangle c 10. 1. |> move 0. 8. ]
  | "Find" -> at [ circle c 6. |> move (-2.) 2.; circle grey 3.5 |> move (-2.) 2.; rectangle c 7. 2.5 |> rotate (-45.) |> move 5. (-5.) ]
  | "Stop" ->
      let octagon = List.init 8 (fun i -> let a = (Float.pi /. 8.) +. (float_of_int i *. Float.pi /. 4.) in (9. *. cos a, 9. *. sin a)) in
      at [ polygon (if c = dark_grey then c else rgb 200 30 30) octagon ]
  | _ -> []

let toolbar (m : model) : shape list =
  List.concat
    (List.mapi
       (fun i (text, active) ->
         let x = button_x i +. (button_w /. 2.) and c = if active then ink else dark_grey in
         panel x toolbar_y button_w 50. @ icon text c x (toolbar_y +. 8.) @ [ words c text |> move x (toolbar_y -. 15.) ])
       (buttons m))

(* the "N", its meteors streaming across while something loads *)
let logo (m : model) : shape list =
  let loading = (match m.state with Loading _ -> true | Shown _ -> false) || m.in_flight <> [] in
  let x, y = (452., 394.) in
  let n = { Browser_text.root_look with size = 48.; bold = true; color = (255, 255, 255) } in
  let meteors =
    if not loading then []
    else
      List.init 4 (fun i ->
          let t = Float.rem ((m.time *. 1.5) +. (float_of_int i *. 0.25)) 1. in
          rectangle (rgb 255 255 200) 10. 2. |> rotate (-35.) |> move (x -. 34. +. (68. *. t)) (y +. 30. -. (60. *. t)))
  in
  [ rectangle netscape_blue 76. 76. |> move x y ]
  @ meteors
  @ (Browser_draw.glyphs { text = "N"; look = n; x = x -. 17.; width = 0.; baseline = 0.; picture = None; control = None; element = Dom.element "span" [] }
    |> List.map (fun s -> s |> move 0. (y -. 17.)))

(* the key: broken for http:// (anyone on the way can read), whole for
 * https:// (Netscape's SSL, 1994) *)
let key (m : model) (x : number) (y : number) : shape list =
  let secure = starts_with "https://" (current_url m) in
  let c = if secure then rgb 30 60 180 else dark_grey in
  [ circle c 6. |> move (x -. 8.) y; circle grey 3. |> move (x -. 8.) y; rectangle c 12. 3. |> move (x +. 3.) y;
    rectangle c 2. 5. |> move (x +. 7.) (y -. 3.) ]
  @ if secure then [] else [ rectangle grey 3. 7. |> rotate 30. |> move (x +. 1.) y ]

(* the status line: the link under the pointer, else how far it is *)
let status (m : model) : string =
  match (m.state, hovered m) with
  | Shown p, Some href -> resolve p.url href
  | Loading url, _ -> "Connect: Contacting host: " ^ url
  | Shown _, None when m.in_flight <> [] || m.queue <> [] ->
      Printf.sprintf "Transferring pictures: %d of %d" (m.total - List.length m.queue - List.length m.in_flight) m.total
  | Shown p, None when p.status = 0 -> "Failed: " ^ p.url
  | Shown _, None -> "Document: Done."

let progress (m : model) : float option =
  if m.total > 0 && (m.in_flight <> [] || m.queue <> []) then
    Some (float_of_int (m.total - List.length m.queue - List.length m.in_flight) /. float_of_int m.total)
  else None

let page_shapes (m : model) (p : page) : shape list =
  let scroll = float_of_int m.scroll *. line_height in
  (p.drawn @ Browser_draw.controls_drawn ~value:(Browser_page.value_of p) ~focus:m.focus p.layout)
  |> List.filter (fun (top, bottom, _) -> bottom > scroll && top < scroll +. area_height)
  |> List.map (fun (_, _, s) -> s)
  |> group
  |> move area_left (area_top +. scroll)
  |> fun s -> [ s ]

let view (m : model) : shape list =
  let body =
    match (m.state, m.view) with
    | Shown p, Page -> page_shapes m p
    | Shown p, Source ->
        List.filteri (fun i _ -> i >= m.scroll && i < m.scroll + visible) p.lines
        |> List.mapi (fun i line -> monospace (-480.) (area_top -. 12. -. (14. *. float_of_int i)) ink line)
        |> List.concat
    | Loading _, _ -> []
  in
  let title = match m.state with Shown p when p.title <> "" -> "Netscape - [" ^ p.title ^ "]" | _ -> "Netscape" in
  let location = if m.editing then m.location ^ "_" else m.location in
  (* claude: the page's own background, <body bgcolor> (Netscape 1.1) *)
  let background =
    match (m.state, m.view) with Shown { background = Some (r, g, b); _ }, Page -> rgb r g b | _ -> grey
  in
  [ rectangle grey 1000. 1000. ]
  (* the page, sunken, drawn first: the chrome covers what overflows *)
  @ [ rectangle dark_grey 982. (area_height +. 12.) |> move_y (area_top -. (area_height /. 2.)); rectangle background 980. (area_height +. 10.) |> move_y (area_top -. (area_height /. 2.)) ]
  @ body
  @ [ rectangle grey 1000. (500. -. area_top -. 6.) |> move_y ((500. +. area_top +. 6.) /. 2.);
      rectangle grey 1000. (500. -. area_height +. area_top -. 6.) |> move_y (-.((500. +. area_height -. area_top +. 6.) /. 2.)) ]
  (* the window manager's title bar *)
  @ [ rectangle (rgb 70 90 140) 1000. 20. |> move_y 490. ] @ monospace (-490.) 490. (rgb 255 255 255) title
  (* the menus *)
  @ List.concat
      (List.mapi (fun i menu -> monospace (-488. +. (70. *. float_of_int i)) 466. ink menu)
         [ "File"; "Edit"; "View"; "Go"; "Bookmarks"; "Options"; "Directory"; "Help" ])
  @ toolbar m
  (* the Location field *)
  @ monospace (-488.) location_y ink "Location:"
  @ [ rectangle dark_grey (location_w +. 2.) 22. |> move (location_x +. (location_w /. 2.)) location_y;
      rectangle (if m.editing then rgb 255 255 230 else rgb 255 255 255) location_w 20. |> move (location_x +. (location_w /. 2.)) location_y ]
  @ monospace (location_x +. 4.) location_y ink (Browser_text.tail 130 location)
  @ logo m
  (* the status bar *)
  @ key m (-472.) (-472.)
  @ monospace (-450.) (-472.) ink (status m)
  @ (match progress m with
    | Some f -> [ rectangle dark_grey 160. 12. |> move 400. (-472.); rectangle netscape_blue (158. *. f) 10. |> move (321. +. (79. *. f)) (-472.) ]
    | None -> [])

(*****************************************************************************)
(* The app *)
(*****************************************************************************)

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

(* claude: threads on unless the command line says otherwise (N2):
 * what blocks, on the platform's threads *)
let main =
  Cap.main (fun caps ->
      let flags = Playground_platform.flags () in
      let flags = if List.mem_assoc "threads" flags then flags else ("threads", "on") :: flags in
      Playground_platform.run_app ~flags (app caps))
