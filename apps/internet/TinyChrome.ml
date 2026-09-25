(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Google Chrome (September 2008): the last of the
 * family, and the one meant for the web as it is.
 *
 * TinyMosaic, TinyNetscape and TinyFirefox show how a browser grew;
 * this one tries to show real pages mostly right, as TinyOffice is the
 * office grown whole (plan_tiny_chrome.md: which sites, and what they
 * ask). Its engine is CSS's own, from scratch in libs/web/style and
 * libs/web/layout:
 *
 *   bytes -> tree -> Cascade (the page's sheets over the browser's,
 *   ua.css; @media, var(), the attributes' hints) -> Computed (a record
 *   of values per element) -> Box_layout (CSS 2.1's box model: margins,
 *   borders, paddings, auto margins centring, collapsing margins,
 *   floats, inline-blocks shrunk to fit, positioning, tables, lists;
 *   and flexbox, Flex_layout)
 *   -> Browser_boxes (backgrounds, borders, the words, SVG)
 *
 * where the teaching browsers have Mosaic's looks and Html_layout. Each
 * tab is theirs (Browser_tab: the page, its history, its pictures), with
 * Browser_page's setting boxes on: a page's <link rel=stylesheet>s and
 * their @imports are then fetched with its pictures, ahead of them, and
 * the page laid out again as each arrives -- shown at once plain, then
 * dressed (Chrome waits a moment instead, to spare that flash).
 *
 * Chrome's window: the **tabs** on top, in the frame (click one to see
 * it, its x to close it, + for a new one), each a Browser_tab of its
 * own, its answers routed to it by its number; below, Back, Forward,
 * Reload, and the **omnibox** (click it, type, Return): an address, or
 * words -- then a search, Wikipedia's (search_url: the engines' pages
 * without scripts that answer a program; DuckDuckGo's, search=duckduckgo,
 * its links going through a <meta http-equiv=refresh>, which the tab
 * follows). A
 * link's address shows in a bubble at the bottom left, as Chrome's
 * status bubble. JavaScript is off on the web, as in Chrome with it
 * disabled -- most sites' scripts are more than our engine reads, and
 * many sites are written to work without (a <noscript> is then shown)
 * -- and on for the built-in pages (the plan's C8: a few sites' too).
 * The arrows, Page Up and Down, Space and the wheel scroll, Backspace
 * goes back.
 *
 * The **developer tools** (F12, or the wrench), after Chrome's Web
 * Inspector: Elements -- click Inspect, then an element of the page:
 * its place in the tree, its box (outlined on the page), its children,
 * and its styles, each declaration that won with the rule and the sheet
 * it came from (Browser_devtools, over Cascade.explain) -- and Network,
 * each request of the page, its status, kind, size and time, stamped
 * by this program's clock as the tab's log changes.
 *
 *   dune exec apps/internet/TinyChrome.exe
 *   http://localhost:8001/apps/internet/web/TinyChrome.html
 *
 * flags url= (about:chrome), the first page; css=off, the browser's
 * own sheet alone (what a page looks like unstyled); panel=elements or
 * panel=network, the tools open; search=duckduckgo, the omnibox's
 * engine (wikipedia).
 *
 * Uses: appkits/browser (the tab, the page, Browser_boxes,
 * Browser_devtools, the forms), libs/web (Cascade, Computed, Box_layout,
 * Flex_layout, and Hit through the page's Html_layout view),
 * graphics/images/svg (Svg) through Browser_boxes and Browser_picture,
 * the built-in site (Site). Its own: the chrome, the tabs, the panel.
 *
 * Tried live: Hacker News (its tables, attributes and news.css), a
 * Wikipedia article (its two sheets from load.php; its header and tabs
 * flex rows; its contents a grid column, here above the article),
 * Google's home page (its no-script version: a search needs
 * JavaScript), a GitHub repository (41 sheets; its file list's
 * messages written by its scripts, so missing), DuckDuckGo's searches.
 * Their logos and icons are SVG: HN's "Y" and vote arrows, Wikipedia's
 * wordmark and icons, GitHub's octicons.
 *
 * <video> and <audio> play (Browser_media, over TinyMediaPlayer's
 * readers: MPEG-1 and MP2, AVI, FLC, Y4M, GIF, MP3), and about:tube is
 * a video site of our own (Tube).
 *
 * To come (plan_tiny_chrome.md): speed (C10).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a tab, and when each of its requests started and ended (the
 * network panel's times, this program's clock) *)
type tab = { id : int; tab : Browser_tab.t; times : (string * (float * float option)) list }

type panel = Closed | Elements | Network

type model = {
  tabs : tab list;
  current : int; (* the id of the tab shown *)
  next_id : int;
  omnibox : string; (* its text, while it is typed into *)
  editing : bool;
  fresh : bool; (* just clicked: typing replaces what is there *)
  mouse : float * float;
  time : float;
  css : bool; (* the page's style sheets honoured *)
  panel : panel;
  inspecting : bool; (* the next click on the page picks an element *)
  selected : Dom.element option;
  engine : string; (* the omnibox's searches: search_url *)
  allowed : string list; (* the sites whose scripts run (hosts): Chrome's per-site setting *)
}

type msg =
  | Got of int * string * (Http.response, Http.error) result
  | Got_picture of int * string * (Http.response, Http.error) result
  | Tick of float
  | Key of string
  | Typed of string
  | Wheel of float
  | Mouse_move of float * float
  | Click

let home = "about:chrome"
let characters = Browser_text.characters
let resolve = Browser_url.resolve

(*****************************************************************************)
(* The window's geometry *)
(*****************************************************************************)

(* the page area, below the toolbar, above the panel if it is open *)
let area_top = 414.
let area_left = -500.
let area_bottom (m : model) : float = if m.panel = Closed then -500. else -130.
let area_height (m : model) : float = area_top -. area_bottom m
let page_width = 1000.
let line_height = 16.

(* the tab strip; the toolbar's buttons and the omnibox *)
let tab_y = 481.
let tab_left = -490.
let toolbar_y = 438.
let button_x (i : int) : float = -476. +. (38. *. float_of_int i)
let omnibox_x = -362.
let omnibox_w = 820.
(* the omnibox's "JS", the page's scripts on (blue) or off (grey) *)
let js_x = omnibox_x +. omnibox_w -. 30.
let wrench_x = 478.

(* the panel: its header, its two views' names, the Inspect button *)
let panel_top = -130.
let panel_header_y = -142.
let cell = 6.

(* the tabs' width, sharing the strip *)
let tab_width (m : model) : float = Float.min 220. (900. /. float_of_int (max 1 (List.length m.tabs)))
let tab_x (m : model) (i : int) : float = tab_left +. (float_of_int i *. (tab_width m +. 2.))

(*****************************************************************************)
(* The tabs: Chrome's settings *)
(*****************************************************************************)

let settings (css : bool) (tab : Browser_tab.t) : Browser_page.settings =
  {
    extensions = true;
    css;
    (* CSS 2.1's box model: Cascade, Computed, Box_layout *)
    boxes = true;
    width = page_width;
    breaker = Html_layout.greedy;
    visited = (fun url -> List.mem url tab.visited);
    picture = (fun url -> List.assoc_opt url tab.pictures);
    sheet = (fun url -> List.assoc_opt url tab.sheets);
  }

(* "news.ycombinator.com" of https://news.ycombinator.com/item?id=1 *)
let host_of (url : string) : string =
  match String.index_opt url ':' with
  | Some i when i + 3 <= String.length url && String.sub url (i + 1) 2 = "//" ->
      let rest = String.sub url (i + 3) (String.length url - i - 3) in
      let stop = List.fold_left (fun m c -> match String.index_opt rest c with Some j -> min m j | None -> m) (String.length rest) [ '/'; '?'; '#'; ':' ] in
      String.sub rest 0 stop
  | _ -> ""

(* the sites whose scripts are small and old enough for our engine
 * (plan_tiny_chrome.md, "Famous sites with simple scripts") *)
let default_allowed = [ "news.ycombinator.com" ]

let config (m : model) (id : int) : msg Browser_tab.config =
  {
    settings = settings m.css;
    (* the built-in site, and TinyTube in it *)
    about = (fun name -> match Tube.about name with Some x -> Some x | None -> Site.about name);
    got = (fun url r -> Got (id, url, r));
    got_picture = (fun url r -> Got_picture (id, url, r));
    connections = 6;
    visible = int_of_float (area_height m /. line_height);
    line_height;
    (* the built-in pages' scripts, and the allowed sites' *)
    scripts = (fun url -> Browser_url.starts_with "about:" url || List.mem (host_of url) m.allowed);
    seed = 1;
  }

let current (m : model) : tab = match List.find_opt (fun t -> t.id = m.current) m.tabs with Some t -> t | None -> List.hd m.tabs
let current_tab (m : model) : Browser_tab.t = (current m).tab

(* a tab's requests given times: a new one its start, an answered one
 * its end *)
let stamp (time : float) (t : tab) : tab =
  let times =
    List.map
      (fun (r : Browser_tab.request) ->
        match List.assoc_opt r.url t.times with
        | Some (start, stop) ->
            (* asked for before the clock's first tick: from its first *)
            let start = if start = 0. && time > 0. && stop = None then time else start in
            (r.url, (start, match (stop, r.status) with Some e, _ -> Some e | None, Some _ -> Some time | None, None -> None))
        | None -> (r.url, (time, if r.status = None then None else Some time)))
      t.tab.requests
  in
  { t with times }

(* the tab [id] changed by [f] (its commands its own) *)
let on_tab (m : model) (id : int) (f : msg Browser_tab.config -> Browser_tab.t -> Browser_tab.t * msg Cmd.t) : model * msg Cmd.t =
  match List.find_opt (fun t -> t.id = id) m.tabs with
  | None -> (m, Cmd.none)
  | Some t ->
      let tab, cmd = f (config m id) t.tab in
      let t = stamp m.time { t with tab } in
      ({ m with tabs = List.map (fun t' -> if t'.id = id then t else t') m.tabs }, cmd)

let on_current m f = on_tab m m.current f
let current_url (m : model) : string = Browser_tab.current_url (current_tab m)
let scrolled (by : int) (m : model) : model = fst (on_current m (fun cfg tab -> (Browser_tab.scrolled cfg by tab, Cmd.none)))
let visit network url m = on_current { m with editing = false; selected = None } (fun cfg tab -> Browser_tab.visit cfg network url tab)
let load network url m = on_current m (fun cfg tab -> Browser_tab.load cfg network url tab)

(* a new tab, shown, loading [url] *)
let open_tab (network : < Cap.network ; .. >) (url : string) (m : model) : model * msg Cmd.t =
  let target, fragment = Browser_url.split_fragment url in
  let tab = { (Browser_tab.empty ~images:true) with visited = [ target ]; fragment } in
  let m = { m with tabs = m.tabs @ [ { id = m.next_id; tab; times = [] } ]; current = m.next_id; next_id = m.next_id + 1; selected = None } in
  load network target m

let close_tab (network : < Cap.network ; .. >) (id : int) (m : model) : model * msg Cmd.t =
  match List.filter (fun t -> t.id <> id) m.tabs with
  | [] -> open_tab network home { m with tabs = [] }
  | rest ->
      let current = if m.current = id then (List.nth rest (max 0 (List.length rest - 1))).id else m.current in
      ({ m with tabs = rest; current; selected = None }, Cmd.none)

(* where words typed in the omnibox are searched: an engine whose page
 * works without scripts and answers a program -- Wikipedia's (the
 * default: Google's needs JavaScript, and DuckDuckGo's page without
 * scripts, like Mojeek's, soon takes a program asking again and again
 * for a robot and asks it to pick ducks), or DuckDuckGo's
 * (search=duckduckgo) *)
let search_url (engine : string) (words : string) : string =
  match engine with
  | "duckduckgo" -> "https://html.duckduckgo.com/html/?" ^ Urlencoded.encode [ ("q", words) ]
  | _ -> "https://en.wikipedia.org/w/index.php?" ^ Urlencoded.encode [ ("search", words) ]

(* what is typed in the omnibox: an address (a scheme, or a host with a
 * dot), else words searched *)
let typed_url (engine : string) (s : string) : string =
  let s = String.trim s in
  if String.contains s ':' && not (String.contains s ' ') then s
  else if String.contains s '.' && not (String.contains s ' ') then "https://" ^ s
  else search_url engine s

(*****************************************************************************)
(* The pointer *)
(*****************************************************************************)

let page_point (m : model) : (float * float) option =
  let mx, my = m.mouse in
  if my <= area_top && my >= area_bottom m then Some (mx -. area_left, area_top -. my +. (float_of_int (current_tab m).scroll *. line_height))
  else None

let hovered (m : model) : string option =
  match ((current_tab m).state, page_point m) with Shown p, Some (x, y) -> Hit.link_at p.layout ~x ~y | _ -> None

let pointed_control (m : model) : Dom.element option =
  match ((current_tab m).state, page_point m) with
  | Shown p, Some (x, y) -> ( match Hit.fragment_at p.layout ~x ~y with Some { control = Some c; _ } -> Some c.element | _ -> None)
  | _ -> None

let loading (tab : Browser_tab.t) : bool = (match tab.state with Loading _ -> true | Shown _ -> false) || tab.in_flight <> [] || tab.queue <> []

let buttons (m : model) : (string * bool) list =
  let tab = current_tab m in
  [ ("Back", tab.history.behind <> []); ("Forward", tab.history.ahead <> []); ((if loading tab then "Stop" else "Reload"), true) ]

let near (x0 : float) (y0 : float) (w : float) (h : float) (m : model) : bool =
  let mx, my = m.mouse in
  mx >= x0 && mx <= x0 +. w && Float.abs (my -. y0) <= h /. 2.

let button_at (m : model) : string option =
  List.mapi (fun i b -> (i, b)) (buttons m)
  |> List.find_map (fun (i, (text, active)) -> if active && near (button_x i -. 16.) toolbar_y 32. 32. m then Some text else None)

let on_omnibox (m : model) : bool = near omnibox_x toolbar_y omnibox_w 28. m

(* the tab strip: a tab's close box, a tab, the + *)
type strip = Close_tab of int | Show_tab of int | New_tab

let strip_at (m : model) : strip option =
  let w = tab_width m in
  let on_tab =
    List.mapi (fun i t -> (i, t)) m.tabs
    |> List.find_map (fun (i, t) ->
           let x = tab_x m i in
           if near (x +. w -. 26.) tab_y 18. 20. m then Some (Close_tab t.id) else if near x tab_y w 28. m then Some (Show_tab t.id) else None)
  in
  match on_tab with Some _ -> on_tab | None -> if near (tab_x m (List.length m.tabs)) tab_y 26. 26. m then Some New_tab else None

(* the panel's header: its views' names and Inspect *)
let panel_button (m : model) : string option =
  if m.panel = Closed then None
  else if near (-490.) panel_header_y 60. 16. m then Some "Inspect"
  else if near (-410.) panel_header_y 60. 16. m then Some "Elements"
  else if near (-330.) panel_header_y 60. 16. m then Some "Network"
  else None

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let init (network : < Cap.network ; .. >) (flags : flags) : model * msg Cmd.t =
  let url = Option.value (List.assoc_opt "url" flags) ~default:home in
  let panel = match List.assoc_opt "panel" flags with Some "elements" -> Elements | Some "network" -> Network | _ -> Closed in
  let m =
    { tabs = []; current = 0; next_id = 0; omnibox = url; editing = false; fresh = false; mouse = (1000., 1000.); time = 0.;
      css = List.assoc_opt "css" flags <> Some "off"; panel; inspecting = false; selected = None;
      engine = Option.value (List.assoc_opt "search" flags) ~default:"wikipedia";
      allowed = (match List.assoc_opt "scripts" flags with Some "off" -> [] | Some hosts -> String.split_on_char ',' hosts | None -> default_allowed) }
  in
  let m, cmd = open_tab network url m in
  (* with the elements' view open, the page's <body> shown in it *)
  let selected = match (panel, (current_tab m).state) with Elements, Shown p -> List.nth_opt (Dom.find_all "body" p.tree) 0 | _ -> None in
  ({ m with selected }, cmd)

let backspace (s : string) : string =
  let cs = characters s in
  String.concat "" (List.filteri (fun i _ -> i < List.length cs - 1) cs)

let edit_omnibox (network : < Cap.network ; .. >) (key : string) (m : model) : model * msg Cmd.t =
  match key with
  | "enter" | "return" -> visit network (typed_url m.engine m.omnibox) m
  | "escape" -> ({ m with editing = false }, Cmd.none)
  | "backspace" -> ({ m with omnibox = (if m.fresh then "" else backspace m.omnibox); fresh = false }, Cmd.none)
  | _ -> (m, Cmd.none)

let form (network : < Cap.network ; .. >) ~(keep_focus : bool) (effect : Browser_forms.effect) (m : model) : model * msg Cmd.t =
  on_current m (fun cfg tab -> Browser_tab.form_effect cfg network ~keep_focus effect tab)

(* a task of the page's scripts done by [f], then the page laid out
 * again if its tree changed (Browser_tab.after_task) *)
let task (network : < Cap.network ; .. >) (m : model) (f : Browser_script.t -> bool) : model * msg Cmd.t * bool =
  match (current_tab m).script with
  | Some s ->
      let r = f s in
      let m, cmd = on_current m (fun cfg tab -> Browser_tab.after_task cfg network tab) in
      (m, cmd, r)
  | None -> (m, Cmd.none, false)

let click_page (network : < Cap.network ; .. >) (m : model) : model * msg Cmd.t =
  match ((current_tab m).state, page_point m) with
  | Shown p, Some (x, y) when m.inspecting -> ({ m with inspecting = false; selected = Hit.element_at p.layout ~x ~y }, Cmd.none)
  (* a player: played or paused *)
  | Shown p, Some (x, y)
    when (match Hit.fragment_at p.layout ~x ~y with
         | Some f -> Browser_media.click ~now:m.time ~media:(fun u -> List.assoc_opt u (current_tab m).media) p f.element
         | None -> false) ->
      (m, Cmd.none)
  | Shown p, Some (x, y) -> (
      (* the page's scripts first (the element under the pointer, its
       * click bubbling); then, unless one prevented it, the browser's *)
      let control = pointed_control m and link = hovered m in
      let m, cmd, prevented = task network m (fun s -> match Hit.element_at p.layout ~x ~y with Some e -> Browser_script.click s e | None -> false) in
      if prevented then (m, cmd)
      else
        let m, cmd2 =
          match (control, link, (current_tab m).state) with
          | Some e, _, Shown p -> form network ~keep_focus:false (Browser_forms.click p e) m
          | _, Some href, Shown p -> visit network (resolve p.url href) m
          | _ -> on_current m (fun _ tab -> ({ tab with focus = None }, Cmd.none))
        in
        (m, Cmd.batch [ cmd; cmd2 ]))
  | _ -> (m, Cmd.none)

let pages (m : model) (by : int) : int = by * (int_of_float (area_height m /. line_height) - 2)

let toggle_panel (m : model) : model = { m with panel = (if m.panel = Closed then Elements else Closed); inspecting = false }

let update (network : < Cap.network ; .. >) (msg : msg) (m : model) : model * msg Cmd.t =
  Browser_media.install ();
  match msg with
  | Got (id, url, r) -> on_tab m id (fun cfg tab -> Browser_tab.got cfg network url r tab)
  | Got_picture (id, url, r) -> on_tab m id (fun cfg tab -> Browser_tab.got_picture cfg network url r tab)
  | Tick time ->
      (* the shown tab's timers on the frame clock (the others wait, as
       * Chrome slows a hidden tab's) *)
      let m, cmd, _ = task network { m with time } (fun s -> Browser_script.advance s (1000. /. 60.); false) in
      (m, cmd)
  | Wheel notches -> (scrolled (3 * int_of_float (Float.round notches)) m, Cmd.none)
  | Mouse_move (x, y) -> ({ m with mouse = (x, y) }, Cmd.none)
  | Click -> (
      let m = { m with editing = false } in
      if on_omnibox m then
        on_current { m with editing = true; fresh = true; omnibox = current_url m } (fun _ tab -> ({ tab with focus = None }, Cmd.none))
      else if near (wrench_x -. 12.) toolbar_y 24. 28. m then (toggle_panel m, Cmd.none)
      else if near js_x toolbar_y 22. 20. m then
        (* the site's scripts on or off, and the page loaded again *)
        let host = host_of (current_url m) in
        let allowed = if List.mem host m.allowed then List.filter (( <> ) host) m.allowed else host :: m.allowed in
        load network (current_url m) { m with allowed }
      else
        match (strip_at m, panel_button m, button_at m) with
        | Some (Close_tab id), _, _ -> close_tab network id m
        | Some (Show_tab id), _, _ -> ({ m with current = id; selected = None; inspecting = false }, Cmd.none)
        | Some New_tab, _, _ -> open_tab network home m
        | None, Some "Inspect", _ -> ({ m with inspecting = not m.inspecting }, Cmd.none)
        | None, Some "Elements", _ -> ({ m with panel = Elements }, Cmd.none)
        | None, Some "Network", _ -> ({ m with panel = Network; inspecting = false }, Cmd.none)
        | None, _, Some "Back" -> on_current m (fun cfg tab -> Browser_tab.back cfg network tab)
        | None, _, Some "Forward" -> on_current m (fun cfg tab -> Browser_tab.forward cfg network tab)
        | None, _, Some "Reload" -> load network (current_url m) m
        | None, _, Some "Stop" -> on_current m (fun cfg tab -> (Browser_tab.stop cfg tab, Cmd.none))
        | _ -> if page_point m <> None then click_page network m else (m, Cmd.none))
  | Typed s when m.editing -> ({ m with omnibox = (if m.fresh then s else m.omnibox ^ s); fresh = false }, Cmd.none)
  | Key key when m.editing -> edit_omnibox network (String.lowercase_ascii key) m
  | Typed s when (current_tab m).focus <> None -> (
      match ((current_tab m).state, (current_tab m).focus) with
      | Shown p, Some e -> form network ~keep_focus:true (Changed (Browser_forms.typed p e s)) m
      | _ -> (m, Cmd.none))
  | Key key when (current_tab m).focus <> None && not (List.mem (String.lowercase_ascii key) [ "arrowdown"; "arrowup"; "pagedown"; "pageup" ]) -> (
      match ((current_tab m).state, (current_tab m).focus) with
      | Shown p, Some e -> form network ~keep_focus:true (Browser_forms.key p e (String.lowercase_ascii key)) m
      | _ -> (m, Cmd.none))
  | Typed _ -> (m, Cmd.none)
  | Key key -> (
      match String.lowercase_ascii key with
      | "arrowdown" | "down" -> (scrolled 2 m, Cmd.none)
      | "arrowup" | "up" -> (scrolled (-2) m, Cmd.none)
      | "pagedown" | " " | "space" -> (scrolled (pages m 1) m, Cmd.none)
      | "pageup" -> (scrolled (pages m (-1)) m, Cmd.none)
      | "home" -> (scrolled (-(current_tab m).scroll) m, Cmd.none)
      | "f12" -> (toggle_panel m, Cmd.none)
      | "backspace" -> on_current m (fun cfg tab -> Browser_tab.back cfg network tab)
      | _ -> (m, Cmd.none))

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* Chrome 1.0 on Windows: the blue frame, the tab and toolbar light *)
let frame = rgb 91 132 196
let toolbar = rgb 234 240 250
let edge = rgb 160 176 204
let white = rgb 255 255 255
let ink = rgb 32 33 36
let muted = rgb 120 124 130
let inspector_blue = rgb 66 133 244

let monospace ?(max = 160) (x : number) (y : number) (color : color) (s : string) : shape list =
  characters s
  |> List.mapi (fun i c -> (i, c))
  |> List.filter (fun (i, c) -> c <> " " && i < max)
  |> List.map (fun (i, c) -> words color c |> move (x +. (cell *. float_of_int i) +. (cell /. 2.)) y)

(* the toolbar's pictures, Chrome's flat arrows *)
let icon (name : string) (active : bool) (x : number) (y : number) : shape list =
  let c = if active then rgb 70 90 120 else rgb 180 186 196 in
  let at shapes = List.map (fun s -> s |> move x y) shapes in
  match name with
  | "Back" -> at [ polygon c [ (-9., 0.); (1., 9.); (1., -9.) ]; rectangle c 8. 5. |> move 4. 0. ]
  | "Forward" -> at [ polygon c [ (9., 0.); (-1., 9.); (-1., -9.) ]; rectangle c 8. 5. |> move (-4.) 0. ]
  | "Reload" -> at [ circle c 9.; circle toolbar 5.; rectangle toolbar 6. 6. |> move 5. 5.; polygon c [ (2., 3.); (10., 3.); (6., 10.) ] ]
  | "Stop" -> at [ rectangle c 16. 3. |> rotate 45.; rectangle c 16. 3. |> rotate (-45.) ]
  | _ -> []

(* the tabs: trapezoids with their pages' titles, the one shown light,
 * a spinner while one loads; then + *)
let tabs (m : model) : shape list =
  let w = tab_width m in
  let chars = int_of_float ((w -. 60.) /. cell) in
  List.concat
    (List.mapi
       (fun i t ->
         let x = tab_x m i in
         let shown = t.id = m.current in
         let title = match t.tab.state with Shown p when p.title <> "" -> p.title | Shown p -> p.url | Loading _ -> "Loading..." in
         let title = if List.length (characters title) > chars then Browser_text.tail chars title else title in
         let angle = if loading t.tab then m.time *. 360. else 0. in
         [ polygon (if shown then toolbar else rgb 168 192 228) [ (x, tab_y -. 15.); (x +. 12., tab_y +. 13.); (x +. w -. 12., tab_y +. 13.); (x +. w, tab_y -. 15.) ];
           group [ circle (rgb 66 133 244) 7.; rectangle (if shown then toolbar else rgb 168 192 228) 3. 8. |> move 0. 4. ] |> rotate angle |> move (x +. 26.) tab_y ]
         @ monospace (x +. 38.) tab_y ink title
         @ [ rectangle muted 9. 2. |> rotate 45. |> move (x +. w -. 17.) tab_y; rectangle muted 9. 2. |> rotate (-45.) |> move (x +. w -. 17.) tab_y ])
       m.tabs)
  @
  let x = tab_x m (List.length m.tabs) in
  [ rectangle (rgb 120 155 210) 22. 18. |> move (x +. 13.) (tab_y -. 2.); rectangle white 10. 2. |> move (x +. 13.) (tab_y -. 2.);
    rectangle white 2. 10. |> move (x +. 13.) (tab_y -. 2.) ]

(* the status bubble: a link's address, or what is loading *)
let bubble (m : model) : shape list =
  let tab = current_tab m in
  let text =
    match (tab.state, hovered m) with
    | _, _ when m.inspecting -> Some "Inspect: click an element of the page"
    | Shown p, Some href -> Some (resolve p.url href)
    | Loading url, _ -> Some ("Waiting for " ^ url ^ "...")
    | Shown _, None when List.exists (fun u -> List.mem u tab.sheet_urls) tab.in_flight -> Some "Loading style sheets..."
    | Shown _, None when tab.in_flight <> [] -> Some "Loading pictures..."
    | _ -> None
  in
  let y = area_bottom m +. 11. in
  match text with
  | Some t ->
      let t = Browser_text.tail 100 t in
      let w = (cell *. float_of_int (List.length (characters t))) +. 12. in
      [ rectangle edge (w +. 2.) 20. |> move (-500. +. ((w +. 2.) /. 2.)) y; rectangle toolbar w 18. |> move (-500. +. (w /. 2.)) y ]
      @ monospace (-494.) y ink t
  | None -> []

(* the page, and the element inspected outlined on it *)
let page_shapes (m : model) (p : Browser_page.t) : shape list =
  let tab = current_tab m in
  let scroll = float_of_int tab.scroll *. line_height in
  let outline =
    match (m.panel, m.selected) with
    | Elements, Some e -> (
        match Browser_devtools.box_of p e with
        | Some (x, y, w, h) ->
            [ (y, y +. h, group [ rectangle inspector_blue w h |> fade 0.18 |> move (x +. (w /. 2.)) (-.(y +. (h /. 2.))); Browser_draw.frame inspector_blue x y w h ]) ]
        | None -> [])
    | _ -> []
  in
  (p.drawn
  @ Browser_draw.controls_drawn ~value:(Browser_page.value_of p) ~focus:tab.focus p.layout
  (* what plays in its <video>s and <audio>s, drawn at each frame *)
  @ Browser_media.draw ~now:m.time ~media:(fun u -> List.assoc_opt u tab.media) p
  @ outline)
  |> List.filter (fun (top, bottom, _) -> bottom > scroll && top < scroll +. area_height m)
  |> List.map (fun (_, _, s) -> s)
  |> group
  |> move area_left (area_top +. scroll)
  |> fun s -> [ s ]

(* the developer tools: the header, then the view's lines *)
let panel (m : model) : shape list =
  let tab = current_tab m in
  let lines ~x ~max (ls : Browser_devtools.line list) =
    let rows = int_of_float ((panel_top -. 20. +. 500.) /. 14.) - 1 in
    List.concat
      (List.mapi
         (fun i ((text, (r, g, b)) : Browser_devtools.line) -> if i >= rows then [] else monospace ~max x (panel_header_y -. 18. -. (14. *. float_of_int i)) (rgb r g b) text)
         ls)
  in
  let header name x active = [ rectangle (if active then white else toolbar) 60. 16. |> move (x +. 30.) panel_header_y ] @ monospace (x +. 4.) panel_header_y ink name in
  let body =
    match (m.panel, tab.state, m.selected) with
    | Network, _, _ -> lines ~x:(-490.) ~max:160 (Browser_devtools.network tab.requests ~times:(fun url -> List.assoc_opt url (current m).times))
    | Elements, Shown p, Some e ->
        lines ~x:(-490.) ~max:80 (Browser_devtools.element p e)
        @ [ rectangle edge 1. (panel_top +. 500. -. 20.) |> move 0. ((panel_top -. 520.) /. 2.) ]
        @ lines ~x:6. ~max:80 (Browser_devtools.styles (settings m.css tab) p e)
    | Elements, _, _ -> lines ~x:(-490.) ~max:120 [ ("Click Inspect, then an element of the page.", (110, 110, 110)) ]
    | Closed, _, _ -> []
  in
  [ rectangle (rgb 250 250 250) 1000. (panel_top +. 500.) |> move_y ((panel_top -. 500.) /. 2.); rectangle edge 1000. 1. |> move_y panel_top;
    rectangle toolbar 1000. 22. |> move_y panel_header_y ]
  @ [ rectangle (if m.inspecting then inspector_blue else toolbar) 60. 16. |> move (-460.) panel_header_y ]
  @ monospace (-486.) panel_header_y (if m.inspecting then white else ink) "Inspect"
  @ header "Elements" (-410.) (m.panel = Elements)
  @ header "Network" (-330.) (m.panel = Network)
  @ body

let view (m : model) : shape list =
  let tab = current_tab m in
  let body = match tab.state with Shown p -> page_shapes m p | Loading _ -> [] in
  let background = match tab.state with Shown { background = Some (r, g, b); _ } -> rgb r g b | _ -> white in
  let omnibox = if m.editing then m.omnibox ^ "_" else current_url m in
  (* the address as Chrome shows it: the scheme and host dark, the rest
   * grey *)
  let host_end =
    match String.index_from_opt omnibox (min (String.length omnibox) (try String.index omnibox ':' + 3 with Not_found -> 0)) '/' with
    | Some i when not m.editing -> i
    | _ -> String.length omnibox
  in
  let shown = Browser_text.tail 128 omnibox in
  let dark = String.sub shown 0 (min (String.length shown) host_end) in
  [ rectangle background 1000. 1000. ]
  @ body
  (* the chrome over what overflows *)
  @ [ rectangle frame 1000. 44. |> move_y 478.;
      rectangle toolbar 1000. 42. |> move_y (area_top +. 21.);
      rectangle edge 1000. 1. |> move_y area_top ]
  @ tabs m
  @ List.concat (List.mapi (fun i (name, active) -> icon name active (button_x i) toolbar_y) (buttons m))
  @ [ rectangle edge (omnibox_w +. 2.) 30. |> move (omnibox_x +. (omnibox_w /. 2.)) toolbar_y;
      rectangle white omnibox_w 28. |> move (omnibox_x +. (omnibox_w /. 2.)) toolbar_y ]
  @ monospace (omnibox_x +. 10.) toolbar_y muted shown
  @ monospace (omnibox_x +. 10.) toolbar_y ink dark
  @ (let on = tab.script <> None in
     [ rectangle (if on then inspector_blue else rgb 200 204 210) 22. 16. |> move (js_x +. 11.) toolbar_y ]
     @ monospace (js_x +. 5.) toolbar_y white "JS")
  (* the wrench: Chrome's one menu, here the developer tools *)
  @ [ rectangle (if m.panel <> Closed then inspector_blue else rgb 70 90 120) 4. 18. |> rotate 45. |> move wrench_x toolbar_y;
      circle (if m.panel <> Closed then inspector_blue else rgb 70 90 120) 5. |> move (wrench_x -. 5.) (toolbar_y +. 5.) ]
  @ (if m.panel <> Closed then panel m else [])
  @ bubble m

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

(* threads on, as in TinyNetscape (N2): a name resolved, an https://
 * page fetched, on the platform's threads *)
let main =
  Cap.main (fun caps ->
      let flags = Playground_platform.flags () in
      let flags = if List.mem_assoc "threads" flags then flags else ("threads", "on") :: flags in
      Playground_platform.run_app ~flags (app caps))
