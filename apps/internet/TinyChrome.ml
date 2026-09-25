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
 *   -> Browser_boxes (backgrounds, borders, the words)
 *
 * where the teaching browsers have Mosaic's looks and Html_layout. The
 * tab is theirs (Browser_tab: the page, its history, its pictures), with
 * Browser_page's setting boxes on: a page's <link rel=stylesheet>s and
 * their @imports are then fetched with its pictures, ahead of them, and
 * the page laid out again as each arrives -- shown at once plain, then
 * dressed (Chrome waits a moment instead, to spare that flash).
 *
 * Chrome's window: the tabs on top, in the frame (one here, the page's
 * title); below, Back, Forward, Reload, and the omnibox (click it,
 * type an address, Return). A link's address shows in a bubble at the
 * bottom left when the pointer is on it, as Chrome's status bubble.
 * JavaScript is off on the web, as in Chrome with it disabled -- most
 * sites' scripts are more than our engine reads, and many sites are
 * written to work without (a <noscript> is then shown, as its content
 * is when nothing hides it) -- and on for the built-in pages (the
 * plan's C8: a few sites' too). The arrows, Page Up and Down and the
 * wheel scroll, Backspace goes back.
 *
 *   dune exec apps/internet/TinyChrome.exe
 *   http://localhost:8001/apps/internet/web/TinyChrome.html
 *
 * flags url= (about:chrome), the first page; css=off, the browser's
 * own sheet alone (what a page looks like unstyled).
 *
 * Uses: appkits/browser (the tab, the page, Browser_boxes, the
 * forms), libs/web (Cascade, Computed, Box_layout, and Hit through the
 * page's Html_layout view), the built-in site (Site). Its own: the
 * chrome.
 *
 * Tried live: Hacker News (its tables, attributes and news.css), a
 * Wikipedia article (its two sheets from load.php; its header and tabs
 * flex rows; its contents a grid column, here above the article),
 * Google's home page (its no-script version: a search needs
 * JavaScript), a GitHub repository (41 sheets; its file list's
 * messages written by its scripts, so missing).
 *
 * To come (plan_tiny_chrome.md): SVG (C6), several tabs, the omnibox's
 * search and the developer tools (C7), the ES5 core (C8), video (C9).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  tab : Browser_tab.t;
  omnibox : string; (* its text, while it is typed into *)
  editing : bool;
  fresh : bool; (* just clicked: typing replaces what is there *)
  mouse : float * float;
  time : float; (* the tab's throbber *)
  css : bool; (* the page's style sheets honoured *)
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

let home = "about:chrome"
let characters = Browser_text.characters
let resolve = Browser_url.resolve

(*****************************************************************************)
(* The window's geometry *)
(*****************************************************************************)

(* the page area, the whole window below the toolbar *)
let area_top = 414.
let area_left = -500.
let area_bottom = -500.
let area_height = area_top -. area_bottom
let page_width = 1000.
let line_height = 16.

(* the tab strip; the toolbar's buttons and the omnibox *)
let tab_y = 481.
let toolbar_y = 438.
let button_x (i : int) : float = -476. +. (38. *. float_of_int i)
let omnibox_x = -362.
let omnibox_w = 830.

(*****************************************************************************)
(* The tab: Chrome's settings *)
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

let config (m : model) : msg Browser_tab.config =
  {
    settings = settings m.css;
    about = Site.about;
    got = (fun url r -> Got (url, r));
    got_picture = (fun url r -> Got_picture (url, r));
    connections = 6;
    visible = int_of_float (area_height /. line_height);
    line_height;
    (* the built-in pages' scripts, not the web's (C8: a few sites') *)
    scripts = (fun url -> Browser_url.starts_with "about:" url);
    seed = 1;
  }

let with_tab (m : model) ((tab, cmd) : Browser_tab.t * msg Cmd.t) : model * msg Cmd.t = ({ m with tab }, cmd)
let current_url (m : model) : string = Browser_tab.current_url m.tab
let scrolled (by : int) (m : model) : model = { m with tab = Browser_tab.scrolled (config m) by m.tab }
let visit network url m = with_tab { m with editing = false } (Browser_tab.visit (config m) network url m.tab)
let load network url m = with_tab m (Browser_tab.load (config m) network url m.tab)

(* what is typed in the omnibox: an address (a scheme, or a host) *)
let typed_url (s : string) : string =
  let s = String.trim s in
  if String.contains s ':' then s else "https://" ^ s

(*****************************************************************************)
(* The pointer *)
(*****************************************************************************)

let page_point (m : model) : (float * float) option =
  let mx, my = m.mouse in
  if my <= area_top && my >= area_bottom then Some (mx -. area_left, area_top -. my +. (float_of_int m.tab.scroll *. line_height)) else None

let hovered (m : model) : string option =
  match (m.tab.state, page_point m) with Shown p, Some (x, y) -> Hit.link_at p.layout ~x ~y | _ -> None

let pointed_control (m : model) : Dom.element option =
  match (m.tab.state, page_point m) with
  | Shown p, Some (x, y) -> ( match Hit.fragment_at p.layout ~x ~y with Some { control = Some c; _ } -> Some c.element | _ -> None)
  | _ -> None

let loading (m : model) : bool = (match m.tab.state with Loading _ -> true | Shown _ -> false) || m.tab.in_flight <> [] || m.tab.queue <> []

let buttons (m : model) : (string * bool) list =
  [ ("Back", m.tab.history.behind <> []); ("Forward", m.tab.history.ahead <> []); ((if loading m then "Stop" else "Reload"), true) ]

let near (x0 : float) (y0 : float) (w : float) (h : float) (m : model) : bool =
  let mx, my = m.mouse in
  mx >= x0 && mx <= x0 +. w && Float.abs (my -. y0) <= h /. 2.

let button_at (m : model) : string option =
  List.mapi (fun i b -> (i, b)) (buttons m)
  |> List.find_map (fun (i, (text, active)) -> if active && near (button_x i -. 16.) toolbar_y 32. 32. m then Some text else None)

let on_omnibox (m : model) : bool = near omnibox_x toolbar_y omnibox_w 28. m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let init (network : < Cap.network ; .. >) (flags : flags) : model * msg Cmd.t =
  let target, fragment = Browser_url.split_fragment (Option.value (List.assoc_opt "url" flags) ~default:home) in
  let tab = { (Browser_tab.empty ~images:true) with visited = [ target ]; fragment } in
  let m =
    { tab; omnibox = target; editing = false; fresh = false; mouse = (1000., 1000.); time = 0.; css = List.assoc_opt "css" flags <> Some "off" }
  in
  load network target m

let backspace (s : string) : string =
  let cs = characters s in
  String.concat "" (List.filteri (fun i _ -> i < List.length cs - 1) cs)

let edit_omnibox (network : < Cap.network ; .. >) (key : string) (m : model) : model * msg Cmd.t =
  match key with
  | "enter" | "return" -> visit network (typed_url m.omnibox) m
  | "escape" -> ({ m with editing = false }, Cmd.none)
  | "backspace" -> ({ m with omnibox = (if m.fresh then "" else backspace m.omnibox); fresh = false }, Cmd.none)
  | _ -> (m, Cmd.none)

let form (network : < Cap.network ; .. >) ~(keep_focus : bool) (effect : Browser_forms.effect) (m : model) : model * msg Cmd.t =
  with_tab m (Browser_tab.form_effect (config m) network ~keep_focus effect m.tab)

let click_page (network : < Cap.network ; .. >) (m : model) : model * msg Cmd.t =
  match (pointed_control m, hovered m, m.tab.state) with
  | Some e, _, Shown p -> form network ~keep_focus:false (Browser_forms.click p e) m
  | _, Some href, Shown p -> visit network (resolve p.url href) m
  | _ -> ({ m with tab = { m.tab with focus = None } }, Cmd.none)

let pages (by : int) : int = by * (int_of_float (area_height /. line_height) - 2)

let update (network : < Cap.network ; .. >) (msg : msg) (m : model) : model * msg Cmd.t =
  let cfg = config m in
  match msg with
  | Got (url, r) -> with_tab m (Browser_tab.got cfg network url r m.tab)
  | Got_picture (url, r) -> with_tab m (Browser_tab.got_picture cfg network url r m.tab)
  | Tick time -> ({ m with time }, Cmd.none)
  | Wheel notches -> (scrolled (3 * int_of_float (Float.round notches)) m, Cmd.none)
  | Mouse_move (x, y) -> ({ m with mouse = (x, y) }, Cmd.none)
  | Click -> (
      let m = { m with editing = false } in
      if on_omnibox m then ({ m with editing = true; fresh = true; omnibox = current_url m; tab = { m.tab with focus = None } }, Cmd.none)
      else
        match button_at m with
        | Some "Back" -> with_tab m (Browser_tab.back cfg network m.tab)
        | Some "Forward" -> with_tab m (Browser_tab.forward cfg network m.tab)
        | Some "Reload" -> load network (current_url m) m
        | Some "Stop" -> ({ m with tab = Browser_tab.stop cfg m.tab }, Cmd.none)
        | _ -> if page_point m <> None then click_page network m else (m, Cmd.none))
  | Typed s when m.editing -> ({ m with omnibox = (if m.fresh then s else m.omnibox ^ s); fresh = false }, Cmd.none)
  | Key key when m.editing -> edit_omnibox network (String.lowercase_ascii key) m
  | Typed s when m.tab.focus <> None -> (
      match (m.tab.state, m.tab.focus) with
      | Shown p, Some e -> form network ~keep_focus:true (Changed (Browser_forms.typed p e s)) m
      | _ -> (m, Cmd.none))
  | Key key when m.tab.focus <> None && not (List.mem (String.lowercase_ascii key) [ "arrowdown"; "arrowup"; "pagedown"; "pageup" ]) -> (
      match (m.tab.state, m.tab.focus) with
      | Shown p, Some e -> form network ~keep_focus:true (Browser_forms.key p e (String.lowercase_ascii key)) m
      | _ -> (m, Cmd.none))
  | Typed _ -> (m, Cmd.none)
  | Key key -> (
      match String.lowercase_ascii key with
      | "arrowdown" | "down" -> (scrolled 2 m, Cmd.none)
      | "arrowup" | "up" -> (scrolled (-2) m, Cmd.none)
      | "pagedown" | " " -> (scrolled (pages 1) m, Cmd.none)
      | "pageup" -> (scrolled (pages (-1)) m, Cmd.none)
      | "home" -> (scrolled (-m.tab.scroll) m, Cmd.none)
      | "backspace" -> with_tab m (Browser_tab.back cfg network m.tab)
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
let cell = 6.

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

(* the tab: a trapezoid with the page's title; turning while loading *)
let tab (m : model) : shape list =
  let title = match m.tab.state with Shown p when p.title <> "" -> p.title | Shown p -> p.url | Loading _ -> "Loading..." in
  let angle = if loading m then m.time *. 360. else 0. in
  [ polygon toolbar [ (-490., tab_y -. 15.); (-476., tab_y +. 13.); (-266., tab_y +. 13.); (-252., tab_y -. 15.) ];
    group [ circle (rgb 66 133 244) 7.; rectangle toolbar 3. 8. |> move 0. 4. ] |> rotate angle |> move (-462.) tab_y ]
  @ monospace (-450.) tab_y ink (Browser_text.tail 29 title |> fun t -> if List.length (characters title) > 29 then t ^ "..." else t)
  @ [ rectangle (rgb 120 155 210) 22. 18. |> move (-236.) (tab_y -. 2.) ]

(* the status bubble: a link's address, or what is loading *)
let bubble (m : model) : shape list =
  let text =
    match (m.tab.state, hovered m) with
    | Shown p, Some href -> Some (resolve p.url href)
    | Loading url, _ -> Some ("Waiting for " ^ url ^ "...")
    | Shown _, None when List.exists (fun u -> List.mem u m.tab.sheet_urls) m.tab.in_flight -> Some "Loading style sheets..."
    | Shown _, None when m.tab.in_flight <> [] -> Some "Loading pictures..."
    | _ -> None
  in
  match text with
  | Some t ->
      let t = Browser_text.tail 100 t in
      let w = (cell *. float_of_int (List.length (characters t))) +. 12. in
      [ rectangle edge (w +. 2.) 20. |> move (-500. +. ((w +. 2.) /. 2.)) (-489.); rectangle toolbar w 18. |> move (-500. +. (w /. 2.)) (-489.) ]
      @ monospace (-494.) (-489.) ink t
  | None -> []

let page_shapes (m : model) (p : Browser_page.t) : shape list =
  let scroll = float_of_int m.tab.scroll *. line_height in
  (p.drawn @ Browser_draw.controls_drawn ~value:(Browser_page.value_of p) ~focus:m.tab.focus p.layout)
  |> List.filter (fun (top, bottom, _) -> bottom > scroll && top < scroll +. area_height)
  |> List.map (fun (_, _, s) -> s)
  |> group
  |> move area_left (area_top +. scroll)
  |> fun s -> [ s ]

let view (m : model) : shape list =
  let body = match m.tab.state with Shown p -> page_shapes m p | Loading _ -> [] in
  let background = match m.tab.state with Shown { background = Some (r, g, b); _ } -> rgb r g b | _ -> white in
  let omnibox = if m.editing then m.omnibox ^ "_" else current_url m in
  (* the address as Chrome shows it: the scheme and host dark, the rest
   * grey *)
  let host_end =
    match String.index_from_opt omnibox (min (String.length omnibox) (try String.index omnibox ':' + 3 with Not_found -> 0)) '/' with
    | Some i when not m.editing -> i
    | _ -> String.length omnibox
  in
  let shown = Browser_text.tail 136 omnibox in
  let dark = String.sub shown 0 (min (String.length shown) host_end) in
  [ rectangle background 1000. 1000. ]
  @ body
  (* the chrome over what overflows *)
  @ [ rectangle frame 1000. 44. |> move_y 478.;
      rectangle toolbar 1000. 42. |> move_y (area_top +. 21.);
      rectangle edge 1000. 1. |> move_y area_top ]
  @ tab m
  @ List.concat (List.mapi (fun i (name, active) -> icon name active (button_x i) toolbar_y) (buttons m))
  @ [ rectangle edge (omnibox_w +. 2.) 30. |> move (omnibox_x +. (omnibox_w /. 2.)) toolbar_y;
      rectangle white omnibox_w 28. |> move (omnibox_x +. (omnibox_w /. 2.)) toolbar_y ]
  @ monospace (omnibox_x +. 10.) toolbar_y muted shown
  @ monospace (omnibox_x +. 10.) toolbar_y ink dark
  (* the wrench: Chrome's one menu *)
  @ [ rectangle (rgb 70 90 120) 4. 18. |> rotate 45. |> move 482. toolbar_y; circle (rgb 70 90 120) 5. |> move 477. (toolbar_y +. 5.) ]
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
