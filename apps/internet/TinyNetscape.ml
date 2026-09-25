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
 * history; forms; the tab, Browser_tab: the page, its history, its
 * pictures four at a time -- shared with TinyFirefox), and with
 * TinyMosaic the built-in site (Site); web's Hit; Playground.Http. Its
 * own: the chrome, and Netscape's settings.
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

type model = {
  tab : Browser_tab.t; (* the page, its history, its pictures: Browser_tab *)
  css : bool; (* the pages' style sheets honoured (N5) *)
  location : string; (* the Location field's text, while it is typed into *)
  editing : bool; (* typing into it *)
  fresh : bool; (* just clicked: what is there is selected, typing replaces it *)
  mouse : float * float;
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

(*****************************************************************************)
(* The tab: Netscape's settings *)
(*****************************************************************************)

let page_width = 976.
let visible = 56
let line_height = 14.

let settings (m : model) (tab : Browser_tab.t) : Browser_page.settings =
  {
    (* claude: Netscape's own extensions to HTML (N3) *)
    extensions = true;
    (* claude: CSS1, Netscape 4's (N5); c or css=off for the page
     * without its style sheets *)
    css = m.css;
    width = page_width;
    boxes = false;
    breaker = Html_layout.greedy;
    visited = (fun url -> List.mem url tab.visited);
    picture = (fun url -> List.assoc_opt url tab.pictures);
    sheet = (fun _ -> None);
  }

(* Netscape's four connections at once; no scripts (Netscape 2's, 1995,
 * are TinyFirefox's) *)
let config (m : model) : msg Browser_tab.config =
  {
    settings = settings m;
    about = Site.about;
    got = (fun url r -> Got (url, r));
    got_picture = (fun url r -> Got_picture (url, r));
    connections = 4;
    visible;
    line_height;
    scripts = (fun _ -> false);
    seed = 1;
  }

let with_tab (m : model) ((tab, cmd) : Browser_tab.t * msg Cmd.t) : model * msg Cmd.t = ({ m with tab }, cmd)
let current_url (m : model) : string = Browser_tab.current_url m.tab
let scrolled (by : int) (m : model) : model = { m with tab = Browser_tab.scrolled (config m) by m.tab }
let visit network url m = with_tab { m with editing = false } (Browser_tab.visit (config m) network url m.tab)
let load network url m = with_tab m (Browser_tab.load (config m) network url m.tab)

(* what was typed into the Location field, as a URL: one with no
 * scheme is taken for a site's name, as Netscape did ("info.cern.ch") *)
let typed_url (s : string) : string =
  let s = String.trim s in
  if String.contains s ':' then s else "http://" ^ s

(*****************************************************************************)
(* The pointer *)
(*****************************************************************************)

(* the page area: its top and left edges, its height *)
let area_top = 352.
let area_left = -488.
let area_height = 792.

let page_point (m : model) : (float * float) option =
  let mx, my = m.mouse in
  if m.tab.view = Page && my <= area_top && my >= area_top -. area_height then
    Some (mx -. area_left, area_top -. my +. (float_of_int m.tab.scroll *. line_height))
  else None

let hovered (m : model) : string option =
  match (m.tab.state, page_point m) with Shown p, Some (x, y) -> Hit.link_at p.layout ~x ~y | _ -> None

let pointed_control (m : model) : Dom.element option =
  match (m.tab.state, page_point m) with
  | Shown p, Some (x, y) -> (
      match Hit.fragment_at p.layout ~x ~y with Some { control = Some c; _ } -> Some c.element | _ -> None)
  | _ -> None

let loading (m : model) : bool = (match m.tab.state with Loading _ -> true | Shown _ -> false) || m.tab.in_flight <> [] || m.tab.queue <> []

(* the toolbar: its buttons, whether each does something now *)
let buttons (m : model) : (string * bool) list =
  [ ("Back", m.tab.history.behind <> []); ("Forward", m.tab.history.ahead <> []); ("Home", true); ("Reload", true);
    ("Images", not m.tab.images); ("Open", true); ("Print", false); ("Find", false); ("Stop", loading m) ]

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
  let tab = { (Browser_tab.empty ~images:(List.assoc_opt "images" flags <> Some "off")) with visited = [ target ]; fragment } in
  let m =
    { tab; css = List.assoc_opt "css" flags <> Some "off"; location = target; editing = false; fresh = false; mouse = (0., 0.); time = 0. }
  in
  load network target m

(* a key while the Location field is typed into *)
let edit_location (network : < Cap.network ; .. >) (key : string) (m : model) : model * msg Cmd.t =
  match key with
  | "enter" | "return" -> visit network (typed_url m.location) m
  | "escape" -> ({ m with editing = false }, Cmd.none)
  | "backspace" ->
      let cs = characters m.location in
      let location = if m.fresh then "" else String.concat "" (List.filteri (fun i _ -> i < List.length cs - 1) cs) in
      ({ m with location; fresh = false }, Cmd.none)
  | _ -> (m, Cmd.none)

(* the Location field clicked: what is there selected, to be typed over *)
let start_editing (m : model) : model = { m with editing = true; fresh = true; location = current_url m }

let form (network : < Cap.network ; .. >) ~(keep_focus : bool) (effect : Browser_forms.effect) (m : model) : model * msg Cmd.t =
  with_tab m (Browser_tab.form_effect (config m) network ~keep_focus effect m.tab)

let update (network : < Cap.network ; .. >) (msg : msg) (m : model) : model * msg Cmd.t =
  let cfg = config m in
  match msg with
  | Got (url, r) -> with_tab m (Browser_tab.got cfg network url r m.tab)
  | Got_picture (url, r) -> with_tab m (Browser_tab.got_picture cfg network url r m.tab)
  | Tick time -> ({ m with time }, Cmd.none)
  | Wheel notches -> (scrolled (3 * int_of_float (Float.round notches)) m, Cmd.none)
  | Mouse_move (x, y) -> ({ m with mouse = (x, y) }, Cmd.none)
  | Click -> (
      if on_location m then (start_editing { m with tab = { m.tab with focus = None } }, Cmd.none)
      else
        let m = { m with editing = false } in
        match (button_at m, pointed_control m, hovered m, m.tab.state) with
        | Some "Back", _, _, _ -> with_tab m (Browser_tab.back cfg network m.tab)
        | Some "Forward", _, _, _ -> with_tab m (Browser_tab.forward cfg network m.tab)
        | Some "Home", _, _, _ -> visit network home m
        | Some "Reload", _, _, _ -> load network (current_url m) m
        | Some "Images", _, _, _ -> with_tab m (Browser_tab.load_images cfg network m.tab)
        | Some "Open", _, _, _ -> (start_editing m, Cmd.none)
        | Some "Stop", _, _, _ -> ({ m with tab = Browser_tab.stop cfg m.tab }, Cmd.none)
        | _, Some e, _, Shown p -> form network ~keep_focus:false (Browser_forms.click p e) m
        | _, _, Some href, Shown p -> visit network (resolve p.url href) m
        | _ -> ({ m with tab = { m.tab with focus = None } }, Cmd.none))
  (* the Location field typed into *)
  | Typed s when m.editing -> ({ m with location = (if m.fresh then s else m.location ^ s); fresh = false }, Cmd.none)
  | Key key when m.editing -> edit_location network (String.lowercase_ascii key) m
  (* a form's field *)
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
      | "arrowdown" | "down" -> (scrolled 1 m, Cmd.none)
      | "arrowup" | "up" -> (scrolled (-1) m, Cmd.none)
      | "pagedown" | " " | "space" -> (scrolled (visible - 2) m, Cmd.none)
      | "pageup" -> (scrolled (-(visible - 2)) m, Cmd.none)
      | "b" | "backspace" -> with_tab m (Browser_tab.back cfg network m.tab)
      | "f" -> with_tab m (Browser_tab.forward cfg network m.tab)
      | "r" -> load network (current_url m) m
      | "h" -> visit network home m
      | "s" -> ({ m with tab = { m.tab with view = Source; scroll = 0 } }, Cmd.none)
      | "p" -> ({ m with tab = { m.tab with view = Page; scroll = 0 } }, Cmd.none)
      | "c" ->
          (* claude: the style sheets off, or on again: the same tree
           * laid out again *)
          let m = { m with css = not m.css } in
          ({ m with tab = Browser_tab.relaid (config m) m.tab }, Cmd.none)
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
  let loading = (match m.tab.state with Loading _ -> true | Shown _ -> false) || m.tab.in_flight <> [] in
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
  let tab = m.tab in
  match (tab.state, hovered m) with
  | Shown p, Some href -> resolve p.url href
  | Loading url, _ -> "Connect: Contacting host: " ^ url
  | Shown _, None when tab.in_flight <> [] || tab.queue <> [] ->
      Printf.sprintf "Transferring pictures: %d of %d" (tab.total - List.length tab.queue - List.length tab.in_flight) tab.total
  | Shown p, None when p.status = 0 -> "Failed: " ^ p.url
  | Shown _, None -> "Document: Done."

let progress (m : model) : float option =
  let tab = m.tab in
  if tab.total > 0 && (tab.in_flight <> [] || tab.queue <> []) then
    Some (float_of_int (tab.total - List.length tab.queue - List.length tab.in_flight) /. float_of_int tab.total)
  else None

let page_shapes (m : model) (p : page) : shape list =
  let scroll = float_of_int m.tab.scroll *. line_height in
  (p.drawn @ Browser_draw.controls_drawn ~value:(Browser_page.value_of p) ~focus:m.tab.focus p.layout)
  |> List.filter (fun (top, bottom, _) -> bottom > scroll && top < scroll +. area_height)
  |> List.map (fun (_, _, s) -> s)
  |> group
  |> move area_left (area_top +. scroll)
  |> fun s -> [ s ]

let view (m : model) : shape list =
  let body =
    match (m.tab.state, m.tab.view) with
    | Shown p, Page -> page_shapes m p
    | Shown p, Source ->
        List.filteri (fun i _ -> i >= m.tab.scroll && i < m.tab.scroll + visible) p.lines
        |> List.mapi (fun i line -> monospace (-480.) (area_top -. 12. -. (14. *. float_of_int i)) ink line)
        |> List.concat
    | Loading _, _ -> []
  in
  let title = match m.tab.state with Shown p when p.title <> "" -> "Netscape - [" ^ p.title ^ "]" | _ -> "Netscape" in
  let location = if m.editing then m.location ^ "_" else current_url m in
  (* claude: the page's own background, <body bgcolor> (Netscape 1.1) *)
  let background =
    match (m.tab.state, m.tab.view) with Shown { background = Some (r, g, b); _ }, Page -> rgb r g b | _ -> grey
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
