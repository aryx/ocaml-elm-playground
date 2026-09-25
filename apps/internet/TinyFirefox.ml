(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Firefox 1.0 (the Mozilla Foundation, November 2004):
 * Netscape's code, opened in 1998, reborn as a small browser -- and a
 * browser that runs the page's own program.
 *
 * Mosaic read pages (TinyMosaic); Netscape drew them as their authors
 * asked, and did not wait (TinyNetscape); since Netscape 2 (1995) a
 * page is also a program, in JavaScript, which the browser runs. This
 * one runs it with an engine written from scratch
 * (libs/languages/javascript: a lexer, a Pratt parser, a tree walker)
 * over the page's tree (appkits/browser's Browser_script: the DOM, the
 * events, the timers), and shows what the scripts do in a panel under
 * the page, after Firebug (Joe Hewitt, 2006, a Firefox extension, the
 * ancestor of every browser's developer tools):
 *
 *   - on the left, the **console**: what console.log said, and each
 *     script's error with its line; below it, a command line (click it,
 *     type JavaScript, Enter): the page's own world, to look in and
 *     change;
 *   - on the right, the page's **tree**, as the scripts leave it after
 *     each task: watch an element appear when a click adds it.
 *
 * The browser is TinyNetscape's grown up: the same tab (Browser_tab:
 * the page, its history, its pictures, now six at a time), Netscape's
 * extensions to HTML, tables and CSS all honoured, and the scripts on.
 * A task runs to its end -- a click's handlers, a timer's function --
 * then the page is laid out again if its tree changed, once
 * (notes_javascript.md section 10). Its home page, about:firefox, leads
 * to the classic first programs of the web: a counter, a to-do list, a
 * stopwatch, tic-tac-toe.
 *
 * Keys go to the page first (keydown, bubbling to the document); only
 * if it does not prevent them does the browser scroll (the arrows,
 * Page Up and Down) or go back (Backspace). The toolbar: Back, Forward,
 * Reload, Stop, Home; the location bar (click it, type, Return); the
 * throbber, turning while something loads; the status bar's Console
 * button shows or hides the panel.
 *
 *   dune exec apps/internet/TinyFirefox.exe
 *   http://localhost:8001/apps/internet/web/TinyFirefox.html
 *
 * flags url= (about:firefox), the first page; panel=off, no panel;
 * seed=n, Math.random's.
 *
 * Uses: appkits/browser (the page, the tab, the scripts, the drawing,
 * the forms), libs/languages/javascript through it, the built-in site
 * (Site); web's Hit. Its own: the chrome, the panel, the dialogs.
 *
 * Exercises: tabs (a list of Browser_tab.t, the one shown chosen by a
 * row of tabs: Firefox's signature); the search box; View Source; the
 * panel's tree scrolled, an element picked on the page shown in it;
 * the console's command line remembering its commands (the arrows).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type page = Browser_page.t

type model = {
  tab : Browser_tab.t;
  location : string; (* the location bar's text, while it is typed into *)
  editing : bool;
  fresh : bool; (* just clicked: typing replaces what is there *)
  mouse : float * float;
  time : float; (* the throbber's *)
  panel : bool; (* the console and the tree shown *)
  command : string; (* the console's command line *)
  commanding : bool; (* typing into it *)
  alerts : string list; (* alert()'s dialogs waiting, the next first *)
  seed : int;
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

let home = "about:firefox"
let characters = Browser_text.characters
let resolve = Browser_url.resolve

(*****************************************************************************)
(* The window's geometry *)
(*****************************************************************************)

(* the page area: its top and left edges, its bottom above the panel *)
let area_top = 416.
let area_left = -490.
let area_bottom (m : model) : float = if m.panel then -150. else -474.
let area_height (m : model) : float = area_top -. area_bottom m
let page_width = 976.
let line_height = 14.

(* the panel: its top, its two halves split at x = 0; the command line *)
let panel_top = -158.
let command_y = -462.

(* the toolbar's buttons, the location bar, the throbber *)
let toolbar_y = 438.
let button_x (i : int) : float = -468. +. (46. *. float_of_int i)
let location_x = -240.
let location_w = 680.
let throbber_x = 470.

(* the status bar, and its Console button *)
let status_y = -488.
let console_button_x = 440.

(*****************************************************************************)
(* The tab: Firefox's settings *)
(*****************************************************************************)

let settings (tab : Browser_tab.t) : Browser_page.settings =
  {
    (* Netscape's extensions, tables and CSS, all of them *)
    extensions = true;
    css = true;
    width = page_width;
    breaker = Html_layout.greedy;
    visited = (fun url -> List.mem url tab.visited);
    picture = (fun url -> List.assoc_opt url tab.pictures);
  }

let config (m : model) : msg Browser_tab.config =
  {
    settings;
    about = Site.about;
    got = (fun url r -> Got (url, r));
    got_picture = (fun url r -> Got_picture (url, r));
    (* a modern browser's six connections to a server *)
    connections = 6;
    visible = int_of_float (area_height m /. line_height);
    line_height;
    scripts = true;
    seed = m.seed;
  }

(* alert()'s messages: the dialogs to show once the task is done *)
let collect (m : model) : model =
  match m.tab.script with Some s -> { m with alerts = m.alerts @ Browser_script.take_alerts s } | None -> m

let with_tab (m : model) ((tab, cmd) : Browser_tab.t * msg Cmd.t) : model * msg Cmd.t = (collect { m with tab }, cmd)
let current_url (m : model) : string = Browser_tab.current_url m.tab
let scrolled (by : int) (m : model) : model = { m with tab = Browser_tab.scrolled (config m) by m.tab }
let visit network url m = with_tab { m with editing = false } (Browser_tab.visit (config m) network url m.tab)
let load network url m = with_tab m (Browser_tab.load (config m) network url m.tab)

(* a task of the page's scripts done by [f]: then the page laid out
 * again if its tree changed, alert()s collected *)
let task (network : < Cap.network ; .. >) (m : model) (f : Browser_script.t -> 'a) ~(default : 'a) : model * msg Cmd.t * 'a =
  match m.tab.script with
  | Some s ->
      let r = f s in
      let m, cmd = with_tab m (Browser_tab.after_task (config m) network m.tab) in
      (m, cmd, r)
  | None -> (m, Cmd.none, default)

let typed_url (s : string) : string =
  let s = String.trim s in
  if String.contains s ':' then s else "http://" ^ s

(*****************************************************************************)
(* The pointer *)
(*****************************************************************************)

let page_point (m : model) : (float * float) option =
  let mx, my = m.mouse in
  if my <= area_top && my >= area_bottom m then Some (mx -. area_left, area_top -. my +. (float_of_int m.tab.scroll *. line_height))
  else None

let hovered (m : model) : string option =
  match (m.tab.state, page_point m) with Shown p, Some (x, y) -> Hit.link_at p.layout ~x ~y | _ -> None

let pointed_control (m : model) : Dom.element option =
  match (m.tab.state, page_point m) with
  | Shown p, Some (x, y) -> ( match Hit.fragment_at p.layout ~x ~y with Some { control = Some c; _ } -> Some c.element | _ -> None)
  | _ -> None

let pointed_element (m : model) : Dom.element option =
  match (m.tab.state, page_point m) with Shown p, Some (x, y) -> Hit.element_at p.layout ~x ~y | _ -> None

let loading (m : model) : bool = (match m.tab.state with Loading _ -> true | Shown _ -> false) || m.tab.in_flight <> [] || m.tab.queue <> []

let buttons (m : model) : (string * bool) list =
  [ ("Back", m.tab.history.behind <> []); ("Forward", m.tab.history.ahead <> []); ("Reload", true); ("Stop", loading m); ("Home", true) ]

let near (x0 : float) (y0 : float) (w : float) (h : float) (m : model) : bool =
  let mx, my = m.mouse in
  mx >= x0 && mx <= x0 +. w && Float.abs (my -. y0) <= h /. 2.

let button_at (m : model) : string option =
  List.mapi (fun i b -> (i, b)) (buttons m)
  |> List.find_map (fun (i, (text, active)) -> if active && near (button_x i -. 20.) toolbar_y 40. 34. m then Some text else None)

let on_location (m : model) : bool = near location_x toolbar_y location_w 22. m
let on_command (m : model) : bool = m.panel && near area_left command_y 485. 16. m
let on_console_button (m : model) : bool = near console_button_x status_y 52. 16. m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let init (network : < Cap.network ; .. >) (flags : flags) : model * msg Cmd.t =
  let target, fragment = Browser_url.split_fragment (Option.value (List.assoc_opt "url" flags) ~default:home) in
  let tab = { (Browser_tab.empty ~images:true) with visited = [ target ]; fragment } in
  let m =
    {
      tab;
      location = target;
      editing = false;
      fresh = false;
      (* outside the window until it moves: no link under it *)
      mouse = (1000., 1000.);
      time = 0.;
      panel = List.assoc_opt "panel" flags <> Some "off";
      command = "";
      commanding = false;
      alerts = [];
      seed = Option.value (Option.bind (List.assoc_opt "seed" flags) int_of_string_opt) ~default:1;
    }
  in
  load network target m

let backspace (s : string) : string =
  let cs = characters s in
  String.concat "" (List.filteri (fun i _ -> i < List.length cs - 1) cs)

let edit_location (network : < Cap.network ; .. >) (key : string) (m : model) : model * msg Cmd.t =
  match key with
  | "enter" | "return" -> visit network (typed_url m.location) m
  | "escape" -> ({ m with editing = false }, Cmd.none)
  | "backspace" -> ({ m with location = (if m.fresh then "" else backspace m.location); fresh = false }, Cmd.none)
  | _ -> (m, Cmd.none)

(* the console's command line: Enter runs it in the page's world, its
 * value (or its error) printed under it *)
let edit_command (network : < Cap.network ; .. >) (key : string) (m : model) : model * msg Cmd.t =
  match key with
  | "enter" | "return" when String.trim m.command <> "" ->
      let command = m.command in
      let m, cmd, () =
        task network { m with command = "" } ~default:() (fun s ->
            Browser_script.print s ("> " ^ command);
            match Browser_script.eval s command with Ok v -> Browser_script.print s (Js_value.display v) | Error _ -> ())
      in
      (m, cmd)
  | "escape" -> ({ m with commanding = false }, Cmd.none)
  | "backspace" -> ({ m with command = backspace m.command }, Cmd.none)
  | _ -> (m, Cmd.none)

let form (network : < Cap.network ; .. >) ~(keep_focus : bool) (effect : Browser_forms.effect) (m : model) : model * msg Cmd.t =
  with_tab m (Browser_tab.form_effect (config m) network ~keep_focus effect m.tab)

(* a click on the page: the scripts first (the element under the
 * pointer, bubbling); then, unless one prevented it, what the browser
 * does -- a form's control, a link *)
let click_page (network : < Cap.network ; .. >) (m : model) : model * msg Cmd.t =
  let control = pointed_control m and link = hovered m and element = pointed_element m in
  let m, cmd, prevented = task network m ~default:false (fun s -> match element with Some e -> Browser_script.click s e | None -> false) in
  if prevented then (m, cmd)
  else
    let m, cmd2 =
      match (control, link, m.tab.state) with
      | Some e, _, Shown p -> form network ~keep_focus:false (Browser_forms.click p e) m
      | _, Some href, Shown p -> visit network (resolve p.url href) m
      | _ -> ({ m with tab = { m.tab with focus = None } }, Cmd.none)
    in
    (m, Cmd.batch [ cmd; cmd2 ])

let update (network : < Cap.network ; .. >) (msg : msg) (m : model) : model * msg Cmd.t =
  let cfg = config m in
  match msg with
  | Got (url, r) -> with_tab m (Browser_tab.got cfg network url r m.tab)
  | Got_picture (url, r) -> with_tab m (Browser_tab.got_picture cfg network url r m.tab)
  | Tick time ->
      (* the page's clock is the frame clock: a sixtieth of a second a
       * frame (the window's rate), so that the same frames see the
       * same timers run -- a golden frame's, under -fixed-time, too *)
      let m, cmd, () = task network { m with time } ~default:() (fun s -> Browser_script.advance s (1000. /. 60.)) in
      (m, cmd)
  | Wheel notches -> (scrolled (3 * int_of_float (Float.round notches)) m, Cmd.none)
  | Mouse_move (x, y) -> ({ m with mouse = (x, y) }, Cmd.none)
  (* a dialog waiting: a click is its OK *)
  | Click when m.alerts <> [] -> ({ m with alerts = List.tl m.alerts }, Cmd.none)
  | Click -> (
      let m = { m with editing = false; commanding = false } in
      if on_location m then ({ m with editing = true; fresh = true; location = current_url m; tab = { m.tab with focus = None } }, Cmd.none)
      else if on_command m then ({ m with commanding = true; tab = { m.tab with focus = None } }, Cmd.none)
      else if on_console_button m then ({ m with panel = not m.panel }, Cmd.none)
      else
        match button_at m with
        | Some "Back" -> with_tab m (Browser_tab.back cfg network m.tab)
        | Some "Forward" -> with_tab m (Browser_tab.forward cfg network m.tab)
        | Some "Reload" -> load network (current_url m) m
        | Some "Stop" -> ({ m with tab = Browser_tab.stop cfg m.tab }, Cmd.none)
        | Some "Home" -> visit network home m
        | _ -> if page_point m <> None then click_page network m else (m, Cmd.none))
  | Key key when m.alerts <> [] && List.mem (String.lowercase_ascii key) [ "enter"; "return"; "escape" ] ->
      ({ m with alerts = List.tl m.alerts }, Cmd.none)
  | Typed s when m.editing -> ({ m with location = (if m.fresh then s else m.location ^ s); fresh = false }, Cmd.none)
  | Key key when m.editing -> edit_location network (String.lowercase_ascii key) m
  | Typed s when m.commanding -> ({ m with command = m.command ^ s }, Cmd.none)
  | Key key when m.commanding -> edit_command network (String.lowercase_ascii key) m
  (* a form's field: its text, the script told (Browser_tab) *)
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
      (* the page's first: keydown, bubbling to the document *)
      let m, cmd, prevented = task network m ~default:false (fun s -> Browser_script.key s key) in
      if prevented then (m, cmd)
      else
        match String.lowercase_ascii key with
        | "arrowdown" | "down" -> (scrolled 1 m, cmd)
        | "arrowup" | "up" -> (scrolled (-1) m, cmd)
        | "pagedown" -> (scrolled (int_of_float (area_height m /. line_height) - 2) m, cmd)
        | "pageup" -> (scrolled (-(int_of_float (area_height m /. line_height) - 2)) m, cmd)
        | "backspace" ->
            let m, cmd2 = with_tab m (Browser_tab.back cfg network m.tab) in
            (m, Cmd.batch [ cmd; cmd2 ])
        | _ -> (m, cmd))

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* Firefox 1.0 on Windows XP: the Luna window, its beige chrome *)
let chrome = rgb 236 233 216
let shade = rgb 172 168 153
let light = rgb 255 255 255
let ink = rgb 0 0 0
let grey_ink = rgb 150 150 150
let title_blue = rgb 0 84 227
let firefox_orange = rgb 230 96 0
let globe_blue = rgb 30 70 150
let cell = 6.

let monospace ?(max = 160) (x : number) (y : number) (color : color) (s : string) : shape list =
  characters s
  |> List.mapi (fun i c -> (i, c))
  |> List.filter (fun (i, c) -> c <> " " && i < max)
  |> List.map (fun (i, c) -> words color c |> move (x +. (cell *. float_of_int i) +. (cell /. 2.)) y)

(* the toolbar's pictures, Firefox's round arrows reduced to shapes *)
let icon (name : string) (active : bool) (x : number) (y : number) : shape list =
  let at shapes = List.map (fun s -> s |> move x y) shapes in
  let c = if active then rgb 40 120 40 else grey_ink in
  match name with
  | "Back" -> at [ circle (if active then rgb 60 150 60 else shade) 14.; polygon light [ (-8., 0.); (4., 8.); (4., -8.) ] ]
  | "Forward" -> at [ circle (if active then rgb 60 150 60 else shade) 14.; polygon light [ (8., 0.); (-4., 8.); (-4., -8.) ] ]
  | "Reload" -> at [ circle (rgb 50 110 200) 10.; circle chrome 6.; polygon (rgb 50 110 200) [ (6., 2.); (14., 2.); (10., 9.) ] ]
  | "Stop" -> at [ circle (if active then rgb 200 40 40 else shade) 11.; rectangle light 10. 3. |> rotate 45.; rectangle light 10. 3. |> rotate (-45.) ]
  | "Home" -> at [ polygon c [ (-11., 1.); (0., 11.); (11., 1.) ]; rectangle c 14. 10. |> move 0. (-4.) ]
  | _ -> []

(* the throbber: Firefox's globe, a fox curled round it, turning while
 * something loads *)
let throbber (m : model) : shape list =
  let angle = if loading m then m.time *. 360. else 0. in
  [ circle globe_blue 13. |> move throbber_x toolbar_y;
    group [ circle firefox_orange 15.; circle globe_blue 12. |> move 4. 3. ] |> rotate angle |> move throbber_x toolbar_y ]

let status (m : model) : string =
  match (m.tab.state, hovered m) with
  | Shown p, Some href -> resolve p.url href
  | Loading url, _ -> "Waiting for " ^ url ^ "..."
  | Shown _, None when m.tab.in_flight <> [] || m.tab.queue <> [] -> "Transferring data..."
  | Shown _, None -> "Done"

let page_shapes (m : model) (p : page) : shape list =
  let scroll = float_of_int m.tab.scroll *. line_height in
  (p.drawn @ Browser_draw.controls_drawn ~value:(Browser_page.value_of p) ~focus:m.tab.focus p.layout)
  |> List.filter (fun (top, bottom, _) -> bottom > scroll && top < scroll +. area_height m)
  |> List.map (fun (_, _, s) -> s)
  |> group
  |> move area_left (area_top +. scroll)
  |> fun s -> [ s ]

(* the panel: the console on the left, the page's tree on the right,
 * each its last lines that fit *)
let panel (m : model) : shape list =
  let rows = int_of_float ((panel_top -. command_y -. 24.) /. line_height) in
  let last n xs = let k = List.length xs in List.filteri (fun i _ -> i >= k - n) xs in
  let console = match m.tab.script with Some s -> Browser_script.console s | None -> [] in
  let tree =
    match m.tab.state with
    | Shown p -> (
        (* the scripts left out: the panel shows what they made *)
        let rec without_scripts (e : Dom.element) : Dom.element =
          { e with
            children =
              List.filter_map
                (fun (n : Dom.node) -> match n with Element { name = "script"; _ } -> None | Element c -> Some (Dom.Element (without_scripts c)) | t -> Some t)
                e.children }
        in
        match Dom.find_all "body" (Dom.without_blank_text p.tree) with [ b ] -> Dom.to_lines (without_scripts b) | _ -> [])
    | Loading _ -> []
  in
  let lines x rows_of color = List.concat (List.mapi (fun i l -> monospace ~max:78 x (panel_top -. 26. -. (line_height *. float_of_int i)) (color l) l) rows_of) in
  let error_red l = if String.starts_with ~prefix:"Uncaught" l then rgb 190 20 20 else if String.starts_with ~prefix:"> " l then rgb 30 70 150 else ink in
  [ rectangle chrome 1000. (panel_top +. 500.) |> move_y ((panel_top -. 500.) /. 2.);
    rectangle shade 1000. 1. |> move_y panel_top;
    rectangle light 485. (panel_top -. command_y +. 6.) |> move (area_left +. 242.) ((panel_top +. command_y -. 18.) /. 2.);
    rectangle light 485. (panel_top -. command_y +. 6.) |> move 247. ((panel_top +. command_y -. 18.) /. 2.) ]
  @ monospace (area_left +. 4.) (panel_top -. 9.) ink "Console"
  @ monospace 10. (panel_top -. 9.) ink "Document (the tree, as the scripts left it)"
  @ lines (area_left +. 6.) (last rows console) error_red
  @ lines 12. (List.filteri (fun i _ -> i < rows + 1) tree) (fun _ -> rgb 60 60 60)
  (* the command line *)
  @ [ rectangle (if m.commanding then rgb 255 255 230 else rgb 245 245 245) 485. 16. |> move (area_left +. 242.) command_y ]
  @ monospace (area_left +. 6.) command_y (rgb 30 70 150) (">>> " ^ Browser_text.tail 70 (m.command ^ if m.commanding then "_" else ""))

(* alert()'s dialog, over everything *)
let dialog (message : string) : shape list =
  [ rectangle (rgb 0 0 0) 424. 144. |> fade 0.3 |> move 4. (-4.); rectangle chrome 420. 140.; rectangle title_blue 420. 20. |> move_y 60. ]
  @ monospace (-200.) 60. light "The page says:"
  @ monospace ~max:64 (-190.) 20. ink message
  @ [ rectangle shade 72. 24. |> move_y (-40.); rectangle light 70. 22. |> move_y (-40.) ]
  @ monospace (-6.) (-40.) ink "OK"

let view (m : model) : shape list =
  let body = match m.tab.state with Shown p -> page_shapes m p | Loading _ -> [] in
  let title = match m.tab.state with Shown p when p.title <> "" -> p.title ^ " - Mozilla Firefox" | _ -> "Mozilla Firefox" in
  let background = match m.tab.state with Shown { background = Some (r, g, b); _ } -> rgb r g b | _ -> light in
  let location = if m.editing then m.location ^ "_" else current_url m in
  let bottom = area_bottom m in
  [ rectangle chrome 1000. 1000.;
    rectangle background 980. (area_top -. bottom) |> move_y ((area_top +. bottom) /. 2.) ]
  @ body
  (* the chrome over what overflows *)
  @ [ rectangle chrome 1000. (500. -. area_top) |> move_y ((500. +. area_top) /. 2.) ]
  @ (if m.panel then panel m else [ rectangle chrome 1000. (500. +. bottom) |> move_y ((bottom -. 500.) /. 2.) ])
  @ [ rectangle title_blue 1000. 20. |> move_y 490. ]
  @ monospace (-490.) 490. light title
  @ List.concat (List.mapi (fun i s -> monospace (-488. +. (64. *. float_of_int i)) 468. ink s) [ "File"; "Edit"; "View"; "Go"; "Bookmarks"; "Tools"; "Help" ])
  @ List.concat (List.mapi (fun i (name, active) -> icon name active (button_x i) toolbar_y) (buttons m))
  @ [ rectangle shade (location_w +. 2.) 24. |> move (location_x +. (location_w /. 2.)) toolbar_y;
      rectangle (if m.editing then rgb 255 255 230 else light) location_w 22. |> move (location_x +. (location_w /. 2.)) toolbar_y ]
  @ monospace (location_x +. 6.) toolbar_y ink (Browser_text.tail 110 location)
  @ throbber m
  (* the status bar *)
  @ [ rectangle shade 1000. 1. |> move_y (status_y +. 10.) ]
  @ monospace (-488.) status_y ink (status m)
  @ [ rectangle (if m.panel then rgb 200 210 230 else chrome) 52. 16. |> move (console_button_x +. 26.) status_y ]
  @ monospace (console_button_x +. 5.) status_y ink "Console"
  @ match m.alerts with message :: _ -> dialog message | [] -> []

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
