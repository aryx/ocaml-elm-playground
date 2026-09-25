(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_tab.mli *)
open Playground

type state = Loading of string | Shown of Browser_page.t
type view = Page | Source
type entry = { at : string; kept : (Browser_page.t * Browser_script.t option) option; scrolled_to : int }

type t = {
  state : state;
  view : view;
  scroll : int;
  history : entry Browser_history.t;
  visited : string list;
  fragment : string option;
  pictures : (string * Browser_picture.t) list;
  sheets : (string * string) list;
  sheet_urls : string list;
  queue : string list;
  in_flight : string list;
  total : int;
  images : bool;
  focus : Dom.element option;
  script : Browser_script.t option;
}

type 'msg config = {
  settings : t -> Browser_page.settings;
  about : string -> (string * string) option;
  got : string -> (Http.response, Http.error) result -> 'msg;
  got_picture : string -> (Http.response, Http.error) result -> 'msg;
  connections : int;
  visible : int;
  line_height : float;
  scripts : string -> bool;
  seed : int;
}

let empty ~(images : bool) : t =
  {
    state = Loading "";
    view = Page;
    scroll = 0;
    history = Browser_history.empty;
    visited = [];
    fragment = None;
    pictures = [];
    sheets = [];
    sheet_urls = [];
    queue = [];
    in_flight = [];
    total = 0;
    images;
    focus = None;
    script = None;
  }

let current_url (tab : t) : string = match tab.state with Loading url -> url | Shown p -> p.url
let starts_with = Browser_url.starts_with

(*****************************************************************************)
(* Scrolling *)
(*****************************************************************************)

let line_count (cfg : 'msg config) (tab : t) : int =
  match (tab.state, tab.view) with
  | Shown p, Page -> int_of_float (Float.ceil (p.layout.height /. cfg.line_height))
  | Shown p, Source -> List.length p.lines
  | _ -> 0

let scrolled (cfg : 'msg config) (by : int) (tab : t) : t =
  { tab with scroll = max 0 (min (line_count cfg tab - cfg.visible) (tab.scroll + by)) }

let to_fragment (cfg : 'msg config) (tab : t) : t =
  match (tab.state, tab.fragment) with
  | Shown p, Some name -> (
      let tab = { tab with fragment = None } in
      match Hit.anchor p.layout name with
      | Some y -> scrolled cfg 0 { tab with scroll = int_of_float (y /. cfg.line_height) }
      | None -> tab)
  | _ -> tab

(*****************************************************************************)
(* A page shown *)
(*****************************************************************************)

let relaid (cfg : 'msg config) (tab : t) : t =
  match tab.state with Shown p -> { tab with state = Shown (Browser_page.laid_out (cfg.settings tab) p) } | Loading _ -> tab

(* a page just read: its scripts run first, if the browser has them,
 * and the page laid out from the tree they leave *)
let arrive (cfg : 'msg config) (tab : t) (url : string) (status : int) (content_type : string option) (bytes : string) : t =
  let p = Browser_page.read (cfg.settings tab) url status content_type bytes in
  if not (cfg.scripts url) then { tab with state = Shown p; script = None }
  else
    let s = Browser_script.create ~seed:cfg.seed p.tree in
    Browser_script.run_scripts s;
    { tab with state = Shown (Browser_page.with_tree (cfg.settings tab) p (Browser_script.tree s)); script = Some s }

let failed (cfg : 'msg config) (tab : t) (url : string) (why : string) : t = arrive cfg tab url 0 None (Browser_page.error_html url why)

(*****************************************************************************)
(* Pictures, four at a time; style sheets first *)
(*****************************************************************************)

(* a picture had (or not): the page laid out again with it *)
let with_arrived (cfg : 'msg config) (tab : t) (url : string) (pic : Browser_picture.t) : t =
  relaid cfg { tab with pictures = (url, pic) :: List.remove_assoc url tab.pictures }

(* a style sheet had (or not: then empty): the page laid out again with
 * it -- its colours, its boxes, as a picture moves the text *)
let with_sheet (cfg : 'msg config) (tab : t) (url : string) (text : string) : t =
  let tab = relaid cfg { tab with sheets = (url, text) :: List.remove_assoc url tab.sheets } in
  (* its @imports, first in the queue *)
  match tab.state with
  | Shown p ->
      let more =
        List.filter (fun u -> not (List.mem u tab.queue || List.mem u tab.in_flight)) (Browser_page.sheets_wanted (cfg.settings tab) p)
      in
      { tab with queue = more @ tab.queue; sheet_urls = more @ tab.sheet_urls; total = tab.total + List.length more }
  | Loading _ -> tab

(* more pictures on their way, while fewer than [connections] are:
 * Netscape's way, where Mosaic had one; a built-in one decoded at once *)
let rec fetch_more (cfg : 'msg config) (network : < Cap.network ; .. >) ((tab, cmd) : t * 'msg Cmd.t) : t * 'msg Cmd.t =
  match tab.queue with
  | url :: rest when List.length tab.in_flight < cfg.connections ->
      let tab = { tab with queue = rest } in
      if starts_with "about:" url && List.mem url tab.sheet_urls then
        let text = match cfg.about (String.sub url 6 (String.length url - 6)) with Some (bytes, _) -> bytes | None -> "" in
        fetch_more cfg network (with_sheet cfg tab url text, cmd)
      else if starts_with "about:" url then
        let pic =
          match cfg.about (String.sub url 6 (String.length url - 6)) with
          | Some (bytes, _) -> Browser_picture.decode bytes
          | None -> Browser_picture.Broken
        in
        fetch_more cfg network (with_arrived cfg tab url pic, cmd)
      else
        let get = Http.get network ~url ~expect:(Http.expect_response (cfg.got_picture url)) in
        fetch_more cfg network ({ tab with in_flight = url :: tab.in_flight }, Cmd.batch [ cmd; get ])
  | _ -> (tab, cmd)

(* a page shown: its style sheets not had yet queued (by the box
 * model: Browser_page.sheets_wanted), then its pictures (if Auto Load
 * Images), the ones of the page before dropped *)
let with_pictures (cfg : 'msg config) (network : < Cap.network ; .. >) ((tab, cmd) : t * 'msg Cmd.t) : t * 'msg Cmd.t =
  match tab.state with
  | Loading _ -> (tab, cmd)
  | Shown p ->
      let had url = match List.assoc_opt url tab.pictures with Some (Arrived _ | Broken) -> true | _ -> false in
      let fresh = List.fold_left (fun acc u -> if List.mem u acc || List.mem u tab.in_flight then acc else acc @ [ u ]) [] in
      let sheets = fresh (Browser_page.sheets_wanted (cfg.settings tab) p) in
      let pictures =
        Dom.find_all "img" p.tree
        |> List.filter_map (fun e -> Option.map (Browser_url.resolve p.url) (Box_layout.picture_src e))
        |> List.filter (fun u -> not (had u))
        |> fresh
      in
      let pictures = if tab.images then pictures else [] in
      let urls = sheets @ pictures in
      fetch_more cfg network
        ({ tab with queue = urls; sheet_urls = sheets @ tab.sheet_urls; total = List.length urls + List.length tab.in_flight }, cmd)

let load_images cfg network tab = with_pictures cfg network ({ tab with images = true }, Cmd.none)

(*****************************************************************************)
(* Going places *)
(*****************************************************************************)

let load ?post (cfg : 'msg config) (network : < Cap.network ; .. >) (url : string) (tab : t) : t * 'msg Cmd.t =
  let tab = { tab with scroll = 0; focus = None; queue = []; total = 0 } in
  if starts_with "about:" url then
    let name, query = Browser_url.split_query (String.sub url 6 (String.length url - 6)) in
    let show bytes content_type = with_pictures cfg network (to_fragment cfg (arrive cfg tab url 200 (Some content_type) bytes), Cmd.none) in
    match (name, post) with
    | "echo", Some (_, body) -> show (Browser_page.echo_html "POST" body) "text/html; charset=utf-8"
    | "echo", None -> show (Browser_page.echo_html "GET" (Option.value query ~default:"")) "text/html; charset=utf-8"
    | _ -> (
        match cfg.about name with
        | Some (bytes, content_type) -> show bytes content_type
        | None -> (failed cfg tab url "There is no such page in the built-in site.", Cmd.none))
  else
    let expect = Http.expect_response (cfg.got url) in
    let tab = { tab with state = Loading url } in
    match post with
    | None -> (tab, Http.get network ~url ~expect)
    | Some (content_type, body) -> (tab, Http.post network ~url ~content_type ~body ~expect)

let entry_of (tab : t) : entry =
  match tab.state with
  | Shown p -> { at = p.url; kept = Some (p, tab.script); scrolled_to = tab.scroll }
  | Loading url -> { at = url; kept = None; scrolled_to = 0 }

let visit ?post (cfg : 'msg config) (network : < Cap.network ; .. >) (url : string) (tab : t) : t * 'msg Cmd.t =
  let target, fragment = Browser_url.split_fragment url in
  let tab =
    {
      tab with
      history = Browser_history.visit (entry_of tab) tab.history;
      visited = (if List.mem target tab.visited then tab.visited else target :: tab.visited);
      fragment;
    }
  in
  match tab.state with
  | Shown p when post = None && fragment <> None && target = fst (Browser_url.split_fragment p.url) ->
      (to_fragment cfg (relaid cfg tab), Cmd.none)
  | _ -> load ?post cfg network target tab

let restore (cfg : 'msg config) (network : < Cap.network ; .. >) (e : entry) (tab : t) : t * 'msg Cmd.t =
  match e.kept with
  | Some (p, script) ->
      with_pictures cfg network (scrolled cfg 0 { (relaid cfg { tab with state = Shown p; script }) with scroll = e.scrolled_to }, Cmd.none)
  | None -> load cfg network e.at tab

let back cfg network tab =
  match Browser_history.back (entry_of tab) tab.history with
  | Some (e, history) -> restore cfg network e { tab with history }
  | None -> (tab, Cmd.none)

let forward cfg network tab =
  match Browser_history.forward (entry_of tab) tab.history with
  | Some (e, history) -> restore cfg network e { tab with history }
  | None -> (tab, Cmd.none)

let stop (cfg : 'msg config) (tab : t) : t =
  let tab =
    match tab.state with
    | Loading url -> failed cfg tab url "Stopped."
    | Shown _ -> tab
  in
  { tab with queue = []; in_flight = []; total = 0 }

let got (cfg : 'msg config) (network : < Cap.network ; .. >) (url : string) (result : (Http.response, Http.error) result) (tab : t) :
    t * 'msg Cmd.t =
  match result with
  | Ok r ->
      let content_type =
        List.find_map (fun (name, value) -> if String.lowercase_ascii name = "content-type" then Some value else None) r.headers
      in
      with_pictures cfg network (to_fragment cfg (arrive cfg tab r.url r.status content_type r.body), Cmd.none)
  | Error e -> (failed cfg tab url (String.capitalize_ascii (Http.error_to_string e) ^ "."), Cmd.none)

let got_picture (cfg : 'msg config) (network : < Cap.network ; .. >) (url : string) (result : (Http.response, Http.error) result)
    (tab : t) : t * 'msg Cmd.t =
  if not (List.mem url tab.in_flight) then (* one Stop said not to wait for *) (tab, Cmd.none)
  else if List.mem url tab.sheet_urls then
    (* a style sheet: laid out with it, its @imports queued *)
    let text = match result with Ok r when r.status / 100 = 2 -> r.body | _ -> "" in
    fetch_more cfg network (with_sheet cfg { tab with in_flight = List.filter (( <> ) url) tab.in_flight } url text, Cmd.none)
  else
    let pic = match result with Ok r when r.status / 100 = 2 -> Browser_picture.decode r.body | _ -> Browser_picture.Broken in
    fetch_more cfg network (with_arrived cfg { tab with in_flight = List.filter (( <> ) url) tab.in_flight } url pic, Cmd.none)

(*****************************************************************************)
(* Forms, and the scripts' tasks *)
(*****************************************************************************)

(* where an element is in its tree: the indexes of the elements down to
 * it -- how the field in focus is found again in a tree a script froze
 * anew *)
let rec path_to (root : Dom.element) (e : Dom.element) : int list option =
  if root == e then Some []
  else
    let children = List.filter_map (fun (n : Dom.node) -> match n with Element c -> Some c | Text _ -> None) root.children in
    List.find_map (fun (i, c) -> Option.map (fun p -> i :: p) (path_to c e)) (List.mapi (fun i c -> (i, c)) children)

let rec at_path (root : Dom.element) (path : int list) : Dom.element option =
  match path with
  | [] -> Some root
  | i :: rest -> (
      let children = List.filter_map (fun (n : Dom.node) -> match n with Element c -> Some c | Text _ -> None) root.children in
      match List.nth_opt children i with Some c -> at_path c rest | None -> None)

let after_task (cfg : 'msg config) (network : < Cap.network ; .. >) (tab : t) : t * 'msg Cmd.t =
  match (tab.state, tab.script) with
  | Shown p, Some s when Browser_script.changed s ->
      let tree = Browser_script.tree s in
      let focus = Option.bind tab.focus (fun e -> Option.bind (path_to p.tree e) (at_path tree)) in
      with_pictures cfg network ({ tab with state = Shown (Browser_page.with_tree (cfg.settings tab) p tree); focus }, Cmd.none)
  | _ -> (tab, Cmd.none)

let form_effect (cfg : 'msg config) (network : < Cap.network ; .. >) ~(keep_focus : bool) (effect : Browser_forms.effect) (tab : t) :
    t * 'msg Cmd.t =
  match effect with
  | Nothing -> (tab, Cmd.none)
  | Focus e -> ({ tab with focus = Some e }, Cmd.none)
  | Unfocus -> ({ tab with focus = None }, Cmd.none)
  | Changed p -> (
      let tab = { tab with state = Shown p; focus = (if keep_focus then tab.focus else None) } in
      (* the script told: the field's text in its value=, its input event *)
      match (tab.script, tab.focus) with
      | Some s, Some e ->
          Browser_script.input s e (Browser_page.value_of p e).text;
          after_task cfg network tab
      | _ -> (tab, Cmd.none))
  | Submit { url; post; page } -> visit ?post cfg network url { tab with state = Shown page; focus = None }
