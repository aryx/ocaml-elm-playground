(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tinybox_menu.mli.
 *
 * A Playground game like the others, its world the catalogue
 * (Tinybox_data, made at build time from CATALOG.md and the golden
 * frames). The screen, 16:9, 1778 by 1000 (run_app's ~screen, scaled
 * to the window):
 *
 *   TINYBOX  GAMES  APPS  / search
 *   < Platform >              3 / 15  +-------------+  TinyMario  2D
 *   Run and jump from platform to...  |             |  After Super Mario
 *   +----+ +----+ +----+ +----+ +----+|  the chosen |  Bros. (...)
 *   |    | |    | |    | |    | |    ||  one, live  |  Run, jump, stomp...
 *   +----+ +----+ +----+ +----+ +----+|             |  The side-scroller:
 *   TinyMario TinySonic ...           +-------------+  ...
 *   +----+ +----+ +----+ +----+ +----++--------------------------------+
 *   ...                               |  its code (Codemap.preview),   |
 *                                     |  a click (or s) opens it all   |
 *                                     +--------------------------------+
 *   arrows move  tab section  enter play  / search  s code
 *
 * Keys: the arrows, Tab and Shift-Tab (the next section, across both
 * shelves), g and a (the games' and the apps' first section), Enter
 * (play), / (search every program by name, original and line; Escape
 * leaves it). The mouse: a click chooses, a double click plays, the
 * wheel scrolls, the tabs and arrows at the top are buttons.
 *
 * Groups and filters, after Batocera's and RetroBox's (the catalogue's
 * Year, Platform and Players columns): b groups the programs by genre
 * (the catalogue's sections, the default), by era (a decade a
 * section, oldest first), by platform or by players; p, e, m and l
 * filter by players (1, 2, over the network), era, machine and look
 * (2D, 2.5D, 3D, app), each key going through the values and back to
 * any; c clears them; r jumps to a random program of the grid. The
 * filter bar under the section's title says what is chosen, and a click
 * on one of its words does what its key does.
 *
 * The look: a dark cabinet's colours, the chosen thumbnail's frame
 * pulsing, scanlines over everything (thin translucent rectangles, a
 * CRT's gaps between its lines).
 *
 * The thumbnails are PNGs (250 by 250) in the binary, decoded when first
 * shown; a backend keeps the last 32 bitmaps it converted, so the grid
 * shows 12 at a time, plus the chosen one enlarged (the same bitmap).
 *
 * The code: s opens the chosen program's code map (codemap/, after
 * codemap), its files and what it uses as a treemap to zoom into, a file
 * read by clicking on it; Escape comes back.
 *
 * The previews: a second (60 frames) on a program, and its picture comes
 * alive -- the program itself, playing its golden scene's script in the
 * detail panel, run by the menu (see "Previews" below).
 *)

open Playground

(*****************************************************************************)
(* The catalogue *)
(*****************************************************************************)

let sections : Catalogue.section array = Array.of_list (Catalogue.parse Tinybox_data.catalogue)

let thumbnails : (string, Rgba_image.t Lazy.t) Hashtbl.t =
  let h = Hashtbl.create 256 in
  List.iter (fun (name, png) -> Hashtbl.replace h name (lazy (Png.decode png))) Tinybox_data.thumbnails;
  h

let everything : Catalogue.program list =
  Array.to_list sections |> List.concat_map (fun (s : Catalogue.section) -> s.programs)

let lowercase = String.lowercase_ascii

(*****************************************************************************)
(* Groups and filters *)
(*****************************************************************************)

type grouping = By_genre | By_era | By_platform | By_players

let groupings = [ By_genre; By_era; By_platform; By_players ]

let grouping_name = function
  | By_genre -> "genre"
  | By_era -> "era"
  | By_platform -> "machine"
  | By_players -> "players"

(* None: any *)
type filters = { players : string option; (* "1", "2", "net" *) era : int option; platform : string option; look : string option }

let no_filters = { players = None; era = None; platform = None; look = None }

let passes (f : filters) (p : Catalogue.program) : bool =
  (match f.players with
  | None -> true
  | Some "net" -> Catalogue.online p
  | Some "2" -> Catalogue.plays p 2
  | Some _ -> Catalogue.plays p 1)
  && (match f.era with None -> true | Some d -> Catalogue.decade p = d)
  && (match f.platform with None -> true | Some pf -> p.platform = pf)
  && match f.look with None -> true | Some l -> p.look = l

(* the values a filter goes through: only those some program has *)
let decades : int list = List.sort_uniq compare (List.map Catalogue.decade everything)

let platforms : string list =
  List.filter (fun pf -> List.exists (fun (p : Catalogue.program) -> p.platform = pf) everything) Catalogue.platforms

let looks = [ "2D"; "2.5D"; "3D"; "app" ]

let platform_title = function
  | "arcade" -> "Arcade"
  | "console" -> "Consoles"
  | "handheld" -> "Handhelds"
  | "computer" -> "Home computers"
  | "PC" -> "The PC"
  | "Mac" -> "The Macintosh"
  | "workstation" -> "Workstations"
  | "mainframe" -> "Mainframes and minicomputers"
  | "web" -> "The web"
  | "phone" -> "Phones"
  | "tabletop" -> "Tabletop"
  | "instrument" -> "Instruments"
  | pf -> String.capitalize_ascii pf

(* a section of the grid: the catalogue's, or made by a grouping *)
type group = { title : string; intro : string; games : bool option; programs : Catalogue.program list }

let by_year (ps : Catalogue.program list) = List.stable_sort (fun (a : Catalogue.program) b -> compare a.year b.year) ps

let groups (grouping : grouping) (f : filters) : group array =
  let keep ps = List.filter (passes f) ps in
  let make title intro programs = { title; intro; games = None; programs } in
  (match grouping with
  | By_genre ->
      Array.to_list sections
      |> List.map (fun (s : Catalogue.section) -> { title = s.title; intro = s.intro; games = Some s.games; programs = keep s.programs })
  | By_era ->
      decades
      |> List.map (fun d ->
             make (Printf.sprintf "The %ds" d)
               (Printf.sprintf "The programs whose original came out in the %ds, the oldest first." d)
               (by_year (keep (List.filter (fun p -> Catalogue.decade p = d) everything))))
  | By_platform ->
      platforms
      |> List.map (fun pf ->
             make (platform_title pf)
               (Printf.sprintf "The programs whose original ran on: %s. The oldest first." pf)
               (by_year (keep (List.filter (fun (p : Catalogue.program) -> p.platform = pf) everything))))
  | By_players ->
      [
        make "One player" "Alone, or against the computer." (keep (List.filter (fun p -> Catalogue.plays p 1) everything));
        make "Two players" "Two on one keyboard (or split screen)." (keep (List.filter (fun p -> Catalogue.plays p 2) everything));
        make "Over the network" "Two computers, one game: net=host on one, net=join on the other (Multiplayer)."
          (keep (List.filter Catalogue.online everything));
      ])
  |> List.filter (fun g -> g.programs <> [])
  |> Array.of_list

(* the value after [cur] in [values], and after the last one, any *)
let cycle (values : 'a list) (cur : 'a option) : 'a option =
  match cur with
  | None -> List.nth_opt values 0
  | Some v ->
      let rec after = function [] | [ _ ] -> None | x :: (y :: _ as rest) -> if x = v then Some y else after rest in
      after values

let contains (s : string) (q : string) : bool =
  let s = lowercase s and q = lowercase q in
  let n = String.length q in
  let rec at i = i + n <= String.length s && (String.sub s i n = q || at (i + 1)) in
  at 0

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type child = { pid : int; name : string }

type model = {
  grouping : grouping;
  filters : filters;
  section : int; (* in [groups m.grouping m.filters] *)
  pos : int; (* in [shown] *)
  search : string option; (* Some q: searching, the grid is every match *)
  before : string Set_.t; (* the keys down the frame before *)
  repeat : (string * float) option; (* an arrow held, when it moves again *)
  child : child option; (* the program running *)
  status : string; (* what happened to the last one *)
  code : Codemap.t option; (* the chosen one's code, shown instead of the menu *)
}

let initial_model : model =
  { grouping = By_genre; filters = no_filters; section = 0; pos = 0; search = None; before = Set_.empty; repeat = None; child = None; status = ""; code = None }

(* the section shown, if any passes the filters *)
let current_group (m : model) : group option =
  let gs = groups m.grouping m.filters in
  if gs = [||] then None else Some gs.(min m.section (Array.length gs - 1))

(* the programs in the grid *)
let shown (m : model) : Catalogue.program list =
  match m.search with
  | Some q when q <> "" ->
      List.filter
        (fun (p : Catalogue.program) -> passes m.filters p && (contains p.name q || contains p.after q || contains p.one_line q))
        everything
  | _ -> ( match current_group m with Some g -> g.programs | None -> [])

let chosen (m : model) : Catalogue.program option = List.nth_opt (shown m) m.pos

(*****************************************************************************)
(* Previews *)
(*****************************************************************************)

(* claude: the chosen program playing in the detail panel, after
 * Netflix's autoplay: every program is linked in tinybox and already
 * initialized, so starting one is instant -- no process, no window. Its
 * main is called with Playground.capture set, and the native platform's
 * run_app hands its app over instead of opening a window; the menu then
 * plays the app itself, a small platform: each frame the keys its golden
 * scene's script presses (Scenes_2d, Input_script), then a tick, each
 * turned into the app's messages by its subscriptions (Sub.event), as
 * Native_loop_2d turns SDL's events; its view scaled into the panel.
 * Silently (Audio.silently): a preview is seen, not heard.
 *
 * Not previewed, kept as pictures: the 3D programs (their main would open
 * an OpenGL window), and the programs whose main calls Cap.main (a second
 * Cap.main fails: a preview gets no authority, by design). A program
 * without a scripted scene previews its title screen, animated. *)

type preview =
  | Playing : {
      name : string;
      app : ('model, 'msg) app;
      mutable model : 'model;
      mutable frame : int;
      script : Input_script.t option;
      length : int; (* the frames before it starts over *)
    }
      -> preview
  (* claude: a 3D program: no messages, a computer given each frame, its
   * keyboard kept here; its views drawn by the software rasterizer *)
  | Playing3d : {
      name : string;
      app : ('model, 'msg) Playground3d.app3d;
      mutable model : 'model;
      mutable frame : int;
      script : Input_script.t option;
      length : int;
      mutable keyboard : keyboard;
      mutable computer : computer; (* the last one, for its views *)
      mutable ms : float; (* the rasterizer's time a frame, averaged *)
      options : Render.options; (* the program's rendering, as the software backend makes it *)
      mutable image : Rgba_image.t option; (* the last frame rasterized *)
    }
      -> preview

let preview_name = function Playing p -> p.name | Playing3d p -> p.name

(* the menu's frames, counted by update: the previews' clock, frames
 * rather than seconds, so that -fixed-time shows them too *)
let frames = ref 0

(* the program chosen, and the frame it was *)
let chosen_since : (string * int) ref = ref ("", 0)
let preview : preview option ref = ref None

(* what a program's main gave, taken once *)
type captured = App of any_app | App3d of Playground3d.any_app3d * Playground3d.rendering
let captured : (string, captured option) Hashtbl.t = Hashtbl.create 16

let capture (p : Catalogue.program) : captured option =
  match Hashtbl.find_opt captured p.name with
  | Some a -> a
  | None ->
      let got = ref None in
      (match List.assoc_opt p.name (Program.collected ()) with
      | None -> ()
      | Some entry ->
          Playground.capture := Some (fun a -> got := Some (App a));
          Playground3d.capture3d := Some (fun a rendering -> got := Some (App3d (a, rendering)));
          (* claude: a main calling Cap.main fails here, by design *)
          (try Audio.silently entry with _ -> ());
          Playground.capture := None;
          Playground3d.capture3d := None);
      Hashtbl.replace captured p.name !got;
      !got

(* the program's first scripted scene: its keys, and how long it lasts *)
let scene (name : string) : (Input_script.t * int) option =
  List.find_map
    (fun ((exe, _, frame, script) : Golden_scene.scripted) ->
      if Filename.basename exe <> name then None
      else match Input_script.parse script with Ok sc -> Some (sc, frame) | Error _ -> None)
    (Scenes_2d.scripted @ Scenes_3d.scripted)

(* the screen a preview's program sees: the window it would have had *)
let preview_screen = to_screen 1000. 1000.

let start_preview (name : string) (c : captured) : preview option =
  let script, length = match scene name with Some (sc, n) -> (Some sc, n + 90) | None -> (None, 600) in
  match c with
  | App (Any_app app) -> (
      match Audio.silently (fun () -> app.init []) with
      | model, _cmd -> Some (Playing { name; app; model; frame = 0; script; length })
      | exception _ -> None)
  | App3d (Playground3d.Any_app3d app, rendering) -> (
      match Audio.silently (fun () -> Playground3d.init3d app ()) with
      | model ->
          let computer = { initial_computer with screen = preview_screen } in
          (* claude: as software/Playground3d_platform.ml makes its options *)
          let options =
            {
              Render.default_options with
              shading =
                (match rendering.shading with
                | No_lighting -> Shading.Flat_color
                | Flat -> Shading.Flat_shading
                | Smooth -> Shading.Phong);
              backface_culling = rendering.backface_culling;
              bilinear = rendering.smooth_textures;
            }
          in
          Some
            (Playing3d { name; app; model; frame = 0; script; length; keyboard = computer.keyboard; computer; ms = 0.; options; image = None })
      | exception _ -> None)

(* one frame of the preview: the script's keys, then a tick *)
let step_preview (now : number) (pv : preview) : unit =
  match pv with
  | Playing p ->
      let apply event =
        match Sub.event_to_msgopt event (p.app.subscriptions p.model) with
        | Some msg -> p.model <- fst (p.app.update msg p.model)
        | None -> ()
      in
      p.frame <- p.frame + 1;
      Audio.silently (fun () ->
          Option.iter
            (fun sc -> List.iter (fun (key, down) -> apply (Sub.EKeyChanged (down, key))) (Input_script.changes sc p.frame))
            p.script;
          apply (Sub.ETick now))
  | Playing3d p ->
      p.frame <- p.frame + 1;
      Option.iter
        (fun sc ->
          List.iter (fun (key, down) -> p.keyboard <- update_keyboard down key p.keyboard) (Input_script.changes sc p.frame))
        p.script;
      p.computer <- { p.computer with keyboard = p.keyboard; time = Time now };
      p.model <- Audio.silently (fun () -> Playground3d.update3d p.app p.computer p.model)

(* the preview of the program chosen: started after 60 frames on it,
 * started again when its scene is over, stopped on a failure *)
let preview_of (now : number) (chosen : Catalogue.program option) : unit =
  incr frames;
  let frame_length = function Playing q -> (q.frame, q.length) | Playing3d q -> (q.frame, q.length) in
  match chosen with
  | None -> preview := None
  | Some p -> (
      if fst !chosen_since <> p.name then (
        chosen_since := (p.name, !frames);
        preview := None);
      match !preview with
      | Some pv when preview_name pv = p.name ->
          let frame, length = frame_length pv in
          if frame >= length then preview := Option.bind (capture p) (start_preview p.name)
          else (try step_preview now pv with _ -> preview := None)
      | _ ->
          if !frames - snd !chosen_since >= 60 then preview := Option.bind (capture p) (start_preview p.name))

(* claude: the 3D previews' drawing: a view at a time into a framebuffer
 * the panel's size (a split screen's views each into their area, as
 * the software backend's draw_view), then the pixels as an image *)
let buffers : (int * int, Framebuffer.t * Zbuffer.t) Hashtbl.t = Hashtbl.create 4

let buffer (w : int) (h : int) : Framebuffer.t * Zbuffer.t =
  match Hashtbl.find_opt buffers (w, h) with
  | Some b -> b
  | None ->
      let b = (Framebuffer.create ~width:w ~height:h, Zbuffer.create ~width:w ~height:h) in
      Hashtbl.replace buffers (w, h) b;
      b

let image_of (fb : Framebuffer.t) : Rgba_image.t =
  let img = Rgba_image.create ~width:fb.width ~height:fb.height in
  for y = 0 to fb.height - 1 do
    for x = 0 to fb.width - 1 do
      let c = Framebuffer.get_rgb fb ~x ~y and i = 4 * ((y * fb.width) + x) in
      Bigarray.Array1.unsafe_set img.rgba i ((c lsr 16) land 0xFF);
      Bigarray.Array1.unsafe_set img.rgba (i + 1) ((c lsr 8) land 0xFF);
      Bigarray.Array1.unsafe_set img.rgba (i + 2) (c land 0xFF);
      Bigarray.Array1.unsafe_set img.rgba (i + 3) 0xFF
    done
  done;
  img

let render3d (options : Render.options) (size : int) (views : Playground3d.view list) : Rgba_image.t =
  let fb, _ = buffer size size in
  Texture_decode.load_queued ();
  Framebuffer.clear fb ~rgb:0xFFFFFF;
  List.iter
    (fun (v : Playground3d.view) ->
      let x0 = int_of_float (Float.round (v.area.x *. float_of_int size)) in
      let x1 = int_of_float (Float.round ((v.area.x +. v.area.w) *. float_of_int size)) in
      (* the framebuffer's rows go down, the area's y up *)
      let y0 = int_of_float (Float.round ((1. -. v.area.y -. v.area.h) *. float_of_int size)) in
      let y1 = int_of_float (Float.round ((1. -. v.area.y) *. float_of_int size)) in
      let w = x1 - x0 and h = y1 - y0 in
      if w > 0 && h > 0 then begin
        let sub, zb = buffer w h in
        if sub != fb then Framebuffer.clear sub ~rgb:0xFFFFFF;
        Preview3d_render.render ~options sub zb v.camera (Playground3d.group3d v.shapes);
        if sub != fb then
          for r = 0 to h - 1 do
            Bigarray.Array1.blit (Bigarray.Array2.slice_left sub.pixels r)
              (Bigarray.Array1.sub (Bigarray.Array2.slice_left fb.pixels (y0 + r)) x0 w)
          done
      end)
    views;
  image_of fb

(* the panel's size in pixels (Layout's [shot]) *)
let preview_pixels = 400

(* claude: a scene slower than this to rasterize is rasterized every
 * n-th frame only (its program still updated every frame), the last
 * picture shown between: the menu stays smooth (TinyMinecraft: 236 ms) *)
let budget_ms = 20.

let every (ms : float) : int = max 1 (int_of_float (ms /. budget_ms))

(* the preview's view, in the program's 1000 by 1000, if it is the
 * program's: a 2D program's shapes; a 3D program's frame, rasterized at
 * the panel's size and shown at 1000 (so, in the panel, pixel for pixel),
 * and its HUD's shapes over it *)
let preview_shapes (p : Catalogue.program) : shape list option =
  match !preview with
  | Some (Playing q) when q.name = p.name -> ( try Some (q.app.view q.model) with _ -> None)
  | Some (Playing3d q) when q.name = p.name -> (
      try
        let views = Playground3d.views3d q.app q.computer q.model in
        (match q.image with
        | Some _ when q.frame mod every q.ms <> 0 -> ()
        | _ ->
            let t0 = Unix.gettimeofday () in
            q.image <- Some (render3d q.options preview_pixels views);
            let ms = (Unix.gettimeofday () -. t0) *. 1000. in
            q.ms <- (if q.ms = 0. then ms else (0.8 *. q.ms) +. (0.2 *. ms)));
        match q.image with
        | Some img -> Some (bitmap 1000. 1000. img :: Playground3d.views_hud preview_screen views)
        | None -> None
      with _ -> None)
  | _ -> None

(* the 3D preview's rasterizer time, for the panel *)
let preview_ms (p : Catalogue.program) : float option =
  match !preview with Some (Playing3d q) when q.name = p.name && q.frame > 10 -> Some q.ms | _ -> None

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* claude: the menu's screen, 16:9 (run_app's ~screen): the grid on the
 * left, the chosen one on the right *)
let screen_w = 1778
let screen_h = 1000
let left_edge = -869. (* the left margin's end *)

let cols = 5
let rows = 4
let thumb = 132.
let cell_w = 160.
let cell_h = 172.
let grid_left = left_edge (* the first column's left edge *)
let grid_top = 285. (* the first row's top edge *)
let shot = 400. (* the chosen one's screenshot *)
let shot_x = 160.
let shot_y = 230.
let text_x = 385. (* what the catalogue says, right of the screenshot *)
let text_w = 480.

(* its code, under both: where Code_map draws it (its top left, pixels) *)
let code_area = (-40., 10., 909, 440)

let in_code_area ((mx, my) : number * number) : bool =
  let x, y, w, h = code_area in
  mx >= x && mx <= x +. float_of_int w && my <= y && my >= y -. float_of_int h

(* the first row shown: the chosen one's row kept in view *)
let first_row (m : model) : int = max 0 ((m.pos / cols) - rows + 1)

(* the centre of the thumbnail of the program at [i] in [shown], if it
 * is in view *)
let cell_centre (m : model) (i : int) : (number * number) option =
  let r = (i / cols) - first_row m in
  if r < 0 || r >= rows then None
  else
    let c = i mod cols in
    Some (grid_left +. (float_of_int c *. cell_w) +. (thumb /. 2.), grid_top -. (float_of_int r *. cell_h) -. (thumb /. 2.))

(* the buttons at the top: the two shelves, the section's arrows *)
let games_tab = (-560., 455.)
let apps_tab = (-460., 455.)
let prev_arrow = (-859., 400.)
let next_arrow = (-499., 400.)

(* the filter bar: each word's left end, and its key *)
let bar_y = 322.
let bar = [ ("b", -869.); ("p", -709.); ("e", -559.); ("m", -434.); ("l", -284.); ("c", -164.) ]
let bar_width = 120.

let near ((x, y) : number * number) ((mx, my) : number * number) ~(w : number) ~(h : number) : bool =
  Float.abs (mx -. x) <= w /. 2. && Float.abs (my -. y) <= h /. 2.

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let now (computer : computer) : number =
  let (Time t) = computer.time in
  t

(* the section after (or before) the one shown, across both shelves *)
let step_section (m : model) (d : int) : int =
  let n = max 1 (Array.length (groups m.grouping m.filters)) in
  (min m.section (n - 1) + d + n) mod n

(* the first section of a shelf, the programs grouped by genre again *)
let to_shelf (m : model) (games : bool) : model =
  let gs = groups By_genre m.filters in
  let rec go i = if i >= Array.length gs || gs.(i).games = Some games then i else go (i + 1) in
  { m with grouping = By_genre; section = min (go 0) (max 0 (Array.length gs - 1)); pos = 0; search = None }

(* claude: the program started in a process of its own, this same
 * binary under its name (tinybox <Name>): its window, its Cap.main, and
 * a crash that is not the menu's *)
let start (_caps : < Cap.fork ; Cap.exec ; .. >) (runnable : string list) (m : model) : model =
  match (chosen m, m.child) with
  | Some p, None when List.mem p.name runnable ->
      let exe = Sys.executable_name in
      let pid = Unix.create_process exe [| exe; p.name |] Unix.stdin Unix.stdout Unix.stderr in
      { m with child = Some { pid; name = p.name }; status = "" }
  | Some p, None -> { m with status = p.name ^ " is not in this tinybox" }
  | _, Some c -> { m with status = c.name ^ " is still running" }
  | None, None -> m

(* the program running, looked at once a frame *)
let wait (_caps : < Cap.wait ; .. >) (m : model) : model =
  match m.child with
  | None -> m
  | Some c -> (
      match Unix.waitpid [ Unix.WNOHANG ] c.pid with
      | 0, _ -> m
      | _, Unix.WEXITED 0 -> { m with child = None; status = "" }
      | _, Unix.WEXITED n -> { m with child = None; status = Printf.sprintf "%s exited with %d" c.name n }
      | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> { m with child = None; status = Printf.sprintf "%s killed by signal %d" c.name n }
      | exception Unix.Unix_error _ -> { m with child = None })

(* an arrow's press, and then, held, again every 0.08 s after 0.4 s *)
let arrow (computer : computer) (m : model) : string option * (string * float) option =
  let keys = computer.keyboard.keys in
  let t = now computer in
  let arrows = [ "ArrowLeft"; "ArrowRight"; "ArrowUp"; "ArrowDown" ] in
  match List.find_opt (fun k -> Set_.mem k keys && not (Set_.mem k m.before)) arrows with
  | Some k -> (Some k, Some (k, t +. 0.4))
  | None -> (
      match m.repeat with
      | Some (k, next) when Set_.mem k keys -> if t >= next then (Some k, Some (k, t +. 0.08)) else (None, m.repeat)
      | _ -> (None, None))

let move_pos (m : model) (key : string) : model =
  let n = List.length (shown m) in
  let pos =
    match key with
    | "ArrowLeft" -> m.pos - 1
    | "ArrowRight" -> m.pos + 1
    | "ArrowUp" -> m.pos - cols
    | "ArrowDown" -> m.pos + cols
    | _ -> m.pos
  in
  { m with pos = max 0 (min (n - 1) pos) }

let to_section (m : model) (i : int) : model = { m with section = i; pos = 0; search = None }

(* a key of the filter bar *)
let bar_key (computer : computer) (m : model) (key : string) : model =
  let f = m.filters in
  let refilter filters = { m with filters; section = 0; pos = 0 } in
  match key with
  | "b" -> { m with grouping = Option.value ~default:By_genre (cycle groupings (Some m.grouping)); section = 0; pos = 0 }
  | "p" -> refilter { f with players = cycle [ "1"; "2"; "net" ] f.players }
  | "e" -> refilter { f with era = cycle decades f.era }
  | "m" -> refilter { f with platform = cycle platforms f.platform }
  | "l" -> refilter { f with look = cycle looks f.look }
  | "c" -> refilter no_filters
  | "r" ->
      (* claude: a random program of the grid, from the clock (the same
       * one under -fixed-time, so a golden frame could show it) *)
      let n = List.length (shown m) in
      if n = 0 then m else { m with pos = Hashtbl.hash (int_of_float (now computer *. 1000.)) mod n }
  | _ -> m

(* the chosen program's code map *)
let open_code (screen : screen) (m : model) : model =
  match chosen m with
  | Some p ->
      (* the whole screen, but for the title above and the status and keys
       * below *)
      let area = (screen.left +. 20., screen.top -. 80., int_of_float screen.width - 40, int_of_float screen.height - 150) in
      { m with code = Some (Codemap.make ~area ~sources:Tinybox_sources.sources ~program:p.name ~path:p.source) }
  | None -> m

let update (caps : < Cap.fork ; Cap.exec ; Cap.wait ; .. >) (runnable : string list) (computer : computer) (m : model) :
    model =
  let m = wait caps m in
  match m.code with
  | Some code ->
      let keys = computer.keyboard.keys in
      let pressed k = Set_.mem k keys && not (Set_.mem k m.before) in
      let key, repeat = arrow computer m in
      { m with code = Codemap.update computer ~pressed ~arrow:key code; repeat; before = keys }
  | None ->
  let keys = computer.keyboard.keys in
  let pressed k = Set_.mem k keys && not (Set_.mem k m.before) in
  let shift = Set_.mem "Shift" keys in
  let key, repeat = arrow computer m in
  let m = { m with repeat } in
  let m = match key with Some k -> move_pos m k | None -> m in
  let m =
    match m.search with
    | Some q ->
        (* claude: typing: every character typed is the query's, but
         * for the / that opened it, already typed this frame *)
        let typed = String.concat "" (List.map (String.make 1) (List.filter (fun c -> c >= ' ' && c <> '/') (List.of_seq (String.to_seq computer.keyboard.typed)))) in
        if pressed "Escape" then { m with search = None; pos = 0 }
        else if pressed "Backspace" && q <> "" then { m with search = Some (String.sub q 0 (String.length q - 1)); pos = 0 }
        else if typed <> "" then { m with search = Some (q ^ typed); pos = 0 }
        else m
    | None ->
        if pressed "/" then { m with search = Some ""; pos = 0 }
        else if pressed "Tab" then to_section m (step_section m (if shift then -1 else 1))
        else if pressed "PageDown" then to_section m (step_section m 1)
        else if pressed "PageUp" then to_section m (step_section m (-1))
        else if pressed "g" then to_shelf m true
        else if pressed "a" then to_shelf m false
        else if pressed "s" then open_code computer.screen m
        else
          match List.find_opt pressed [ "b"; "p"; "e"; "m"; "l"; "c"; "r" ] with
          | Some k -> bar_key computer m k
          | None -> m
  in
  let m = if pressed "Enter" then start caps runnable m else m in
  (* the mouse *)
  let mouse = computer.mouse in
  let at = (mouse.mx, mouse.my) in
  let m =
    if mouse.mwheel > 0. then move_pos m "ArrowUp" else if mouse.mwheel < 0. then move_pos m "ArrowDown" else m
  in
  let m =
    if not (mouse.mclick || mouse.mdouble) then m
    else if near games_tab at ~w:90. ~h:36. then to_shelf m true
    else if near apps_tab at ~w:80. ~h:36. then to_shelf m false
    else if in_code_area at then open_code computer.screen m
    else if near prev_arrow at ~w:40. ~h:40. then to_section m (step_section m (-1))
    else if near next_arrow at ~w:40. ~h:40. then to_section m (step_section m 1)
    else
      match List.find_opt (fun (_, x) -> near (x +. (bar_width /. 2.), bar_y) at ~w:bar_width ~h:24.) bar with
      | Some (k, _) -> bar_key computer m k
      | None ->
      let n = List.length (shown m) in
      match List.find_opt (fun i -> match cell_centre m i with Some c -> near c at ~w:thumb ~h:(thumb +. 30.) | None -> false) (List.init n Fun.id) with
      | Some i -> if mouse.mdouble then start caps runnable { m with pos = i } else { m with pos = i }
      | None -> m
  in
  preview_of (now computer) (chosen m);
  { m with before = keys }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let background = rgb 12 10 28
let panel = rgb 26 22 56
let cyan = rgb 0 225 255
let magenta = rgb 255 60 170
let yellow = rgb 255 215 70
let ink = rgb 228 228 240
let dim = rgb 140 140 180
let grey = rgb 80 80 100

(* claude: how wide a character is, for the size given: the playground
 * centres words and cannot say how wide they are, so a left end is
 * placed by an estimate -- 0.47 of the size for Cairo's sans-serif,
 * measured on this menu's own frames (Widget.text_width's 0.6 is the
 * Hershey font's) *)
let em = 0.47

(* words at [size], their left end at [x] *)
let text ?(size = 16.) (color : color) (x : number) (y : number) (s : string) : shape =
  let w = em *. size *. float_of_int (String.length s) in
  words color s |> scale (size /. words_font_size) |> move (x +. (w /. 2.)) y

let centred ?(size = 16.) (color : color) (x : number) (y : number) (s : string) : shape =
  words color s |> scale (size /. words_font_size) |> move x y

(* [s] cut at the last character that fits in [width] at [size], with
 * "..." *)
let cut ~(size : number) ~(width : number) (s : string) : string =
  let n = int_of_float (width /. (em *. size)) in
  if String.length s <= n then s else String.sub s 0 (max 0 (n - 3)) ^ "..."

(* [s] in lines of at most [width] at [size], at most [lines] of them *)
let wrap ~(size : number) ~(width : number) ~(lines : int) (s : string) : string list =
  let n = max 1 (int_of_float (width /. (em *. size))) in
  let words = List.filter (( <> ) "") (String.split_on_char ' ' s) in
  let rec go acc line = function
    | [] -> List.rev (if line = "" then acc else line :: acc)
    | w :: rest ->
        let next = if line = "" then w else line ^ " " ^ w in
        if String.length next <= n || line = "" then go acc next rest else go (line :: acc) w rest
  in
  let all = go [] "" words in
  if List.length all <= lines then all
  else List.filteri (fun i _ -> i < lines - 1) all @ [ cut ~size ~width (String.concat " " (List.filteri (fun i _ -> i >= lines - 1) all)) ]

let paragraph ?(size = 14.) (color : color) (x : number) (y : number) ~(width : number) ~(lines : int) (s : string) :
    shape list =
  List.mapi (fun i l -> text ~size color x (y -. (float_of_int i *. size *. 1.45)) l) (wrap ~size ~width ~lines s)

(* a frame [w] by [h] around (0, 0), [t] thick *)
let frame (color : color) (w : number) (h : number) (t : number) : shape =
  group
    [
      rectangle color (w +. (2. *. t)) t |> move_y ((h +. t) /. 2.);
      rectangle color (w +. (2. *. t)) t |> move_y (-.(h +. t) /. 2.);
      rectangle color t h |> move_x (-.(w +. t) /. 2.);
      rectangle color t h |> move_x ((w +. t) /. 2.);
    ]

let picture (p : Catalogue.program) (size : number) : shape =
  match Hashtbl.find_opt thumbnails p.name with
  | Some img -> bitmap size size (Lazy.force img)
  | None -> group [ rectangle panel size size; centred dim 0. 0. "no picture" ]

let header (computer : computer) (m : model) : shape list =
  let shelf = match (m.search, current_group m) with None, Some g -> g.games | _ -> None in
  let games = shelf = Some true and apps = shelf = Some false in
  let tab on (x, y) label = [ text ~size:22. (if on then yellow else dim) (x -. 40.) y label ] @ if on then [ rectangle yellow 70. 3. |> move (x -. 2.) (y -. 17.) ] else [] in
  [ text ~size:40. magenta left_edge 452. "TINY"; text ~size:40. cyan (left_edge +. 96.) 452. "BOX" ]
  @ tab games games_tab "GAMES"
  @ tab apps apps_tab "APPS"
  @ [
      (match m.search with
      | Some q ->
          let cursor = if Float.rem (now computer) 1. < 0.5 then "_" else " " in
          text ~size:18. yellow (-340.) 452. (cut ~size:18. ~width:250. ("/" ^ q ^ cursor))
      | None -> text ~size:14. dim (-330.) 452. "/ to search");
    ]

let section_bar (m : model) : shape list =
  let n = List.length (shown m) in
  match m.search with
  | Some q -> [ text ~size:24. yellow left_edge 400. (Printf.sprintf "Search: %s" q); text ~size:14. dim (-150.) 400. (Printf.sprintf "%d found" n) ]
  | None -> (
      let gs = groups m.grouping m.filters in
      match current_group m with
      | None -> [ text ~size:24. magenta left_edge 400. "Nothing passes the filters"; text ~size:13. dim left_edge 360. "c clears them" ]
      | Some g ->
          [
            centred ~size:24. cyan (fst prev_arrow) (snd prev_arrow) "<";
            text ~size:24. yellow (-834.) 400. (cut ~size:24. ~width:320. g.title);
            centred ~size:24. cyan (fst next_arrow) (snd next_arrow) ">";
            text ~size:14. dim (-150.) 400. (Printf.sprintf "%d / %d" (min m.section (Array.length gs - 1) + 1) (Array.length gs));
          ]
          @ paragraph ~size:13. dim left_edge 360. ~width:800. ~lines:1 g.intro)

(* the filter bar: each key, what it chooses, its value (yellow when it
 * filters) *)
let filter_bar (m : model) : shape list =
  let f = m.filters in
  let any = function Some v -> (v, true) | None -> ("any", false) in
  let words = function
    | "b" -> ("group", (grouping_name m.grouping, m.grouping <> By_genre))
    | "p" -> ("players", any f.players)
    | "e" -> ("era", any (Option.map (fun d -> Printf.sprintf "%ds" d) f.era))
    | "m" -> ("machine", any f.platform)
    | "l" -> ("look", any f.look)
    | _ -> ("clear  r random", ("", false))
  in
  List.concat_map
    (fun (k, x) ->
      let label, (value, on) = words k in
      (* claude: each word placed on its own, with room to spare: short
       * words' widths are the estimate's worst *)
      [ text ~size:13. cyan x bar_y k; text ~size:13. dim (x +. 16.) bar_y (if value = "" then label else label ^ ":");
        text ~size:13. (if on then yellow else ink) (x +. 24. +. (8. *. float_of_int (String.length label))) bar_y value ])
    bar

let grid (computer : computer) (runnable : string list) (m : model) : shape list =
  shown m
  |> List.mapi (fun i (p : Catalogue.program) -> (i, p))
  |> List.concat_map (fun (i, (p : Catalogue.program)) ->
         match cell_centre m i with
         | None -> []
         | Some (x, y) ->
             let on = i = m.pos in
             let ok = List.mem p.name runnable in
             let glow = wave 0.35 1. 1.2 computer.time in
             [
               group
                 ((if on then [ frame cyan thumb thumb 4. |> fade glow ] else [ frame grey thumb thumb 1. ])
                 @ [ picture p thumb |> fade (if ok then 1. else 0.35) ])
               |> move x y;
               centred ~size:13. (if on then yellow else if ok then ink else grey) x (y -. (thumb /. 2.) -. 16.) (cut ~size:13. ~width:cell_w p.name);
             ])

(* "1 player", "1-2 players, over the network" *)
let players_text (p : Catalogue.program) : string =
  let count = match String.index_opt p.players ' ' with Some i -> String.sub p.players 0 i | None -> p.players in
  (count ^ if count = "1" then " player" else " players") ^ if Catalogue.online p then ", over the network" else ""

(* claude: the chosen one's code at a glance, its own code's map
 * (Codemap.preview) in the panel under its picture: made once it has
 * been chosen for 20 frames (lexing its files is not free while the
 * arrows run through the grid), the last one kept (a map keeps its
 * picture) *)
let code_preview : (string * Code_map.t) option ref = ref None

let code_of (p : Catalogue.program) : Code_map.t option =
  match !code_preview with
  | Some (name, c) when name = p.name -> Some c
  | _ ->
      if fst !chosen_since = p.name && !frames - snd !chosen_since >= 20 then begin
        let c = Codemap.preview ~area:code_area ~sources:Tinybox_sources.sources ~program:p.name ~path:p.source in
        code_preview := Some (p.name, c);
        Some c
      end
      else None

let code_panel (computer : computer) (p : Catalogue.program) : shape list =
  let x, y, w, h = code_area in
  let w = float_of_int w and h = float_of_int h in
  let cx = x +. (w /. 2.) and cy = y -. (h /. 2.) in
  (match code_of p with
  | Some c -> Code_map.view ~chrome:false computer c
  | None -> [ rectangle panel w h |> move cx cy; centred ~size:14. dim cx cy "its code..." ])
  @ [ frame cyan w h 2. |> move cx cy; text ~size:12. cyan x (y -. h -. 16.) "its code: a click, or s, explores it" ]
  (* claude: the mouse over it: a magnifying glass, the code under it
   * readable (Code_map.lens), over everything else *)
  @ match code_of p with Some c -> Code_map.lens computer c | None -> []

(* the chosen one, large, and what the catalogue says of it *)
let details (computer : computer) (runnable : string list) (m : model) : shape list =
  match chosen m with
  | None -> [ centred ~size:18. dim shot_x shot_y "nothing here" ]
  | Some p ->
      let left = text_x in
      let ok = List.mem p.name runnable in
      [ frame magenta shot shot 3. |> move shot_x shot_y ]
      @ (match preview_shapes p with Some _ -> [] | None -> [ picture p shot |> move shot_x shot_y ])
      @ [
          text ~size:24. ink left 410. (cut ~size:24. ~width:(text_w -. 60.) p.name);
          text ~size:16. yellow (left +. text_w -. 40.) 410. p.look;
          text ~size:13. yellow left 380. (Printf.sprintf "%d   %s   %s" p.year p.platform (players_text p));
        ]
      (* claude: the software rasterizer's time on a 3D preview: tinybox
       * as its stress test, every 3D game drawn live *)
      @ (match preview_ms p with
        | Some ms ->
            let n = every ms in
            [ text ~size:11. dim left 358.
                (Printf.sprintf "rasterized in %.0f ms%s" ms (if n = 1 then "" else Printf.sprintf ", one frame in %d" n)) ]
        | None -> [])
      @ paragraph ~size:14. cyan left 330. ~width:text_w ~lines:2 ("After " ^ p.after)
      @ paragraph ~size:14. ink left 280. ~width:text_w ~lines:3 p.one_line
      @ paragraph ~size:12. dim left 210. ~width:text_w ~lines:9 p.brought
      @ (if ok then [] else [ text ~size:14. magenta left 45. "not in this tinybox" ])
      @ code_panel computer p

let footer (m : model) : shape list =
  let playing = match m.child with Some c -> [ text ~size:18. yellow left_edge (-440.) ("> " ^ c.name ^ " is running") ] | None -> [] in
  [ text ~size:13. dim left_edge (-475.) "arrows move   tab section   g/a games/apps   b group   p e m l filter   / search   s code   enter play" ]
  @ playing
  @ if m.status = "" then [] else [ text ~size:16. magenta (-400.) (-440.) (cut ~size:16. ~width:340. m.status) ]

(* claude: a CRT's lines, every 4 pixels a darker one over everything *)
let scanlines (screen : screen) : shape list =
  List.init (int_of_float (screen.height /. 4.)) (fun i ->
      rectangle black screen.width 1.5 |> move_y (screen.top -. (float_of_int i *. 4.)) |> fade 0.18)

(* claude: the preview, its 1000 by 1000 scaled to the panel's 400, and
 * then the background's colour all round the panel, over whatever the
 * program draws beyond its screen (the playground has no clipping) *)
let live (screen : screen) (m : model) : shape list =
  match Option.bind (chosen m) preview_shapes with
  | None -> []
  | Some shapes ->
      let half = shot /. 2. in
      let band w h x y = rectangle background w h |> move x y in
      [
        rectangle black shot shot |> move shot_x shot_y;
        group shapes |> scale (shot /. 1000.) |> move shot_x shot_y;
        band screen.width (screen.top -. (shot_y +. half)) 0. ((screen.top +. shot_y +. half) /. 2.);
        band screen.width ((shot_y -. half) -. screen.bottom) 0. ((screen.bottom +. shot_y -. half) /. 2.);
        band ((shot_x -. half) -. screen.left) shot ((screen.left +. shot_x -. half) /. 2.) shot_y;
        band (screen.right -. (shot_x +. half)) shot ((screen.right +. shot_x +. half) /. 2.) shot_y;
      ]

let view (runnable : string list) (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  match m.code with
  | Some code -> Codemap.view computer code
  | None ->
  [ rectangle background screen.width screen.height ]
  @ live screen m
  @ header computer m @ section_bar m @ filter_bar m @ grid computer runnable m @ details computer runnable m @ footer m @ scanlines screen

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run (caps : < Cap.fork ; Cap.exec ; Cap.wait ; .. >) (runnable : string list) : unit =
  let caps = (caps :> < Cap.fork ; Cap.exec ; Cap.wait >) in
  Playground_platform.run_app ~screen:(screen_w, screen_h) ~flags:(Playground_platform.flags ()) (game (view runnable) (update caps runnable) initial_model)
