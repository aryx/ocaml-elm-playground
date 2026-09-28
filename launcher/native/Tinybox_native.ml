(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tinybox_native.mli *)

open Playground

(*****************************************************************************)
(* Thumbnails *)
(*****************************************************************************)

(* the PNGs in the binary (Tinybox_thumbs, made at build time from the
 * golden frames), decoded when first shown; a backend keeps the last 32
 * bitmaps it converted, so the grid shows 12 at a time, plus the chosen
 * one enlarged (the same bitmap) *)
let thumbnails : (string, Rgba_image.t Lazy.t) Hashtbl.t =
  let h = Hashtbl.create 256 in
  List.iter (fun (name, png) -> Hashtbl.replace h name (lazy (Png.decode png))) Tinybox_thumbs.thumbnails;
  h

let thumbnail (p : Catalogue.program) (size : number) : shape option =
  Option.map (fun img -> bitmap size size (Lazy.force img)) (Hashtbl.find_opt thumbnails p.name)

(*****************************************************************************)
(* The program started *)
(*****************************************************************************)

type child = { pid : int; name : string }

let child : child option ref = ref None

(* claude: the program started in a process of its own, this same
 * binary under its name (tinybox <Name>): its window, its Cap.main, and
 * a crash that is not the menu's *)
let play (_caps : < Cap.fork ; Cap.exec ; .. >) (runnable : string list) (p : Catalogue.program) : string =
  match !child with
  | None when List.mem p.name runnable ->
      let exe = Sys.executable_name in
      let pid = Unix.create_process exe [| exe; p.name |] Unix.stdin Unix.stdout Unix.stderr in
      child := Some { pid; name = p.name };
      ""
  | None -> p.name ^ " is not in this tinybox"
  | Some c -> c.name ^ " is still running"

(* the program running, looked at once a frame *)
let ended (_caps : < Cap.wait ; .. >) () : string option =
  match !child with
  | None -> None
  | Some c -> (
      let over status = child := None; Some status in
      match Unix.waitpid [ Unix.WNOHANG ] c.pid with
      | 0, _ -> None
      | _, Unix.WEXITED 0 -> over ""
      | _, Unix.WEXITED n -> over (Printf.sprintf "%s exited with %d" c.name n)
      | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> over (Printf.sprintf "%s killed by signal %d" c.name n)
      | exception Unix.Unix_error _ -> over "")

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

(* the preview of the program chosen: started after 60 frames on it
 * ([dwell], the menu's count), started again when its scene is over,
 * stopped on a failure *)
let preview_of ~(now : number) ~(dwell : int) (chosen : Catalogue.program option) : unit =
  let frame_length = function Playing q -> (q.frame, q.length) | Playing3d q -> (q.frame, q.length) in
  match chosen with
  | None -> preview := None
  | Some p -> (
      match !preview with
      | Some pv when preview_name pv = p.name ->
          let frame, length = frame_length pv in
          if frame >= length then preview := Option.bind (capture p) (start_preview p.name)
          else (try step_preview now pv with _ -> preview := None)
      | _ ->
          preview := None;
          if dwell >= 60 then preview := Option.bind (capture p) (start_preview p.name))

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
(* The host *)
(*****************************************************************************)

let host (caps : < Cap.fork ; Cap.exec ; Cap.wait ; .. >) (runnable : string list) : Tinybox_menu.host =
  {
    runnable;
    thumbnail;
    play = play caps runnable;
    running = (fun () -> Option.map (fun c -> c.name) !child);
    ended = ended caps;
    sources = (fun () -> Tinybox_menu.Sources Tinybox_sources.sources);
    preview =
      Some
        {
          step = preview_of;
          shapes = preview_shapes;
          note =
            (fun p ->
              Option.map
                (fun ms ->
                  let n = every ms in
                  Printf.sprintf "rasterized in %.0f ms%s" ms (if n = 1 then "" else Printf.sprintf ", one frame in %d" n))
                (preview_ms p));
        };
  }
