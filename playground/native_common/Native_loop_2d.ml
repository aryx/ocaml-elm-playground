(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Tsdl
module E = Sub

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The SDL side of the native 2D backends (window creation, CLI/logging
 * setup, event draining, frame pacing). What each backend supplies is
 * only how a Playground.shape list becomes pixels.
 *
 * Not the same loop as playground3d/native_common/Native_loop.ml: that one
 * drives Playground3d's computer-based update3d/view3d directly, while this
 * one feeds SDL events through the 2D app's [subscriptions] to get msgs,
 * like the web backend does.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let spf = Printf.sprintf

(* The tsdl library is a heavy user of Result, which is annoying
 * to check at every calls; fortunately OCaml 4.08 allow to define
 * monadic operators to remove some boilerplate!
 *)
let (let*) o f =
  match o with
  | Error (`Msg msg) ->
      failwith (spf "TSDL error: %s" msg)
  | Ok x -> f x

let scancode_to_keystring = function
 | "Left" -> "ArrowLeft"
 | "Right" -> "ArrowRight"
 | "Up" -> "ArrowUp"
 | "Down" -> "ArrowDown"

 | "Q" -> exit 0
 | s -> String.lowercase_ascii s

(* claude: generic -v/-verbose/-debug/-quiet handling for every native
 * example/game, so individual examples don't each need their own
 * Arg.parse boilerplate. Without a reporter installed, Logs.xxx calls
 * anywhere in the program are silently dropped (nothing else in this
 * codebase installs one), so this is also what makes the Image_decode.ml
 * Logs.info calls (e.g. "loading image ...", visible with -v) actually
 * show up. *)
let parse_cli_and_setup_logging () =
  let level = ref (Some Logs.Warning) in
  let cli_flags = [
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " debug mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " quiet mode";
  ] in
  Arg.parse cli_flags
    (fun s -> raise (Arg.Bad (spf "don't know what to do with %s" s)))
    (spf "usage: %s [-v|-verbose|-debug|-quiet]" Sys.argv.(0));
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level !level

(*****************************************************************************)
(* FPS *)
(*****************************************************************************)
(* claude: only the counting; drawing the counter is up to each backend
 * (it gets the current value as [draw]'s ~fps argument) *)

module Fps = struct
(* was in cairo/examples/graphics_demo.ml *)
let lastfps = ref (Unix.gettimeofday ())
let frames = ref 0
let fps = ref 0.

let update_fps () =
  let t = Unix.gettimeofday () in
  let dt = t -. !lastfps in
  if dt > 0.5 then (
    fps := float !frames /. dt;
    frames := 0;
    lastfps := t
  );
  incr frames
end

(*****************************************************************************)
(* Window *)
(*****************************************************************************)

type pixels = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

let create_window ~title ~sx ~sy : Sdl.window * pixels =
  let* () = Sdl.init Sdl.Init.(video + events) in
  let* sdl_window = Sdl.create_window ~w:sx ~h:sy title
    Sdl.Window.shown in

  let* window_surface = Sdl.get_window_surface sdl_window in

  let pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim pixels = sx * sy);

  (* less? need that? *)
  Bigarray.Array1.fill pixels 0xFFFFFFFFl ;
  let pixels =
    try
      let genarray = Bigarray.genarray_of_array1 pixels in
      Bigarray.reshape_2 genarray sy sx
    with _ ->
      let len = Bigarray.Array1.dim pixels in
      failwith (spf
        "Error while reshaping pixel array of length %d to screen size %d x %d"
        len sx sy)
  in
  sdl_window, pixels

let present sdl_window =
  let* () = Sdl.update_window_surface sdl_window in
  ()

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

(* claude: takes the fields of a ('model, 'msg) Playground.app one by one
 * rather than the record itself: this library can't depend on
 * elm_playground (see the .mli) *)
let run ~sdl_window ~sx ~sy ~(init : unit -> 'model * 'msg Cmd.t)
    ~(update : 'msg -> 'model -> 'model * 'msg Cmd.t)
    ~(subscriptions : 'model -> 'msg Sub.t) ~(view : 'model -> 'view)
    ~(draw : fps:float -> 'view -> unit) =
  let sdl_event = Sdl.Event.create () in

  let initmodel, _cmdsTODO = init () in
  let model = ref initmodel in

  (* claude: the loop below has no vsync (we blit to a plain SDL window
   * surface, not an accelerated/vsync'd renderer), so without this cap it
   * free-runs at several hundred fps. Playground.game's update functions
   * (e.g., examples/Mario.ml) use a fixed per-tick dt inherited from the
   * original Elm code, which assumes browser's requestAnimationFrame's
   * ~60Hz pacing (see playground/web/Playground_platform.ml's
   * animation_frame, which re-schedules itself via
   * Window.request_animation_frame); an uncapped native loop breaks that
   * assumption and makes games run several times too fast. *)
  let target_fps = 60. in
  let target_frame_time = 1. /. target_fps in

  (* typing "Q" will cause an 'exit 0' that will exit the loop *)
  while true do
    let frame_start = Unix.gettimeofday () in

    (* one frame *)
    let apply_playground_event pevent =
      let subs = subscriptions !model in
      match E.event_to_msgopt pevent subs with
      | None -> ()
      | Some msg ->
        let newmodel, _cmds = update msg !model in
        model := newmodel
    in

    (* claude: drain the *whole* pending SDL event queue every frame,
     * instead of at most one event, and always additionally deliver a
     * Tick below. Playground.game's update_memory (e.g. Mario's physics)
     * only runs on Tick, not on KeyChanged (see Playground.game_update);
     * with only one SDL event consumed per loop iteration, a burst of
     * queued input events (e.g. OS key-repeat while holding an arrow key)
     * used to starve Tick delivery for several frames in a row, which
     * showed up as the game visibly slowing down while a key was held. *)
    let rec drain_sdl_events () =
      if Sdl.poll_event (Some sdl_event) then begin
        let event_type = Sdl.Event.get sdl_event Sdl.Event.typ in
        (match event_type with
        | x when x = Sdl.Event.mouse_motion ->
          let x = Sdl.Event.(get sdl_event mouse_motion_x) in
          let y = Sdl.Event.(get sdl_event mouse_motion_y) in
          (* claude: window pixel coordinates (origin top-left, y down)
           * to Elm's (origin at the center, y up) *)
          let x = float x -. (float sx /. 2.) in
          let y = -.(float y -. (float sy /. 2.)) in
          apply_playground_event (E.EMouseMove (int_of_float x, int_of_float y))

        | x when x = Sdl.Event.mouse_button_down ->
          apply_playground_event (E.EMouseButton true)

        | x when x = Sdl.Event.mouse_button_up ->
          apply_playground_event (E.EMouseButton false)

        | x when x = Sdl.Event.key_down ->
          let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
          let str = scancode_to_keystring key in
          apply_playground_event (E.EKeyChanged (true, str))

        | x when x = Sdl.Event.key_up ->
          let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
          let str = scancode_to_keystring key in
          apply_playground_event (E.EKeyChanged (false, str))

        (* claude: SDL posts this both when the (only, here) window's
         * close button is clicked, and -- on Unix -- when the process
         * receives SIGINT/SIGTERM, which SDL's own signal handler
         * intercepts and turns into this event instead of the default
         * "terminate the process" behavior. Without handling it, both
         * the close button and e.g. `kill`/Ctrl-C appeared to do
         * nothing: the event was received but silently ignored below. *)
        | x when x = Sdl.Event.quit -> exit 0

        (* other SDL event types (window resize/expose/...): ignored *)
        | _ -> ()
        );
        drain_sdl_events ()
      end
    in
    drain_sdl_events ();
    apply_playground_event (E.ETick (Unix.gettimeofday ()));

    let shapes = view !model in
    draw ~fps:!Fps.fps shapes;
    present sdl_window;

    (* Update our fps counter. *)
    Fps.update_fps ();

    let elapsed = Unix.gettimeofday () -. frame_start in
    if elapsed < target_frame_time
    then Unix.sleepf (target_frame_time -. elapsed);
  done
