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
 * Not the same loop as Native_loop_3d.ml: that one
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

(* claude: a mouse button press/release as a playground event: the right
 * button is ERightMouseButton, any other the (left) EMouseButton *)
let mouse_button_event (sdl_event : Sdl.event) (is_down : bool) : E.event =
  if Sdl.Event.(get sdl_event mouse_button_button) = Sdl.Button.right
  then E.ERightMouseButton is_down
  else E.EMouseButton is_down

(* claude: SDL counts the clicks of a burst for us (mouse_button_clicks
 * is 1, then 2 for a second click inside the system's double-click
 * time and distance), so a double click is the ordinary click of the
 * pair plus this extra event -- a program that ignores EMouseDouble
 * still sees two normal clicks, as before *)
let mouse_double_event (sdl_event : Sdl.event) : E.event option =
  if Sdl.Event.(get sdl_event mouse_button_button) <> Sdl.Button.right
     && Sdl.Event.(get sdl_event mouse_button_clicks) >= 2
  then Some E.EMouseDouble
  else None

(* claude: generic -v/-verbose/-debug/-quiet handling for every native
 * example/game, so individual examples don't each need their own
 * Arg.parse boilerplate. Without a reporter installed, Logs.xxx calls
 * anywhere in the program are silently dropped (nothing else in this
 * codebase installs one), so this is also what makes the Image_decode.ml
 * Logs.info calls (e.g. "loading image ...", visible with -v) actually
 * show up. *)
(* claude: -uncapped: no 60 fps cap, to measure how fast a backend can
 * draw (see notes_opti.md); games then run too fast *)
let uncapped = ref false

(* claude: -debug-keys: the backend's debug keys (e.g. the software
 * rasterizer's "f" for wireframe, "h" for help) are off by default, so
 * every key goes to the app only and a game can use any key it wants;
 * this flag turns them on, for demos and debugging *)
let debug_keys = ref false
let debug_keys_enabled () = !debug_keys

(* claude: deterministic frames, for the golden frame tests (see
 * tests/2d/Golden_frames.ml): the clock the app sees can be frozen,
 * debug keys pressed before the first frame, and a given frame dumped
 * to a file -- the same flags as playground3d's Native_loop_3d *)
let fixed_time : float option ref = ref None
let startup_keys : string ref = ref ""
let dump_frame_number : int option ref = ref None
let dump_frame_file : string ref = ref ""
(* claude: -script, game keys held over given frames (see Input_script) *)
let script : Input_script.t option ref = ref None
(* claude: -dump-audio, with -dump-frame: the sound of those frames *)
let dump_audio_file : string ref = ref ""

let set_script (s : string) : unit =
  match Input_script.parse s with
  | Ok sc -> script := Some sc
  | Error msg -> raise (Arg.Bad msg)

(* claude: parsed once, on first use, by whichever comes first:
 * [parse_cli_and_setup_logging] (run_app's first step) or [app_args]
 * (Playground_platform.flags, usually called in a program's main,
 * i.e. before run_app). The arguments without a dash are the app's
 * (Playground.flags), kept, in order; only Arg knows which bare
 * arguments are rather an option's value (-fixed-time 1000), hence
 * collecting them in its anonymous-argument function rather than
 * filtering Sys.argv. Arg.parse_argv with its own [current] rather
 * than Arg.parse: Arg.parse's position in argv is global, so a second
 * parse in the same program (e.g. playground3d's Native_loop_3d, run
 * after Playground_platform.flags) would find nothing left to parse. *)
let parsed_cli : string list Lazy.t = lazy (
  let level = ref (Some Logs.Warning) in
  let app_args = ref [] in
  let cli_flags = [
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " debug mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " quiet mode";
    "-uncapped", Arg.Set uncapped,
    " no 60 fps cap (to benchmark)";
    "-debug-keys", Arg.Set debug_keys,
    " the backend's debug keys (e.g. h for help), off by default";
    "-fixed-time", Arg.Float (fun t -> fixed_time := Some t),
    "<seconds> the app's clock stays at this time (frozen animations)";
    "-keys", Arg.Set_string startup_keys,
    "<keys> debug keys to press before the first frame, e.g. \"nf\"";
    "-dump-frame",
    Arg.Tuple [ Arg.Int (fun n -> dump_frame_number := Some n); Arg.Set_string dump_frame_file ],
    "<n> <file> write frame n (from 1) to file (a PNG if it ends in .png, else a PPM), then exit";
    "-script", Arg.String set_script,
    "<script> what the person does over frames, e.g. \"right:1-60,space:30,at(0;80):1-60,click:30\"";
    "-dump-audio", Arg.Set_string dump_audio_file,
    "<file> with -dump-frame, also write the sound of those frames to file (a WAV)";
  ] in
  let usage =
    spf "usage: %s [-v|-verbose|-debug|-quiet|-uncapped|-debug-keys] [-fixed-time t] [-keys k] [-dump-frame n file] [-script s] [-dump-audio file] [name=value|name]..."
      Sys.argv.(0)
  in
  (* what Arg.parse does on an error or -help *)
  (try Arg.parse_argv ~current:(ref 0) Sys.argv cli_flags (fun s -> app_args := s :: !app_args) usage with
  | Arg.Bad msg -> prerr_string msg; exit 2
  | Arg.Help msg -> print_string msg; exit 0);
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level !level;
  List.rev !app_args)

let parse_cli_and_setup_logging () = ignore (Lazy.force parsed_cli)
let app_args () = Lazy.force parsed_cli

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
    lastfps := t;
    (* claude: with -debug, for scripts measuring any backend's speed *)
    Logs.debug (fun m -> m "fps %.1f" !fps)
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

(* claude: -dump-frame: the frame as a binary PPM image, the simplest
 * image format there is (a header, then r, g, b bytes for each pixel) *)
let write_frame ~(width : int) ~(height : int) (rgb : int -> int -> int) (file : string) : unit =
  let oc = open_out_bin file in
  if Filename.check_suffix file ".png" then begin
    let img = Rgba_image.create ~width ~height in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let p = rgb x y and o = ((y * width) + x) * 4 in
        img.rgba.{o} <- (p lsr 16) land 0xFF;
        img.rgba.{o + 1} <- (p lsr 8) land 0xFF;
        img.rgba.{o + 2} <- p land 0xFF;
        img.rgba.{o + 3} <- 255
      done
    done;
    output_string oc (Png.encode ~alpha:false img)
  end
  else begin
    Printf.fprintf oc "P6\n%d %d\n255\n" width height;
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let p = rgb x y in
        output_byte oc ((p lsr 16) land 0xFF);
        output_byte oc ((p lsr 8) land 0xFF);
        output_byte oc (p land 0xFF)
      done
    done
  end;
  close_out oc

let dump_pixels (pixels : pixels) (file : string) : unit =
  write_frame ~width:(Bigarray.Array2.dim2 pixels) ~height:(Bigarray.Array2.dim1 pixels)
    (fun x y -> Int32.to_int pixels.{y, x})
    file

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

(* claude: takes the fields of a ('model, 'msg) Playground.app one by one
 * rather than the record itself: this library can't depend on
 * elm_playground (see the .mli) *)
(* claude: the sound: 44,100 samples a second, 735 a frame (1/60 s);
 * SDL's queue kept about 3 frames (50 ms) ahead of what the card has
 * played, topped up each frame by what it used: the two clocks, the
 * game's and the card's, never drift apart (audio/Mixer.mli) *)
let audio_rate = 44100
let frame_samples = audio_rate / 60
let queue_ahead = 3 * frame_samples

let open_audio () : Sdl.audio_device_id option =
  let warn msg = Logs.warn (fun m -> m "no sound: %s" msg); None in
  match Sdl.init_sub_system Sdl.Init.audio with
  | Error (`Msg msg) -> warn msg
  | Ok () -> (
      let spec =
        { Sdl.as_freq = audio_rate; as_format = Sdl.Audio.s16_sys; as_channels = 2; as_silence = 0;
          as_samples = 1024; as_size = 0l; as_callback = None }
      in
      match Sdl.open_audio_device None false spec 0 with
      | Error (`Msg msg) -> warn msg
      | Ok (device, _) ->
          Sdl.pause_audio_device device false;
          Some device)

(* claude: the two channels interleaved, as SDL (and WAV files) want
 * them: left, right, left, right... *)
let queue_samples (device : Sdl.audio_device_id) ((left, right) : float array * float array) : unit =
  let n = Array.length left in
  let ba = Bigarray.Array1.create Bigarray.int16_signed Bigarray.c_layout (2 * n) in
  let int16 x = max (-32768) (min 32767 (int_of_float (Float.round (x *. 32767.)))) in
  for i = 0 to n - 1 do
    ba.{2 * i} <- int16 left.(i);
    ba.{(2 * i) + 1} <- int16 right.(i)
  done;
  match Sdl.queue_audio device ba with
  | Ok () -> ()
  | Error (`Msg msg) -> Logs.warn (fun m -> m "queue_audio: %s" msg)

let run ~sdl_window ~sx ~sy ~(init : unit -> 'model * 'msg Cmd.t)
    ~(update : 'msg -> 'model -> 'model * 'msg Cmd.t)
    ~(subscriptions : 'model -> 'msg Sub.t) ~(view : 'model -> 'view)
    ~(draw : fps:float -> 'view -> unit) ~(on_key_press : string -> unit)
    ~(dump_frame : string -> unit)
    ~(pull_audio : int -> float array * float array) ~(dump_audio : string -> float array * float array -> unit) =
  (* claude: without this, SDL sends no text_input events at all (it is
   * off until a program says it wants text); with it, every key press
   * that produces a character also produces one, which is what
   * computer.keyboard.typed is (see plan_gui_teaching.md, phase 0) *)
  Sdl.start_text_input ();
  let sdl_event = Sdl.Event.create () in
  (* claude: no sound device for -dump-frame: exactly a frame's samples
   * each frame instead, kept for -dump-audio *)
  let audio_device = if !dump_frame_number <> None then None else open_audio () in
  let dumped_audio = ref [] in
  (* claude: -keys, as if pressed before the first frame *)
  String.iter (fun c -> on_key_press (String.make 1 c)) !startup_keys;
  let frame_number = ref 0 in

  (* claude: the commands of init and update, performed while the
   * frames go on (Commands.mli) *)
  let commands = Commands.create () in
  let initmodel, cmd = init () in
  let model = ref initmodel in
  Commands.perform commands cmd;
  let apply_msg msg =
    let newmodel, cmd = update msg !model in
    model := newmodel;
    Commands.perform commands cmd
  in

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
      | Some msg -> apply_msg msg
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
        (* claude: with -dump-frame, no mouse or keyboard at all: wherever
         * the pointer happens to be when the window opens would otherwise
         * change the frame (e.g. examples/Mouse.ml); -keys is the way to
         * give input then *)
        | x when !dump_frame_number <> None && x <> Sdl.Event.quit -> ()
        | x when x = Sdl.Event.mouse_motion ->
          let x = Sdl.Event.(get sdl_event mouse_motion_x) in
          let y = Sdl.Event.(get sdl_event mouse_motion_y) in
          (* claude: window pixel coordinates (origin top-left, y down)
           * to Elm's (origin at the center, y up) *)
          let x = float x -. (float sx /. 2.) in
          let y = -.(float y -. (float sy /. 2.)) in
          apply_playground_event (E.EMouseMove (int_of_float x, int_of_float y));
          (* claude: the relative move too, y up (mdx/mdy) *)
          let dx = Sdl.Event.(get sdl_event mouse_motion_xrel) in
          let dy = Sdl.Event.(get sdl_event mouse_motion_yrel) in
          apply_playground_event (E.EMouseMoveBy (float dx, -.(float dy)))

        | x when x = Sdl.Event.mouse_button_down ->
          apply_playground_event (mouse_button_event sdl_event true);
          (match mouse_double_event sdl_event with
           | Some e -> apply_playground_event e
           | None -> ())

        | x when x = Sdl.Event.mouse_wheel ->
          (* claude: notches, y up; SDL flips the sign itself on
           * "natural" scrolling (mouse_wheel_flipped), so undo that to
           * keep one meaning everywhere: positive is scrolling up *)
          let y = float Sdl.Event.(get sdl_event mouse_wheel_y) in
          let y =
            if Sdl.Event.(get sdl_event mouse_wheel_direction)
               = Sdl.Event.mouse_wheel_flipped
            then -.y else y
          in
          apply_playground_event (E.EMouseWheel y)

        (* claude: the characters a key press produced, which the key
         * events cannot give (shift, dead keys, a non-US layout): SDL
         * decides, we pass it on. Enabled by Sdl.start_text_input
         * below. *)
        | x when x = Sdl.Event.text_input ->
          let str = Sdl.Event.(get sdl_event text_input_text) in
          apply_playground_event (E.ETyped str)

        | x when x = Sdl.Event.mouse_button_up ->
          apply_playground_event (mouse_button_event sdl_event false)

        | x when x = Sdl.Event.key_down ->
          let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
          let str = scancode_to_keystring key in
          (* claude: while a key is held, SDL keeps re-sending key_down
           * at the keyboard's repeat rate, with keyboard_repeat > 0;
           * [on_key_press] is for one-shot toggles, so only the first
           * press counts (see Native_loop_3d.ml
           * for the same filter and the bug it fixed) *)
          let first = Sdl.Event.(get sdl_event keyboard_repeat) = 0 in
          (* claude: Ctrl + a key is the debug key alone, not given to
           * the app: the way to reach a debug key the game uses itself
           * (AudioPiano's "h") *)
          let ctrl = Sdl.Event.(get sdl_event keyboard_keymod) land Sdl.Kmod.ctrl <> 0 in
          if !debug_keys && ctrl then (if first then on_key_press str)
          else begin
            if !debug_keys && first then on_key_press str;
            apply_playground_event (E.EKeyChanged (true, str))
          end

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
    (* claude: -script, what the person does at this frame: the keys
     * going down or up, where the pointer is, and its buttons *)
    (match !script with
    | Some sc ->
        let frame = !frame_number + 1 in
        Input_script.changes sc frame
        |> List.iter (fun (key, is_down) -> apply_playground_event (E.EKeyChanged (is_down, key)));
        (match Input_script.mouse sc frame with
        | Some (x, y) ->
            (* already playground coordinates, as the SDL branch above
             * converts them to *)
            apply_playground_event (E.EMouseMove (int_of_float x, int_of_float y))
        | None -> ());
        Input_script.button_changes sc frame
        |> List.iter (fun (right, is_down) ->
               apply_playground_event
                 (if right then E.ERightMouseButton is_down else E.EMouseButton is_down));
        (match Input_script.typed sc frame with
        | "" -> ()
        | s -> apply_playground_event (E.ETyped s))
    | None -> ());
    (* claude: the answers of the commands finished since last frame *)
    List.iter apply_msg (Commands.step commands);
    let now = match !fixed_time with Some t -> t | None -> Unix.gettimeofday () in
    apply_playground_event (E.ETick now);
    (* claude: the sounds this frame's update played, to the card *)
    (match audio_device with
    | Some device ->
        (* claude: 4 bytes a sample frame: two channels of 16 bits *)
        let queued = Sdl.get_queued_audio_size device / 4 in
        if queued < queue_ahead then queue_samples device (pull_audio (queue_ahead - queued))
    | None ->
        let samples = pull_audio frame_samples in
        if !dump_audio_file <> "" then dumped_audio := samples :: !dumped_audio);

    let shapes = view !model in
    (* claude: with -dump-frame, a fixed fps for the counter some
     * backends draw in the frame, which would otherwise differ from
     * run to run *)
    let fps = if !dump_frame_number <> None then 0. else !Fps.fps in
    draw ~fps shapes;
    present sdl_window;

    (* claude: -dump-frame *)
    incr frame_number;
    (match !dump_frame_number with
    | Some n when n = !frame_number ->
        dump_frame !dump_frame_file;
        if !dump_audio_file <> "" then (
          let frames = List.rev !dumped_audio in
          dump_audio !dump_audio_file (Array.concat (List.map fst frames), Array.concat (List.map snd frames)));
        exit 0
    | _ -> ());

    (* Update our fps counter. *)
    Fps.update_fps ();

    let elapsed = Unix.gettimeofday () -. frame_start in
    if elapsed < target_frame_time && not !uncapped
    then Unix.sleepf (target_frame_time -. elapsed);
  done
