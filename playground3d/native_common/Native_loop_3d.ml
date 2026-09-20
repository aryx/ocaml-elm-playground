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

let ( let* ) o f =
  match o with
  | Error (`Msg msg) -> failwith (Printf.sprintf "TSDL error: %s" msg)
  | Ok x -> f x

(* claude: deterministic frames, to check that a refactoring of a
 * renderer doesn't change a single pixel (see
 * docs/claude_notes/done/plan_code_reorg_teaching_3d.md, phase 0): the
 * clock the app sees can be frozen, debug keys pressed before the
 * first frame, and a given frame dumped to a file *)
let fixed_time : float option ref = ref None
let startup_keys : string ref = ref ""
let dump_frame_number : int option ref = ref None
let dump_frame_file : string ref = ref ""
(* claude: -script, game keys held over given frames (see Input_script) *)
let script : Input_script.t option ref = ref None

let set_script (s : string) : unit =
  match Input_script.parse s with
  | Ok sc -> script := Some sc
  | Error msg -> raise (Arg.Bad msg)

(* claude: -uncapped, no 60 fps cap (no sleep between frames), to
 * measure how fast a renderer really is: with -fixed-time and
 * -dump-frame n, the time to render n frames of the same scene *)
let uncapped : bool ref = ref false

(* claude: -debug-keys, like Native_loop_2d's: the backend's debug keys
 * are off by default, so every key goes to the app only *)
let debug_keys : bool ref = ref false
let debug_keys_enabled () = !debug_keys

let parse_cli_and_setup_logging () =
  let level = ref (Some Logs.Warning) in
  let cli_flags =
    [ ("-v", Arg.Unit (fun () -> level := Some Logs.Info), " verbose mode");
      ("-verbose", Arg.Unit (fun () -> level := Some Logs.Info), " verbose mode");
      ("-debug", Arg.Unit (fun () -> level := Some Logs.Debug), " debug mode");
      ("-quiet", Arg.Unit (fun () -> level := None), " quiet mode");
      ("-fixed-time", Arg.Float (fun t -> fixed_time := Some t),
       "<seconds> the app's clock stays at this time (frozen animations)");
      ("-keys", Arg.Set_string startup_keys,
       "<keys> debug keys to press before the first frame, e.g. \"fz\"");
      ("-dump-frame", Arg.Tuple [ Arg.Int (fun n -> dump_frame_number := Some n); Arg.Set_string dump_frame_file ],
       "<n> <file> write frame n (from 1) to file, then exit");
      ("-script", Arg.String set_script,
       "<script> game keys held over frames, e.g. \"up:1-60,space:30\"");
      ("-uncapped", Arg.Set uncapped, " no 60 fps cap, to measure speed");
      ("-debug-keys", Arg.Set debug_keys, " the backend's debug keys (e.g. h for help), off by default")
    ]
  in
  let usage =
    Printf.sprintf
      "usage: %s [-v|-verbose|-debug|-quiet] [-fixed-time t] [-keys k] [-dump-frame n file] [-script s] [-uncapped] [-debug-keys] [name=value|name]..."
      Sys.argv.(0)
  in
  (* claude: the arguments without a dash are the app's flags (see
   * Playground.flags), read by the program's main through
   * Playground_platform.flags, i.e. by the 2D backend's own parse of
   * the same command line (Native_loop_2d, which must therefore know
   * the same dashed options as here, with the same arities), and given
   * back to run_app3d ~flags: nothing to do with them here. Parsed with
   * our own [current] rather than Arg.parse's global one, which that
   * earlier parse may have left at the end of argv. *)
  (try Arg.parse_argv ~current:(ref 0) Sys.argv cli_flags (fun _app_flag -> ()) usage with
  | Arg.Bad msg ->
      prerr_string msg;
      exit 2
  | Arg.Help msg ->
      print_string msg;
      exit 0);
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level !level

let mouse_move mx my (mouse : Playground.mouse) : Playground.mouse = { mouse with mx; my }
let mouse_down mdown (mouse : Playground.mouse) : Playground.mouse = { mouse with mdown }

(* claude: a press/release of the right button sets mrdown, of any
 * other mdown (the left, main one) *)
let mouse_button (sdl_event : Sdl.event) (is_down : bool) (mouse : Playground.mouse) : Playground.mouse =
  if Sdl.Event.(get sdl_event mouse_button_button) = Sdl.Button.right then { mouse with mrdown = is_down }
  else mouse_down is_down mouse

let update_keyboard (is_down : bool) (key : string) (keyboard : Playground.keyboard) :
    Playground.keyboard =
  let keys = if is_down then Set_.add key keyboard.keys else Set_.remove key keyboard.keys in
  match key with
  | "ArrowUp" -> { keyboard with keys; kup = is_down }
  | "ArrowDown" -> { keyboard with keys; kdown = is_down }
  | "ArrowLeft" -> { keyboard with keys; kleft = is_down }
  | "ArrowRight" -> { keyboard with keys; kright = is_down }
  | "w" -> { keyboard with keys; kw = is_down }
  | "s" -> { keyboard with keys; ks = is_down }
  | "a" -> { keyboard with keys; ka = is_down }
  | "d" -> { keyboard with keys; kd = is_down }
  | "space" -> { keyboard with keys; kspace = is_down }
  | _ -> { keyboard with keys }

let scancode_to_keystring = function
  | "Left" -> "ArrowLeft"
  | "Right" -> "ArrowRight"
  | "Up" -> "ArrowUp"
  | "Down" -> "ArrowDown"
  | "Q" -> exit 0
  | s -> String.lowercase_ascii s

let run ~(sdl_window : Sdl.window) ~(sx : int) ~(sy : int) ~(title_prefix : string)
    ~(on_key_press : string -> unit) ~(init : unit -> 'model)
    ~(update : Playground.computer -> 'model -> 'model) ~(view : Playground.computer -> 'model -> 'view)
    ~(draw : Playground.computer -> 'view -> unit) ~(present : unit -> unit) ?(dump_frame : (string -> unit) option)
    ?(title_keys : (unit -> string) option) ?(capture_mouse = false) ?(flags = []) () : unit =
  (* claude: without this, SDL sends no text_input events at all (it is
   * off until a program says it wants text); with it, every key press
   * that produces a character also produces one, which is what
   * computer.keyboard.typed is (see plan_gui_teaching.md, phase 0) *)
  Sdl.start_text_input ();
  let sdl_event = Sdl.Event.create () in
  (* claude: capture_mouse: SDL's relative mouse mode, the cursor hidden
   * and held in the window, only mouse_motion's xrel/yrel (mdx/mdy)
   * changing; the way first-person games turn the camera with no limit.
   * Not with -dump-frame (no mouse then, see drain_sdl_events). *)
  let captured = ref false in
  let set_captured (b : bool) : unit =
    match Sdl.set_relative_mouse_mode b with
    | Ok () -> captured := b
    | Error (`Msg msg) -> Logs.warn (fun m -> m "can't capture the mouse: %s" msg)
  in
  if capture_mouse && !dump_frame_number = None then set_captured true;
  (* claude: -keys, as if pressed before the first frame *)
  String.iter (fun c -> on_key_press (String.make 1 c)) !startup_keys;
  let frame_number = ref 0 in

  let model = ref (init ()) in
  let computer = ref { Playground.initial_computer with flags } in

  let target_fps = 60. in
  let target_frame_time = 1. /. target_fps in

  while true do
    let frame_start = Unix.gettimeofday () in

    let rec drain_sdl_events () =
      if Sdl.poll_event (Some sdl_event) then begin
        let event_type = Sdl.Event.get sdl_event Sdl.Event.typ in
        (match event_type with
        (* claude: with -dump-frame, no mouse or keyboard at all: wherever
         * the pointer happens to be when the window opens would otherwise
         * change the frame (e.g. InteractiveCube3d's mouse-driven
         * turntable); -keys is the way to give input then *)
        | x when !dump_frame_number <> None && x <> Sdl.Event.quit -> ()
        | x when x = Sdl.Event.mouse_motion ->
            let mx = Sdl.Event.(get sdl_event mouse_motion_x) in
            let my = Sdl.Event.(get sdl_event mouse_motion_y) in
            let px = float_of_int mx -. (float_of_int sx /. 2.) in
            let py = (float_of_int sy /. 2.) -. float_of_int my in
            (* claude: and the relative move (mdx/mdy, y up), summed
             * until the next update: the only one that keeps counting
             * when the mouse is captured *)
            let dx = float_of_int Sdl.Event.(get sdl_event mouse_motion_xrel) in
            let dy = -.float_of_int Sdl.Event.(get sdl_event mouse_motion_yrel) in
            let m = (!computer).mouse in
            computer := { !computer with mouse = { (mouse_move px py m) with mdx = m.mdx +. dx; mdy = m.mdy +. dy } }
        (* claude: capture_mouse, released (Escape): a click captures the
         * mouse again, and is only that, not a click in the game (like
         * the original Minecraft's on_mouse_press) *)
        | x when x = Sdl.Event.mouse_button_down && capture_mouse && not !captured -> set_captured true
        | x when x = Sdl.Event.mouse_button_down ->
            computer := { !computer with mouse = mouse_button sdl_event true (!computer).mouse };
            (* claude: SDL counts a burst's clicks for us; the second one
             * is an ordinary click plus this flag (plan_gui_teaching.md,
             * phase 0) *)
            if Sdl.Event.(get sdl_event mouse_button_button) <> Sdl.Button.right
               && Sdl.Event.(get sdl_event mouse_button_clicks) >= 2
            then computer := { !computer with mouse = { (!computer).mouse with mdouble = true } }

        | x when x = Sdl.Event.mouse_wheel ->
            (* claude: notches, positive scrolling up; SDL flips the sign
             * itself with "natural" scrolling, so undo that *)
            let y = float_of_int Sdl.Event.(get sdl_event mouse_wheel_y) in
            let y =
              if Sdl.Event.(get sdl_event mouse_wheel_direction) = Sdl.Event.mouse_wheel_flipped
              then -.y else y
            in
            let m = (!computer).mouse in
            computer := { !computer with mouse = { m with mwheel = m.mwheel +. y } }

        (* claude: the characters a key press produced, which key_down
         * cannot give (shift, dead keys, a non-US layout) *)
        | x when x = Sdl.Event.text_input ->
            let str = Sdl.Event.(get sdl_event text_input_text) in
            let k = (!computer).keyboard in
            computer := { !computer with keyboard = { k with typed = k.typed ^ str } }
        | x when x = Sdl.Event.mouse_button_up ->
            computer := { !computer with mouse = mouse_button sdl_event false (!computer).mouse }
        | x when x = Sdl.Event.key_down ->
            let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
            let str = scancode_to_keystring key in
            (* claude: bugfix -- SDL does NOT send exactly one key_down
             * per physical press: while a key stays held, the OS/SDL
             * keeps re-sending key_down for it at the keyboard's repeat
             * rate (the same mechanism that makes a held letter key
             * spam "aaaaaa" into a text field), and Sdl.Event.get
             * ...keyboard_repeat is 0 for the original press but > 0
             * for each of those repeats. [on_key_press] is meant for
             * one-shot toggles (a single press = a single cycle), so
             * it's only called when keyboard_repeat = 0 -- reacting to
             * every key_down instead makes holding a key even slightly
             * past the repeat delay (typically ~500ms) flip a toggle 2,
             * 3, or more times in a row, which looked like "the key
             * does nothing" before this guard existed. (Held-key
             * actions like the arrow keys don't have this problem:
             * they don't use key_down events at all, only
             * computer.keyboard's continuously-updated held/not-held
             * state below, which a game's update3d re-reads every Tick
             * regardless of any of this.) *)
            let first = Sdl.Event.(get sdl_event keyboard_repeat) = 0 in
            (* claude: Ctrl + a key is the debug key alone, not given to
             * the app: the way to reach a debug key the game uses
             * itself *)
            let ctrl = Sdl.Event.(get sdl_event keyboard_keymod) land Sdl.Kmod.ctrl <> 0 in
            if !debug_keys && ctrl then (if first then on_key_press str)
            else begin
              if !debug_keys && first then on_key_press str;
              (* claude: capture_mouse: Escape gives the mouse back *)
              if capture_mouse && str = "escape" then set_captured false;
              computer := { !computer with keyboard = update_keyboard true str (!computer).keyboard }
            end
        | x when x = Sdl.Event.key_up ->
            let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
            let str = scancode_to_keystring key in
            computer := { !computer with keyboard = update_keyboard false str (!computer).keyboard }
        | x when x = Sdl.Event.quit -> exit 0
        | _ -> ());
        drain_sdl_events ()
      end
    in
    drain_sdl_events ();
    (* claude: -script, the keys going down or up at this frame *)
    (match !script with
    | Some sc ->
        Input_script.changes sc (!frame_number + 1)
        |> List.iter (fun (key, is_down) ->
               computer := { !computer with keyboard = update_keyboard is_down key (!computer).keyboard })
    | None -> ());

    let now = match !fixed_time with Some t -> t | None -> Unix.gettimeofday () in
    computer := { !computer with time = Playground.Time now };
    model := update !computer !model;
    (* claude: the moves [update] just saw are consumed *)
    computer :=
      { !computer with
        mouse = { (!computer).mouse with mdx = 0.; mdy = 0.; mwheel = 0.; mdouble = false };
        keyboard = { (!computer).keyboard with typed = "" } };

    let t0 = Unix.gettimeofday () in
    let v = view !computer !model in
    let t1 = Unix.gettimeofday () in
    draw !computer v;
    let t2 = Unix.gettimeofday () in

    (* claude: -dump-frame *)
    incr frame_number;
    (match !dump_frame_number with
    | Some n when n = !frame_number ->
        (match dump_frame with
        | Some dump -> dump !dump_frame_file
        | None -> Logs.err (fun m -> m "-dump-frame: this backend can't dump its frames"));
        exit 0
    | _ -> ());

    let elapsed = Unix.gettimeofday () -. frame_start in
    (* claude: -debug shows this every frame, so a scene that suddenly
     * gets slow (e.g. games3d/TinyMinecraft.ml's ~50k-block world, see
     * plan_tiny_minecraft.md's Phase 2) can be diagnosed without
     * adding a throwaway Printf.eprintf each time -- is [view] itself
     * slow (building the shape3d list), or [draw] (turning it into
     * pixels)? *)
    Logs.debug (fun m ->
        m "frame: %.3fs total (view: %.3fs, draw: %.3fs) -- %.0f fps" elapsed (t1 -. t0) (t2 -. t1)
          (1. /. Stdlib.max 0.001 elapsed));
    Sdl.set_window_title sdl_window
      (Printf.sprintf "%s -- %dx%d -- %.0f fps%s" title_prefix sx sy
         (1. /. Stdlib.max 0.001 elapsed)
         (match title_keys with Some keys -> " -- " ^ keys () | None -> ""));
    present ();

    if (not !uncapped) && elapsed < target_frame_time then Unix.sleepf (target_frame_time -. elapsed)
  done
