(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The SDL window-event-draining/Playground.computer-bookkeeping/
 * frame-pacing loop shared by every SDL-based playground3d backend
 * (the native software rasterizer, and -- see
 * docs/claude_notes/plan_opengl.md -- a future OpenGL backend). Each
 * backend still creates its own SDL window (and, for a GPU backend,
 * its own GL context) and still supplies its own way of turning a
 * "view" value into pixels; only the parts that would otherwise be
 * identical between backends live here.
 *
 * Generic over the model/view types (Playground3d.app3d's 'model and
 * 'view = camera * shape3d list aren't named here) rather than
 * depending on Playground3d directly: this library only depends on
 * Playground (elm_playground), not Playground3d (elm_playground_3d).
 * elm_playground_3d_software "(implements elm_playground_3d)" already
 * lists elm_playground_3d itself as that implementation's one
 * legitimate edge to it -- dune forbids that same virtual library from
 * also being reachable via a second path (e.g. through this library),
 * which is exactly what would happen if this module depended on
 * Playground3d too. *)

(* [Ok x -> f x], [Error (`Msg msg) -> failwith msg] -- every Tsdl call
 * returns this same result type, so this one operator threads through
 * any sequence of them without a match at every step. Useful in a
 * backend's own window/context-creation code too, not just here. *)
val ( let* ) : ('a, [ `Msg of string ]) result -> ('a -> 'b) -> 'b

(* claude: a straight copy of playground/native_common/Native_loop_2d.ml's
 * own parse_cli_and_setup_logging (2D run_app calls it as its first
 * action) -- same -v/-verbose/-debug/-quiet convention, so individual
 * playground3d examples3d/games3d files don't each need their own
 * Arg.parse boilerplate either. Duplicated rather than shared only
 * because a virtual module's implementation is sealed to exactly its
 * own .mli, the same reason Shape_render_native had to be extracted
 * into its own module instead of reused directly -- see
 * docs/claude_notes/done/plan_hud.md. Without a reporter installed,
 * every Logs.xxx call anywhere in a playground3d program is silently
 * dropped, so this is also what makes [run]'s own per-frame
 * Logs.debug fps line (see below), or any Logs.debug call you add
 * temporarily while investigating something, actually show up: prefer
 * that over a throwaway Printf.eprintf you have to remember to revert
 * -- run with -debug once and delete it when you're done, or just
 * leave it, since it costs nothing when no reporter is installed. *)
val parse_cli_and_setup_logging : unit -> unit

(* -debug-keys was given: [run] calls its [on_key_press] for the
 * backend's debug keys. Off by default, so that all keys go to the app
 * only (a game may use "f" or "h" itself); -keys still presses its keys
 * either way. With it, a key is both the app's and the debug key's,
 * and Ctrl + the key is the debug key alone (Ctrl-h: the help, even in
 * a game using "h"). *)
val debug_keys_enabled : unit -> bool

val mouse_move : float -> float -> Playground.mouse -> Playground.mouse
val mouse_down : bool -> Playground.mouse -> Playground.mouse
val update_keyboard : bool -> string -> Playground.keyboard -> Playground.keyboard

(* Tsdl's key names for the arrow keys ("Left", "Right", ...) don't match
 * Playground.keyboard's ("ArrowLeft", "ArrowRight", ...); "Q" quits
 * immediately (matching playground/native's own convention); everything
 * else is passed through lowercased. *)
val scancode_to_keystring : string -> string

(* [run ~sdl_window ~sx ~sy ~title_prefix ~on_key_press ~init ~update
 *   ~view ~draw ~present]
 * drives the standard playground3d Model-View-Update game loop,
 * forever (like Playground_platform.run_app, this never returns under
 * normal operation -- "Q" or the window's close button call [exit]
 * directly): each iteration, drains the SDL event queue into a
 * Playground.computer (updating mouse/keyboard state, and calling
 * [on_key_press str] once per physical key press -- repeats from a
 * held key are already filtered out -- for backend-specific one-shot
 * hotkeys, e.g. the native rasterizer's shading/culling/wireframe/
 * z-buffer/interpolation toggles), calls [update]/[view], hands the
 * resulting "view" value to [draw] to actually put pixels somewhere,
 * calls [present] (e.g. Sdl.update_window_surface, or
 * Sdl.gl_swap_window), and paces to 60fps. [sdl_window]'s title is
 * updated every frame to "<title_prefix> -- <sx>x<sy> -- <fps> fps",
 * followed by " -- " and [title_keys ()] if given (e.g. the debug keys
 * and their state),
 * and the same fps/frame-time is logged via Logs.debug every frame
 * (see [parse_cli_and_setup_logging] -- run with -debug to see it;
 * useful for e.g. spotting whether [view]/[draw] itself is the slow
 * part without needing to add your own timing).
 *
 * The caller is responsible for Sdl.init and creating [sdl_window]
 * (and any GL context) beforehand -- this function only drives the
 * loop, it never creates or destroys a window.
 *
 * claude: for reproducible frames (to check a refactoring changes no
 * pixel), [parse_cli_and_setup_logging] also understands
 * -fixed-time t (the app's clock stays at t), -keys k (the debug keys
 * k pressed, through [on_key_press], before the first frame),
 * -dump-frame n file (after drawing frame n, counted from 1, call
 * [dump_frame file], then exit), and -script s (game keys held over
 * given frames, see Input_script). With -uncapped, frames aren't paced
 * at 60 fps: the three together time the rendering of n frames.
 * [on_key_press] is only called with -debug-keys (see
 * [debug_keys_enabled]), except for -keys. *)
val run :
  sdl_window:Tsdl.Sdl.window ->
  sx:int ->
  sy:int ->
  title_prefix:string ->
  on_key_press:(string -> unit) ->
  init:(unit -> 'model) ->
  update:(Playground.computer -> 'model -> 'model) ->
  view:(Playground.computer -> 'model -> 'view) ->
  draw:(Playground.computer -> 'view -> unit) ->
  present:(unit -> unit) ->
  ?dump_frame:(string -> unit) ->
  ?title_keys:(unit -> string) ->
  ?capture_mouse:bool ->
  ?flags:Playground.flags ->
  unit ->
  unit
(* claude: [capture_mouse] (default false): see
 * Playground3d_platform.run_app3d's; Escape gives the mouse back, a
 * click captures it again. [flags] (default none): the computer's
 * flags, see Playground.flags. *)
