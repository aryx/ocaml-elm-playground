open Basics
open Color
module E = Sub
open Tsdl

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Native backend of Playground using Cairo and SDL.
 *
 * history:
 *  - use Graphics, but no keydown/keyup
 *  - use ocaml-SDL, but initialy lack example to work with Cairo
 *  - use TSDL+cairo
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* was in my Commom.ml before, could move in a core/Regexp_.ml *)

let spf = Printf.sprintf

let ( =~ ) s re =
  Str.string_match (Str.regexp re) s 0
let matched (i: int) (s: string) : string = Str.matched_group i s
let _matched1 s = matched 1 s
let _matched2 s = (matched 1 s, matched 2 s)
let matched3 s = (matched 1 s, matched 2 s, matched 3 s)

(*****************************************************************************)
(* Render (independent of Playground) *)
(*****************************************************************************)

let (g_cr: Cairo.context option ref) = ref None
let get_cr () = 
  match !g_cr with
  | None -> failwith "no cr"
  | Some x -> x

let g_sx = ref 0
let g_sy = ref 0

(* less: save_excursion? *)
let with_cr f = 
  let cr = get_cr () in
  Cairo.save cr;
  let res = f cr in
  Cairo.restore cr;
  res

let set_color cr color alpha =
  let (r,g,b) =
    match color with
    | Rgb (r,g,b) -> float r / 255., float g / 255., float b / 255.
    | Hex s ->
        let s = String.lowercase_ascii s in
        if s =~ "^#\\([a-f0-9][a-f0-9]\\)\\([a-f0-9][a-f0-9]\\)\\([a-f0-9][a-f0-9]\\)$"
        then 
          let (a, b, c) = matched3 s in
          let f x =
            (("0x" ^ x) |> int_of_string |> float) / 255.
          in
          f a, f b, f c
        else failwith (spf "wrong color format: %s" s)
  in
  Cairo.set_source_rgba cr r g b (clamp 0. 1. alpha)

let debug_coordinates cr = 
  let sx = !g_sx in
  let sy = !g_sy in
  let (x0,y0) = Cairo.device_to_user cr 0. 0. in
  let (xmax, ymax) = Cairo.device_to_user cr (float sx) (float sy) in
  Logs.debug (fun m -> m "device 0,0 => %.1f %.1f, device %d,%d => %.1f %.1f"
    x0 y0 sx sy xmax ymax)

(* Cairo (0,0) is at the top left of the screen, in which as y goes up,
 * the coordinates are down on the physical screen. Elm uses a better
 * default where if your y goes up, then it's upper on the screen.
 * Here we convert the Elm coordinate system to Cairo. Note that
 * it's hard to use one of the rotate/translate Cairo function to emulate
 * that as only y need to change (maybe need to create a special matrix?)
 *)
let convert (x, y) =
  x, -. y


let render_transform cr x y angle s =
  Cairo.translate cr x y;
  Cairo.rotate cr (-. (Basics.degrees_to_radians angle));
  Cairo.scale cr s s;
  ()


let rec ngon_points cr i n radius =
  if i == n 
  then ()
  else begin
    let a = turns (float i / float n - 0.25) in
    let x = radius * cos a in
    let y = radius * sin a in
    (if i = 0
    then Cairo.move_to cr x y
    else Cairo.line_to cr x y
    );
    ngon_points cr (Stdlib.(+) i 1) n radius
  end

let render_ngon hook color n radius x y angle s alpha = 
  (*pr2_gen (x,y,n,radius);*)
  let (x, y) = convert (x, y) in

  with_cr (fun cr ->
    hook cr;
    set_color cr color alpha;
    render_transform cr x y angle s;

    ngon_points cr 0 n radius;
    Cairo.fill cr;
  )


let render_oval hook color w h x y _angle _s alpha = 
  (*pr2_gen (x,y,w,h);*)

  let x = x - (w / 2.) in
  let y = y + (h / 2.) in
  let (x,y) = convert (x,y) in

  with_cr (fun cr -> 
    hook cr;
    set_color cr color alpha;

    (* code in cairo.mli to draw ellipsis *)
    Cairo.translate cr (x +. w /. 2.) (y +. h /. 2.);
    Cairo.scale cr (w /. 2.) (h /. 2.);

    Cairo.arc cr 0. 0. 1. 0. pi2;
    Cairo.fill cr;
  )


let render_circle hook color radius x y angle s alpha =
  (*pr2_gen (x,y, radius);*)
  let (x,y) = convert (x,y) in

  with_cr (fun cr ->
    hook cr;
    set_color cr color alpha;
    render_transform cr x y angle s;

    Cairo.arc cr 0. 0. radius 0. pi2;
    Cairo.fill cr;
  )

let render_polygon hook color points x y angle s alpha =
  let (x,y) = convert (x,y) in

  with_cr (fun cr ->
    hook cr;
    set_color cr color alpha;
    render_transform cr x y angle s;

    (match points with
    | [] -> failwith "not enough points in polygon"
    | (x, y)::xs ->
       let (x,y) = convert (x,y) in
       Cairo.move_to cr x y;
       xs |> List.iter (fun (x, y) ->
         let (x,y) = convert (x,y) in
         Cairo.line_to cr x y
       );
       Cairo.line_to cr x y;
       Cairo.fill cr;
    )
  )


let render_rectangle hook color w h x y angle s alpha = 
  render_polygon hook color 
    [ (-. w / 2., h /. 2.);
      (   w / 2., h /. 2.);
      (   w / 2., -.h /. 2.);
      (-. w / 2., -.h /. 2.);
    ] x y angle s alpha

let render_words hook color str x y angle s alpha =
  let (x,y) = convert (x,y) in

  with_cr (fun cr ->
    hook cr;
    set_color cr color alpha;
    render_transform cr x y angle s;

    (* claude: same font as the web backend (see
     * Playground.words_font_size), instead of Cairo's default 10 *)
    Cairo.select_font_face cr Playground.words_font_family;
    Cairo.set_font_size cr Playground.words_font_size;

    (* claude: center the text on (x, y) the same way the web backend does
     * with text-anchor="middle" and dominant-baseline="central": use the
     * advance width (not the width of the inked part), and the font's
     * ascent/descent (not the inked height of this particular string,
     * which made e.g. "aaa" and "Ag" sit at different heights). The
     * baseline goes (ascent - descent) / 2 below the center, which puts
     * the middle of the font's box on y. *)
    let (text_ext : Cairo.text_extents) = Cairo.text_extents cr str in
    let (font_ext : Cairo.font_extents) = Cairo.font_extents cr in
    Cairo.move_to cr
      (-. text_ext.x_advance / 2.)
      ((font_ext.ascent - font_ext.descent) / 2.);
    Cairo.show_text cr str;
  )

let render_image hook w h src x y angle s _alpha =
  let (x,y) = convert (x,y) in

  (* claude: surface_of_url_at to animate animated GIFs (e.g., Mario's
   * walk sprites), like browsers do on the web *)
  match Image_native.surface_of_url_at ~time:(Unix.gettimeofday ()) src with
  | None -> ()
  | Some surface ->
    (* claude: Cairo.set_source_surface only positions a surface's origin,
     * it never resizes its pixel content to fill (w,h) -- so without this
     * scale, an image whose native decoded size differs from the
     * requested (w,h) (e.g. Mario's 35x35 GIF sprites drawn via
     * "image 70. 70. ...") is drawn at its native size, anchored at the
     * top-left of the (w,h) box instead of filling/centering it. *)
    let surface_w = float (Cairo.Image.get_width surface) in
    let surface_h = float (Cairo.Image.get_height surface) in
    with_cr (fun cr ->
      hook cr;
      render_transform cr x y angle s;
      Cairo.scale cr (w /. surface_w) (h /. surface_h);

      Cairo.set_source_surface cr surface
        ~x:(-. surface_w /. 2.) ~y:(-. surface_h /. 2.);
      Cairo.paint cr;
    )

(*****************************************************************************)
(* Render playground *)
(*****************************************************************************)
open Playground

(* ugly, to handle Group *)
type hook = Cairo.context -> unit
let empty_hook = (fun _cr -> ())

let rec (render_shape: hook -> shape -> unit) = 
  fun hook { x; y; angle; scale; alpha; form} ->
  match form with
  | Circle (color, radius) -> 
     render_circle hook color radius x y angle scale alpha
  | Oval (color, width, height) ->
     render_oval hook color width height x y angle scale alpha
  | Rectangle (color, width, height) ->
     render_rectangle hook color width height x y angle scale alpha
  | Ngon (color, n, radius) ->
     render_ngon hook color n radius x y angle scale alpha
  | Polygon (color, points) -> 
     render_polygon hook color points x y angle scale alpha
  | Words (color, str) ->
     render_words hook color str x y angle scale alpha
  | Image (w, h, src) ->
     render_image hook w h src x y angle scale alpha
  | Group xs ->
     (* TODO: alpha *)
     let hook = (fun cr -> 
              hook cr; 
              let (x, y) = convert (x, y) in
              render_transform cr x y angle scale
     ) in
     List.iter (render_shape hook) xs
  
let (render: shape list -> unit) = fun shapes ->
    List.iter (render_shape empty_hook) shapes


(*****************************************************************************)
(* FPS (using Cairo) *)
(*****************************************************************************)

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

let draw_fps cr width height =
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.move_to cr (0.05 *. width) (0.95 *. height);
  Cairo.show_text cr (Printf.sprintf "%gx%g -- %.0f fps" width height !fps)
end

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

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

(* claude: preload_image just queues -- see Image_native.preload -- so it
 * has no ordering dependency on anything and is safe to call anytime,
 * including before run_app has even started (examples/Mario.ml calls it
 * at module init, before run_app). run_app is what actually downloads
 * the queue (via Image_native.load_queued), once it has parsed argv, set
 * up logging, and created its window -- so the window is visible and
 * -v/-debug output makes sense before any blocking network call
 * happens. *)
let preload_image = Image_native.preload

(* claude: generic -v/-verbose/-debug/-quiet handling for every native
 * example/game, so individual examples don't each need their own
 * Arg.parse boilerplate. Without a reporter installed, Logs.xxx calls
 * anywhere in the program are silently dropped (nothing else in this
 * codebase installs one), so this is also what makes the Image_native.ml
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

let run_app app =
  parse_cli_and_setup_logging ();
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let* () = Sdl.init Sdl.Init.(video + events) in
  let* sdl_window = Sdl.create_window ~w:sx ~h:sy "Playground using SDL+Cairo"
    Sdl.Window.shown in
  let sdl_event = Sdl.Event.create () in

  let* window_surface = Sdl.get_window_surface sdl_window in

  let pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim pixels = sx *.. sy);

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
  (* Create a Cairo surface to write on the pixels *)
  let sdl_surface =
    Cairo.Image.create_for_data32 ~w:sx ~h:sy pixels
  in
  let cr = Cairo.create sdl_surface in

  Cairo.identity_matrix cr;
  g_cr := Some cr;
  g_sx := sx;
  g_sy := sy;
  debug_coordinates cr;

  (* claude: show a "Loading..." message right away, then run any queued
   * preload_image downloads -- without this the window doesn't show
   * anything until preloading finishes, which looks like the app just
   * hung for a few seconds with no feedback. *)
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.move_to cr (float sx /. 2. -. 40.) (float sy /. 2.);
  Cairo.show_text cr "Loading...";
  let* () = Sdl.update_window_surface sdl_window in
  Image_native.load_queued ();

  let initmodel, _cmdsTODO = app.Playground.init () in
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
    Cairo.save cr;

    (* reset the surface content *)
    Cairo.set_source_rgba cr 1. 1. 1. 1.;
    Cairo.paint cr;

    (* elm-convetion: set the origin (0, 0) in the center of the surface *)
    Cairo.identity_matrix cr;
    Cairo.translate cr (float sx / 2.) (float sy / 2.);
    (*debug_coordinates cr;*)

    (* one frame *)
    let apply_playground_event pevent =
      let subs = app.Playground.subscriptions !model in
      match E.event_to_msgopt pevent subs with
      | None -> ()
      | Some msg ->
        let newmodel, _cmds = app.Playground.update msg !model in
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
          let (x, y) = Cairo.device_to_user cr (float x) (float y) in
          let (x, y) = convert (x, y) in
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

    let shapes = app.Playground.view !model in
    render shapes;

    Cairo.restore cr;
    Fps.draw_fps cr (float sx) (float sy);

    (* Don't forget to flush the surface before using its content. *)
    Cairo.Surface.flush sdl_surface;
    let* () = Sdl.update_window_surface sdl_window in

    (* Update our fps counter. *)
    Fps.update_fps ();

    let elapsed = Unix.gettimeofday () -. frame_start in
    if elapsed < target_frame_time
    then Unix.sleepf (target_frame_time -. elapsed);
  done
