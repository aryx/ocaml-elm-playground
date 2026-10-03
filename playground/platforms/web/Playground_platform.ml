open Playground
module E = Sub
open Js_browser
module V = Web_vdom

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Web backend of Playground.
 *
 * history: ocaml-vdom's Vdom vs our hand-made mini virtual DOM
 * ---------------------------------------------------------------------
 * This file uses the "vdom" opam package (LexiFi's ocaml-vdom), but only
 * its Js_browser module (typed OCaml bindings to the browser API:
 * Window, Document, Element, Event, Date, ...), not its Vdom module.
 *
 * 1) First version (2020): I originally wrote a real Vdom app (see the
 *    commented vdom_app_of_app/run_app near the end of this file). Vdom works
 *    like Elm: you give it view/update functions, it owns the main loop, and
 *    after each update it diffs the new virtual DOM with the previous
 *    one and patches the real DOM. But two things playground needs were
 *    missing, which I asked about in the ocaml-vdom GitHub issues
 *    at https://github.com/LexiFi/ocaml-vdom/issues/ :
 *    - #35: no canvas in Vdom; the answer was to use SVG, which we do.
 *    - #37: no way to get a Tick message on each animation frame
 *      (Elm's onAnimationFrame subscription);
 *    - #36: onkeydown on a <div> only fires when that div (or a child)
 *      has the keyboard focus, i.e., only after clicking in the page,
 *      whereas games need to see all key presses (and key releases);
 *    The maintainers' answer for #36/#37 was to use "custom elements":
 *    Vdom's escape hatch (a Vdom.custom node in the view, plus a
 *    handler registered with Vdom_blit.register (Vdom_blit.custom ...))
 *    to include in the view an element managed by your own
 *    imperative code; that code, run when the element is created, could
 *    start a requestAnimationFrame loop, or install key listeners on
 *    window (which receives key presses whatever has the focus), and
 *    send the results as messages to the Vdom app.
 *    But this originally looked complicated to me and I didn't fully understand.
 *
 * 2) Current version: instead of plugging our own code into Vdom's
 *    main loop via custom elements, run_app below *is* the main loop, so
 *    it does directly what those custom elements would have done:
 *    - animation_frame re-registers itself with
 *      Window.request_animation_frame and sends ETick to update (#37);
 *    - Window.add_event_listener window Keydown/Keyup/Mouse... receives
 *      all key and mouse events, whatever has the focus (#36).
 *    What we lost by leaving Vdom is its diffing: the first direct-DOM
 *    version rebuilt the whole <svg> at each frame, which was simple but
 *    made <image> sprites disappear now and then (a new <image> element
 *    decodes its picture asynchronously) and restarted animated GIFs.
 *    Claude then adjusted the module V below with a tiny hand-made virtual
 *    DOM (~100 lines) with its own diff/patch (V.patch), specialized for
 *    our case (flat lists of SVG shapes, children matched by position)
 *    see https://github.com/aryx/ocaml-elm-playground/commit/e97df45
 *
 * If one day playground needs to be embedded in a bigger Vdom page
 * (e.g., with buttons or text inputs around the game), going back to
 * Vdom with custom elements as described in 1) would make sense. For a
 * standalone full-page playground, the direct approach is simpler.
 *
 * TODO:
 *  - still? switch to Canvas, use CanvasToCairo? so closer to native playground?
 *    I originally used SVG because that's what elm-playground is using, but
 *    I had pbs with the mouse coordinate and the bounding box conversion,
 *    so was maybe simpler to switch to Canvas?
 *    claude: the mouse coordinate problem is fixed now (see adjust_x_y),
 *    which removes one reason to switch to Canvas.
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* When set to true, we generate a new frame only when there is an event.
 * It makes things easier to observe with Chrome developer tools.
 *)
let debug = ref false

(*****************************************************************************)
(* run_app *)
(*****************************************************************************)

(* alt: when using Vdom (but Tick and Key issues, see notes about it before):
let (vdom_app_of_app: ('model, 'msg) Playground.app -> ('model, 'msg) V.app) = 
 fun { Playground. init; view; update; subscriptions = _subTODO } ->
  V.app 
      ~init:(
      let (model, _cmdsTODO) = init () in
      model, V.Cmd.Batch [])
      ~view:(fun model ->
        (* TODO: can change! *)
        let screen = Playground.to_screen 600. 600. in
        let shapes = view model in
        render screen shapes
      )
      ~update:(fun model msg -> 
        let model, _cmds = update msg model in
        model, V.Cmd.Batch []
       )
     ()
let run_app app =
  let app = vdom_app_of_app app in
  let run () = 
    Vdom_blit.run app 
    |> Vdom_blit.dom 
    |> Element.append_child (Document.body document) in
  let () = Window.set_onload window run in
  ()
*)

(* claude: start downloading (and decoding) the image right away, and
 * keep a reference to the Image object so the browser keeps it in its
 * memory cache; this way an <svg:image> switching to this url later
 * (e.g., Mario going from "walk" to "jump") can paint it without waiting
 * for the network. The old code did nothing here, so the first jump
 * could show no sprite until the download was done.
 *
 * The Ojs code below is the OCaml version of the JavaScript
 *   let img = new Image(); img.src = url;
 * (Ojs.global is the JS global object, where the Image class lives, and
 * Ojs.new_obj calls a JS constructor.) Setting src is what starts the
 * download; the image is not inserted in the page. We store img in
 * [preloaded] so it is not garbage collected (which could evict it
 * from the browser memory cache).
 *)
let preloaded : (string, Ojs.t) Hashtbl.t = Hashtbl.create 16

let preload_image (url : string) =
  if not (Hashtbl.mem preloaded url) then begin
    let img = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Image") [||] in
    Ojs.set_prop_ascii img "src" (Ojs.string_to_js url);
    Hashtbl.replace preloaded url img
  end

(* claude: the page's URL parameters, "?level=5&fast" ->
 * [("level", "5"); ("fast", "")], the web's command line (see
 * Playground.flags). The Ojs code is the JavaScript
 * window.location.search (vdom's Location has no binding for it). No
 * %-decoding: flags are meant to be short names and values. *)
let flags () : Playground.flags =
  let search = Ojs.string_of_js (Ojs.get_prop_ascii (Ojs.get_prop_ascii Ojs.global "location") "search") in
  let search =
    if String.length search > 0 && search.[0] = '?'
    then String.sub search 1 (Stdlib.(-) (String.length search) 1)
    else search
  in
  Playground.flags_of_strings (String.split_on_char '&' search)

(* claude: the browser's zone; getTimezoneOffset counts the other way,
 * the minutes UTC is ahead of the local time (-120 in Paris in summer) *)
(* claude: see Playground_platform.mli; 1. for now (the browser's
 * devicePixelRatio and the page's scaling would say more) *)
let pixel_ratio () : float = 1.

(* claude: see Playground_platform.mli; the page's body's CSS cursor *)
let set_cursor (c : Playground.cursor) : unit =
  let name = match c with Arrow -> "default" | Hand -> "pointer" | Text -> "text" | Crosshair -> "crosshair" | Hidden -> "none" in
  let body = Ojs.get_prop_ascii (Ojs.get_prop_ascii Ojs.global "document") "body" in
  Ojs.set_prop_ascii (Ojs.get_prop_ascii body "style") "cursor" (Ojs.string_to_js name)

let utc_offset (Playground.Time t) : int =
  Stdlib.( ~- ) (Date.get_timezone_offset (Date.new_date (t *. 1000.)))

(* claude: documents, in localStorage (Web_store); the capability is
 * the caller's proof it may, see the .mli *)
let store (_ : < Cap.open_out; .. >) name bytes = Web_store.store name bytes
let fetch (_ : < Cap.open_in; .. >) name = Web_store.fetch name
let stored (_ : < Cap.readdir; .. >) = Web_store.stored ()
let export (_ : < Cap.open_out; .. >) name bytes = Web_store.export name bytes

(*****************************************************************************)
(* run_app (the simple DOM) *)
(*****************************************************************************)

(* when using the simple DOM *)
(* claude: [network] unused: the browser downloads the images, by its
 * own rules (the page's site, or CORS) *)
let run_app ?(rendering = Playground.default_rendering) ?(flags = []) ?network:_ ?window:(w = Playground.default_window) app =
  (* claude: the web's has only the screen's shape to follow *)
  let screen = w.Playground.screen_size in
  Audio.set_fetcher Web_http.fetch_web;
  Transport.set_connect Web_connect.connect;
  Window.set_onload window (fun () ->

    (* claude: the program's screen, 1000 by 1000 unless it asks for
     * another shape (tinybox's menu, 16:9), as natively; the viewBox
     * follows, the browser letterboxing it to the window *)
    let sx, sy =
      match screen with
      | Some (w, h) -> (float_of_int w, float_of_int h)
      | None -> (Playground.default_width, Playground.default_height)
    in
    let resized = screen in
    let screen = Playground.to_screen sx sy in

    let (initmodel, init_cmd) = app.Playground.init flags in
    let model = ref initmodel in

    (* claude: the commands of init and update: a request's answer
     * comes back when the browser has it, a Cmd.Msg at the next frame
     * (as natively, Commands.mli) *)
    let next_frame_msgs = ref [] in
    let rec apply_msg msg =
      let newmodel, cmd = app.Playground.update msg !model in
      model := newmodel;
      perform cmd
    and perform cmd =
      Cmd.to_list cmd
      |> List.iter (fun (c : _ Cmd.t) ->
             match c with
             | Msg msg -> next_frame_msgs := !next_frame_msgs @ [ msg ]
             (* the capability checked where the command was built; the
              * browser has its own rules (the same site, or CORS) *)
             | Http_get (_caps, url, k) -> Web_http.fetch_response url (fun result -> apply_msg (k result))
             | Http_post (_caps, url, post, k) -> Web_http.fetch_response ~post url (fun result -> apply_msg (k result))
             | None | Batch _ -> ())
    in
    perform init_cmd;
    (* claude: a screen other than the default, said to the program
     * before its first frame, through its own subscriptions (as the
     * native platform does) *)
    Option.iter
      (fun (w, h) ->
        match E.event_to_msgopt (E.EResized (w, h)) (app.Playground.subscriptions !model) with
        | Some msg -> apply_msg msg
        | None -> ())
      resized;

    let process_playground_event event = 
      let subs = app.Playground.subscriptions !model in
      let msg_opt = E.event_to_msgopt event subs in
      (match msg_opt with
      | None -> ()
      | Some msg -> apply_msg msg
     );
    in
   
    (* claude: game speed.
     *
     * Window.request_animation_frame window f asks the browser to call
     * f just before it next redraws the screen (f receives the current
     * time in milliseconds). animation_frame below re-registers itself
     * each time, so it is called once per screen refresh.
     *
     * The problem with the old code: it delivered one Tick per call,
     * i.e., one per screen refresh. Playground games advance by a fixed
     * amount per Tick (e.g., examples/Mario.ml's dt), tuned for 60
     * refreshes per second, but many screens refresh faster (Mac
     * ProMotion screens: 120 per second), so Mario ran 2x too fast.
     *
     * The new code: count how much time passed since the previous call
     * (in [pending]), and deliver one Tick per full 1/60s elapsed. At 120Hz
     * that's a Tick every other frame; at 60Hz one per frame; at 30Hz two
     * per frame. The screen is still redrawn every frame. This is the
     * classic "fixed timestep" game loop (same idea as the 60fps cap in
     * playground/platforms/native/).
     *)
    let tick_period = 1. /. 60. in
    (* tolerate jitter in rAF timestamps on 60Hz displays, otherwise
     * we would sometimes skip a Tick and then do 2 in the next frame *)
    let tick_slack = 0.002 in
    let last_time = ref None in
    let pending = ref 0. in

    (* measured rAF rate, logged once, to help diagnose timing issues *)
    let rate_start = ref None in
    let rate_frames = ref 0 in

    (* the current <svg> and the vdom it was built from *)
    let current = ref None in

    (* one frame *)
    let rec animation_frame time =
      let time = time /. 1000. in

      (match !rate_start with
      | None -> rate_start := Some time
      | Some start ->
          incr rate_frames;
          if !rate_frames = 120 then
            Web_events.log (Printf.sprintf "requestAnimationFrame rate: %.0f Hz"
                   (float_of_int !rate_frames /. (time -. start)))
      );

      (* time elapsed since the previous frame (one Tick on the
       * very first frame) *)
      (match !last_time with
      | None -> pending := tick_period
      | Some last -> pending := !pending +. (time -. last)
      );
      last_time := Some time;
      (* after a long pause (e.g., the tab was hidden) don't try to catch up *)
      if !pending > 0.25 then pending := tick_period;
      (* claude: the Tick carries the wall-clock time (seconds since
       * 1970), not [time] (seconds since the page was loaded, which is
       * what requestAnimationFrame gives us). That's what the native
       * backend passes (Unix.gettimeofday) and what Elm's
       * onAnimationFrame passes (Time.Posix), and games rely on it:
       * Tetris.ml and Asteroid.ml initialize their last_tick
       * with Unix.gettimeofday() and compute [now -. last_tick] on each
       * Tick. With [time], that delta was about -1.8 billion seconds:
       * Tetris' piece started 1.8 billion rows above the well (and a
       * full drop with space then looped 1.8 billion times, freezing the
       * tab), and Asteroid ignored all Ticks (delta < tick) so it never
       * started. *)
      let wall_clock = Date.now () /. 1000. in
      (let msgs = !next_frame_msgs in
       next_frame_msgs := [];
       List.iter apply_msg msgs);
      let ticks = ref 0 in
      while !pending >= tick_period -. tick_slack do
        process_playground_event (E.ETick wall_clock);
        pending := !pending -. tick_period;
        incr ticks
      done;
      (* claude: the sounds those updates played *)
      Web_audio.play_audio !ticks;

      (* redraw: compute the description of the new frame, then either
       * build the real <svg> (first frame) or update the existing one
       * (see V.patch) *)
      let shapes = app.Playground.view !model in
      let node = Web_render.render ~rendering screen shapes in
      let body = Document.body document in
      (match !current with
      | None ->
          Element.remove_all_children body;
          let elt = V.create node in
          Element.append_child body elt;
          current := Some (node, elt)
      | Some (old, elt) ->
          let elt = V.patch ~parent:body elt old node in
          current := Some (node, elt)
      );

      if not !debug 
      then Window.request_animation_frame window animation_frame;
    in
    Window.request_animation_frame window animation_frame;

    (* claude: the keys this page saw go down and not yet up. A key-up
     * can go elsewhere -- the browser or the desktop taking the focus
     * for a moment on a shortcut -- and the key then stays held for the
     * program: a Control held forever turned every letter typed after a
     * Ctrl-Y into a control code (TinyTurboPascal typed nothing more).
     * So every event says where the modifiers really are (ctrlKey,
     * altKey, shiftKey, metaKey) and one held that is not is released;
     * and the page losing the focus releases them all *)
    let held : string list ref = ref [] in
    let release (k : string) =
      if List.mem k !held then begin
        held := List.filter (fun h -> h <> k) !held;
        process_playground_event (E.EKeyChanged (false, k))
      end
    in
    let sync_modifiers evt =
      let flag prop = Ojs.bool_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) prop) in
      List.iter
        (fun (k, prop) -> if List.mem k !held && not (flag prop) then release k)
        [ ("Control", "ctrlKey"); ("Alt", "altKey"); ("Shift", "shiftKey"); ("Meta", "metaKey") ]
    in
    ignore
      (Ojs.call Ojs.global "addEventListener"
         [| Ojs.string_to_js "blur"; Ojs.fun_to_js 1 (fun _ -> List.iter release !held) |]);
    let on_js_event evt =
      (* claude: the browser lets sound start only after an input *)
      Web_audio.resume_audio ();
      sync_modifiers evt;
      (* the root <svg>, needed to convert mouse coordinates (see
       * adjust_x_y); None if the first frame is not drawn yet *)
      let svg_opt = Option.map snd !current in
      let evt_opt = Web_events.js_event_to_event evt svg_opt in
      (match evt_opt with
      | None -> ()
      | Some event ->
          (match event with
          | E.EKeyChanged (true, k) -> if not (List.mem k !held) then held := k :: !held
          | E.EKeyChanged (false, k) -> held := List.filter (fun h -> h <> k) !held
          | _ -> ());
          process_playground_event event
      );
      (* claude: the relative move too (mdx/mdy), from movementX/Y (not
       * in vdom's binding, hence Ojs), which keep counting when the
       * pointer is locked (captured, see playground3d's webgl backend),
       * whereas clientX/Y then stop; y up, like the playground's *)
      if Event.type_ evt = "mousemove" then begin
        let get prop = Ojs.float_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) prop) in
        process_playground_event (E.EMouseMoveBy (get "movementX", -. (get "movementY")))
      end;
      (* claude: a key press that produced a character also feeds
       * computer.keyboard.typed, beside the key event above *)
      if Event.type_ evt = "keydown" then begin
        match Web_events.typed_of_key (Event.key evt) with
        | Some str -> process_playground_event (E.ETyped str)
        | None -> ()
      end;
      if !debug then Window.request_animation_frame window animation_frame;
    in
    [
      Event.Mousemove;
      Event.Mousedown;
      Event.Mouseup;
      Event.Keydown;
      Event.Keyup;
      Event.Wheel;
      Event.Dblclick;
    ] |> List.iter (fun evt_kind ->
       Window.add_event_listener window evt_kind on_js_event true
    );
    (* claude: a right click is the game's (Playground.mouse.mrdown), not
     * the browser's context menu *)
    Window.add_event_listener window Event.Contextmenu Event.prevent_default true;
    (* claude: and Tab is the game's key too (TinyCrush's turn), not the
     * browser's move to the next focusable element -- which would also
     * swallow its keyup, leaving it held *)
    (* claude: and the function keys F1 to F10, a terminal program's
     * (TinyTurboPascal's F9, its debugger's F7 and F8), not the
     * browser's help, find, reload and caret browsing; F11 (full
     * screen) and F12 (the developer tools) stay the browser's *)
    let game_key k =
      k = "Tab"
      || String.length k >= 2 && k.[0] = 'F'
         && (match int_of_string_opt (String.sub k 1 (Stdlib.( - ) (String.length k) 1)) with Some n -> n >= 1 && n <= 10 | None -> false)
    in
    (* claude: and Control and a digit, the stand-in for Control and an F
     * key on a keyboard without them (TinyTurboPascal's Ctrl+9, Run),
     * not the browser's move to the nth tab *)
    let ctrl_digit evt k =
      Ojs.bool_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) "ctrlKey")
      && String.length k = 1 && k.[0] >= '0' && k.[0] <= '9'
    in
    Window.add_event_listener window Event.Keydown
      (fun evt -> let k = Event.key evt in if game_key k || ctrl_digit evt k then Event.prevent_default evt)
      true;
    (* claude: and a phone's: its width, its fingers (Phones, above) *)
    Web_phone.phone_page ();
    Web_phone.listen_to_fingers ~svg:(fun () -> Option.map snd !current) ~process:process_playground_event
      ~keys:(Web_phone.phone_keys ~process:process_playground_event ~svg:(fun () -> Option.map snd !current));
  )
