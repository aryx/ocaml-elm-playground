open Playground_common

(*****************************************************************************)
(* Render (Platform dependent) *)
(*****************************************************************************)

let (render_shape: shape -> 'msg Html.vdom) = 
  fun { x; y; angle; scale; alpha; form} ->
  match form with
  | Circle (color, radius) -> 
     Playground_platform.render_circle color radius x y angle scale alpha
  | Oval (color, width, height) ->
     Playground_platform.render_oval color width height x y angle scale alpha
  | Rectangle (color, width, height) ->
     Playground_platform.render_rectangle color width height x y angle scale alpha
  | Ngon (color, n, radius) ->
     Playground_platform.render_ngon color n radius x y angle scale alpha
  

let (render: screen -> shape list -> 'msg Html.vdom) = fun screen shapes ->
    let _w = screen.width in
    let _h = screen.height in
    let _x = screen.left |> string_of_number  in
    let _y = screen.bottom |> string_of_number in

    let _vdoms = List.map render_shape shapes in
    Html.VNone

(*****************************************************************************)
(* Playground: picture *)
(*****************************************************************************)

let (picture: shape list -> (unit, screen, msg1) Platform.program) = 
 fun shapes ->
  let init () = 
      to_screen 600. 600., Cmd.none
  in
  let view screen = 
      { UI.
        title = "Playground";
        body = [ render screen shapes ]
      }
  in
  let update msg  _model = 
    match msg with
    | Resized1 (width, height) ->
       to_screen (float width) (float height), Cmd.none
  in
  let subscriptions _ =
      (* TODO: on_resize *)
      Sub.none
  in
  UI.document { UI. init; view; update; subscriptions }

(*****************************************************************************)
(* Playground: animation *)
(*****************************************************************************)

let (animation: (time -> shape list) -> (unit, animation, msg) Platform.program) =
 fun view_frame ->
   let init () = 
     Animation (Event.Visible, 
                to_screen 600. 600., 
                (* bugfix: use 1, not 0, otherwise get div_by_zero exn in
                 * to_frac if use spin/wave/... *)
                Time (Time.millis_to_posix 1)),
     Cmd.none
   in
   let view (Animation (_, screen, time)) =
     { UI.
       title = "Playground";
       body = [render screen (view_frame time)];
     }
   in
   let update msg model = 
     animation_update msg model,
     Cmd.none
   in
  let subscriptions _ =
      (* TODO: on_resize *)
      Event.on_animation_frame (fun x -> Tick x)
  in
  UI.document { UI. init; view; update; subscriptions }

(*****************************************************************************)
(* Playground: game *)
(*****************************************************************************)

let (game: 
  (computer -> 'memory -> shape list) ->
  (computer -> 'memory -> 'memory) ->
  'memory ->
  (unit, 'memory game, msg) Platform.program) = 
 fun view_memory update_memory initial_memory ->

  let init () =
      Game (Event.Visible, initial_memory, initial_computer),
      Cmd.none (* TODO: Task.perform GotViewport Dom.getViewport *)
  in
  let view (Game (_, memory, computer)) =
    let elt = render computer.screen (view_memory computer memory) in
    { UI.
      title = "Playground";
      body = [elt];
    }
  in
  let update msg model =
      game_update update_memory msg model,
      Cmd.none
  in
  let subscriptions _ = Sub.batch [
      (* TODO: on_resize *)
      Event.on_animation_frame (fun x -> Tick x);
      Event.on_mouse_move (fun x -> MouseMove x);
      Event.on_mouse_down (fun () -> MouseButton true);
      Event.on_mouse_up   (fun () -> MouseButton false);
      Event.on_key_down (fun key -> KeyChanged (true, key));
      Event.on_key_up   (fun key -> KeyChanged (false, key));
  ]
  in
  UI.document { UI. init; view; update; subscriptions }

(*****************************************************************************)
(* Run app (Platform dependent)  *)
(*****************************************************************************)
let run_app app =
  Playground_platform.run_app app
