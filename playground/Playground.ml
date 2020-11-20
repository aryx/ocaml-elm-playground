open Common
open Basics (* elm-core *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the amazing Elm Playground library to OCaml.
 *
 * See https://github.com/evancz/elm-playground/blob/master/src/Playground.elm
 * for more information.
 * 
 * I also partially ported some of the libraries elm-playground depends on
 * (e.g., elm-core) instead of using directly the equivalent OCaml functions
 * in the hope that if I later need to also port Elm games, I will have 
 * less work.
 *
 * I've changed a few things to make the code more portable, so we can
 * use the library in a Native context (with Cairo+SDL), or in 
 * a Web context (with js_of_ocaml+SVG+ocaml-vdom):
 *  - I've introduced a new 'app' type instead of using 'Platform.program'
 *  - I avoid to use the 'vdom' type and the views functions returns
 *    instead of a vdom a list of shapes. Anyway, we do not attach
 *    any messages in the returned vdom; we use global system events.
 *)

(*****************************************************************************)
(* Number *)
(*****************************************************************************)

type number = float

let string_of_number x = 
  spf "%f" x

(*****************************************************************************)
(* Time (and animations) *)
(*****************************************************************************)

type time = Time of Time.posix

let (to_frac: float -> time -> float) = fun period (Time posix) ->
    let ms = Time.posix_to_millis posix in
    let p = period *. 1000. in
    if p = 0. || ms = 0
    then failwith "division by zero in to_frac";
    (*pr2_gen (ms, p);*)
    float (mod_by (round p) ms) / p

(* period is in seconds *)
let (spin: number -> time -> number) = fun period time ->
    360. * to_frac period time
    (*|> (fun x -> pr2_gen (period, time, x); x)*)

let (wave: number -> number -> number -> time -> number) = 
 fun lo hi period time ->
    lo + (hi - lo) * (1. + cos (turns (to_frac period time))) / 2.

let (zigzag: number -> number -> number -> time -> number) = 
 fun lo hi period time ->
    lo + (hi - lo) * abs_float (2. * to_frac period time - 1.)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)
(* in core/Color.ml now *)
type color = Color.t
include Color

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

type shape = {
    x: number; 
    y: number; 
    (* in degrees *)
    angle: number;
    scale: number;
    (* [0..1] range *)
    alpha: number;

    form: form
}
and form = 
  | Circle of color * number (* radius *)
  | Oval      of color * number * number
  | Rectangle of color * number * number
  | Ngon of color * int * number
  (* TODO: Word, Image *)

(* less: could use deriving constructor? *)
let shape x y angle scale alpha form =
  { x; y; angle; scale; alpha; form }

(*-------------------------------------------------------------------*)
(* Shape constructors *)
(*-------------------------------------------------------------------*)

let (circle: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Circle (color, radius))

let (oval: color -> number -> number -> shape) = fun color width height ->
  shape 0. 0. 0. 1. 1. (Oval (color, width, height))

let (rectangle: color -> number -> number -> shape) = fun color width height ->
  shape 0. 0. 0. 1. 1. (Rectangle (color, width, height))

let (square: color -> number -> shape) = fun color n ->
  shape 0. 0. 0. 1. 1. (Rectangle (color, n, n))

let (triangle: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Ngon (color, 3, radius))

let (pentagon: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Ngon (color, 5, radius))

let (hexagon: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Ngon (color, 6, radius))

let (octagon: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Ngon (color, 8, radius))

(*-------------------------------------------------------------------*)
(* Move shapes *)
(*-------------------------------------------------------------------*)

let (move: number -> number -> shape -> shape) = 
  fun dx dy {x; y; angle; scale; alpha; form } ->
    {x = x + dx; y = y + dy; angle; scale; alpha; form}

let (move_left: number -> shape -> shape) = 
  fun dx {x; y; angle; scale; alpha; form } ->
    {x = x - dx; y; angle; scale; alpha; form}

let (move_down: number -> shape -> shape) = 
  fun dy {x; y; angle; scale; alpha; form } ->
    {x; y = y - dy; angle; scale; alpha; form}

let (move_x: number -> shape -> shape) = 
  fun dx {x; y; angle; scale; alpha; form } ->
    {x = x + dx; y; angle; scale; alpha; form}

let (move_y: number -> shape -> shape) = 
  fun dy {x; y; angle; scale; alpha; form } ->
    {x; y = dy + y; angle; scale; alpha; form}

let move_right = move_x
let move_up = move_y

(*-------------------------------------------------------------------*)
(* Customize shapes *)
(*-------------------------------------------------------------------*)

let (rotate: number -> shape -> shape) = 
  fun da {x; y; angle; scale; alpha; form } ->
    {x; y; angle = angle + da; scale; alpha; form}

let (fade: number -> shape -> shape) = 
  fun o {x; y; angle; scale; alpha = _; form } ->
    {x; y; angle; scale; alpha = o; form}
  

(*****************************************************************************)
(* Computer *)
(*****************************************************************************)

(*-------------------------------------------------------------------*)
(* Screen *)
(*-------------------------------------------------------------------*)

type screen = {
  width: number;
  height: number;
  top: number;
  left: number;
  right: number;
  bottom: number;
}

let (to_screen: number -> number -> screen) = fun width height ->
  { width; 
    height; 
    (* the origin (0, 0) is at the center of the screen *)
    top = height / 2.;
    left = (-. width) / 2.;
    right = width / 2.;
    bottom = (-. height) / 2.;
  }

(*-------------------------------------------------------------------*)
(* Mouse *)
(*-------------------------------------------------------------------*)
(* in screen-centered coordinate (0, 0) at center of screen *)
type mouse = {
  mx: number;
  my: number;

  mdown: bool;
  mclick: bool;
}

let mouse_move mx my mouse = 
  { mouse with mx; my }
let mouse_click mclick mouse =
  { mouse with mclick }
let mouse_down mdown mouse =
  { mouse with mdown }

(*-------------------------------------------------------------------*)
(* Keyboard *)
(*-------------------------------------------------------------------*)
type keyboard = {
  (* player1 usually *)
  kup: bool;
  kdown: bool;
  kleft: bool;
  kright: bool;

  (* player2 (not in original Playground.elm) *)
  kw: bool;
  ks: bool;
  ka: bool;
  kd: bool;
  
  kspace: bool;
  kenter: bool;
  kshift: bool;
  kbackspace: bool;
  
  keys: string Set.t;  
}

let empty_keyboard = {
  kup = false; kdown = false; kleft = false; kright = false;
  kw = false; ks = false; ka = false; kd = false;
  kspace = false; kenter = false; kshift = false; kbackspace = false;
  keys = Set.empty
}

let to_x keyboard =
  (if keyboard.kright then 1. else 0.) - (if keyboard.kleft then 1. else 0.)

let to_y keyboard =
  (if keyboard.kup then 1. else 0.) - (if keyboard.kdown then 1. else 0.)

let to_x2 keyboard =
  (if keyboard.kd then 1. else 0.) - (if keyboard.ka then 1. else 0.)

let to_y2 keyboard =
  (if keyboard.kw then 1. else 0.) - (if keyboard.ks then 1. else 0.)

let square_root_two =
  sqrt 2.

let to_xy keyboard =
  let x = to_x keyboard in
  let y = to_y keyboard in
  if x <> 0. && y <> 0.
  then (x / square_root_two, y / square_root_two)
  else (x, y)

let update_keyboard is_down key keyboard =
  let keys = 
    if is_down
    then Set.insert key keyboard.keys
    else Set.remove key keyboard.keys
  in
  match key with
  | "ArrowUp"    -> { keyboard with keys; kup = is_down }
  | "ArrowDown"  -> { keyboard with keys; kdown = is_down }
  | "ArrowLeft"  -> { keyboard with keys; kleft = is_down }
  | "ArrowRight" -> { keyboard with keys; kright = is_down }
  | "w"          -> { keyboard with keys; kw = is_down }
  | "s"          -> { keyboard with keys; ks = is_down }
  | "a"          -> { keyboard with keys; ka = is_down }
  | "d"          -> { keyboard with keys; kd = is_down }
  | "space"          -> { keyboard with keys; kspace = is_down }
  | _ -> { keyboard with keys }


(*-------------------------------------------------------------------*)
(* Memory *)
(*-------------------------------------------------------------------*)

(*-------------------------------------------------------------------*)
(* Computer *)
(*-------------------------------------------------------------------*)
type computer = {
  mouse: mouse;
  keyboard: keyboard;
  screen: screen;
  time: time;
}

let initial_computer = {
  mouse = { mx = 0.; my = 0.; mdown = false; mclick = false };
  keyboard = empty_keyboard;
  screen = to_screen 600. 600.;
  time = Time (Time.millis_to_posix 1);
}

(*****************************************************************************)
(* App *)
(*****************************************************************************)
(* was in Platform.elm but makes its harder to have cross-platform playground*)

type ('model, 'msg) app = 
  {
    init: (unit -> ('model * 'msg Cmd.t));
    update: ('msg -> 'model -> ('model * 'msg Cmd.t));
    (* old: removed dependency to vdom, harder to port to native
     * view: ('model -> 'msg Html.vdom);
     *)
    view: ('model -> shape list);
    subscriptions: ('model -> 'msg Sub.t);
  }

(*****************************************************************************)
(* Playground: picture *)
(*****************************************************************************)
type msg1 = 
  | Resized1 of int * int

let (picture: shape list -> (screen, msg1) app) = 
 fun shapes ->
  let init () = 
      to_screen 600. 600., Cmd.none
  in
  let view _screen = shapes in
  let update msg  _model = 
    match msg with
    | Resized1 (width, height) ->
       to_screen (float width) (float height), Cmd.none
  in
  let subscriptions _ =
      (* TODO: on_resize *)
      Sub.none
  in
  { init; view; update; subscriptions }

(*****************************************************************************)
(* Playground: animation *)
(*****************************************************************************)

(* this is also used by the game playground *)
type msg =
  | Tick of Time.posix
  | Resized of int * int
  (* ... *)

  | KeyChanged of bool * string

  | MouseMove of (float * float)
  | MouseClick
  | MouseButton of bool (* true = down, false = up *)


type animation = Animation of (*Event.visibility * *) screen * time

let animation_update msg (Animation (s, t) as state) =
  match msg with
  | Tick posix -> 
    Animation (s, (Time posix))
  | Resized (w, h) -> 
    Animation (to_screen (float w) (float h), t)
  | MouseMove _ 
  | MouseClick 
  | MouseButton _
  | KeyChanged _
    -> state

let (animation: (time -> shape list) -> (animation, msg) app) =
 fun view_frame ->
   let init () = 
     Animation ((* Event.Visible, *)
                to_screen 600. 600., 
                (* bugfix: use 1, not 0, otherwise get div_by_zero exn in
                 * to_frac if use spin/wave/... *)
                Time (Time.millis_to_posix 1)),
     Cmd.none
   in
   let view (Animation (_screen, time)) =
      view_frame time
   in
   let update msg model = 
     animation_update msg model,
     Cmd.none
   in
  let subscriptions _ =
      (* TODO: on_resize *)
      Sub.on_animation_frame (fun x -> Tick x)
  in
  { init; view; update; subscriptions }

(*****************************************************************************)
(* Playground: game *)
(*****************************************************************************)

type 'memory game = Game of (*Event.visibility **) 'memory * computer

let (game_update: (computer -> 'memory -> 'memory) -> msg -> 'memory game ->
 'memory game) =
 fun update_memory msg (Game (memory, computer)) ->
    match msg with
    | Tick time ->
        (* todo: remove click in mouse? *)
        Game (update_memory computer memory,
          { computer with time = Time time })
    | Resized (_w, _h) ->
        raise Todo
    (* we assume the x, y is in playground coordinate system (0,0) at the
     * center of the screen.
     *)
    | MouseMove (x, y) ->
        (* old: in web context:
         * let x = computer.screen.left + page_x in
         * let y = computer.screen.top - page_y in
         *)
        Game (memory, 
             { computer with mouse = mouse_move x y computer.mouse })
    | MouseClick ->
        Game (memory, 
             { computer with mouse = 
                (* Vdom does not provide OnMouseUp, so we use MouseClick *)
                 mouse_down false 
                   (mouse_click true computer.mouse) })
    | MouseButton is_down ->
        Game (memory, 
             { computer with mouse = mouse_down is_down computer.mouse })
    | KeyChanged (is_down, key) ->
        Game (memory,
             { computer with keyboard = update_keyboard is_down key 
                 computer.keyboard })

let (game: 
  (computer -> 'memory -> shape list) ->
  (computer -> 'memory -> 'memory) ->
  'memory ->
  ('memory game, msg) app) = 
 fun view_memory update_memory initial_memory ->

  let init () =
      Game (initial_memory, initial_computer),
      Cmd.none (* TODO: Task.perform GotViewport Dom.getViewport *)
  in
  let view (Game (memory, computer)) =
    view_memory computer memory
  in
  let update msg model =
      game_update update_memory msg model,
      Cmd.none
  in
  let subscriptions _ = Sub.batch [
      (* TODO: on_resize *)
      Sub.on_animation_frame (fun x -> Tick x);
      Sub.on_mouse_move (fun x -> MouseMove x);
      Sub.on_mouse_down (fun () -> MouseButton true);
      Sub.on_mouse_up   (fun () -> MouseButton false);
      Sub.on_key_down (fun key -> KeyChanged (true, key));
      Sub.on_key_up   (fun key -> KeyChanged (false, key));
  ]
  in
  { init; view; update; subscriptions }
