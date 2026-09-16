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
 *  - I've avoided to use the 'vdom' type and the views functions returns
 *    instead of a vdom a list of shapes. Anyway, we do not attach
 *    any messages in the returned vdom; we use global system events.
 *)

(*****************************************************************************)
(* Config *)
(*****************************************************************************)
(* orig: was 600 x 600.
 * todo: note that some games assumes a square grid (e.g., Snake)
 *)
let default_width = 1000.
let default_height = 1000.

(*****************************************************************************)
(* Number *)
(*****************************************************************************)

type number = float

(*****************************************************************************)
(* Time (and animations) *)
(*****************************************************************************)

type time = Time of Time.posix

(* [to_frac period time] returns where [time] is within the current
 * [period] (in seconds), as a fraction in [0, 1), e.g., with a period
 * of 3 seconds: 0.0 at the start of each 3-second cycle, 0.5 after 1.5s,
 * and close to 1.0 just before the next cycle. spin/wave/zigzag below
 * turn this fraction into an angle, a position, etc.
 *
 * claude: this used to be (same as Elm's toFrac):
 *
 *   let (to_frac: float -> time -> float) = fun period (Time posix) ->
 *       let ms = Time.posix_to_millis posix in
 *       let p = period *. 1000. in
 *       if p = 0. || ms = 0
 *       then failwith "division by zero in to_frac";
 *       float (mod_by (round p) ms) / p
 *
 * The problem: [posix] is the wall-clock time in seconds since 1970 (see
 * the ETick in each backend's Playground_platform.ml), so
 * Time.posix_to_millis converts it to an OCaml int around 1.8e12
 * (e.g., 1789590431780 on 2026-09-16). That's fine natively (63-bit
 * ints), and in Elm (whose ints are JavaScript doubles), but with
 * js_of_ocaml, OCaml ints are 32 bits (-2^31 .. 2^31-1 = about +-2.1e9)
 * and int_of_float silently keeps only the low 32 bits:
 *
 *   ms (float)          ms (32-bit int)   old frac (period 3s)  new frac
 *   1789590431780.  ->  -1410930652       -0.217                 0.927
 *   1789590431796.  ->  -1410930636       -0.212                 0.932
 *
 * so on the web:
 *  - the int was negative, hence [mod] (which keeps the sign of its
 *    first argument in OCaml) returned a negative remainder, and the
 *    "fraction" was in (-1, 0] instead of [0, 1). spin and wave happen
 *    to still look right with a negative fraction (an angle of -78
 *    degrees is the same as 282, cos is symmetric), but zigzag does not:
 *    [abs (2 * frac - 1)] is then between 1 and 3 instead of between 0
 *    and 1, so e.g. the red rectangle of examples/Animation.ml, which
 *    should zigzag between -2 and 2 degrees, was tilted between 2 and
 *    10 degrees (-2 + 4 * 1.434 = 3.7 degrees for frac = -0.217).
 *  - every 2^32 ms (~49.7 days) the 32-bit value jumps from +2^31-1 to
 *    -2^31 (next time: 2026-10-28), making every animation jump, e.g.
 *    from 0.877 to -0.883 for a 3s period, instead of 0.021 -> 0.027.
 *  - [ms = 0] could happen at each wrap (without any real division by
 *    zero), raising an exception.
 *
 * The new code does the same computation on floats, which hold the
 * milliseconds exactly (doubles are exact for integers up to 2^53 =~
 * 9e15), both natively and in JavaScript, and Float.rem of two positive
 * numbers is always positive. For non-overflowing values (native), the
 * result is exactly the same as before.
 *)
let (to_frac: float -> time -> float) = fun period (Time posix) ->
    let ms = Float.round (posix *. 1000.) in
    let p = period *. 1000. in
    if p = 0.
    then failwith "division by zero in to_frac";
    Float.rem ms (Float.round p) / p

(* period is in seconds *)
let (spin: number -> time -> number) = fun period time ->
    360. * to_frac period time

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

(* The coordinate system assumes (0, 0) is at the center of the screen, not
 * the top left corner as in Cairo, or bottom left corner as in Graphic.
 *)
type shape = {
    x: number; 
    y: number; 
    (* in degrees, counter-clockwise! *)
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
  | Polygon of color * (number (* x *) * number (* y *)) list

  | Image of number (* width *) * number (* height *) * string (* url *)
  | Words of color * string

  | Group of shape list

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

let (polygon: color -> (number * number) list -> shape) = fun color points ->
  shape 0. 0. 0. 1. 1. (Polygon (color, points))

let (words: color -> string -> shape) = fun color str ->
  shape 0. 0. 0. 1. 1. (Words (color, str))

(* claude: the font used to render [words], shared by the native and web
 * backends so that text looks the same in both. It used to be each
 * backend's default: Cairo's 10 units and sans-serif font natively, the
 * browser's 16px and serif font (e.g., Times) on the web, so the web text
 * was 1.6 times bigger and looked different. We use 10, the old native
 * size, because the [scale] factors in games/ were tuned with it (e.g.,
 * Pong's score is "words ... |> scale 10."; with 16 it was way too big),
 * even though elm-playground gets 16 (its renderWords sets no font size,
 * so the browser default applies). The size is in playground units,
 * which are pixels for the default 1000x1000 native window; the web
 * scales it with the window like the other shapes. "sans-serif" is a generic font family name
 * understood by both Cairo (select_font_face) and browsers (CSS).
 *)
let words_font_size = 10.
let words_font_family = "sans-serif"

let (image: number -> number -> string -> shape) = fun w h src ->
  shape 0. 0. 0. 1. 1. (Image (w, h, src))

let (group: shape list -> shape) = fun xs ->
  shape 0. 0. 0. 1. 1. (Group xs)


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

(* the degrees go counter-clockwise! *)
let (rotate: number -> shape -> shape) = 
  fun da {x; y; angle; scale; alpha; form } ->
    {x; y; angle = angle + da; scale; alpha; form}

let (fade: number -> shape -> shape) = 
  fun o {x; y; angle; scale; alpha = _; form } ->
    {x; y; angle; scale; alpha = o; form}

let (scale: number -> shape -> shape) =
  fun ns {x; y; angle; scale; alpha; form } ->
    {x; y; angle; scale = scale * ns; alpha; form}


(*****************************************************************************)
(* Computer *)
(*****************************************************************************)

(*-------------------------------------------------------------------*)
(* Screen *)
(*-------------------------------------------------------------------*)

type screen = {
  width: number;
  height: number;

  (* Derived from width and height. The origin (0, 0) is at center. *)
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

  (* player2 (pad: not in original Playground.elm) *)
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
(* the user-defined "model" *)

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
  screen = to_screen default_width default_height;
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
      to_screen default_width default_height, Cmd.none
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

  | KeyChanged of bool * string

  | MouseMove of (float * float)
  | MouseClick (* reset after a Tick *)
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
                to_screen default_width default_height, 
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
        (* todo: remove click in mouse! after the call to update_memory,
         * to kinda ack the click
         *)
        Game (update_memory computer memory,
          { computer with time = Time time })
    | Resized (_w, _h) ->
        failwith "Todo"
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
                (* old: Vdom does not provide OnMouseUp, so we use MouseClick
                 * mouse_down false *)
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
