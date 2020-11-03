open Common
module V = Vdom

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers modules *)
(*****************************************************************************)

let log s = 
  Js_browser.Console.log Js_browser.console (Ojs.string_to_js s)

module Basics = struct
let (/..) = (/)
let (+..) = (+)
let (-..) = (-)
let ( *.. ) = ( * )

let (/) = (/.)
let (+) = (+.)
let (-) = (-.)
let ( * ) = ( *. )

let (round: float -> int) = fun f ->
    int_of_float (floor (f +. 0.5))

let (turns: float -> float) = fun angle_in_turns ->
    2. * Float.pi * angle_in_turns

let (clamp: 'number -> 'number -> 'number -> 'number) = fun low high number ->
  if number < low then
    low
  else if number > high then
    high
  else
    number

end
open Basics

module Set = struct
type 'a t = 'a list
let empty = []
end

module Time = struct
type posix = float
let millis_to_posix n =
  float_of_int n
let posix_to_millis n =
  int_of_float n
end

module Platform = struct
(* flags? *)
type ('flags, 'model, 'msg) program =
  ('model, 'msg) V.app
end

module Cmd = struct
type 'msg t = 'msg V.Cmd.t
let none = V.Cmd.Batch []
end

module Html = struct
type 'msg t = 'msg V.vdom
let style = V.style

let div a b =
  V.div ~a b
end

module Svg = struct
type 'msg t = 'msg V.vdom

let svg attrs xs = 
  V.svg_elt "svg" ~a:attrs xs

let trusted_node s attrs xs = 
  V.svg_elt s ~a:attrs xs

(* !subtle! need to eta-expand. You can't factorize with
 * let circle = trusted_node "circle" otherwise
 * we don't get the general 'msg vdom type inferred but
 * the first call to circle in this file will bind forever the type
 * parameter (e.g., to `Resize of int * int).
 * You can see the wrongly inferred type by using ocamlc -i on
 * this file (you may need dune --verbose to get the full list of -I first).
 *)
let circle a b  =
  trusted_node "circle" a b
let ellipse a b =
  trusted_node "ellipse" a b
let rect a b =
  trusted_node "rect" a b
let polygon a b =
  trusted_node "polygon" a b

module Attributes = struct
let viewBox = V.attr "viewBox"

let width = V.attr "width"
let height = V.attr "height"

let r = V.attr "r"
let rx = V.attr "rx"
let ry = V.attr "ry"
let fill = V.attr "fill"
let points = V.attr "points"
let transform = V.attr "transform"
let opacity = V.attr "opacity"

end
end

module Browser = struct
type 'msg document = {
  title: string;
  body: 'msg Html.t list;
}

type ('flags, 'model, 'msg) app = {
  init: 'flags -> ('model * 'msg Cmd.t);
  view: 'model -> 'msg document;
  update: 'msg -> 'model -> ('model * 'msg Cmd.t);
  (* subscriptions: 'model -> 'msg V.Sub?? *)
}

let (document: ('flags, 'model, 'msg) app -> 
               ('flags, 'model, 'msg) Platform.program) 
 = fun { init; view; update } ->
  V.app 
      ~init:(init ()) 
      ~view:(fun model ->
        match view model with
        (* TODO: do a div? *)
        | {title=_; body} -> List.hd body
      )
      ~update:(fun model msg -> update msg model) 
     ()

end

module Event = struct
type visibility = 
  | Visible
end

(*****************************************************************************)
(* Real Start of Playground *)
(*****************************************************************************)
(* in Playground.elm *)

(* You should not depend on Vdom starting from here, but instead
 * rely on the helper modules above (which internally use Vdom).
 *)

(*****************************************************************************)
(* Number *)
(*****************************************************************************)

type number = float

let string_of_floatint x = 
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
    float_of_int ((round p) mod ms) / p

let (spin: number -> time -> number) = fun period time ->
    360. * to_frac period time

let (wave: number -> number -> number -> time -> number) = 
 fun lo _hi _period _time -> lo
(*
    lo + (hi - lo) * (1. + cos (turns (to_frac period time))) / 2.
*)

let (zigzag: number -> number -> number -> time -> number) = 
 fun lo _hi _period _time -> lo
(*
    lo + (hi - lo) * abs_float (2. * to_frac period time - 1.)
*)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

type color =
  | Hex of string
  | Rgb of int * int * int

let render_color color =
  match color with
  | Hex str -> str
  | Rgb (r,g,b) -> spf "rgb(%d,%d,%d)"  r g b

let white = Hex "#FFFFFF"
let black = Hex "#000000"

let red   = Hex "#cc0000"
let green = Hex "#73d216"
let blue  = Hex "#3465a4"

let brown = Hex "#c17d11"

(*-------------------------------------------------------------------*)
(* Light colors *)
(*-------------------------------------------------------------------*)

let lightYellow = Hex "#fce94f"
let lightPurple = Hex "#ad7fa8"

(*-------------------------------------------------------------------*)
(* Dark colors *)
(*-------------------------------------------------------------------*)

(*-------------------------------------------------------------------*)
(* Shades of grey *)
(*-------------------------------------------------------------------*)

let darkGray = Hex "#babdb6"

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

type shape = {
    x: number; 
    y: number; 
    angle: number;
    scale: number;
    alpha: number;

    form: form
}
and form = 
  | Circle of color * number (* radius *)
  | Oval      of color * number * number
  | Rectangle of color * number * number
  | Ngon of color * int * number

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
    top = height / 2.;
    left = (-. width) / 2.;
    right = width / 2.;
    bottom = (-. height) / 2.;
  }

(*-------------------------------------------------------------------*)
(* Mouse *)
(*-------------------------------------------------------------------*)
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
  kup: bool;
  kdown: bool;
  kleft: bool;
  kright: bool;
  
  kspace: bool;
  kenter: bool;
  kshift: bool;
  kbackspace: bool;
  
  keys: string Set.t;  
}

let empty_keyboard = {
  kup = false; kdown = false; kleft = false; kright = false;
  kspace = false; kenter = false; kshift = false; kbackspace = false;
  keys = Set.empty
}

let to_x keyboard =
  (if keyboard.kright then 1. else 0.) - (if keyboard.kleft then 1. else 0.)

let to_y keyboard =
  (if keyboard.kup then 1. else 0.) - (if keyboard.kdown then 1. else 0.)

let square_root_two =
  sqrt 2.

let to_xy keyboard =
  let x = to_x keyboard in
  let y = to_y keyboard in
  if x <> 0. && y <> 0.
  then (x / square_root_two, y / square_root_two)
  else (x, y)

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
(* Render *)
(*****************************************************************************)
    
let render_transform x y a s =
  if a = 0. then
    if s = 1.
    then
      spf "translate(%s, %s)" 
        (string_of_floatint x) (string_of_floatint (-. y))
    else
      spf "translate(%s, %s) scale(%s)" 
        (string_of_floatint x) (string_of_floatint (-. y))
        (string_of_floatint s)
 else
  if s = 1.
  then
      spf "translate(%s, %s) rotate(%s)" 
        (string_of_floatint x) (string_of_floatint (-. y))
        (string_of_floatint (-. a))
  else
      spf "translate(%s, %s) rotate(%s) scale(%s) " 
        (string_of_floatint x) (string_of_floatint (-. y))
        (string_of_floatint (-. a))
        (string_of_floatint s)

let render_rect_transform width height x y angle s =
  render_transform x y angle s ^
  spf " translate(%s, %s)" 
     (string_of_floatint (-. width / 2.))
     (string_of_floatint (-. height / 2.))


let render_alpha alpha =
  if alpha = 1.
  then []
  else [Svg.Attributes.opacity (string_of_floatint (clamp 0. 1. alpha))]

let render_circle color radius x y angle s alpha =
  Svg.circle 
    (Svg.Attributes.r (string_of_floatint radius) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_oval color width height x y angle s alpha = 
  Svg.ellipse
    (Svg.Attributes.rx (string_of_floatint (width / 2.)) ::
     Svg.Attributes.ry (string_of_floatint (height / 2.)) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let render_rectangle color w h x y angle s alpha = 
  Svg.rect
    (Svg.Attributes.width (string_of_floatint (w)) ::
     Svg.Attributes.height (string_of_floatint (h)) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_rect_transform w h x y angle s)::
     render_alpha alpha
    )
    []

let rec to_ngon_points i n radius str =
  if i == n 
  then str
  else
    let a = turns (float_of_int i / float_of_int n - 0.25) in
    let x = radius * cos a in
    let y = radius * sin a in
    to_ngon_points (Stdlib.(+) i 1) n radius
      (spf "%s%s,%s " str (string_of_floatint x) (string_of_floatint y))

let render_ngon color n radius x y angle s alpha = 
  Svg.polygon
    (Svg.Attributes.points (to_ngon_points 0 n radius "") ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle s)::
     render_alpha alpha
    )
    []

let (render_shape: shape -> 'msg Svg.t) = 
  fun { x; y; angle; scale; alpha; form} ->
  match form with
  | Circle (color, radius) -> 
     render_circle color radius x y angle scale alpha
  | Oval (color, width, height) ->
     render_oval color width height x y angle scale alpha
  | Rectangle (color, width, height) ->
     render_rectangle color width height x y angle scale alpha
  | Ngon (color, n, radius) ->
     render_ngon color n radius x y angle scale alpha


let (render: screen -> shape list -> 'msg Svg.t) = fun screen shapes ->
    let w = screen.width |> string_of_floatint in
    let h = screen.height |> string_of_floatint  in
    let x = screen.left |> string_of_floatint  in
    let y = screen.bottom |> string_of_floatint in

    Svg.svg
      [Svg.Attributes.viewBox (x ^ " " ^ y ^ " " ^ w ^ " " ^ h);
       Html.style "position" "fixed";
       Html.style "top" "0";
       Html.style "left" "0";
       Svg.Attributes.width "100%";
       Svg.Attributes.height "100%";
      ]
      (List.map render_shape shapes)

(*****************************************************************************)
(* Playground: picture *)
(*****************************************************************************)

type msg1 = 
  | Resized of int * int

let (picture: shape list -> (unit, screen, msg1) Platform.program) = 
 fun shapes ->
  let init () = 
      to_screen 600. 600., Cmd.none
  in
  let view screen = 
      { Browser.
        title = "Playground";
        body = [ render screen shapes ]
      }
  in
  let update msg  _model = 
    match msg with
    | Resized (width, height) ->
       to_screen (float_of_int width) (float_of_int height), Cmd.none
  in
  let _subscriptions _ =
      raise Todo
  in
  Browser.document { Browser. init; view; update }

(*****************************************************************************)
(* Playground: animation *)
(*****************************************************************************)

(* this is also used by the game playground *)
type msg =
  | Tick of Time.posix
  | Resized of int * int
  (* ... *)

  | KeyChanged of bool * string

  | MouseMove of float * float
  | MouseClick
  | MouseButton of bool (* true = down, false = up *)
  


type animation = Animation of Event.visibility * screen * time

let animation_update msg (Animation (v, s, t) as state) =
  match msg with
  | Tick posix -> 
    Animation (v, s, (Time posix))
  | Resized (w, h) -> 
    Animation (v, to_screen (float_of_int w) (float_of_int h), t)
  | MouseMove _ 
  | MouseClick 
  | MouseButton _
  | KeyChanged _
    -> state

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
     { Browser.
       title = "Playground";
       body = [render screen (view_frame time)];
     }
   in
   let update msg model = 
     animation_update msg model,
     Cmd.none
   in
  let _subscriptions _ =
      raise Todo
  in
  Browser.document { Browser. init; view; update }

(*****************************************************************************)
(* Playground: game *)
(*****************************************************************************)
type 'memory game = Game of Event.visibility * 'memory * computer

let (game_update: (computer -> 'memory -> 'memory) -> msg -> 'memory game ->
 'memory game) =
 fun _update_memory msg (Game (vis, memory, computer)) ->
    match msg with
    | Tick _time ->
        raise Todo
    | Resized (_w, _h) ->
        raise Todo
    | MouseMove (page_x, page_y) ->
        let x = computer.screen.left + page_x in
        let y = computer.screen.top - page_y in
        Game (vis, memory, 
             { computer with mouse = mouse_move x y computer.mouse })
    | MouseClick ->
        Game (vis, memory, 
             { computer with mouse = 
                (* Vdom does not provide OnMouseUp, so we use MouseClick *)
                 mouse_down false 
                   (mouse_click true computer.mouse) })
    | MouseButton is_down ->
        Game (vis, memory, 
             { computer with mouse = mouse_down is_down computer.mouse })
    | KeyChanged (_is_down, _key) ->
      raise Todo
        

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

      (* TODO: should use subscription instead *)
    let elt = Html.div [
        V.onmousemove (fun evt -> 
          MouseMove (evt.V.page_x, evt.V.page_y)
        );
        V.onclick (fun _evt -> 
          log "click"; 
          MouseClick
        );
        (* note that Vdom does not provide onmouseup, so we use onclick 
         * to set back mouse.mdown to false *)
        V.onmousedown (fun evt -> 
          log (spf "%d" evt.V.buttons);
          MouseButton (evt.V.buttons > 0)
        );
      ] [elt] 
    in
    { Browser.
      title = "Playground";
      body = [elt];
    }
  in
  let update msg model =
      game_update update_memory msg model,
      Cmd.none
  in
  let _subscriptions _ =
      raise Todo
  in
  Browser.document { Browser. init; view; update }

(*****************************************************************************)
(* run_app *)
(*****************************************************************************)

open Js_browser
let run_app app =
  let run () = 
    Vdom_blit.run app 
    |> Vdom_blit.dom 
    |> Element.append_child (Document.body document) in
  let () = Window.set_onload window run in
  ()
