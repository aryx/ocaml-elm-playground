open Common
module V = Vdom

module Core = struct
type number = float
let (/) = (/.)
let (+) = (+.)
let (-) = (-.)
let ( * ) = ( *. )
end
open Core

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
end

module Svg = struct
type 'msg t = 'msg V.vdom

let svg attrs xs = 
  V.svg_elt "svg" ~a:attrs xs

let trusted_node s attrs xs = 
  V.svg_elt s ~a:attrs xs

let circle =
  trusted_node "circle"

module Attributes = struct
let viewBox = V.attr "viewBox"

let width = V.attr "width"
let height = V.attr "height"

let r = V.attr "r"
let fill = V.attr "fill"
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

(* in Playground.elm *)

type color =
  | Hex of string
  | Rgb of int * int * int

let render_color color =
  match color with
  | Hex str -> str
  | Rgb (r,g,b) -> spf "rgb(%d,%d,%d)"  r g b

let lightYellow = Hex "#fce94f"

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

(* less: could use deriving constructor? *)
let shape x y angle scale alpha form =
  { x; y; angle; scale; alpha; form }

let (circle: color -> number -> shape) = fun color radius ->
  shape 0. 0. 0. 1. 1. (Circle (color, radius))

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
    
let string_of_floatint x = 
  x |> int_of_float |> string_of_int

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

let clamp low high number =
  if number < low then
    low
  else if number > high then
    high
  else
    number


let render_alpha alpha =
  if alpha = 1.
  then []
  else [Svg.Attributes.opacity (string_of_floatint (clamp 0. 1. alpha))]

let render_circle color radius x y angle scale alpha =
  Svg.circle 
    (Svg.Attributes.r (string_of_floatint radius) ::
     Svg.Attributes.fill (render_color color) ::
     Svg.Attributes.transform (render_transform x y angle scale)::
     render_alpha alpha
    )
    []

let (render_shape: shape -> 'msg Svg.t) = 
  fun { x; y; angle; scale; alpha; form} ->
  match form with
  | Circle (color, radius) -> 
     render_circle color radius x y angle scale alpha

let (render: screen -> shape list -> 'msg V.vdom) = fun screen shapes ->
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
(*
      [
      V.svg_elt "circle" []
            ~a:[
              V.int_attr "cx" (10);
              V.int_attr "cy" (10);
              V.int_attr "r" (10);
              V.attr "fill" ("green");
            ]
    ]
*)
let (picture: shape list -> (unit, screen, (int * int)) Platform.program) = 
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
  let update (width, height) _model = 
     to_screen (float_of_int width) (float_of_int height), Cmd.none
  in
  let _subscriptions _ =
      raise Todo
  in
  Browser.document { Browser. init; view; update }
