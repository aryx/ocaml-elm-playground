open Common
module V = Vdom

(* in Core.elm *)

type number = float
let (/) = (/.)
let (+) = (+.)
let (-) = (-.)
let ( * ) = ( *. )

module Platform = struct
type ('flags, 'model, 'msg) program =
  ('model, 'msg) V.app
end

module Cmd = struct
type 'msg t = 'msg V.Cmd.t
let none = V.Cmd.Batch []
end


module Browser = struct
type 'msg document = {
  title: string;
  body: 'msg V.vdom list;
}

type ('flags, 'model, 'msg) app = {
  init: 'flags -> ('model * 'msg V.Cmd.t);
  view: 'model -> 'msg document;
  update: 'msg -> 'model -> ('model * 'msg V.Cmd.t);
  (* subscriptions: 'model -> 'msg V.Sub?? *)
}

let (document: ('flags, 'model, 'msg) app -> 
               ((*'flags,*) 'model, 'msg) V.app) 
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

(* in ???.elm *)

type color = string

let hex s = s

(* in Playground.elm *)

let lightYellow = hex "fce94f"

type shape = {
    x: number; 
    y: number; 
    angle: number;
    scale: number;
    alpha: number;

    form: form
}
and form = 
  | Circle of color * number

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
    

let (render: screen -> shape list -> 'msg V.vdom) = fun screen _shapes ->
    let w = screen.width |> int_of_float |> string_of_int in
    let h = screen.height |> int_of_float |> string_of_int  in
    let x = screen.left |> int_of_float |> string_of_int  in
    let y = screen.bottom |> int_of_float |> string_of_int in

    V.svg_elt "svg"
      ~a:[V.attr "viewBox" (x ^ " " ^ y ^ " " ^ w ^ " " ^ h);
          V.attr "width" "100%";
          V.attr "height" "100%";
         ]
      [
      V.svg_elt "circle" []
            ~a:[
              V.int_attr "cx" (10);
              V.int_attr "cy" (10);
              V.int_attr "r" (10);
              V.attr "fill" ("green");
            ]
    ]

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
    
