open Common

type number = float

type color = string

let hex s = s

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



(* in Platform.elm *)
type ('flags, 'model, 'msg) program =
  Program


let (picture: shape list -> (unit, screen, (int * int)) program) = 
 fun _shapes ->
  raise Todo