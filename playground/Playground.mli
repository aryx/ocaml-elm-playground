(**
  See {!basics}
*)



(* It is more flexible to use float than int for graphical operations.
 * Consider using 'open Basics' to have the +/-/... operators working on 
 * floats.
 *)
type number = float

type time = Time of number

val spin : number -> time -> number
val wave : number -> number -> number -> time -> number
val zigzag : number -> number -> number -> time -> number

type color = Color.t (* Hex of string | Rgb of int * int * int *)

val rgb : int -> int -> int -> color

val white : color
val black : color
val red : color
val green : color
val blue : color
val yellow : color
val brown : color

val lightYellow : color
val lightPurple : color
val gray : color
val darkGray : color

type shape = {
  x : number;
  y : number;
  angle : number;
  scale : number;
  alpha : number;
  form : form;
}
and form =
    Circle of color * number
  | Oval of color * number * number
  | Rectangle of color * number * number
  | Ngon of color * int * number
  | Polygon of color * (number * number) list
  | Image of number * number * string
  | Words of color * string
  | Group of shape list

val shape : number -> number -> number -> number -> number -> form -> shape

val circle : color -> number -> shape
val oval : color -> number -> number -> shape

val rectangle : color -> number -> number -> shape
val square : color -> number -> shape

val triangle : color -> number -> shape
val pentagon : color -> number -> shape
val hexagon : color -> number -> shape
val octagon : color -> number -> shape

val polygon : color -> (number * number) list -> shape

val image : number -> number -> string -> shape
val words : color -> string -> shape

val group : shape list -> shape

val move : number -> number -> shape -> shape
val move_left : number -> shape -> shape
val move_right : number -> shape -> shape
val move_up : number -> shape -> shape
val move_down : number -> shape -> shape
val move_x : number -> shape -> shape
val move_y : number -> shape -> shape

val scale : number -> shape -> shape
val rotate : number -> shape -> shape
val fade : number -> shape -> shape

(* todo: group *)

val default_width : float
val default_height : float

type screen = {
  width : number;
  height : number;
  top : number;
  left : number;
  right : number;
  bottom : number;
}
val to_screen : number -> number -> screen

type mouse = { mx : number; my : number; mdown : bool; mclick : bool; }

type keyboard = {
  kup : bool;
  kdown : bool;
  kleft : bool;
  kright : bool;
  kw : bool;
  ks : bool;
  ka : bool;
  kd : bool;
  kspace : bool;
  kenter : bool;
  kshift : bool;
  kbackspace : bool;
  keys : string Set_.t;
}

val to_x : keyboard -> number
val to_y : keyboard -> number
val to_x2 : keyboard -> number
val to_y2 : keyboard -> number

val to_xy : keyboard -> number * number

type computer = {
  mouse : mouse;
  keyboard : keyboard;
  screen : screen;
  time : time;
}

val initial_computer : computer



type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> shape list;
  subscriptions : 'model -> 'msg Sub.t;
}

type msg1 = Resized1 of int * int

val picture : shape list -> (screen, msg1) app

type msg =
    Tick of number
  | Resized of int * int
  | KeyChanged of bool * string
  | MouseMove of (number * number)
  | MouseClick
  | MouseButton of bool

type animation

val animation : (time -> shape list) -> (animation, msg) app

type 'memory game

val game :
  (computer -> 'memory -> shape list) ->
  (computer -> 'memory -> 'memory) -> 'memory -> ('memory game, msg) app

(** {1:basics Basics}
*)

