open Playground
open Basics

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of the Asteroid clone described at
 * http://www.informatik.uni-bremen.de/~clueth/haskell-in-space/
 * but using OCaml instead of Haskell, and using Playground instead of the 
 * Hugs Graphics Library (HGL).
 * 
 * See https://en.wikipedia.org/wiki/Asteroids_(video_game) for more 
 * information on Asteroids, or its ancestor 
 * Spacewar https://en.wikipedia.org/wiki/Spacewar!
 *
 * 
 * TODO:
 *  - see Elm clones of asteroids:
 *)

(*****************************************************************************)
(* Geometry *)
(*****************************************************************************)

type point = {x: int; y: int }
(* a vector is represented as an arrow from the origin (0, 0) to point *)
type vector = point

let (vector_length: vector -> number) = fun _v ->
    failwith "TODO"

let (vector_add: vector -> vector -> vector) = fun _v1 _v2 ->
    failwith "TODO"

let (polar: number -> number -> vector) = fun _len _angle ->
    failwith "TODO"

type figure = Playground.shape

(* Final "resolved" coordinates of a figure after translation/scale/rotate.
 * Use for collision detection.
*)
type resolved_shape = 
  | Poly of point list
  | Circle of point * number (* radius *)

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type obj = {
  (* current state *)
  pos: point;
  velocity:  vector;
  orientation: number;

  (* todo: resolved_shape at some point 
   * orig: was shape, but simpler to store the figure and compute
   * the resolved shape when we need to.
   *)
  figure: figure;

  (* input *)
  thrust: number;
  h_acceleration: number;
}

type ship = obj

type model = {
  ship: ship;
}

let space_ship = 
  rectangle blue 15. 15.

let a_delta = 1.
let h_delta = 0.3

(* 30 ms in original program *)
let tick = 0.030

let v_max = 20.


let initial_model = {
  ship = {
    pos = { x = 0; y = 0 };
    velocity = { x = 0; y = 0};
    orientation = pi /. 2.;

    thrust = 0.;
    h_acceleration = 0.;

    figure = space_ship;
  }
}

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* todo: resolved_shape_of_obj at some point *)
let (shape_of_obj: obj -> shape) = 
 fun { figure=_; pos=_; orientation=_; _ } ->
   failwith "Todo"

(* todo: draw_resolved_shape? *)
let (draw_shape: shape -> shape) = fun shape ->
  shape

let (draw_ship: ship -> shape) = fun ship ->
  draw_shape (shape_of_obj ship)

let view _computer (model, _last_tick) =
  [draw_ship model.ship]

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let add_modulo_window _screenTODO pos velocity =
  vector_add pos velocity

(* orig: this assumed to be called every tick of 30ms *)
let move_ship screen delta 
  ({ pos; velocity; h_acceleration; thrust; orientation; _} as ship)  = 
  if delta < tick
  then ship
  else
    let new_velocity = vector_add (polar thrust orientation) velocity in
    let l = vector_length new_velocity in

    { ship with
      pos = add_modulo_window screen pos velocity ;
      velocity = if l > v_max then failwith "Todo" else new_velocity;
      orientation = orientation + h_acceleration;
    }
  

type input = {
  up: bool;
  (* -1, 0, 1 *)
  h_accelerate: int;
  (* todo: space *)
}

let input_of_computer computer = 
  let kbd = computer.keyboard in
  { up = kbd.kup;
    h_accelerate = int_of_float (to_x kbd);
  }

let update computer (model, last_tick) =
  let input = input_of_computer computer in
  let (Time now) = computer.time in
  let delta = now - last_tick in

  let ship = model.ship in
  let ship = { ship with 
      h_acceleration = (float input.h_accelerate) *. h_delta; 
      thrust = if input.up then a_delta else 0.;
     }
  in
  { ship = move_ship computer.screen delta ship}, now


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  game view update (initial_model, Unix.gettimeofday())

let main = 
  Playground_platform.run_app app
