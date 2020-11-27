open Playground

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

(* orig: pad: would be simpler to use float everywhere? *)
type point = {x: int; y: int }

(* a vector is represented as an arrow from the origin (0, 0) to point *)
type vector = point

(*
let (point_rotate: number -> point -> point) = fun w p ->
    let x' = float p.x in
    let y' = float p.y in
    { x = Basics.round (x' *. cos w +. y' *. sin w);
      (* orig: was - x' ... but the coordinate system of playground is
       * different than HGL; HGL assumes (0, 0) is the top left corner.
       *)
      y = Basics.round (x' *. sin w +. y' *. cos w);
    }
*)

let (vector_length: vector -> number) = fun v ->
  sqrt (float v.x ** 2. +. float v.y ** 2.)

let (vector_add: vector -> vector -> vector) = fun v1 v2 ->
    {x = v1.x + v2.x; y = v1.y + v2.y }

(* orig: was calling point_rotate but seemed wrong code *)
let (polar: number -> number -> vector) = fun r phi ->
    { x = Basics.round (r *. cos phi); y = Basics.round (r *. sin phi) }


(* orig: we can reuse Playground.shape *)
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

let space_ship c = 
  polygon c [(15., 0.); (-15., 10.); (-10., 0.); (-15., -10.); (15., 0.)]


(* accelereration delta *)
let a_delta = 1.
(* turn delta *)
let h_delta = 0.3

(* 30 ms in original program *)
let tick = 0.030

(* Max velocity *)
let v_max = 20.


let initial_model = {
  ship = {
    pos = { x = 0; y = 0 };
    velocity = { x = 0; y = 0};
    orientation = Basics.pi /. 2.;

    thrust = 0.;
    h_acceleration = 0.;

    figure = space_ship blue;
  }
}

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* todo: resolved_shape_of_obj at some point *)
let (shape_of_obj: obj -> shape) = 
 fun { figure; pos; orientation; _ } ->
   figure 
   |> rotate (Basics.radians_to_degrees orientation)
   |> move (float pos.x) (float pos.y)

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
  Common.pr2_gen (pos, velocity);
  vector_add pos velocity

(* orig: this assumed to be called every tick of 30ms *)
let move_ship screen 
  ({ pos; velocity; h_acceleration; thrust; orientation; _} as ship)  = 
    let new_velocity = vector_add (polar thrust orientation) velocity in
    let l = vector_length new_velocity in

    { ship with
      pos = add_modulo_window screen pos velocity ;
      velocity = if l > v_max then failwith "Todo" else new_velocity;
      orientation = orientation +. h_acceleration;
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
    (* rotate is counter clock-wise => left means adding degrees *)
    h_accelerate = - (int_of_float (to_x kbd));
  }

let update computer (model, last_tick) =
  let input = input_of_computer computer in
  let (Time now) = computer.time in
  let delta = now -. last_tick in

  let ship = model.ship in
  let ship = { ship with 
      h_acceleration = (float input.h_accelerate) *. h_delta; 
      thrust = if input.up then a_delta else 0.;
     }
  in
  if delta < tick
  then { ship }, last_tick
  else { ship = move_ship computer.screen ship}, now


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = 
  game view update (initial_model, Unix.gettimeofday())

let main = 
  Playground_platform.run_app app
