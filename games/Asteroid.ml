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
      y = Basics.round (-. x' *. sin w +. y' *. cos w);
    }
*)

let (vector_length: vector -> number) = fun v ->
  sqrt (float v.x ** 2. +. float v.y ** 2.)

let (vector_add: vector -> vector -> vector) = fun v1 v2 ->
    {x = v1.x + v2.x; y = v1.y + v2.y }

(* orig: was calling point_rotate but simpler to do directly *)
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

type 'a obj = {
  (* current state *)
  pos: point;
  velocity:  vector;

  (* todo: resolved_shape at some point 
   * orig: was shape, but simpler to store the figure and compute
   * the resolved shape when we need to.
   *)
  figure: figure;

  (* only used for the ship *)
  orientation: number;

  xtra: 'a;
}


(* simpler to make mutable *)
type ship = {
  mutable thrust: number;
  mutable h_acceleration: number;
}
(* accelereration delta *)
let a_delta = 1.
(* turn delta *)
let h_delta = 0.3
(* Max velocity *)
let v_max = 20.

(* when drawn horizontally at 0 degrees *)
let space_ship c = 
  polygon c [(15., 0.); (-15., 10.); (-10., 0.); (-15., -10.); (15., 0.)]



type bullet = {
  cnt: int;
}
let v_bullet = 30.
(* number of tick to live *)
let bullet_TTL = 20

let space_bullet =
  circle red 2.

let new_bullet ship =
  { pos = ship.pos; velocity = polar v_bullet ship.orientation;
    orientation = 0.;
    figure = space_bullet;
    xtra = { cnt = 0 } 
  }

type asteroid = {
  size: asteroid_size;
}
  and asteroid_size = ALarge | AMedium | AWee

let v_asteroid = 5.

let random_range (low, high) =
  let diff = high -. low in
  let n = Random.float diff in
  n +. low

let space_asteroid () =
  let corners = random_range (4., 8.) in
  let increment_angle = Basics.pi2 /. corners in
  let rec aux angle  =
    if angle >= Basics.pi2
    then []
    else polar (random_range (30., 50.)) angle
         ::aux (angle +. increment_angle)
  in
  let pts = aux increment_angle |> List.map (fun pt -> 
        float pt.x, float pt.y
  ) in
  Common.pr2_gen pts;
  polygon (Color.Rgb (100, 100, 100)) pts

let new_asteroid screen =
  let pos = {
      x = int_of_float (random_range (screen.left, screen.right));
      y = int_of_float (random_range (screen.bottom, screen.top));
  } in
  let velocity = {
      x = int_of_float (random_range (-. v_asteroid, v_asteroid));
      y = int_of_float (random_range (-. v_asteroid, v_asteroid));
  } in
  { pos; velocity; orientation = 0.; 
    figure = space_asteroid ();
    xtra = { size = ALarge };
  }

type model = {
  ship: ship obj;
  bullets: bullet obj list;
  asteroids: asteroid obj list;
 
  last_tick: float;
}

(* 30 ms in original program *)
let tick = 0.030

let initial_model = {
  ship = {
    pos = { x = 0; y = 0 };
    velocity = { x = 0; y = 0};
    figure = space_ship blue;
    orientation = Basics.pi /. 2.;
    xtra = {
      thrust = 0.;
      h_acceleration = 0.;
    }
  };
  bullets = [];
  asteroids = [
    new_asteroid initial_computer.screen;
    new_asteroid initial_computer.screen;
    new_asteroid initial_computer.screen;
    new_asteroid initial_computer.screen;
    new_asteroid initial_computer.screen;
  ];

  last_tick = Unix.gettimeofday();
}

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* todo: resolved_shape_of_obj at some point *)
let (shape_of_obj: 'a obj -> shape) = 
 fun { figure; pos; orientation; _ } ->
   figure 
   |> rotate (Basics.radians_to_degrees orientation)
   |> move (float pos.x) (float pos.y)

(* todo: draw_resolved_shape? *)
let (draw_shape: shape -> shape) = fun shape ->
  shape

let (draw_ship: ship obj -> shape) = fun ship ->
  draw_shape (shape_of_obj ship)

let (draw_bullet: bullet obj -> shape) = fun x ->
  draw_shape (shape_of_obj x)

let (draw_asteroid: asteroid obj -> shape) = fun x ->
  draw_shape (shape_of_obj x)

let view model =
  draw_ship model.ship ::
  (List.map draw_bullet model.bullets) @
  (List.map draw_asteroid model.asteroids)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type msg = 
  | Tick of float

  | MoveLeft
  | MoveRight
  | StopMove

  | Accelerate of bool
 
  | Shoot

  | Noop

let msg_of_key_down = function
  | "ArrowLeft"  -> MoveLeft
  | "ArrowRight" -> MoveRight
  | "ArrowUp" -> Accelerate true
  | "space" -> Shoot
  | _ -> Noop

let msg_of_key_up = function
  | "ArrowLeft"  -> StopMove
  | "ArrowRight" -> StopMove
  | "ArrowUp" -> Accelerate false
  | "space" -> Noop
  | _ -> Noop


(* orig: could use modulo if the origin was not at the center on the screen *)
let add_modulo_window screen pos velocity =
  let { x; y } = vector_add pos velocity in
  let x = float x in let y = float y in
  (* there is probably something simpler than this code ... *)
  let x = int_of_float (
    match () with
    | _ when x > screen.right -> 
        x -. screen.right +. screen.left
    | _ when x < screen.left -> 
        screen.right -. (screen.left -. x)
    | _ -> x
   )
  in
  let y = int_of_float (
    match () with
    | _ when y > screen.top -> 
        y -. screen.top +. screen.bottom
    | _ when y < screen.bottom -> 
        screen.top -. (screen.bottom -. y)
    | _ -> y
   )
  in
  { x; y }

(* orig: this assumed to be called every tick of 30ms *)
let move_ship screen 
  ({ pos; velocity; orientation; xtra = { h_acceleration; thrust};_} as ship)= 
    let new_velocity = vector_add (polar thrust orientation) velocity in
    let l = vector_length new_velocity in

    { ship with
      pos = add_modulo_window screen pos velocity ;
      velocity = if l > v_max then failwith "Todo" else new_velocity;
      orientation = orientation +. h_acceleration;
    }

let move_bullet screen ({ pos; velocity; xtra = { cnt }; _ } as bullet) =
  { bullet with
    pos = add_modulo_window screen pos velocity;
    xtra = { cnt = cnt + 1 };
  }

let move_bullets screen xs =
  xs 
  |> List.map (move_bullet screen)
  |> List.filter (fun b -> b.xtra.cnt < bullet_TTL)

let move_asteroid screen ({ pos; velocity; _ } as asteroid) =
  { asteroid with pos = add_modulo_window screen pos velocity; }
 

let move_asteroids screen xs =
  xs 
  |> List.map (move_asteroid screen)

let update msg model =
 (match msg with
 | Noop -> model
 | Tick now ->

   let delta = now -. model.last_tick in

   if delta < tick
   then model
   else { ship = move_ship initial_computer.screen model.ship;
          bullets = move_bullets initial_computer.screen model.bullets;
          asteroids = move_asteroids initial_computer.screen model.asteroids;
          last_tick = now;
        }

  | Shoot ->
    let ship = model.ship in 
    { model with bullets = new_bullet ship::model.bullets }

  | MoveLeft -> 
     (* simpler when using mutable *)
     model.ship.xtra.h_acceleration <- 1. *. h_delta;
     model

  | MoveRight -> 
     model.ship.xtra.h_acceleration <- -1. *. h_delta;
     model

  | StopMove -> 
     model.ship.xtra.h_acceleration <- 0.;
     model
  | Accelerate b ->
     model.ship.xtra.thrust <- if b then a_delta else 0.;
     model
  ), Cmd.none

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = { Playground.
  view;
  update;
  init = (fun () -> (initial_model), Cmd.none);
    subscriptions  = (fun _ -> Sub.batch [
      Sub.on_animation_frame (fun x -> Tick x);
      Sub.on_key_down (fun key -> msg_of_key_down key);
      Sub.on_key_up (fun key -> msg_of_key_up key);
    ]);
  }

let main = 
  Playground_platform.run_app app
