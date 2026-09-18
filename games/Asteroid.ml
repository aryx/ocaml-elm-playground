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
 * claude: two physics engines, chosen with the flag physics=engine
 * (?physics=engine in a browser; see Playground.flags):
 *
 *  - the dumb engine, by default, the original code: every 30 ms of
 *    wall-clock time, each object's velocity (pixels per 30 ms) is added
 *    to its position, the thrust to the ship's velocity, and a ship
 *    faster than v_max is a [failwith "Todo"];
 *  - the physics engine (playground/Physics.mli): the same objects, at
 *    every frame (1/60 s: a fixed time step), in pixels and seconds, with
 *    the same numbers converted (see "The physics engine" below), and
 *    two things the dumb engine didn't have: drag (Physics.slow), which
 *    gives the ship a top speed by itself, v_max, instead of a crash;
 *    and bullets that keep the ship's own velocity (Physics.shot_from),
 *    as they would in space.
 *
 * Everything else, the objects, the collisions, the view, the keys, is
 * the same code for both.
 *
 * TODO:
 *  - see Elm clones of asteroids:
 *)

(*****************************************************************************)
(* Geometry *)
(*****************************************************************************)

(* todo? put that in a separate Geometry.ml library? with better types
 * less: advanced geometry types:
 *   https://www.cs.cornell.edu/~asampson/media/papers/gator-oopsla2020-preprint.pdf
 *)

(* orig: pad: would be simpler to use float everywhere?
 * claude: yes, and needed by the physics engine, whose velocities change
 * by less than a pixel per step (integers would round them away) *)
type point = {x: number; y: number }

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

let (vector: point -> point -> vector) = fun p1 p2 ->
    { x = p2.x -. p1.x; y = p2.y -. p1.y }

let (vector_length: vector -> number) = fun v ->
  sqrt (v.x ** 2. +. v.y ** 2.)

let (vector_add: vector -> vector -> vector) = fun v1 v2 ->
    {x = v1.x +. v2.x; y = v1.y +. v2.y }

(* orig: was calling point_rotate but simpler to do directly *)
let (polar: number -> number -> vector) = fun r phi ->
    { x = r *. cos phi; y = r *. sin phi }


(* orig: we can reuse Playground.shape *)
type figure = Playground.shape

(* Final "resolved" coordinates of a figure after translation/rotate.
 * Used for collision detection.
*)
type resolved_shape = 
(* TODO  | Poly of point list *)
  | Circle of point * number (* radius *)

let (intersect: resolved_shape -> resolved_shape -> bool) = fun x1 x2 ->
  match x1, x2 with
  | Circle (c1, r1), Circle (c2, r2) ->
     let v = vector c1 c2 in
     vector_length v <= r1 +. r2

let (contains: resolved_shape -> point -> bool) = fun x p ->
  match x with
  | Circle (c, r) -> 
     let v = vector c p in
     vector_length v <= r

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
        pt.x, pt.y
  ) in
  (* Common.pr2_gen pts; *)
  polygon (Color.Rgb (100, 100, 100)) pts

let new_asteroid screen =
  let pos = {
      x = random_range (screen.left, screen.right);
      y = random_range (screen.bottom, screen.top);
  } in
  let velocity = {
      x = random_range (-. v_asteroid, v_asteroid);
      y = random_range (-. v_asteroid, v_asteroid);
  } in
  { pos; velocity; orientation = 0.; 
    figure = space_asteroid ();
    xtra = { size = ALarge };
  }

type state = Play | Stop

(* the two physics engines, see the prelude *)
type engine = Dumb | Physics_engine

type model = {
  engine: engine;
  ship: ship obj;
  bullets: bullet obj list;
  asteroids: asteroid obj list;
 
  state: state;
  last_tick: float;
}

(* 30 ms in original program *)
let tick = 0.030

let initial_model = {
  engine = Dumb;
  ship = {
    pos = { x = 0.; y = 0. };
    velocity = { x = 0.; y = 0.};
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
  state = Play;
  last_tick = Unix.gettimeofday();
}

(*****************************************************************************)
(* Collision detection *)
(*****************************************************************************)

let (resolved_shape_of_obj: 'a obj -> resolved_shape) =
  fun { figure = _; pos; orientation = _; _ } ->
  (* TODO *)
  Circle (pos, 10.)

let ship_crashed model =
  let ship = model.ship in
  let shp1 = resolved_shape_of_obj ship in
  let shapes = model.asteroids |> List.map resolved_shape_of_obj in
  shapes |> List.exists (fun shp2 -> intersect shp1 shp2)

let directions v =
  let n = 1 + Random.int 3 in
  let rec aux n =
    if n = 0
    then []
    else
      { x = v.x +. random_range (-. v.x, v.x);
        y = v.y +. random_range (-. v.y, v.y);
      }::aux (n - 1)
  in
  aux n

let explode a dirs =
  match a.xtra.size with
  | ALarge -> 
    dirs |> List.map (fun velocity -> 
       { a with figure = a.figure |> scale 0.5; velocity;
         xtra = { size = AMedium }
          })
  | AMedium ->
    dirs |> List.map (fun velocity -> 
       { a with figure = a.figure |> scale 0.75; velocity;
         xtra = { size = AWee }})
  | AWee -> []


let (check_asteroids: model -> asteroid obj list) = 
 fun model ->
  let bullets = model.bullets |> List.map (fun x -> x.pos) in
  model.asteroids |> List.map (fun a ->
     let shp = resolved_shape_of_obj a in
     if bullets |> List.exists (fun p -> contains shp p)
     then 
       let dirs = directions a.velocity in
       explode a dirs
     else [a]
  ) |> List.flatten
   

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* todo: resolved_shape_of_obj at some point *)
let (shape_of_obj: 'a obj -> shape) = 
 fun { figure; pos; orientation; _ } ->
   figure 
   |> rotate (Basics.radians_to_degrees orientation)
   |> move pos.x pos.y

let view model =
  shape_of_obj model.ship ::
  (List.map shape_of_obj model.bullets) @
  (List.map shape_of_obj model.asteroids)

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
  (* there is probably something simpler than this code ... *)
  let x = (
    match () with
    | _ when x > screen.right -> 
        x -. screen.right +. screen.left
    | _ when x < screen.left -> 
        screen.right -. (screen.left -. x)
    | _ -> x
   )
  in
  let y = (
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

(*****************************************************************************)
(* The physics engine *)
(*****************************************************************************)
(* claude: the same objects, moved by playground/Physics at every frame
 * (1/60 s) instead of every 30 ms. The model's velocities stay in
 * pixels per 30 ms tick, the dumb engine's unit (the explosions reuse
 * them); Physics wants pixels per second: divide by [tick], and back.
 * The dumb engine's numbers, converted:
 *
 *   bullets     v_bullet = 30 px/tick         1000 px/s
 *   thrust      a_delta = 1 px/tick per tick  1111 px/s^2
 *   turning     h_delta = 0.3 rad/tick        573 degrees/s
 *   top speed   v_max = 20 px/tick            667 px/s, from drag: a
 *               push against the motion, c times the velocity, balances
 *               the thrust at thrust / c, so c = 1111 / 667 = 1.67
 *   bullets     bullet_TTL = 20 ticks         0.6 s, 36 frames *)

let per_second v = v /. tick
let per_tick v = v *. tick
let drag = a_delta /. (v_max *. tick)
let bullet_frames = int_of_float (float bullet_TTL *. tick *. 60.)

let body_of (o : 'a obj) : Physics.body =
  Physics.body o.figure
  |> Physics.at o.pos.x o.pos.y
  |> Physics.moving (per_second o.velocity.x) (per_second o.velocity.y)
  |> Physics.pointing (Basics.radians_to_degrees o.orientation)

(* [o] where the body is, as fast, turned the same way *)
let with_body (o : 'a obj) (b : Physics.body) : 'a obj =
  { o with
    pos = { x = b.x; y = b.y };
    velocity = { x = per_tick b.vx; y = per_tick b.vy };
    orientation = Basics.degrees_to_radians b.angle;
  }

let physics_ship screen (ship : ship obj) : ship obj =
  body_of ship
  |> Physics.turn (Basics.radians_to_degrees (per_second ship.xtra.h_acceleration))
  |> Physics.thrust (per_second (per_second ship.xtra.thrust))
  |> Physics.slow drag
  |> Physics.step
  |> Physics.wrap screen
  |> with_body ship

(* bullets and asteroids: no push, they just go on, around the screen *)
let physics_drift screen (o : 'a obj) : 'a obj =
  body_of o |> Physics.step |> Physics.wrap screen |> with_body o

let physics_bullets screen (bullets : bullet obj list) : bullet obj list =
  bullets
  |> List.map (fun b -> { (physics_drift screen b) with xtra = { cnt = b.xtra.cnt + 1 } })
  |> List.filter (fun b -> b.xtra.cnt < bullet_frames)

(* from the ship's nose, keeping the ship's own velocity *)
let physics_bullet (ship : ship obj) : bullet obj =
  with_body (new_bullet ship)
    (Physics.body space_bullet |> Physics.shot_from (per_second v_bullet) 0. (body_of ship))

(*****************************************************************************)
(* The update, with either engine *)
(*****************************************************************************)

(* the same rules for both engines: bullets break asteroids, an
 * asteroid touching the ship ends the game *)
let collide model =
  let asteroids = check_asteroids model in
  let state =
    if ship_crashed model
    then Stop
    else Play
  in
  { model with state; asteroids }

let update msg model =
 (match msg with
 | Noop -> model
 | Tick now ->
   (match model.engine with
   | Dumb ->
     let delta = now -. model.last_tick in

     if delta < tick || model.state = Stop
     then model
     else 
      let model = { model with
            ship = move_ship initial_computer.screen model.ship;
            bullets = move_bullets initial_computer.screen model.bullets;
            asteroids = move_asteroids initial_computer.screen model.asteroids;
            last_tick = now;
          } in
       collide model
   (* claude: one step per frame, whatever the time: a fixed step *)
   | Physics_engine ->
     if model.state = Stop
     then model
     else
      let screen = initial_computer.screen in
      collide { model with
        ship = physics_ship screen model.ship;
        bullets = physics_bullets screen model.bullets;
        asteroids = List.map (physics_drift screen) model.asteroids;
        last_tick = now;
      })

  | Shoot ->
    let ship = model.ship in 
    let bullet = match model.engine with Dumb -> new_bullet ship | Physics_engine -> physics_bullet ship in
    { model with bullets = bullet::model.bullets }

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
  (* claude: physics=engine chooses the physics engine, see the prelude *)
  init = (fun flags ->
    let engine = match List.assoc_opt "physics" flags with Some "engine" -> Physics_engine | _ -> Dumb in
    { initial_model with engine }, Cmd.none);
    subscriptions  = (fun _ -> Sub.batch [
      Sub.on_animation_frame (fun x -> Tick x);
      Sub.on_key_down (fun key -> msg_of_key_down key);
      Sub.on_key_up (fun key -> msg_of_key_up key);
    ]);
  }

let main = 
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
