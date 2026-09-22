(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyElite.ml again, with solid ships drawn by playground3d:
 * the same Cobra, the same Coriolis turning in front of Lave, the same
 * three Sidewinders, the same keys (up/down to pitch, left/right to
 * roll, w/s for speed, space for the laser). The rules are the 2D
 * game's, copied (see the coupling: comment): the two games' golden
 * frames are the same flight.
 *
 * What the engine takes over, and what it doesn't:
 *
 *  - The hidden lines. TinyElite draws an edge when either of its two
 *    faces is turned towards you (Elite's LL9); here each face is given
 *    to playground3d as a polygon wound counterclockwise from outside
 *    ([solid]), and the engine's backface culling drops the ones turned
 *    away -- graphics/3d's Cull, the same dot product. Run the software
 *    backend with -debug-keys and press "f" for wireframe: with culling
 *    on, that is Elite's picture, but for a diagonal across each square
 *    face (the engine draws triangles). Filled, the culling changes nothing
 *    you can see (Cull.mli says why): the z-buffer would have hidden
 *    the back faces anyway.
 *
 *  - Ships in front of ships. The one thing LL9 never did, and the
 *    z-buffer does for free: a Sidewinder crossing the station hides
 *    the part of it behind, where in TinyElite they show through each
 *    other.
 *
 *  - The camera: none of it. Elite's trick of turning the universe
 *    round a player who never moves is kept, since it is the game's
 *    rules and not its drawing, so the camera sits at the origin for
 *    ever, looking ahead. An engine does not mind: a fixed camera and
 *    a turning world, or a turning camera and a fixed world, are the
 *    same picture. It even pays: the backdrop of space is one black
 *    quad, far away, that never has to follow anything.
 *
 *  - One mirror. Elite's frame has x to the right, y up and z ahead,
 *    which is left-handed, like most screens; playground3d's is
 *    right-handed, looking down -z. So a point goes to the engine with
 *    its z turned round ([to_engine]), and the faces' winding with it.
 *
 * And the planet is a sphere now, where Elite and TinyElite could
 * afford a circle.
 *
 * Uses: Playground3d (its camera, polygon3d, sphere), Scene2d for the
 * scenes; the HUD is 2D shapes, the scanner and the gauges TinyElite's.
 *
 * Exercises: the sun, a light the ships are lit by (Lighting.mli), and
 * the planet's night side; the laser as a real beam, a long thin box
 * the z-buffer can hide behind a ship; explosions, the hull's faces
 * flying apart (each one a polygon already).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The flight: TinyElite.ml's rules *)
(*****************************************************************************)

(* coupling: from here to the end of [update] (the ships' shapes, the
 * orientations and their small turns, the model and the update), a
 * copy of TinyElite.ml's, so that the same keys fly the same
 * flight. Only the hulls' faces are kept, not their edges: the engine
 * wants faces. *)

(*****************************************************************************)
(* Vectors *)
(*****************************************************************************)

type vec = { x : number; y : number; z : number }

let v (x : number) (y : number) (z : number) : vec = { x; y; z }
let add (a : vec) (b : vec) : vec = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }
let sub (a : vec) (b : vec) : vec = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }
let times (k : number) (a : vec) : vec = { x = k *. a.x; y = k *. a.y; z = k *. a.z }
let dot (a : vec) (b : vec) : number = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

let cross (a : vec) (b : vec) : vec =
  { x = (a.y *. b.z) -. (a.z *. b.y); y = (a.z *. b.x) -. (a.x *. b.z); z = (a.x *. b.y) -. (a.y *. b.x) }

let length (a : vec) : number = Float.sqrt (dot a a)
let normalize (a : vec) : vec = let l = length a in if l = 0. then a else times (1. /. l) a

(*****************************************************************************)
(* Orientation, the 6502's small turns and TIDY *)
(*****************************************************************************)

(* nose, roof and side: where the ship points, where its canopy points,
 * and its right. Three rows of a rotation matrix, carried with it. *)
type orientation = { nose : vec; roof : vec; side : vec }

let upright : orientation = { nose = v 0. 0. 1.; roof = v 0. 1. 0.; side = v 1. 0. 0. }

(* Turning [a] towards [b] by a small angle [t] (radians), the 6502's
 * way: a' = a (1 - t^2/2) + b t, and b' = b (1 - t^2/2) - a t. No cosine
 * and no sine; wrong by about t^3/6, which is nothing for one frame and
 * something after a thousand. *)
let small_turn (t : number) (a : vec) (b : vec) : vec * vec =
  let c = 1. -. (t *. t /. 2.) in
  (add (times c a) (times t b), sub (times c b) (times t a))

(* Straightening the three back to square, Elite's TIDY: the nose made
 * a unit vector, the roof made square to it (the part of it along the
 * nose taken out) and a unit vector, and the side made from the two.
 * Gram and Schmidt, 1907 and 1883. *)
let tidy (o : orientation) : orientation =
  let nose = normalize o.nose in
  let roof = normalize (sub o.roof (times (dot o.roof nose) nose)) in
  { nose; roof; side = cross roof nose }

(* A ship turning itself: pitch turns the nose towards the roof, roll
 * turns the side towards the roof. *)
let pitch_by (t : number) (o : orientation) : orientation =
  let nose, roof = small_turn t o.nose o.roof in
  { o with nose; roof }

let roll_by (t : number) (o : orientation) : orientation =
  let side, roof = small_turn t o.side o.roof in
  { o with side; roof }

(* the same small turn, on two coordinates rather than two vectors *)
let small_turn2 (t : number) (a : number) (b : number) : number * number =
  let c = 1. -. (t *. t /. 2.) in
  ((c *. a) +. (t *. b), (c *. b) -. (t *. a))

(* The universe turning round you: a point (or a vector) in your frame,
 * turned by your pitch and roll *the other way* -- nose up, and what
 * is ahead goes down; roll right, and what is right goes up. *)
let turn_world (pitch : number) (roll : number) (p : vec) : vec =
  let x, y = small_turn2 roll p.x p.y in
  let y, z = small_turn2 pitch y p.z in
  { x; y; z }

(*****************************************************************************)
(* The ships *)
(*****************************************************************************)

(* a hull: its corners, and its faces as corners in order round each one *)
type hull = { corners : vec array; faces : int list list }

let hull (corners : vec list) (faces : int list list) : hull = { corners = Array.of_list corners; faces }

(* where a corner of a ship is, in your frame: its own coordinates
 * along the ship's side, roof and nose, from where the ship is *)
let place (pos : vec) (o : orientation) (c : vec) : vec =
  add pos (add (times c.x o.side) (add (times c.y o.roof) (times c.z o.nose)))

(* The Coriolis station: a cuboctahedron, the twelve middles of a cube's
 * edges, which is what the real one is -- six squares and eight
 * triangles. The docking slot is in the square its nose points out of. *)
let coriolis_size = 160.

let coriolis : hull =
  let s = coriolis_size in
  let corners =
    [ v s s 0.; v s (-.s) 0.; v (-.s) s 0.; v (-.s) (-.s) 0.;
      v s 0. s; v s 0. (-.s); v (-.s) 0. s; v (-.s) 0. (-.s);
      v 0. s s; v 0. s (-.s); v 0. (-.s) s; v 0. (-.s) (-.s) ]
  in
  let faces =
    [ (* the six squares *)
      [ 0; 4; 1; 5 ]; [ 2; 7; 3; 6 ]; [ 0; 9; 2; 8 ]; [ 1; 10; 3; 11 ]; [ 4; 8; 6; 10 ]; [ 5; 11; 7; 9 ];
      (* the eight triangles *)
      [ 0; 8; 4 ]; [ 0; 5; 9 ]; [ 1; 4; 10 ]; [ 1; 11; 5 ];
      [ 2; 6; 8 ]; [ 2; 9; 7 ]; [ 3; 10; 6 ]; [ 3; 7; 11 ] ]
  in
  hull corners faces

(* a Sidewinder, the pirates' little fighter: a flat arrowhead, a
 * pyramid on a diamond *)
let sidewinder : hull =
  hull
    [ v 0. 0. 34.; v (-36.) 0. (-18.); v 0. 10. (-18.); v 36. 0. (-18.); v 0. (-10.) (-18.) ]
    [ [ 0; 1; 2 ]; [ 0; 2; 3 ]; [ 0; 3; 4 ]; [ 0; 4; 1 ]; [ 1; 4; 3; 2 ] ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* anything out there: where it is and which way it points, in *your*
 * frame, since that is the only frame there is *)
type body = { pos : vec; o : orientation; speed : number }

type pirate = { ship : body; alive : bool; cooldown : int }

type flight = {
  planet : vec; (* the planet's middle: a body too, carried round like the rest *)
  station : body;
  pirates : pirate list;
  speed : number;
  shield : int;
  energy : int;
  laser : int; (* frames the beam shows *)
  kills : int;
  frames : int;
  hit : int;
}

type scene = Title | Flying of flight | Docked of flight | Destroyed of flight
type model = scene Scene2d.t

(* the station ahead, turned a little, its slot facing back at you; a
 * few pirates between *)
let new_flight () : flight =
  let facing_you = { nose = v 0. 0. (-1.); roof = v 0. 1. 0.; side = v (-1.) 0. 0. } in
  let pirate x y z = { ship = { pos = v x y z; o = facing_you; speed = 3.2 }; alive = true; cooldown = 90 } in
  (* the pirates far enough out that you see them coming before they
   * can shoot: a first version started them at 1200 and the shield was
   * a third gone before the player had touched a key *)
  { planet = v (-4000.) 2200. 40000.;
    station = { pos = v 120. (-60.) 3400.; o = tidy (roll_by 0.3 facing_you); speed = 0. };
    pirates = [ pirate (-420.) 80. 2200.; pirate 380. (-40.) 2600.; pirate 40. 220. 3000. ];
    speed = 6.;
    shield = 100;
    energy = 100;
    laser = 0;
    kills = 0;
    frames = 0;
    hit = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let pitch_rate = 0.022
let roll_rate = 0.035

(* a body carried round by your turn and slid back by your speed:
 * position and orientation both, since they are all in your frame *)
let carry (pitch : number) (roll : number) (speed : number) (b : body) : body =
  let pos = turn_world pitch roll b.pos in
  let o = { nose = turn_world pitch roll b.o.nose; roof = turn_world pitch roll b.o.roof; side = turn_world pitch roll b.o.side } in
  { b with pos = { pos with z = pos.z -. speed }; o }

(* A pirate turns its nose towards you by the same small turns, then
 * flies along it: the homing is nothing more than its orientation
 * nudged towards the line to the origin, and straightened. *)
let steer (p : pirate) : pirate =
  let b = p.ship in
  let towards = normalize (times (-1.) b.pos) in
  let nose = normalize (add b.o.nose (times 0.03 towards)) in
  let o = tidy { b.o with nose } in
  { p with ship = { b with o; pos = add b.pos (times b.speed o.nose) } }

(* the station turns slowly about its own nose, as the real one did: to
 * dock, you match its roll *)
let spin (b : body) : body = { b with o = roll_by 0.012 b.o }

(* You are docking when you are at the slot -- the middle of the square
 * the station's nose points out of -- slowly, pointing into it, and
 * rolled to match it: the slot is a letterbox, and it turns. *)
let docking (f : flight) : bool =
  let slot = add f.station.pos (times coriolis_size f.station.o.nose) in
  let into = dot (v 0. 0. 1.) (times (-1.) f.station.o.nose) in
  (* the slot's roof, seen from you: square to your view, it must be
   * near your own up *)
  let roof = f.station.o.roof in
  let tilt = Float.abs (atan2 roof.x roof.y) in
  length slot < 70. && into > 0.9 && tilt < 0.4 && f.speed < 8.

let crashed (f : flight) : bool = length f.station.pos < coriolis_size *. 1.05 && not (docking f)

let axis (a : bool) (b : bool) : number = (if a then 1. else 0.) -. if b then 1. else 0.

let step_flight (keys : keyboard) (fire : bool) (f : flight) : flight =
  let pitch = axis keys.kup keys.kdown *. pitch_rate and roll = axis keys.kleft keys.kright *. roll_rate in
  let speed = Basics.clamp 0. 14. (f.speed +. (axis (Set_.mem "w" keys.keys) (Set_.mem "s" keys.keys) *. 0.15)) in
  let planet = let p = turn_world pitch roll f.planet in { p with z = p.z -. speed } in
  let station = spin (carry pitch roll speed f.station) in
  let pirates = List.map (fun p -> if p.alive then steer { p with ship = carry pitch roll speed p.ship } else p) f.pirates in
  (* every sixteen frames, everything straightened, as Elite did: not
   * every frame, since TIDY cost a 6502 dearly *)
  let straighten (b : body) = if f.frames mod 16 = 0 then { b with o = tidy b.o } else b in
  let station = straighten station in
  let pirates = List.map (fun p -> { p with ship = straighten p.ship }) pirates in
  (* the laser: straight ahead, and it hits whatever sits on the
   * crosshair, near enough *)
  let firing = fire && f.energy > 4 in
  let pirates, killed =
    List.fold_right
      (fun (p : pirate) (acc, n) ->
        let on_sight = p.ship.pos.z > 0. && Float.hypot p.ship.pos.x p.ship.pos.y < 40. +. (p.ship.pos.z *. 0.02) in
        if firing && p.alive && on_sight && p.ship.pos.z < 2400. then ({ p with alive = false } :: acc, n + 1)
        else (p :: acc, n))
      pirates ([], 0)
  in
  (* a pirate with its nose on you, near enough, fires back *)
  let shots = ref 0 in
  let pirates =
    List.map
      (fun (p : pirate) ->
        if not p.alive then p
        else
          let aimed = dot p.ship.o.nose (normalize (times (-1.) p.ship.pos)) > 0.97 in
          if aimed && p.cooldown = 0 && length p.ship.pos < 700. then begin
            incr shots;
            { p with cooldown = 110 }
          end
          else { p with cooldown = max 0 (p.cooldown - 1) })
      pirates
  in
  let hit_now = !shots > 0 in
  { planet;
    station;
    pirates;
    speed;
    energy = min 100 (if firing then f.energy - 4 else f.energy + 1);
    laser = (if firing then 6 else max 0 (f.laser - 1));
    kills = f.kills + killed;
    shield = f.shield - (!shots * 9);
    frames = f.frames + 1;
    hit = (if hit_now then 20 else max 0 (f.hit - 1)) }

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  match m.scene with
  | Title -> if space then Scene2d.go (Flying (new_flight ())) m else m
  | Flying f ->
      let f = step_flight computer.keyboard computer.keyboard.kspace f in
      if docking f then Scene2d.go (Docked f) m
      else if crashed f || f.shield <= 0 then Scene2d.go (Destroyed f) m
      else { m with scene = Flying f }
  | Docked _ | Destroyed _ -> if space then Scene2d.go Title m else m


(*****************************************************************************)
(* View: faces, and a camera that never moves *)
(*****************************************************************************)

(* Elite's frame is left-handed (x right, y up, z ahead); the engine's
 * right-handed, looking down -z. One mirror between them. *)
let to_engine (p : vec) : number * number * number = (p.x, p.y, -.p.z)

(* A face as the engine wants it: counterclockwise seen from outside,
 * so that its normal points out of the hull and Cull drops it when it
 * is turned away. The hull's lists are in no particular order, so each
 * is checked, in the engine's frame (after the mirror): its normal
 * must point away from the ship's middle, or the list is reversed. *)
let wound (middle : vec) (pts : vec list) : (number * number * number) list =
  let pts = List.map to_engine pts and mx, my, mz = to_engine middle in
  match pts with
  | (ax, ay, az) :: (bx, by, bz) :: (cx, cy, cz) :: _ ->
      let ux, uy, uz = (bx -. ax, by -. ay, bz -. az) and vx, vy, vz = (cx -. ax, cy -. ay, cz -. az) in
      let nx, ny, nz = ((uy *. vz) -. (uz *. vy), (uz *. vx) -. (ux *. vz), (ux *. vy) -. (uy *. vx)) in
      let out = (nx *. (ax -. mx)) +. (ny *. (ay -. my)) +. (nz *. (az -. mz)) in
      if out >= 0. then pts else List.rev pts
  | _ -> pts

let solid (color : color) (h : hull) (pos : vec) (o : orientation) : shape3d =
  group3d
    (List.map (fun face -> polygon3d color (wound pos (List.map (fun i -> place pos o h.corners.(i)) face))) h.faces)

(* the eye: at the origin, looking ahead, for ever. Its view as wide as
 * TinyElite's divide by 720 on a 1000 high screen *)
let eye : camera =
  camera ~eye:(0., 0., 0.) ~target:(0., 0., -1.) ~fov:(2. *. atan (500. /. 720.) *. 180. /. Float.pi) ~near:2.
    ~far:60000. ()

(* space: a black quad beyond the planet, facing the eye, big enough to
 * fill the view. The camera never turns, so it never has to move *)
let space : shape3d =
  let d = 55000. and s = 100000. in
  polygon3d black [ (-.s, -.s, -.d); (s, -.s, -.d); (s, s, -.d); (-.s, s, -.d) ]

let planet_radius = 10000.

let planet (f : flight) : shape3d =
  let x, y, z = to_engine f.planet in
  sphere (rgb 40 80 140) planet_radius |> move3d x y z

(* the slot: a dark letterbox just out of the square the nose points
 * out of, a unit above it so that the z-buffer never hesitates between
 * the two *)
let slot (f : flight) : shape3d =
  let b = f.station in
  let c = add b.pos (times (coriolis_size +. 1.) b.o.nose) in
  let corner a h = add c (add (times a b.o.side) (times h b.o.roof)) in
  polygon3d (rgb 30 25 10) (wound b.pos [ corner (-44.) (-14.); corner 44. (-14.); corner 44. 14.; corner (-44.) 14. ])

let green = rgb 90 255 110
let text (color : color) (size : number) (str : string) : shape = words color str |> Playground.scale size

(* a 2D line, as a thin rectangle, for the HUD *)
let line (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.max 1. (Float.hypot dx dy)) 2.
  |> rotate (atan2 dy dx *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* the scanner, Elite's own: every ship a dot on an ellipse where it
 * lies around you, on a stick as tall as it is above or below you *)
let scanner (screen : screen) (f : flight) : shape list =
  let cx = 0. and cy = screen.bottom +. 90. in
  let k = 0.06 in
  let blip (color : color) (p : vec) =
    let x = Basics.clamp (-200.) 200. (p.x *. k) and z = Basics.clamp (-70.) 70. (p.z *. k *. 0.35) in
    let y = Basics.clamp (-60.) 60. (p.y *. k *. 0.5) in
    [ line color (cx +. x, cy +. z) (cx +. x, cy +. z +. y); rectangle color 6. 4. |> move (cx +. x) (cy +. z +. y) ]
  in
  [ oval (rgb 0 40 0) 420. 150. |> move cx cy; line (rgb 0 90 0) (cx -. 210., cy) (cx +. 210., cy);
    line (rgb 0 90 0) (cx, cy -. 75.) (cx, cy +. 75.) ]
  @ blip (rgb 120 180 255) f.station.pos
  @ List.concat_map (fun (p : pirate) -> if p.alive then blip (rgb 255 90 90) p.ship.pos else []) f.pirates

let bar (label : string) (value : int) (color : color) (x : number) (y : number) : shape list =
  [ text white 1.8 label |> move (x -. 70.) y;
    rectangle (rgb 30 30 30) 160. 12. |> move (x +. 50.) y;
    rectangle color (1.6 *. float_of_int (max 0 value)) 10. |> move (x +. 50. -. (80. -. (0.8 *. float_of_int (max 0 value)))) y ]


(* the system, which TinyElite works out from the galaxy's seed *)
let lave = "Lave  Dictatorship  Rich Agricultural  TECH 5"

let ships (f : flight) : shape3d list =
  solid (rgb 170 190 220) coriolis f.station.pos f.station.o
  :: slot f
  :: List.filter_map (fun (p : pirate) -> if p.alive then Some (solid green sidewinder p.ship.pos p.ship.o) else None) f.pirates

let view_flight (screen : screen) (f : flight) : camera * shape3d list =
  let laser =
    if f.laser > 0 then
      [ line (rgb 255 80 80) (screen.left +. 120., screen.bottom +. 200.) (0., 0.);
        line (rgb 255 80 80) (screen.right -. 120., screen.bottom +. 200.) (0., 0.) ]
    else []
  in
  let crosshair =
    [ line green (-20., 0.) (-6., 0.); line green (6., 0.) (20., 0.); line green (0., -20.) (0., -6.); line green (0., 6.) (0., 20.) ]
  in
  let flash = if f.hit > 12 then [ rectangle (rgb 120 0 0) screen.width screen.height |> fade 0.35 ] else [] in
  let huds =
    laser @ crosshair @ scanner screen f
    @ bar "SHIELD" f.shield (rgb 90 200 255) (screen.left +. 110.) (screen.bottom +. 150.)
    @ bar "ENERGY" f.energy (rgb 250 210 80) (screen.left +. 110.) (screen.bottom +. 125.)
    @ bar "SPEED" (int_of_float (f.speed *. 7.)) (rgb 120 255 120) (screen.left +. 110.) (screen.bottom +. 100.)
    @ [ text white 2. lave |> move 0. (screen.top -. 30.);
        text green 2. (Printf.sprintf "KILLS %d" f.kills) |> move (screen.right -. 100.) (screen.bottom +. 150.) ]
    @ flash
  in
  (eye, (space :: planet f :: ships f) @ List.map hud huds)

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let over (words : shape list) (f : flight) =
    let cam, shapes = view_flight screen f in
    (cam, shapes @ List.map hud ((rectangle black 800. 150. |> move_y 180. |> fade 0.7) :: words))
  in
  match m.scene with
  | Title ->
      (* the station alone, turning, as the attract mode's ship did *)
      let t = float_of_int m.frames *. 0.01 in
      let o = tidy (roll_by t (pitch_by 0.6 upright)) in
      ( eye,
        [ space; solid (rgb 170 190 220) coriolis (v 0. 0. 700.) o ]
        @ List.map hud
            ([ text green 7. "TINY ELITE 3D" |> move_y 330.;
               text white 2.4 "up/down: pitch   left/right: roll   w/s: speed   space: laser" |> move_y (-300.);
               text white 2.4 "you are at Lave: dock with the station, the slot turns" |> move_y (-340.) ]
            @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-400.) ]) )
  | Flying f -> view_flight screen f
  | Docked f ->
      over
        [ text green 6. "DOCKED" |> move_y 200.;
          text white 2.6 (Printf.sprintf "%d pirates, and the slot on the first try" f.kills) |> move_y 140. ]
        f
  | Destroyed f -> over [ text (rgb 255 90 90) 6. "GAME OVER" |> move_y 200. ] f

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d app
