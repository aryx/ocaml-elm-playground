(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A T-handle thrown spinning, with nothing whatever acting on it, that
 * turns itself over, and over, and over.
 *
 *   1 / 2 / 3   spin it about its long axis / its stem / the third one
 *   g           how the spin is stepped (see below)
 *   space       throw it again
 *
 * This is the intermediate-axis theorem, better known as the tennis
 * racket theorem, and to cosmonauts as the Dzhanibekov effect: Vladimir
 * Dzhanibekov filmed a wing nut doing it aboard Salyut 7 in 1985 (name
 * and date from memory). It is the first example in physics/3d/ because
 * it is the one piece of rigid-body behaviour that has no 2D version at
 * all, and because it needs nothing else: no gravity, no contact, no
 * collision. Press 2 and watch, then press 1 or 3 and watch the same
 * handle refuse to do it.
 *
 * Why it happens (notes_3d_physics.md section 4): a body's resistance
 * to being spun is a matrix, and it turns with the body, so
 *
 *     L = I_world w         is conserved, but
 *     w = I_world^-1 L      is not, because I_world keeps turning.
 *
 * L is the purple arrow, and it never moves. w is the green one, and
 * it wanders: about the largest or the smallest principal axis it
 * wanders in a small circle (a wobble), about the middle one the
 * wandering is unstable -- any error grows as e^(4.6 t) here -- and
 * the body ends up turned over. Then it is unstable again, so it turns
 * back. Nothing is pushing it.
 *
 * Which is also why this is the test a 3D physics engine cannot fake.
 * Three ways to step the same rotation, on "g":
 *
 *   momentum         dL/dt = torque, and w read back as I_world^-1 L.
 *                    |L| is then conserved to 1e-11 by construction.
 *   spin+gyroscopic  step w instead, which needs the w x (I w) term.
 *                    Bullet has it as an option; |L| drifts 2%.
 *   no gyroscopic    the same without that term. The handle now spins
 *                    about a fixed axis for ever -- and every number in
 *                    the corner of the screen says the simulation is
 *                    perfect, because both |L| and the energy are
 *                    conserved exactly. It is just not this universe.
 *
 * The numbers on screen are the honest ones. |L| stays put in the first
 * mode; the energy does not, because the orientation step is first
 * order and the flip is exponentially sensitive to any error at all
 * (Integrate3d.mli measures it: 15% over 10 s at dt = 1/600 about the
 * middle axis, 0.9% at 1/6000). Ten sub-steps per frame here, which is
 * what a stiff spin costs.
 *
 * Units are metres, seconds and kilograms, as everywhere in
 * physics/3d/: the handle is 30 cm across and weighs 460 g.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The handle *)
(*****************************************************************************)

(* a bar, and a stem hanging off it: three different moments of inertia,
 * which is all the theorem asks for *)
let bar_sides = (0.30, 0.04, 0.04)
let bar_mass = 0.30
let stem_sides = (0.04, 0.16, 0.04)
let stem_mass = 0.16

(* the stem's centre, below the bar, and the centre of mass of the two
 * together: the body's origin is the centre of mass, because that is
 * the point a free body turns about *)
let stem_centre_y = -.((0.16 /. 2.) +. (0.04 /. 2.))
let mass = bar_mass +. stem_mass
let com_y = stem_mass *. stem_centre_y /. mass

(* the two tensors, each moved to the centre of mass (the parallel-axis
 * theorem, Body3d.shifted) and added *)
let inertia =
  Mat3.add
    (Body3d.shifted ~mass:bar_mass (0., -.com_y, 0.) (Body3d.box ~mass:bar_mass bar_sides))
    (Body3d.shifted ~mass:stem_mass (0., stem_centre_y -. com_y, 0.) (Body3d.box ~mass:stem_mass stem_sides))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type axis = X | Y | Z

let axis_name = function X -> "x: along the bar (smallest)" | Y -> "y: along the stem (middle)" | Z -> "z: across (largest)"
let law_name = function
  | Integrate3d.Momentum -> "momentum: dL/dt = torque"
  | Integrate3d.Spin_gyroscopic -> "spin, with w x (I w)"
  | Integrate3d.Spin_naive -> "spin, no gyroscopic term"

let next_law = function
  | Integrate3d.Momentum -> Integrate3d.Spin_gyroscopic
  | Integrate3d.Spin_gyroscopic -> Integrate3d.Spin_naive
  | Integrate3d.Spin_naive -> Integrate3d.Momentum

type model = {
  body : Body3d.t;
  axis : axis;
  law : Integrate3d.spin_law;
  (* what to compare the drift against *)
  l0 : number;
  e0 : number;
  (* the body-frame spin along [axis] last frame, to count the flips *)
  was : number;
  flips : int;
  frames : int;
  (* "g" cycles, so it needs the moment it goes down, not that it is
   * down *)
  g_was_down : bool;
}

let rate = 5. (* rad/s, about the chosen axis *)
let nudge = 0.02 (* and a breath about the other two: a real throw is never clean *)
let dt = 1. /. 600. (* ten sub-steps a frame *)
let substeps = 10

let spin_about = function X -> (rate, nudge, nudge) | Y -> (nudge, rate, nudge) | Z -> (nudge, nudge, rate)

(* the spin along the axis it was thrown about, in the body's own frame:
 * where a flip shows (in the world frame w only wobbles around L) *)
let body_spin (m : model) : number =
  let x, y, z = Quat.rotate (Quat.conjugate m.body.Body3d.orientation) m.body.Body3d.spin in
  match m.axis with X -> x | Y -> y | Z -> z

let throw (axis : axis) (law : Integrate3d.spin_law) : model =
  let body = Body3d.make ~spin:(spin_about axis) ~mass ~inertia (0., 0., 0.) in
  let m = { body; axis; law; l0 = 1.; e0 = 1.; was = 0.; flips = 0; frames = 0; g_was_down = false } in
  { m with l0 = Vec3.length (Energy3d.angular body); e0 = Energy3d.kinetic body; was = body_spin m }

let initial_model = throw Y Integrate3d.Momentum

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let down key = Set_.mem key k.keys in
  let g = down "g" in
  let m = { m with g_was_down = g } in
  if down "1" then throw X m.law
  else if down "2" then throw Y m.law
  else if down "3" then throw Z m.law
  else if k.kspace then throw m.axis m.law
  else if g && not m.g_was_down then throw m.axis (next_law m.law)
  else begin
    let body = ref m.body in
    for _ = 1 to substeps do
      body := Integrate3d.spin_step ~law:m.law ~torque:(0., 0., 0.) ~dt !body
    done;
    let m = { m with body = !body; frames = m.frames + 1 } in
    let now = body_spin m in
    { m with was = now; flips = (if now *. m.was < 0. then m.flips + 1 else m.flips) }
  end

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let bar_color = rgb 220 200 60
let bar_tip = rgb 230 90 60
let stem_color = rgb 90 160 230
let l_color = rgb 130 60 180
let w_color = rgb 30 140 80

(* the handle, in its own frame, its centre of mass at the origin *)
let handle_shape : shape3d =
  let bw, bh, bd = bar_sides and sw, sh, sd = stem_sides in
  group3d
    [ box bar_color bw bh bd;
      (* the two ends in different colours: without them a bar that has
       * turned over looks exactly like one that has not *)
      box bar_tip 0.04 (bh +. 0.005) (bd +. 0.005) |> move_x3d ((bw /. 2.) -. 0.02);
      box white 0.04 (bh +. 0.005) (bd +. 0.005) |> move_x3d (-.((bw /. 2.) -. 0.02));
      box stem_color sw sh sd |> move_y3d stem_centre_y ]
  |> move_y3d (-.com_y)

(* an arrow from the origin along [dir], [len] long: a shaft and a head,
 * built along +y and turned onto [dir] -- a quaternion doing the one
 * job it is best at, which is rotating one direction onto another *)
let arrow (c : color) (dir : Vec3.t) (len : number) : shape3d list =
  let d = Vec3.normalize dir in
  if d = (0., 0., 0.) then []
  else
    let up = (0., 1., 0.) in
    let q =
      let axis = Vec3.cross up d in
      if Vec3.length axis < 1e-9 then if Vec3.dot up d > 0. then Quat.identity else Quat.of_axis_angle (1., 0., 0.) Float.pi
      else Quat.of_axis_angle axis (acos (Float.max (-1.) (Float.min 1. (Vec3.dot up d))))
    in
    let dx, dy, dz = Quat.to_euler_xyz q in
    [ group3d [ box c 0.012 len 0.012 |> move_y3d (len /. 2.); box c 0.03 0.05 0.03 |> move_y3d len ]
      |> rotate3d dx dy dz ]

let text color size str = words color str |> scale size

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let cam = Camera3d.from_far ~fov:40. ~offset:(0.45, 0.38, 0.62) (0., 0., 0.) in
  let dx, dy, dz = Quat.to_euler_xyz m.body.Body3d.orientation in
  let l = Energy3d.angular m.body and w = m.body.Body3d.spin in
  let drift now was = 100. *. (now -. was) /. was in
  let seconds = float_of_int m.frames /. 60. in
  ( cam,
    [ handle_shape |> rotate3d dx dy dz ]
    (* L never moves: it is the one fixed thing in the scene. w does,
     * and that is the whole show. *)
    @ arrow l_color l 0.26
    @ arrow w_color w 0.2
    @ List.map hud
        [ text l_color 2.2 "L (purple): the angular momentum. It never moves." |> move_y (screen.top -. 45.);
          text w_color 2.2 "w (green): the angular velocity. It does." |> move_y (screen.top -. 80.);
          text (rgb 90 90 100) 2.2 ("thrown about " ^ axis_name m.axis) |> move_y (screen.bottom +. 150.);
          text (rgb 90 90 100) 2.2 ("stepped as " ^ law_name m.law) |> move_y (screen.bottom +. 115.);
          text (if Float.abs (drift (Vec3.length l) m.l0) < 0.01 then rgb 30 140 80 else rgb 200 90 30) 2.2
            (Printf.sprintf "|L| %+.3f %%    energy %+.2f %%" (drift (Vec3.length l) m.l0) (drift (Energy3d.kinetic m.body) m.e0))
          |> move_y (screen.bottom +. 80.);
          text (rgb 90 90 100) 2.2 (Printf.sprintf "%.1f s, %s" seconds
               (match m.flips with 0 -> "not turned over yet" | 1 -> "turned over once" | n -> Printf.sprintf "turned over %d times" n)) |> move_y (screen.bottom +. 45.);
          text darkGray 2. "1 / 2 / 3: the axes    g: how the spin is stepped    space: again" |> move_y (screen.bottom +. 12.) ] )

let app = game3d view update initial_model

(* flat shading: the handle's faces have to read as faces, or a flip is
 * invisible *)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
