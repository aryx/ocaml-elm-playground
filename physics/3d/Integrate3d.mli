(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* One time step, in 3D: where a body is and which way it points at
 * t + dt (see notes_3d_physics.md section 5).
 *
 * The step has two halves, and only one of them is new.
 *
 * {1 The linear half}
 *
 * physics/2d/Integrate, with a z. The same four methods, the same
 * worked example (a ball thrown up at 10 m/s, g = 10, dt = 0.1: after 5
 * steps explicit Euler is 0.25 m too high, semi-implicit 0.25 m too
 * low, Verlet and RK4 exact), the same convergence orders, the same
 * conclusion: games use semi-implicit Euler because it is cheap and its
 * energy error oscillates instead of growing. Read Integrate.mli for
 * the numbers; nothing about them changes with a third coordinate.
 *
 * {1 The rotational half}
 *
 * This is the new part, and it has a choice in it that the 2D engine
 * cannot even ask. Two quantities describe a spin:
 *
 *     w, the angular velocity        L = I_world w, the momentum
 *     (how it turns, now)            (what is conserved)
 *
 * In 2D these are the same thing up to a constant, because the inertia
 * is a number. In 3D the tensor sits between them and *turns with the
 * body* (Mat3.mli), so they point different ways, and a step can follow
 * either one:
 *
 *   [Momentum] (the default)      dL/dt = torque, and with no torque L
 *                                 does not move at all. w is read back
 *                                 as I_world^-1 L, and *that* is where
 *                                 the wobble comes from: the tensor has
 *                                 turned. Nothing else is needed.
 *
 *   [Spin_gyroscopic]             step w instead, and the same physics
 *                                 arrives as an extra term:
 *                                   w += I^-1 (torque - w x (I w)) dt
 *                                 This is what Bullet does (optionally;
 *                                 Box2D cannot: 2D has no such term).
 *
 *   [Spin_naive]                  the same without w x (I w). Every
 *                                 engine that "has no gyroscopic term"
 *                                 is this one.
 *
 * Measured (the T-handle of examples3d/PhysicsSpin3d.ml, tensor
 * diag(0.00149, 0.00233, 0.00370) kg m^2, spun at 10 rad/s with a
 * 0.02 rad/s nudge on the other two axes, 30 s, dt = 1/600 s):
 *
 *   about       law                flips   |L| drift   energy drift
 *   ---------------------------------------------------------------
 *   x smallest  Momentum             0      8e-13        1.3e-05
 *   y middle    Momentum            48      1e-11        4.8e-01
 *   z largest   Momentum             0      9e-13        2.5e-02
 *   y middle    Spin_gyroscopic      3      2.4e-02      4.8e-02
 *   y middle    Spin_naive           0      4.5e-16      4.8e-16
 *
 * Read the last line carefully: the wrong one looks *best*. Drop the
 * gyroscopic term and every diagnostic goes quiet -- |L| and the energy
 * are conserved to the last bit -- because the body now does something
 * much simpler than physics: it spins about a fixed axis for ever. The
 * flips are gone, and a conservation check cannot tell you so. That is
 * the argument for keeping the intermediate-axis demo around.
 *
 * [Momentum] is the default because L is what the equation of motion
 * actually says is conserved, so it is conserved here by construction
 * (1e-11, which is the quaternion renormalization's own rounding) --
 * where stepping w has to *recover* it and loses 2% in 30 s.
 *
 * The energy is the other way round: it is not conserved by either,
 * because the orientation step is first order in dt and the
 * intermediate-axis motion is exponentially sensitive to any error
 * (the perturbation grows as e^(4.6 t) for the handle above: that is
 * the flip). Smaller steps, spinning at 10 rad/s about the middle
 * axis: 57% energy drift in 10 s at dt = 1/60, 15% at 1/600, 0.9% at
 * 1/6000, while |L| stays at 1e-12 throughout. An integrator that
 * conserves both exists -- a symplectic Lie-group step (Moser-Veselov),
 * or the closed form in Jacobi elliptic functions -- and is named here
 * rather than built.
 *
 * {1 Turning the orientation}
 *
 * Either the differential equation's own first order step
 * ([Quat.integrate], q += dt/2 (0, w) q, renormalized) or the exact
 * turn ([Quat.turned_by], |w| dt about w). Measured, a sphere spun for
 * one second, how far the first-order step falls behind the exact one:
 *
 *      1 rad/s (1 deg/step)      0.0013 deg
 *      5 rad/s (5 deg/step)      0.17 deg
 *     20 rad/s (19 deg/step)    10.4 deg
 *     60 rad/s (57 deg/step)   110 deg
 *
 * First order is what production engines use and is fine at a frame's
 * worth of turn; it falls apart exactly where you would expect, and
 * since the exact step costs one sin and one cos more, [Exact] is the
 * default here.
 *
 * References: as physics/2d/Integrate (Euler 1768; Verlet 1967; Runge
 * 1895 and Kutta 1901; Hairer, Lubich and Wanner 2002), plus Baraff and
 * Witkin's "Physically Based Modeling" for dL/dt = torque, and Erin
 * Catto's GDC talks for what game engines do with it in practice. *)

(* the linear half's method; the same four as in 2D *)
type method_ = Explicit_euler | Semi_implicit_euler | Verlet | Rk4

(* how the spin is stepped: see above *)
type spin_law = Momentum | Spin_gyroscopic | Spin_naive

(* how the orientation is turned by it *)
type turn = First_order | Exact

(* an acceleration, from a position and a velocity -- the 3D twin of
 * physics/2d/Force.t, which Force3d (the plan's phase 2) will build *)
type force = Vec3.t -> Vec3.t -> Vec3.t

(* in that order, the simplest first *)
val methods : method_ list

(* e.g. "semi-implicit Euler" *)
val name : method_ -> string

(* a = (0, 0, 0): a body left alone *)
val no_force : force

(* [step m ?law ?turn ?torque ~force ~dt b]: [b] one tick later. The
 * defaults are the ones a game wants: [Momentum], [Exact], no torque. *)
val step :
  method_ -> ?law:spin_law -> ?turn:turn -> ?torque:Vec3.t -> force:force -> dt:float -> Body3d.t -> Body3d.t

(* the rotational half on its own: the orientation and the spin, which
 * is all a body with no forces on it needs *)
val spin_step : ?law:spin_law -> ?turn:turn -> torque:Vec3.t -> dt:float -> Body3d.t -> Body3d.t

(* The linear half's methods, each on its own, leaving the orientation
 * alone. With a the acceleration at (pos, vel): *)

(* pos += vel dt, then vel += a dt: the obvious one; gains energy *)
val explicit_euler : force:force -> dt:float -> Body3d.t -> Body3d.t

(* vel += a dt, then pos += vel dt (the *new* vel): the one games use *)
val semi_implicit_euler : force:force -> dt:float -> Body3d.t -> Body3d.t

(* velocity Verlet; a velocity-dependent force uses a predicted
 * velocity, as in 2D *)
val verlet : force:force -> dt:float -> Body3d.t -> Body3d.t

(* the classic fourth-order Runge-Kutta *)
val rk4 : force:force -> dt:float -> Body3d.t -> Body3d.t
