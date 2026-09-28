(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* What pushes a body, as an acceleration -- physics/2d/Force with a z,
 * and one new one (see notes_3d_physics.md section 6).
 *
 * Gravity, gravitation, springs and drag are the 2D formulas
 * unchanged, and their .mli is the one to read for them
 * (physics/2d/Force.mli): a third coordinate adds nothing to Newton or
 * to Hooke. They are here so that a 3D game does not have to reach
 * across into the 2D engine for a vector of the wrong width.
 *
 * Buoyancy is the new one, and it is in because floating barrels are
 * one of Half-Life 2's set pieces:
 *
 *     Archimedes (c. 250 BC): the upward force is the weight of the
 *     fluid displaced.
 *
 *          ~~~~~~~+--------+~~~~~~~~   the water line
 *                 |        |  s of h under it
 *                 +--------+
 *
 *     F_up = rho_water * g * V_submerged
 *
 * Divide by the body's mass, rho_body * V, and everything but a ratio
 * cancels: with d = rho_body / rho_water the body's density *relative
 * to the water* (0.6 for a wooden barrel, 2.7 for aluminium), the
 * acceleration up is
 *
 *     a_up = g * (V_submerged / V) / d
 *
 * so a body floats where the submerged fraction is exactly d -- 60% of
 * a barrel under water, and a body of d >= 1 never comes back up.
 * That waterline is what its test checks.
 *
 * Two things left out here, deliberately: the force is applied at the
 * body's centre, so nothing *rights* itself (that needs a torque at the
 * centre of the submerged part, and a torque channel, which the plan's
 * phase 11 brings), and the submerged fraction is read off a height,
 * not a shape (which is Hitbox3d's job, phase 4). What is here is the
 * part that is about Archimedes.
 *
 * References: Newton, Principia (1687); Hooke, De Potentia Restitutiva
 * (1678); Archimedes, On Floating Bodies (c. 250 BC). *)

(* an acceleration, from a position and a velocity, exactly as
 * physics/2d/Force.t and Integrate3d.force *)
type t = Vec3.t -> Vec3.t -> Vec3.t

(* a = (0, 0, 0): a body keeps its velocity (the first law) *)
val none : t

(* [uniform a]: the same acceleration everywhere, e.g. gravity near the
 * ground, (0, -9.8, 0) *)
val uniform : Vec3.t -> t

(* [gravitation ~gm ~center]: an acceleration of gm / r^2 towards
 * [center], r the distance. A body at r needs the speed sqrt (gm / r)
 * to stay on a circular orbit. *)
val gravitation : gm:float -> center:Vec3.t -> t

(* [spring ~k_over_m ~anchor]: Hooke's law, a pull back towards
 * [anchor] proportional to the distance from it *)
val spring : k_over_m:float -> anchor:Vec3.t -> t

(* [drag ~c]: a push against the motion, c times the velocity. Under a
 * gravity of g it gives a terminal speed of g / c. *)
val drag : c:float -> t

(* [buoyancy ~g ~water ~half_height ~density]: the water's surface at
 * the height [water], the body reaching [half_height] above and below
 * its centre, and [density] its density relative to the water. Gives
 * the *net* of Archimedes' push and gravity, plus a drag under the
 * surface ([damping], 1.5 by default: without it a barrel bobs for
 * ever). Out of the water it is a plain fall. *)
val buoyancy : ?damping:float -> g:float -> water:float -> half_height:float -> density:float -> unit -> t

(* [submerged ~water ~half_height y]: the fraction of a body centred at
 * [y] that is under the surface, 0 to 1 -- what [buoyancy] is built on,
 * exposed because it is the interesting line *)
val submerged : water:float -> half_height:float -> float -> float

(* all of them at once *)
val sum : t list -> t
