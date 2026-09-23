(* The conserved quantities: physics' referee of the integrators (see
 * notes_2d_physics.md sections 3 and 5).
 *
 * With no friction, a body's total energy -- kinetic (from its speed)
 * plus potential (from where it is in a force field) -- never changes:
 * a ball going up trades speed for height, a planet near the star goes
 * faster. A simulation that doesn't keep it constant is wrong, and how
 * wrong measures the integrator: explicit Euler's energy grows (the
 * planet spirals out), a symplectic method's oscillates around the true
 * value. Momentum (mass times velocity) is the other: conserved by any
 * interaction between bodies (Newton's third law), the check for
 * collisions.
 *
 * Example: a mass of 2 at speed 3 has a kinetic energy of 2 * 3^2 / 2 =
 * 9 and a momentum of 6. *)

(* m |vel|^2 / 2, plus I spin^2 / 2 for a spinning body: a wheel
 * spinning in place has energy too *)
val kinetic : Body.t -> float

(* m vel *)
val momentum : Body.t -> Vec2.t

(* the angular momentum around [around]: m (r x vel), r from [around]
 * to the body, plus I spin -- conserved by collisions too, the
 * rotational twin of momentum. Example: a mass of 1 at (0, 2) moving
 * at (3, 0), around (0, 0): -6 (it goes clockwise around the origin) *)
val angular_momentum : around:Vec2.t -> Body.t -> float

(* The potential energies of Force's forces, so that kinetic + potential
 * is constant under that force: *)

(* under [Force.uniform (0, -g)]: m g y *)
val gravity : g:float -> Body.t -> float

(* under [Force.gravitation ~gm ~center]: -gm m / r (negative: a bound
 * orbit has a negative total energy) *)
val gravitation : gm:float -> center:Vec2.t -> Body.t -> float

(* under [Force.spring ~k_over_m ~anchor]: k |pos - anchor|^2 / 2, with
 * k = k_over_m * m *)
val spring : k_over_m:float -> anchor:Vec2.t -> Body.t -> float
