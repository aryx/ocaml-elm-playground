(* Flocking: birds, fish and herds from three local rules, and no
 * leader.
 *
 * Craig Reynolds's boids (1987): each one looks only at its neighbours
 * -- the others within a radius -- and wants three things, each a
 * desired velocity for Steering.steer:
 *
 *   separation   away from the ones too close          (don't collide)
 *   alignment    the neighbours' average velocity      (go with them)
 *   cohesion     towards the neighbours' average place (stay together)
 *
 *    separation        alignment          cohesion
 *     o   <-o           o->  o->           o  ->o
 *    ->o                  o->               o<- o
 *     o   o->           o->  o->           o<-  o
 *
 * Nothing in it knows what a flock is, and a flock appears -- splits
 * round a rock, and joins up behind it. The three weights are the whole
 * character of it: separation alone is a gas, cohesion alone a blob,
 * alignment alone a current, and the three together a flock
 * (examples/AiFlock.ml switches each off).
 *
 * Worked example: two boids 10 apart on the x axis, at (0, 0) and
 * (10, 0), both still, top speed 100. For the first, separation wants
 * (-100, 0), straight away from the other; cohesion wants (100, 0),
 * towards their middle... which is the other one: the two pull against
 * each other, and with equal weights they stay where they are -- which
 * is why separation looks at a smaller radius than the others (by
 * default half): close, only it counts; farther, only the others do.
 *
 * The cost is the neighbour search, every boid against every other:
 * n^2 / 2 distances, fine for a few hundred. More than that wants
 * exactly the physics engine's broad phase (physics/2d/Broadphase.mli,
 * a grid), not every pair.
 *
 * References: Craig Reynolds, "Flocks, Herds, and Schools: A Distributed
 * Behavioral Model" (SIGGRAPH 1987), whose flocks later flew in Batman
 * Returns (1992). *)

(* [neighbours ~radius others v]: those of [others] within [radius] of
 * [v], [v] itself left out (by physical equality: [others] may be the
 * whole flock, [v] among them) *)
val neighbours : radius:float -> Steering.vehicle list -> Steering.vehicle -> Steering.vehicle list

(* the desired velocities, from the neighbours given; with none, the
 * vehicle's own velocity (no force) *)
val separation : Steering.vehicle list -> Steering.vehicle -> Steering.vec
val alignment : Steering.vehicle list -> Steering.vehicle -> Steering.vec
val cohesion : Steering.vehicle list -> Steering.vehicle -> Steering.vec

(* [flock ~radius others v]: the steering force of the three rules,
 * weighted ([separation] 1.5, [alignment] 1, [cohesion] 1 by default;
 * 0 turns a rule off), separation among the neighbours within
 * [radius] / 2, the other two within [radius] *)
val flock :
  ?separation:float -> ?alignment:float -> ?cohesion:float -> radius:float -> Steering.vehicle list -> Steering.vehicle -> Steering.vec
