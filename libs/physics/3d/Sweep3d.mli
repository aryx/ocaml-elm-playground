(* Continuous collision: where along its path a moving sphere first
 * touches something (see notes_3d_physics.md section 12).
 *
 * A body moving faster than its own size per step can go through a
 * wall without overlapping it on any frame: a pinball, 27 mm across,
 * moves 50 mm a step at 3 m/s and 167 mm at 10 m/s, and a table's wall
 * is 10 mm thick. So instead of testing where the ball *is*, test the
 * path it takes -- the sphere swept from where it was to where it will
 * be -- and find the first moment of it that touches.
 *
 * The method is Brian Mirtich's *conservative advancement* (1996):
 * from the start of the step, look at the gap between the sphere and
 * the obstacle; nothing in the world can close that gap faster than
 * their relative speed, so the sphere can be advanced by the gap
 * divided by that speed, safely, without passing anything. Repeat
 * until the gap is gone:
 *
 *      o . . . . . .o . . .o. .o.o|      each advance is the gap left
 *      t = 0                      wall   divided by the closing speed:
 *                                        big steps far away, small near
 *
 * It never jumps over a thin wall -- every advance stops short of the
 * nearest thing -- and it needs nothing of the obstacle but a distance,
 * so it works the same against a sphere, a box, a capsule or a plane.
 * And the obstacle may move and turn during the step (a flipper): then
 * the closing speed is bounded by the sphere's speed relative to it
 * plus how fast the obstacle's farthest point can swing round, its
 * turning speed times its reach.
 *
 * What it does not do is the response: it says *when*, and
 * Physics3d.simulate ~continuous stops the ball there and lets the next
 * step's contact bounce it. The alternatives, both named in the notes:
 * sub-stepping (run the world in smaller steps: simple, costly, and
 * only as good as the step is small), and speculative contacts (hand
 * the solver the contact before it happens: smoother, but they lose a
 * bounce's energy, which a pinball cannot afford).
 *
 * Reference: Brian Mirtich, "Impulse-based Dynamic Simulation of Rigid
 * Body Systems" (PhD thesis, Berkeley, 1996), chapter 4. *)

(* [gap p h]: how far the point [p] is from the surface of [h] -- 0 when
 * [p] is inside it *)
val gap : Vec3.t -> Hitbox3d.placed -> float

(* [reach h]: how far the farthest point of [h] is from its middle
 * (infinite for a plane): what bounds how fast it can swing round *)
val reach : Hitbox3d.placed -> float

(* [sphere ~radius ~from ~motion ?moving h]: the fraction of the step
 * ([0..1]) at which a sphere of [radius] going from [from] by [motion]
 * first touches [h] (overlapping it by a millimetre, so that the next
 * step's contact sees it), or [None] if it never does in this step. [h]
 * itself moves by [fst moving] and turns by [snd moving] (a rotation
 * vector: its axis, and its length the angle in radians) during the
 * step; by default it stays put.
 *
 * A sphere touching [h] already at the start of the step gives [None]:
 * resting on it or sliding along it is the solver's business, not the
 * sweep's. *)
val sphere :
  radius:float -> from:Vec3.t -> motion:Vec3.t -> ?moving:Vec3.t * Vec3.t -> Hitbox3d.placed -> float option
