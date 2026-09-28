(* A cable on the rack's back, as Reason draws them: it hangs between its
 * two jacks, sags, and swings when an end moves (plan_tiny_reason.md).
 *
 * A rope of particles and sticks (Particles.mli, Jakobsen's): 14
 * particles, both ends pinned to their jacks (or one to the mouse, the
 * cable being dragged), under gravity. Nothing else: the sag, the
 * swing after a jerk, the trail behind a dragged end, the bounce when a
 * plug goes in, all come out of position Verlet by themselves.
 *
 * Its length: a cable longer than the distance d between its ends, by
 * 8% and 60 pixels, L = 1.08 d + 60 -- so every cable droops, a long
 * one more. It starts hanging, already at rest: the parabola close to
 * the catenary, its sag s from the arc length of a parabola,
 *
 *     L ~ d + 8 s^2 / (3 d),   so   s = sqrt (3 d (L - d) / 8)
 *
 * worked example: d = 300, L = 384, s = sqrt 9450 ~ 97 pixels. Then
 * 90 steps, silent, so that it is at rest before it is drawn -- at 107
 * pixels, lower: the rope stretches under its weight, as a rope of
 * sticks relaxed a few times does (Particles.mli).
 *
 * The air (drag 0.03 a step) and the relaxation damp a swing: an end
 * jerked 100 pixels, the cable's energy (sum of |pos - old|^2) is 26
 * times smaller 30 frames later, a wobble of about half a second, as on
 * Reason's back (Unit_rack_cable). A cable at rest sleeps: no step
 * while its ends stay put and nothing has moved more than 0.05 of a
 * pixel for 30 frames -- 154 frames after that jerk. *)

type t

(* hanging from [a] to [b], at rest *)
val make : Vec2.t -> Vec2.t -> t

(* the rest length for ends [d] apart, and the sag of a parabola that
 * long *)
val rest_length : float -> float
val sag : d:float -> length:float -> float

(* [step t a b]: a frame, its ends moved to [a] and [b] *)
val step : t -> Vec2.t -> Vec2.t -> t

(* its ends let go: it falls (step's ends are then ignored) *)
val release : t -> t

val points : t -> Vec2.t list
val asleep : t -> bool

(* the kinetic energy, sum of |pos - old|^2: how much it still moves *)
val energy : t -> float

(* the cable drawn as a band [width] wide: its points smoothed
 * (Catmull-Rom, three between each two), then pushed out along their
 * normals on one side and back on the other -- one polygon *)
val ribbon : t -> width:float -> Vec2.t list
