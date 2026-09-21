(* Which way a ship is pointing, when it can point anywhere: six
   degrees of freedom.

   A car or a walking player has one angle, its heading, and "up" stays
   up (games/TinyDoom, gamekits/racing/Topdown). A ship in Descent
   (Parallax, 1995) has none of that: it can pitch its nose up until it
   is flying backwards, roll until the floor is the ceiling, and there
   is no "up" left to measure an angle against. What it has instead is
   three directions of its own, which it carries with it:

        up                      forward: where the nose points
        ^   ^ forward           up     : where the canopy points
        |  /                    right  : forward x up
        | /
        +----> right

   Turning is then always around one of its own three directions:
   pitch around [right] (nose up or down), yaw around [up] (nose left
   or right), roll around [forward] (the canopy tips sideways). Each
   one turns the other two vectors and leaves its own axis alone
   ([turn]), so there is no order of angles to agree on, no "gimbal
   lock" (Euler angles, x then y then z, lose a degree of freedom when
   the second turn lines the first and third axes up), and no angle to
   wrap around at 360.

   The price: the three vectors drift out of square as the small turns
   pile up, floating point being what it is, so they are straightened
   after every turn ([turn] does it: Descent did the same to its
   orientation matrices). Three vectors like these ARE a rotation
   matrix, written a row at a time; quaternions are the other usual
   answer, smaller and smoother to interpolate, but harder to read.

   Used by games2.5d/TinyDescent (which makes its own camera out of
   these three, no 3D engine) and games3d/TinyDescent3d (which hands
   [forward] and [up] to Playground3d's camera, hence its [up]). *)

type vec = float * float * float

type t = {
  right : vec;
  up : vec;
  forward : vec;
}

(* right (1, 0, 0), up (0, 1, 0), forward (0, 0, -1): looking the way
 * the camera of a 3D scene looks by default *)
val identity : t

(* [turn ~pitch ~yaw ~roll t]: turned around its own axes, in degrees,
 * pitch (nose up) around [right], yaw (nose left) around [up], roll
 * (canopy tipping right) around [forward]; then straightened. *)
val turn : ?pitch:float -> ?yaw:float -> ?roll:float -> t -> t

(* [heading t] the direction of [forward], as a point [distance] ahead
 * of [from] *)
val ahead : t -> from:vec -> distance:float -> vec

(* [along t (a, b, c)]: the point [a] to the right, [b] up and [c]
 * forward of the origin, in world coordinates, e.g. a gun on the ship's
 * left wing *)
val along : t -> vec -> vec
