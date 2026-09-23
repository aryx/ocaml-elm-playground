(* A player that walks: the character controller.

   A player is not a rigid body (notes_3d_physics.md section 14): a
   rigid capsule on a ramp tips over, spins when a corner clips it,
   slides down slopes, cannot climb a 20 cm step, and goes as fast as
   friction lets it rather than as fast as the player asked. So a
   character is a capsule that never turns, moved by a loop Quake wrote
   down first (SV_FlyMove, id Software, 1996, source released 1999):

     remaining = velocity * dt
     repeat up to 4 times:
       trace the capsule along remaining
       if nothing is hit: move, done
       else: move up to the hit, and
             remaining = remaining - n (remaining . n)     slide along
                                                           the plane

       ---->   |          the part of the move into the wall is
          \    |          dropped, the part along it kept: walking
           \   |          into a wall at an angle slides along it
            v  |

   and three things every engine's controller has on top:

   - a **step offset** ([step], 0.4 m): the move is also tried lifted by
     that much and set down again, and the one that went farther wins
     (Quake's SV_WalkMove), so stairs are walked up without a jump;
   - a **slope limit** ([slope], 45 degrees): a surface steeper than
     that is a wall to the feet -- walking cannot climb it -- but not to
     gravity, which slides the character down it;
   - a **ground check**: a short trace down after the move, which says
     whether the character stands (and on what slope), and which, while
     it walks, keeps it on the ground down stairs and slopes instead of
     leaving it to fall a step at a time.

   The trace is the one piece this module has to invent: the engine's
   sweep (Sweep3d, phase 10) sweeps a sphere, not a capsule. It steps along
   the move in pieces shorter than half the capsule's radius, and when
   a piece ends overlapping a solid, bisects that piece down to where
   the overlap starts; the plane is the contact's normal there
   (Physics3d.contact). A walking character moves a few centimetres a
   frame, so nothing is stepped over; a bullet would need the real
   sweep.

   The game feel on top -- coyote time, a jump buffer, a variable jump
   height -- stays in the game: it is not physics.

   Units are Physics3d's: metres, seconds, one [walk] per tick. *)

open Playground3d

type t = {
  x : number; (* its feet: the bottom of the capsule, on its axis *)
  y : number;
  z : number;
  vy : number; (* how fast it goes up (down, negative), m/s: the one speed kept between ticks *)
  grounded : bool; (* standing on something walkable *)
  ground : number; (* the slope of what it stands on, in degrees (0 when in the air) *)
  radius : number;
  height : number; (* from the feet to the top of the head *)
  step : number; (* the step offset, metres *)
  slope : number; (* the slope limit, degrees *)
}

(* [make ?radius ?height ?step ?slope x y z]: a character standing with
 * its feet at (x, y, z); 0.3 m wide either side, 1.8 m tall, stepping
 * up 0.4 m, walking up 45 degrees *)
val make : ?radius:number -> ?height:number -> ?step:number -> ?slope:number -> number -> number -> number -> t

(* [walk ?gravity ?jump solids (vx, vz) c]: one tick of [c] wanting to
 * go at (vx, vz) metres per second across the ground, among [solids]
 * (bodies, usually [Physics3d.immovable]; only their hitboxes count).
 * [jump] is the speed up given if it stands (0 by default: no jump);
 * [gravity] 9.8. *)
val walk : ?gravity:number -> ?jump:number -> Physics3d.body list -> number * number -> t -> t

(*****************************************************************************)
(* {1 Pieces} *)
(*****************************************************************************)

(* its capsule, as a body: for drawing its hitbox (Physics3d.debug), or
 * for other bodies to meet *)
val capsule : t -> Physics3d.body

(* [trace solids c (dx, dy, dz)]: how much of that move [c] can make
 * before touching a solid (0 to 1), and the normal of what it touches
 * (pointing out of the solid, towards [c]), if anything *)
val trace : Physics3d.body list -> t -> number * number * number -> number * (number * number * number) option
