(* Shots: bullets, lasers, bombs, as values that move.

   A shooter is mostly shots: the player's going up, the enemies' coming
   down or aimed, dozens on the screen. Each is where it is and how fast
   it goes, in pixels per frame; every frame it moves by its speed
   ([advance]), and it's gone when it leaves the screen ([on_screen]) or
   hits something ([near], or the game's own test). No physics: shots
   fly straight, at a constant speed, like the arcade's (Space Invaders
   moved its shots by a fixed number of pixels per frame).

       fire          advance        advance       off the screen: dropped
        o  --vy-->     o    -->       o    -->   |

   Aiming at a target is the one bit of math ([aimed]): the direction to
   it, divided by its length, times the speed. Aiming where the target
   will be, not where it is, is a quadratic (games/TinyXpilot's
   intercept): an exercise here.

   Part of the shoot 'em up kit (kits/shmup/), with Path.mli; used by
   games/TinyInvaders (the cannon's shot, the aliens' bombs) and
   games/TinyGalaga (the fighter's shots, the divers' aimed bullets). *)

open Playground

type t = { x : number; y : number; vx : number; vy : number }

(* [straight x y vx vy]: a shot at (x, y) moving (vx, vy) each frame,
 * e.g. a cannon's shot going up, [straight x y 0. 15.] *)
val straight : number -> number -> number -> number -> t

(* [aimed speed (x, y) (tx, ty)]: a shot at (x, y) towards (tx, ty),
 * [speed] pixels per frame. E.g. at speed 5 from (0, 0) to (30, -40):
 * (3, -4) each frame (the direction (30, -40) is 50 long) *)
val aimed : number -> number * number -> number * number -> t

(* one frame later: moved by its speed *)
val advance : t -> t

(* [on_screen margin screen s]: [s] is still on the screen, or less than
 * [margin] off it *)
val on_screen : number -> screen -> t -> bool

(* [near r (x, y) s]: [s] is less than [r] pixels from (x, y): a hit on
 * a round target *)
val near : number -> number * number -> t -> bool

(* [angle s]: the direction it flies, in degrees (for [rotate]: a long
 * bullet drawn along its way) *)
val angle : t -> number
