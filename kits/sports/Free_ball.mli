(* The ball that is not yours.

   Two games can be told apart by one decision: when a player runs into
   the ball, does the ball go with him, or does it go *away*? Carry it
   and dribbling is steering, and the ball is a part of the man;
   push it ahead and dribbling is a chase you are only just winning,
   because the touch sends it off a little faster than he can run:

     glued                      free
     p o---->                   p   o- - ->
     the ball is his feet       he has to catch it up, and so does
                                whoever else is nearer than he is

   Kick Off (Dino Dini, 1989) is the second; every other football game
   of its day, and Sensible Soccer after it, the first. Speedball's
   ball is the second and heavier still: it never stops, because the
   walls send it back.

   This is that ball, with nothing else in it: where it is, how fast it
   goes, the grass or the metal slowing it down ([roll]), what a player
   does to it when he reaches it ([touch]), and the walls ([bounce_in]).
   Whose ball it is, what it scores, who may touch it next -- all of
   that is the game's, not the ball's.

   Part of the sports kit (kits/sports/), with Formation.mli; used by
   games/TinyKickOff2 and games/TinySpeedball2. *)

open Playground

type t = { x : number; y : number; vx : number; vy : number }

val still : number -> number -> t

(* [roll ~friction ?push b]: one frame later. [friction] is what is
 * left of its speed after it (0.985 on grass), and [push] an
 * acceleration added first -- Kick Off's aftertouch, the arrows
 * bending a ball that is already in the air. *)
val roll : friction:number -> ?push:number * number -> t -> t

(* [touch ~glued ~speed ~reach ~hold (px, py) (dx, dy) b]: what a
 * player at (px, py) running towards (dx, dy) does to the ball if he
 * is within [reach] of it -- [None] if he is not. Free, it leaves at
 * [speed] in his direction; [glued], it is put [hold] in front of him,
 * which is where his feet are (the two radii).
 *
 * [reach] must be more than the distance a player covers in a frame,
 * or a glued ball comes off his feet on the second one. *)
val touch : glued:bool -> speed:number -> reach:number -> hold:number -> number * number -> number * number -> t -> t option

(* [bounce_in ~half_w ~half_h ~keep b]: the ball turned back by the
 * walls of a closed arena, keeping [keep] of its speed (1. loses
 * nothing, 0.8 is a heavy ball on metal). A pitch with touchlines
 * doesn't want this: there the ball goes out, and a referee has
 * something to say about it. *)
val bounce_in : half_w:number -> half_h:number -> keep:number -> t -> t

(* [speed b]: how fast it is going, whatever the direction *)
val speed : t -> number

(* [near r (x, y) b]: the ball is within [r] of that point *)
val near : number -> number * number -> t -> bool
