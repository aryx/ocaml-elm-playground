(* A team is a shape, not eleven brains.

   The cheapest team that looks like a team: every player has a *place*
   -- a spot in a formation, written as fractions of the half, the way
   a tactics screen draws it -- and every frame he walks towards that
   place pulled part of the way towards the ball. Only the man nearest
   the ball chases it. Nobody knows the plan, and the shape appears:

       ball up the pitch                   ball back
        . o .    the whole side              . . .
       o  o  o   slides up with it          o  o  o
        .  k                                 . o .
                                              k

   It is the same trick as a flock (Reynolds, 1987): local rules, no
   choreographer. What it is not is any kind of football intelligence;
   what it buys is that the side keeps its shape while you watch the
   ball, which is the thing you actually see.

   Part of the sports kit (kits/sports/), with Free_ball.mli; used by
   games/TinyKickOff2 and games/TinySpeedball2. *)

open Playground

(* a spot in the formation: across the pitch and up it, from -1 to 1,
 * where -1 is your own goal line. E.g. (0., -0.95) is the keeper *)
type spot = number * number

(* [at ~half_w ~half_h ~up spot]: where that spot is on the pitch, for
 * a side shooting up the screen ([up]) or down it *)
val at : half_w:number -> half_h:number -> up:bool -> spot -> number * number

(* [belongs ~pull ~home ~ball]: where a player should be *now* -- his
 * place, moved [pull] of the way towards the ball (0.33 keeps a shape,
 * 1. makes everyone chase) *)
val belongs : pull:number -> home:number * number -> ball:number * number -> number * number

(* [nearest where keep (x, y) players]: the index of the player nearest
 * (x, y) among those [keep] accepts (the keeper is usually left out of
 * the chase), or [None] if none are *)
val nearest : ('p -> number * number) -> ('p -> bool) -> number * number -> 'p list -> int option

(* [run_to ~speed ~bounds (px, py) (tx, ty)]: one step of a player
 * towards a place, as fast as he runs, kept inside [bounds] (half the
 * pitch's width and height, plus whatever room the game allows outside
 * the lines): the new position and the direction he now faces *)
val run_to : speed:number -> bounds:number * number -> number * number -> number * number -> (number * number) * (number * number)
