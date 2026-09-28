(* How the other movers of a maze choose their way: at each tile's
   center, among the corridors open around them, never turning back
   (unless it's a dead end), either the way towards a target, or one at
   random.

       target
         X         at this junction, the ghost looks one tile ahead in
         :         each open way (but not back), and takes the one whose
     .---+---.     tile is the closest to the target, in a straight line
         |
         G  (came from below)

   That's all Pac-Man's ghosts do (Jamey Pittman, "The Pac-Man
   Dossier", 2009): their personalities are only their targets (the
   player's tile, 4 tiles ahead of him, ...), chosen by the game. They
   don't look further than the next tile -- no pathfinding -- so they
   can be lured the wrong way around a block; the never-turning-back
   rule is what makes them look purposeful. Bomberman's balloons (Hudson
   Soft, 1983) just wander: the same rule, a random choice.

   For real pathfinding, the shortest way through the maze (breadth-first
   search, or the A* algorithm), see plan_teaching_other.md's game AI.

   Part of the maze kit (gamekits/maze/), with Grid_move.
*)

open Grid_move

(* the open ways from the mover's tile, in the order up, left, down,
 * right, but not back the way it came; back if there's no other way *)
val ways : grid -> open_:(int * int -> bool) -> mover -> dir list

(* [toward grid ~open_ ~goal m]: [m] turned to the way whose next tile
 * is the closest to [goal] (col, row) -- ties go to the first in the
 * order of [ways]. E.g. a mover at (5, 5), come from below, with the
 * ways up, left and right open, and the goal at (8, 2): up gives (5, 4),
 * 3 * 3 + 2 * 2 = 13 away (squared), right (6, 5), 2 * 2 + 3 * 3 = 13
 * too, left more: up, the first. *)
val toward : grid -> open_:(int * int -> bool) -> goal:int * int -> mover -> mover

(* [at_random grid ~open_ n m]: [m] turned to the way number [n] (modulo
 * their number) of [ways], [n] a random number (see [next_random]) *)
val at_random : grid -> open_:(int * int -> bool) -> int -> mover -> mover

(* Pac-Man's random numbers: r := r * 5 + 1, modulo 8192 (the arcade
 * then read a byte of its ROM at that address). Deterministic: the
 * same game gives the same "random" turns, which is why players could
 * learn patterns. *)
val next_random : int -> int
