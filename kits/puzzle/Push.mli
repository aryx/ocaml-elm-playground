(* Push: moving on a grid, pushing what's in front.

   Sokoban (Hiroyuki Imabayashi, 1982) is made of one move: the player
   steps into the next cell, and if a box is there, it is pushed one
   cell further -- unless a wall, or another box, is behind it. Baba Is
   You (Arvi Teikari, 2019) pushes whole rows of things at once, and
   Boulder Dash (1984) its boulders one at a time: the same move, with a
   different limit on the chain.

        @$$ .    moving right: the chain is the two boxes; pushed if
                 the cell after them is free (here), and if the limit
                 allows two (Sokoban's is one: blocked)

   The grid isn't this module's: the game says which cells hold
   something pushable, and which are blocked (walls, the map's edges),
   and moves what the chain says. What's in a cell is the game's too: a
   character of a Tilemap (games/TinySokoban), a list of objects
   (games/TinyBabaIsYou).

   Part of the puzzle kit (kits/puzzle/), with Undo.mli. *)

(* [chain ~blocked ~pushable ?limit from dir]: moving from the cell [from]
 * one cell in the direction [dir]: the cells whose contents are pushed
 * along, one cell each, nearest first (the empty list: nothing in the
 * way), or None if the move is blocked -- the first cell after the
 * chain of [pushable] ones is [blocked], or the chain is longer than
 * [limit]. A cell both pushable and blocked is pushable. E.g. on the row
 * "@$$ ", moving right from (0, 0): Some [(1, 0); (2, 0)], but None with
 * a limit of 1; on "@$#": None; on "@ ": Some []. *)
val chain : blocked:(int * int -> bool) -> pushable:(int * int -> bool) -> ?limit:int -> int * int -> int * int -> (int * int) list option
