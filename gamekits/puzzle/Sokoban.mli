(* Sokoban: its rules, its level format, and a solver -- what the game
   (TinySokoban) and its level editor (TinySokobanEd) share.

   The format is Sokoban's own text, which players have exchanged levels
   in since the 1990s (the ".xsb" files of XSokoban and its successors):

        #######      '#' a wall        '$' a box     '*' a box on a goal
        #     #      ' ' the floor     '.' a goal    '+' the player on
        # $@$ #      '@' the player                      a goal
        # . . #
        #######      ('-' and '_' are the floor too, where spaces get
                      lost: in a URL, in an e-mail)

   A file holds levels one after the other, separated by blank lines;
   any other line, a ';' comment or a "Title: ...", is not part of a map.
   The format is the reason the editor needs no format of its own: what
   it writes, a text editor can edit, and the game reads it back (games/
   puzzle/dune embeds the file in the game at build time).

   The editor and the game each keep what is theirs: the game its scenes
   and its levels, the editor its cursor and brush. The rules and the
   look are here because the editor plays too (a level is tested by
   playing it), and the solver because a level editor's first question
   is whether the level can be solved at all.

   Part of the puzzle kit (gamekits/puzzle/), with Push.mli and Undo.mli. *)

(*****************************************************************************)
(* {1 The rules} *)
(*****************************************************************************)

(* the map holds the walls, goals and boxes ('#', '.', '$', '*', ' '); the
 * player is at (col, row), apart, so that what is under them stays *)
type board = { map : Tilemap.t; col : int; row : int; moves : int; pushes : int }

(* [start size rows]: the level's board, its cells [size] wide, the player
 * taken out of the map (at (0, 0) if there is none) *)
val start : Playground.number -> string list -> board

(* [step b (dc, dr)]: one step in direction (dc, dr) -- into a free cell,
 * or pushing a box into the free cell behind it -- or None (a wall, two
 * boxes in a row). E.g. on "@$ .", right: " @$." *)
val step : board -> int * int -> board option

(* solved: no box left off a goal *)
val solved : board -> bool

(*****************************************************************************)
(* {1 The look} *)
(*****************************************************************************)

(* a cell of the map, 60 wide, for [Tilemap.view] *)
val tile : char -> Playground.shape

(* the player, drawn apart from the map *)
val player : Playground.shape

(*****************************************************************************)
(* {1 The file format} *)
(*****************************************************************************)

(* [of_xsb text]: the levels of a file, each its rows, e.g. "; 1\n###\n#@#\n
 * \n###" gives [["###"; "#@#"]; ["###"]]. '-' and '_' become ' '. *)
val of_xsb : string -> string list list

(* [to_xsb levels]: the file, each level after a "; n" comment line and
 * followed by a blank line, e.g. [["###"; "#@#"]] gives "; 1\n###\n#@#\n\n";
 * [of_xsb] gives the levels back *)
val to_xsb : string list list -> string

(* [trim rows]: the level without the empty space around it -- the blank
 * rows above and below, the columns of spaces on the left, the spaces
 * at the end of a row. E.g. [""; "   ##  "; "   #"; " "] gives ["##"; "#"]. *)
val trim : string list -> string list

(*****************************************************************************)
(* {1 Checking a level} *)
(*****************************************************************************)

(* [problems rows]: what makes the level unplayable, none if it is fine:
 * not exactly one player, no box, not as many boxes as goals, or every
 * box already on a goal. E.g. ["#@$ #"] gives ["1 box, 0 goals"]. *)
val problems : string list -> string list

(* what the solver found *)
type solution =
  | Moves of string (* the shortest solution, in the LURD notation *)
  | Unsolvable (* every position reachable has been seen *)
  | Gave_up of int (* after that many positions *)

(* [solve ?max_positions rows]: a breadth-first search over the
 * positions (the boxes and the player), one move each step, so that the
 * first solution found is a shortest one, in moves. It is written in
 * the notation Sokoban players exchange solutions in: a letter per
 * move, l, u, r or d, in upper case when the move pushes a box, e.g.
 * "ulDurrD" for the level above (7 moves, 2 of them pushes). Gives up
 * after [max_positions] (100000 by default): a level's positions grow
 * exponentially with its boxes (Sokoban is PSPACE-complete, Culberson
 * 1997). Real solvers search pushes rather than moves, and prune the
 * positions where a box is stuck in a corner (see TinySokoban's
 * exercises). *)
val solve : ?max_positions:int -> string list -> solution
