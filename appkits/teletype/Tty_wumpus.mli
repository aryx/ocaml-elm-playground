(* Hunt the Wumpus (Gregory Yob, 1973): a cave of twenty rooms in the
   dark, a Wumpus asleep in one of them, and five crooked arrows.

   The hide-and-seek games of the People's Computer Company's
   newsletter (Hurkle, Snark, Mugwump: find the creature on a 10 by 10
   grid from "north-east" or "distance 4" hints) bored Gregory Yob with
   their grid, so he put his creature on a dodecahedron: twenty
   vertices, each joined to three, the regular solid with no up or
   left. The player can't see the cave; each room tells only its number
   and the three tunnels out, and what is near: a smell (the Wumpus), a
   draft (a bottomless pit), a rustle (the super bats, which carry you
   anywhere). Mapping the cave in your head, or on paper, is the game.
   (Names and dates from memory, to check.)

       the cave as the listing numbers it: three rings, and the
       tunnels between them (Schlegel's diagram of the dodecahedron,
       flattened: a pentagon, a decagon around it, a pentagon around
       that)

         outer ring (5)     1 - 2 - 3 - 4 - 5 - 1
         spokes             1-8   2-10   3-12   4-14   5-6
         middle ring (10)   6 - 7 - 8 - 9 - 10 - 11 - 12 - 13 - 14 - 15 - 6
         spokes             7-17  9-18   11-19  13-20  15-16
         inner ring (5)     16 - 17 - 18 - 19 - 20 - 16

       5 + 5 + 10 + 5 + 5 = 30 tunnels, three from each room (the
       tests check it against [tunnels])

   Shooting is the clever part: an arrow is told the rooms it will fly
   through, up to five, so you can shoot around corners at a Wumpus
   smelt two rooms away. Name a room with no tunnel from the last one,
   and the arrow flies somewhere at random -- it could find you. Miss,
   and the noise wakes the Wumpus, which moves to a neighbouring room
   three times in four.

   The Wumpus world became artificial intelligence's teaching example:
   Russell and Norvig's "Artificial Intelligence: A Modern Approach"
   (1995) reasons about its smells and drafts in propositional logic,
   what a good player does on paper.

   References: Gregory Yob, "Hunt the Wumpus", Creative Computing
   (September-October 1975); David H. Ahl, "The Best of Creative
   Computing, Volume 1" (1976). *)

(*****************************************************************************)
(* {1 The cave} *)
(*****************************************************************************)

(* [tunnels.(r)]: the three rooms joined to room [r], from 1 to 20
   ([tunnels.(0)] is unused, the listing counting from 1) *)
val tunnels : int array array

type cave = {
  you : int;
  wumpus : int;
  pits : int list;
  bats : int list;
  arrows : int;
}

type outcome = Won | Lost

(*****************************************************************************)
(* {1 The game} *)
(*****************************************************************************)

(* [play cave]: turns until the Wumpus is shot, or you are lost *)
val play : cave -> outcome Teletype.talk

(* a cave set up at random (six different rooms: you, the Wumpus, two
   pits, two bats), the instructions if asked, games until you stop *)
val program : unit Teletype.talk
