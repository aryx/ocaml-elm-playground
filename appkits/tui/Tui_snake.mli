(* Snake, in characters: a full-screen program of the terminal.

   The snake moves on its own, a cell every tenth of a second or so,
   and turns when told: so the program can't wait for a key, as the
   Teletype games do; it runs on time, an update per key and per tick
   (Tui.mli). Eating the food, a star, makes it one cell longer; its
   own body or the wall ends the game.

   The game is older than terminals' curses -- Blockade (Gremlin, 1976),
   two players' trails in an arcade -- and became everyone's through
   the telephones, Nokia's Snake (Taneli Armanto, 1997); in characters
   it was a curses program on Unix (worm(6), BSD, 1980s), which this
   is after.

   The keys: the arrows, or h j k l as Rogue and vi move (the ADM-3A
   terminal Bill Joy wrote vi on had its arrows on those keys); q
   quits, r starts again after a crash. *)

type model

val program : model Tui.program

(* the cells of the snake, its head first, from (0, 0) the board's
   top-left, and whether it has crashed: for the tests *)
val body : model -> (int * int) list
val crashed : model -> bool
val score : model -> int
