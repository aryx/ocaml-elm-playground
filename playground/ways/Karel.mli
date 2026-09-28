(* Karel the Robot, on top of the Playground.

   Richard E. Pattis, at Stanford, 1981 ("Karel the Robot: A Gentle
   Introduction to the Art of Programming", named after Karel Capek,
   whose play R.U.R., 1920, gave us the word robot): a robot in a city
   of streets (the rows, counted from the bottom) and avenues (the
   columns, from the left), walls between some corners, and beepers on
   some corners, which it can pick up into its bag and put down. It
   can do five things:

     move          one corner ahead (into a wall: an error, it stops)
     turn_left     a quarter turn -- and no turn_right: teach it one
     pick_beeper   one beeper from its corner into its bag
     put_beeper    one from its bag onto its corner
     turn_off      the end

   and ask about what it can see from where it stands: is the way ahead
   (or on its left, its right) clear, is there a beeper here, is it
   facing north, has it any beeper in its bag. Nothing else: no
   coordinates, no numbers, no variables. The one real idea is that
   you teach it new words:

     Pattis:  DEFINE-NEW-INSTRUCTION turnright AS
              BEGIN turnleft; turnleft; turnleft END
     OCaml:   let turn_right = block [ turn_left; turn_left; turn_left ]

   and then a harvest is a row harvested twice, a row is a beeper picked
   five times, and the program reads like the task. It is the
   anti-Logo (Logo.mli): the turtle knows where it is and draws; Karel
   only knows what is around it, which makes it about procedures and
   decomposition before anything else. Forty years of first lectures
   open with it (Stanford's CS106A still does, in Java).

   A program is data, as in Logo: a [command list], run a primitive at
   a time ([step]) so that it can be watched. Pattis's language itself,
   BEGINNING-OF-PROGRAM and all, is TinyKarel's, a parser that builds
   these commands; so is [call], a new instruction by name, which a
   parser needs (its definitions can call each other, and themselves);
   an OCaml program uses its own let instead.

   A world is written as strings, a character per corner, and one
   between corners for a wall:

     ". . . ."      a corner: '.' empty, '1' to '9' beepers,
     "  -    "      Karel '>' '<' '^' 'v' (facing east, west,
     ".|> . 2"      north, south; on an empty corner)
                    between two corners on a row: '|' a wall
                    on the rows between: '-' a wall under that corner

   so the strings of a world W avenues wide and S streets high are
   2S - 1 of 2W - 1 characters, the top street first; the city's edges
   are walls. E.g. above, a city 4 avenues wide and 2 streets high:
   Karel on 2nd Avenue and 1st Street facing east, a wall behind it
   (west) and one on its left (north), two beepers on 4th Avenue.

   References: Richard E. Pattis, "Karel the Robot: A Gentle
   Introduction to the Art of Programming", Wiley, 1981 (its language,
   its 18 conditions, its exercises: the harvest, the stairs, the maze);
   Stanford's "Karel the Robot Learns Java" (Eric Roberts, 2005), whose
   first assignment, the newspaper, is TinyKarel's first level. Its
   descendants as games: Lightbot (Danny Yaroslavski, 2008), and Human
   Resource Machine (Tomorrow Corporation, 2015).
*)

type direction = North | East | South | West

(*****************************************************************************)
(* {1 The world} *)
(*****************************************************************************)

type world

(* [world ?bag rows]: the world the strings draw (see above), Karel
   with [bag] beepers (default 0) *)
val world : ?bag:int -> string list -> world

(* back to strings, the same way (Karel on a corner hides its beepers) *)
val to_strings : world -> string list

(* [beepers w avenue street]: how many on that corner *)
val beepers : world -> int -> int -> int

(* all the beepers still in the world *)
val beepers_left : world -> int

(* where Karel is: its avenue, its street, which way it faces; and how
   many beepers in its bag *)
val karel : world -> int * int * direction
val bag : world -> int

(*****************************************************************************)
(* {1 The program} *)
(*****************************************************************************)

type condition

val front_is_clear : condition
val left_is_clear : condition
val right_is_clear : condition
val next_to_a_beeper : condition
val any_beepers_in_beeper_bag : condition
val facing : direction -> condition

(* the other nine of Pattis's eighteen: front_is_blocked is
   [not_ front_is_clear], and so on *)
val not_ : condition -> condition

type command

val move : command
val turn_left : command
val pick_beeper : command
val put_beeper : command
val turn_off : command

(* [iterate n body]: Pattis's ITERATE n TIMES *)
val iterate : int -> command list -> command

(* [if_ c body ~else_]: IF c THEN body ELSE else_ (default nothing) *)
val if_ : ?else_:command list -> condition -> command list -> command

(* [while_ c body]: WHILE c DO body *)
val while_ : condition -> command list -> command

(* [block body]: [body], as one command: a new instruction's *)
val block : command list -> command

(* [call name]: the instruction [name] of the [definitions] given to
   [start]: how a parser writes a use of DEFINE-NEW-INSTRUCTION, which
   may come before its definition, or be inside it (recursion) *)
val call : string -> command

(*****************************************************************************)
(* {1 Running} *)
(*****************************************************************************)

type status =
  | Running
  | Finished (* turned off, or nothing left to do *)
  | Error of string (* Pattis's "error shutoff": into a wall, no beeper to pick ... *)

type run

val start : ?definitions:(string * command list) list -> world -> command list -> run

(* [step r]: up to and including the next of the five primitives (the
   loops and tests before it are free, as far as watching goes) *)
val step : run -> run

val status : run -> status
val current : run -> world

(* the primitives done so far *)
val steps : run -> int

(* [execute w program]: run to the end, or an error after [max_steps]
   primitives (default 10000: a loop that never ends) *)
val execute : ?definitions:(string * command list) list -> ?max_steps:int -> world -> command list -> run

(*****************************************************************************)
(* {1 Drawing} *)
(*****************************************************************************)

(* [draw ~size w]: the city, [size] pixels between corners (default
   50), centered on (0, 0): the corners as dots, the walls, the beepers
   (a diamond, and how many if more than one), and Karel *)
val draw : ?size:Playground.number -> world -> Playground.shape

(*****************************************************************************)
(* {1 Applications} *)
(*****************************************************************************)

(* [picture w program]: the world once the program has run *)
val picture : world -> command list -> (Playground.screen, Playground.msg1) Playground.app

(* [animation ?speed w program]: Karel running it, [speed] primitives a
   second (default 4); space runs it again *)
val animation : ?speed:Playground.number -> world -> command list -> (run Scene2d.t Playground.game, Playground.msg) Playground.app
