(* Guess the number: the computer thinks of a number up to a limit you
   choose, and answers each guess with "too low" or "too high".

   The first program of many a BASIC course, in David Ahl's "101 BASIC
   Computer Games" (1973) as GUESS, and a lesson hiding in a game: the
   best strategy is to halve what is left, binary search, so a limit
   of 100 never needs more than 7 guesses (2^7 = 128 > 100). The game
   says so at the end: it computes that bound, the integer part of
   log2 of the limit, plus one, and tells you whether you did as well.

   Run by TinyTerminal's shell as its command guess. *)

val program : unit Teletype.talk

(* the most guesses halving needs for a number from 1 to [limit]:
   7 for 100, 10 for 1000 *)
val best : int -> int
