(* Basic_disk: a floppy of BASIC programs, as a 1978 Apple II user
   had them, of our own writing in the style of David Ahl's books.

   On the Apple II's disk (DOS 3.3, 1980; 3.1, 1978), CATALOG listed the
   files, each with its type -- I for an Integer BASIC program, A for
   an Applesoft one -- and its size in sectors of 256 bytes; LOAD read
   one in, RUN read it in and ran it, switching to its BASIC first if
   need be, which is how one disk held programs for both. SAVE wrote the
   program in memory; here it lasts as long as the session, the disk
   being a value (Playground_platform.store would make it last: an
   exercise).

   The programs, each a page:

   - GUESS (Integer): Guess the Number, the same game as Tty_guess,
     which the tests check line by line on the same answers;
   - BAGELS (Applesoft): the number game of the Lawrence Hall of
     Science (1970s), in Ahl's books: three different digits, and for
     each guess, PICO for a digit in the wrong place, FERMI for one in
     the right place, BAGELS for none;
   - MANDEL (Applesoft): the Mandelbrot set in characters, 79 by 23,
     a character per point for how quickly it escapes -- the fractal
     that 1980s magazines printed as listings, a few minutes on an
     Apple II, a few frames here;
   - SIERPINSKI (Applesoft): Pascal's triangle with its odd numbers as
     stars, which draws Sierpinski's triangle (1915): the fractal from
     the arithmetic, no geometry;
   - SINE (Applesoft): a word riding a sine wave down the screen,
     after Ahl's SINE WAVE, the one-screen program every 1978
     magazine had;
   - MATCHES (Integer): 23 Matches, a Nim of one heap -- take 1, 2 or
     3, the last one loses; the computer leaves 4K + 1 when it can,
     so the player moving first wins only by doing the same;
   - ANIMAL (Applesoft): the program that learns. It guesses an
     animal by yes-or-no questions down a binary tree, and when it is
     wrong asks for the animal and a question telling it apart: the
     leaf becomes a question with two leaves. Kept in three arrays,
     the tree grows for as long as the program runs;
   - LUNAR (Applesoft): landing on the Moon a burn at a time, a table
     of seconds, height, speed and fuel; the first of the lunar
     landers, in text, before Atari's (TinyLunarLander).

   Each listing names its sources in its first REMs, as the magazines
   credited theirs (from memory, to check): David Ahl's "101 BASIC
   Computer Games" (DEC, 1973) and "BASIC Computer Games" (Creative
   Computing, 1978), where Bagels, Animal, 23 Matches, Lunar and Sine
   Wave are; Mandelbrot's book (1982) and A. K. Dewdney's "Computer
   Recreations" in Scientific American (August 1985), which set the
   world to drawing the set. The listings are ours, not the books'. *)

(* a file: its name, its BASIC, its lines ("10 PRINT ...") *)
type file = { name : string; dialect : Basic_run.dialect; lines : string list }

val files : file list

(* Guess the Number's listing, GUESS's lines *)
val guess : string list

(* the CATALOG of these files, as DOS 3.3 printed it:
     DISK VOLUME 254

      I 002 GUESS
      A 003 BAGELS
   the sectors a program's text would take, rounded up *)
val catalog : file list -> string
