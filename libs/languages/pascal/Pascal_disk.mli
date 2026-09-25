(* Pascal_disk: programs to compile and run, a floppy's worth.

   Each shows a part of the compiler or of the machine:

   - HELLO.PAS: the smallest, and a listing short enough to read whole;
   - FACT.PAS: a recursive function, its result in its frame's first
     word; 8! would overflow 16 bits, so it stops at 7!;
   - SIEVE.PAS: Eratosthenes' sieve, an array of booleans;
   - HANOI.PAS: the towers of Hanoi, recursion with four parameters;
   - QUEENS.PAS: Niklaus Wirth's eight queens, from "Algorithms + Data
     Structures = Programs" (Prentice-Hall, 1976), section 3.5, with its
     three arrays of free rows and diagonals -- backtracking;
   - SCOPES.PAS: a procedure inside a procedure, reaching its
     variables through the static link (lod 1,o);
   - PARITY.PAS: two functions calling each other, one declared
     forward;
   - POINTS.PAS: records, an array of them, a var parameter;
   - GUESS.PAS: readln and random, the game every language's book has.

   Their names in capitals, eight characters and an extension, as on a
   CP/M or MS-DOS floppy, where Turbo Pascal lived. *)

val files : (string * string) list
