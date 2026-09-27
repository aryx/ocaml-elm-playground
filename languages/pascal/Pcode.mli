(* Pcode: the instructions of the P-machine, Pascal's portable
   computer.

   In 1973 Wirth's group at ETH published Pascal-P: a compiler from
   Pascal to the code of an imaginary stack computer, the P-machine,
   written in Pascal itself, and the P-machine's interpreter, a few
   pages. To bring Pascal to a new computer, one wrote the interpreter
   in its assembler -- days, not the months of a compiler -- and ran the
   compiler on it. It is how Pascal spread to sixty kinds of machine,
   and how UCSD Pascal (Kenneth Bowles, 1977) ran the same p-code on the
   Apple II and the IBM PC; Java's bytecode (1995) is the same idea.

   The machine has one memory, a stack of words, and a few registers:
   pc the next instruction, sp the top of the stack, mp the frame of
   the procedure running (its "mark"). An expression is computed on the
   top of the stack, as a reverse Polish calculator would:

       x := a + b * 2       lod 0,5  lod 0,6  ldc 2  mpi  adi  str 0,4

   A procedure's frame, from mp up (the mark, then its parameters and
   variables, then the expressions being computed):

       mp+0  the function's result
       mp+1  static link: the frame of the procedure it is declared in
       mp+2  dynamic link: the frame of its caller
       mp+3  return address
       mp+4  the parameters, then the local variables...

   Pascal's procedures nest, and an inner one reaches the variables of
   those around it: a variable is addressed by a *level difference* and
   an offset, lod 1,5 being "5 words into the frame one level out", and
   the machine finds that frame by following the static link once
   (twice for lod 2,...). The static links are the scope written in the
   program, the dynamic links the calls made at run time -- recursion
   makes them differ. A display (an array of the frames of each level,
   Dijkstra's, 1960) makes the lookup one step; an exercise.

   The instructions, Pascal-P's mnemonics (without their type suffixes:
   every word here is an integer, a character or a boolean):

     ldc n        push n
     lod d,o      push the word at offset o of the frame d levels out
     lda d,o      push its address
     str d,o      pop into it
     ind o        pop an address, push the word o after it
     sto          pop a value, pop an address, store
     ldm n        pop an address, push the n words there: an array's
                  or a record's value
     stm n        pop n words, pop an address, store them there: an
                  array assigned
     ixa n        pop an index, pop an address: address + index * n
     inc n        add n to the top
     chk lo,hi    stop unless the top is between lo and hi
     adi sbi mpi dvi mod ngi abi sqi odd   arithmetic, 16 bits
     equ neq les leq grt geq and ior not  comparisons and logic
     ujp l / fjp l    jump; jump if the popped value is false
     mst d        mark the stack for a call: result, static link (the
                  frame d levels out), dynamic link, return address
     cup n,l      call: its n words of parameters are on the stack
     ent n        enter: the frame n words long, variables zeroed
     retp / retf  return from a procedure / a function (its result
                  left on the stack)
     csp name     a standard procedure: wri (an integer, a width), wrc
                  (a character), wrb (a boolean), wrs (a string), wln,
                  rdi rdc (read an integer, a character, into an
                  address), rln (to the next line), rnd (random)
     stp          stop *)

type csp =
  | Wri (* pops a width, an integer *)
  | Wrc (* a width, a character *)
  | Wrb (* a width, a boolean *)
  | Wrs of string (* a width *)
  | Wln
  | Rdi (* pops an address *)
  | Rdc
  | Rln
  | Rnd (* pops n, pushes 0 to n - 1 *)
  | Eol (* pushes whether the input line is used up: eoln *)

type instr =
  | Ldc of int
  | Lod of int * int
  | Lda of int * int
  | Str of int * int
  | Ind of int
  | Sto
  | Ldm of int
  | Stm of int
  | Ixa of int
  | Inc of int
  | Chk of int * int
  | Adi | Sbi | Mpi | Dvi | Mod | Ngi | Abi | Sqi | Odd
  | Equ | Neq | Les | Leq | Grt | Geq | And | Ior | Not
  | Ujp of int
  | Fjp of int
  | Mst of int
  | Cup of int * int
  | Ent of int
  | Retp
  | Retf
  | Csp of csp
  | Stp

(* What the compiler leaves for the debugger, besides the code: the
   P-code's DWARF. A debugger sees only a machine running instructions;
   to show a source line and a variable by its name it needs

   - where each statement's code begins, and its line: [statements] (-1
     at the addresses inside a statement), where F7 and F8 stop;
   - each procedure's code, from its entry to its return, and its
     variables: their names, offsets in its frame and types, so that
     "x" read in a paused program is the word at mp + 5, or the one a
     static link away when x belongs to an enclosing procedure. *)

type vtype = Vint | Vbool | Vchar | Varray of int * int * vtype | Vrecord of (string * int * vtype) list

type variable = { vname : string; offset : int; vtype : vtype; by_ref : bool; param : bool }

type procedure = {
  pname : string;
  level : int; (* its body's: the main program's is 0 *)
  parent : int; (* the procedure it is declared in, an index in [procedures]; -1 for the main program *)
  first : int; (* its code: its entry (ent) to its return (retp, retf, stp) *)
  last : int;
  variables : variable list; (* its parameters first *)
}

(* a compiled program: its code, the source line of each instruction
   (for the errors at run time), and the debugger's information; the
   main program is procedures.(0) *)
type program = { code : instr array; lines : int array; statements : int array; procedures : procedure array }

(* the words a frame's mark takes: result, static and dynamic links,
   return address *)
val mark : int

val show : instr -> string

(* the program as a listing: an instruction a line, its address and
   the source line it came from:

       0  ujp 12        ; 1
       1  ent 6         ; 3 ... *)
val listing : program -> string
