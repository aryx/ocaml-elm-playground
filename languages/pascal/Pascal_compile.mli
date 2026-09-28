(* Pascal_compile: Pascal to P-code, in one pass.

   Wirth's compilers read the program once, from left to right, and
   emit the code as they recognize it: there is no syntax tree. Each
   rule of the grammar is a function (recursive descent, as in
   Formula.mli), and each function, as it parses, checks the types and
   writes the instructions of what it parsed:

       parse_term reads "b * 2":
         parse_factor: b is a variable at offset 6   -> lod 0,6
         sees *, parse_factor: 2 is a constant       -> ldc 2
         both integers, so                           -> mpi

   The code of a term comes out in reverse Polish order simply because
   an operator is emitted after its operands are parsed. That is the
   whole code generator of a one-pass compiler, and it is why Pascal's
   grammar is what it is: everything declared before it is used
   (constants, types, variables, then procedures, then the body), so
   that when the parser meets a name it already knows what it is and
   where it lives. Where that can't be -- two procedures calling each
   other -- Pascal has "forward": the heading first, the body later.

   Jumps forward (the end of an if, a call to a procedure compiled
   later) go to *labels*, numbered as they are needed and given an
   address when the parser gets there; a last step replaces each label
   by its address. Pascal-P patched its jumps in place instead
   (backpatching): the same idea.

   The language: Wirth's Pascal (the User Manual and Report, 1974)
   without reals, sets, pointers, files, variant records, goto and
   with: integer (16 bits, as on the machines of Turbo Pascal's time:
   maxint is 32767), boolean, char, subranges (1..10, checked when
   assigned), arrays (of arrays: a[i, j]) and records; constants
   (strings too, for write); procedures and functions, nested, with
   value and var parameters, recursion and forward; if, case (with
   Turbo Pascal's else), while, repeat, for; write, writeln (a width
   after a colon: write(x:5)), read, readln, and the functions abs, sqr,
   odd, ord, chr, succ, pred, eoln and Turbo's random(n).

   The first error stops the compilation, with its line and column, as
   Turbo Pascal did (it then put the editor's cursor there): "Error:
   Type mismatch". *)

type error = { line : int; col : int; message : string }

val compile : string -> (Pcode.program, error) result
