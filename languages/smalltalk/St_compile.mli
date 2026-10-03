(* St_compile: a method's tree compiled to the Blue Book's bytecodes.

   One pass over the tree (St_ast.mli), emitting as it goes, as the
   Smalltalk-80 compiler did -- which was written in Smalltalk and
   compiled itself; this one is in OCaml, which is what makes the
   bootstrap simple (St_boot.mli). The Smalltalk one is an exercise.

   A name is looked for, in order: the temporaries and arguments of
   the blocks around it and of the method; the instance variables;
   the class variables, up the superclass chain; the globals. The
   first three are pushed by index, a global by its Association, a
   literal of the method.

   **Blocks** are compiled in the middle of their method, as the Blue
   Book did: "[:x | x + 1]" becomes

     push thisContext, push 1, send blockCopy:, jump over the body,
     the body: pop into temporary x, push x, push 1, send +,
     block return top

   blockCopy: makes a BlockContext that starts after the jump. Its
   arguments and temporaries are temporaries *of the method* -- the
   Blue Book's blocks, which is why a block cannot call itself
   recursively (it would overwrite its own arguments).

   **Closures** came in 2008 (Squeak, Eliot Miranda's compiler), and
   are what is compiled when the kernel has a BlockClosure class
   (St_kernel.squeak): every value of a block then runs in a context of
   its own, with its own arguments and temporaries. What is left to
   decide is how a block reaches the temporaries *outside* it, whose
   context may be gone when it runs. Two answers, by what happens to
   them:

   - a temporary that no longer changes once a block has it is
     **copied**: its value is pushed when the block is made and kept in
     the BlockClosure; in the block it is one more temporary, after the
     arguments. "adder: n ^[:x | x + n]" is

       0 push temporary 0 (n)    1 push a closure of 1 argument copying 1
       5 push temporary 0 (x)    6 push temporary 1 (its n)
       7 send +   8 block return top               9 return top

   - one that changes cannot be copied, the copies would part: it lives
     in a **temp vector**, an Array made when its method (or block)
     starts, and it is the vector that blocks copy, all sharing the one
     variable in it. "counter | n | n := 0. ^[n := n + 1]" is

       0 push a new Array of 1   2 pop into temporary 0 (the vector)
       3 push 0                  4 pop into temporary 0 of the vector
       7 push temporary 0        8 push a closure of 0 arguments copying 1
       12 push temporary 0 of the vector in 0   15 push 1   16 send +
       17 store into temporary 0 of the vector  20 block return top
       21 return top

   A temporary "changes" if it is assigned from a block inside the one
   that declares it, or after a block that uses it, or in a loop: a
   simple rule that may put in a vector a temporary that could have
   been copied, never the reverse (a finer one is an exercise).

   Knowing all that takes a look at the whole method before the first
   byte is emitted, so with closures the method is compiled twice: a
   first pass, thrown away, that only learns which temporaries each
   block uses and which change; a second that lays them out and emits.
   The Blue Book's blocks need one.

   to:do: with a literal block is compiled as a loop over a temporary,
   no block made, which would give every turn of the loop the
   same variable; when a block holds it, the loop is sent instead, to
   Number>>to:do:, and each turn has its own (the first pass runs again
   once it has found such a loop). The trap left: a temporary declared
   in a whileTrue:'s literal block is its method's, one for all the
   turns.

   **Inlined messages**: ifTrue:, ifFalse:, ifTrue:ifFalse:,
   ifFalse:ifTrue:, and:, or:, whileTrue:, whileFalse: and whileTrue,
   when their arguments are literal blocks, become jumps and no send
   at all, as in the Blue Book:

     x > 0 ifTrue: ['pos'] ifFalse: ['neg']

     push x, push 0, send >, jump on false over, push 'pos', jump to
     the end, push 'neg'

   So they are fast, and redefining True>>ifTrue: changes nothing:
   the price of the trick, which the notes show.

   A method that does not return explicitly returns self. Every send
   leaves in the pc map where it is and what text it came from, which
   the debugger highlights. *)

type oop = St_memory.oop

(* a mistake: where, and Smalltalk-80's message for it ("Undeclared
 * variable", "Cannot store into") *)
exception Error of int * string

(* [compile m ~cls ~source ~category meth]: the CompiledMethod of a
 * parsed method, in class cls (not installed). With [~declare] (the
 * Workspace, the bootstrap), an unknown name becomes a global holding
 * nil; without, it is an error. *)
val compile : St_memory.t -> cls:oop -> source:string -> ?declare:bool -> St_ast.method_ -> oop

(* parse, compile, and install in the class's method dictionary, filed
 * under [category]: what the Browser's "accept" does. Its selector. *)
val compile_and_install : St_memory.t -> cls:oop -> category:string -> ?declare:bool -> string -> string

(* compile again, from their sources, the methods of a class whose
 * instance variables changed, and of its subclasses; the ones that no
 * longer compile, with their error *)
val recompile : St_memory.t -> oop -> (string * string) list

(* a Workspace's text as the method DoIt of the receiver's class *)
val compile_doit : St_memory.t -> receiver_class:oop -> string -> oop

(* a literal as an object: an Integer, a Float, a Character, a String,
 * a Symbol, an Array *)
val literal_object : St_memory.t -> St_ast.literal -> oop
