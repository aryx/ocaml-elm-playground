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
   recursively (it would overwrite its own arguments). Real closures
   came in 2008 (Squeak, Eliot Miranda): an exercise.

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
