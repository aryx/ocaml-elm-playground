(* St_chunk: the format Smalltalk-80 filed code in and out with.

   A file is chunks of text, each ended by "!" (a "!" inside one
   doubled, "!!"). A chunk is an expression to evaluate -- a class
   defined, a global set -- unless it follows an empty chunk: then it
   is an expression answering a reader, "Point methodsFor: 'arithmetic'",
   and the chunks after it are that category's methods, until an
   empty one.

     Object subclass: #Point
       instanceVariableNames: 'x y'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Graphics-Primitives'!

     !Point methodsFor: 'accessing'!
     x
         ^x!
     y
         ^y! !

   It is how the kernel is written here (kernel/*.st), bootstrapped by
   St_boot, and what the Browser's "file out" writes: the Blue Book's
   world travelled between machines as this text, the sources file of
   every image in it too. *)

type item =
  (* an expression, and where it starts in the file *)
  | Doit of string * int
  (* "Point methodsFor: 'accessing'" (or "Point class methodsFor:"),
   * then the methods' texts and where each starts *)
  | Methods of { class_name : string; meta : bool; category : string; methods : (string * int) list }

exception Error of int * string

val read : string -> item list

(* a chunk written: its "!"s doubled, a "!" after *)
val chunk : string -> string

(* a category of methods written as the reader wants them *)
val methods_chunk : class_name:string -> meta:bool -> category:string -> string list -> string
