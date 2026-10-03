(* Highlight_st: every token of a Smalltalk chunk file (kernel/*.st)
   given its category (Highlight_code's, shared by every language), the
   colour a code view draws it in -- as Highlight_ml for OCaml and
   Highlight_c for C.

   St_lexer reads a method for the compiler: it drops the comments and
   knows nothing of a file. Here the whole file is read, comments kept,
   and its chunks told apart (St_chunk.mli): a class's definition, the
   line that opens a class's methods, each method.

     Object subclass: #Morph                      Morph: Def_type
       instanceVariableNames: 'bounds owner' ...
     !Morph methodsFor: 'drawing'!                the line: Comment_section,
                                                  one span: a section's title
     drawOn: aCanvas                              drawOn:: Def_function
       | r |                                      aCanvas: Parameter
       r := bounds.                               r: Local
       ^aCanvas fill: r color: Color red          bounds: Field
                                                  ^: Keyword_control
                                                  Color: Global

   What a name is, from the file alone: a method's arguments and a
   block's are Parameters, the temporaries Locals; an instance variable
   is a Field when its class, or a superclass, is defined in the same
   file (the Blue Book's kernel defines its classes in one file and
   their methods in others: there, a Normal name); a capitalized name is
   a Global, a class most often; self, super, nil, true, false and
   thisContext are Keywords, though the language has none; the
   selectors of control (ifTrue:, whileTrue:, do:...) are
   Keyword_control, the other messages Normal.

   For the other files (Highlight_code.analysis): the classes a file
   defines, and its methods as Class>>selector; the classes it names
   and does not define. *)

(* [src]'s tokens, each with its text and its category, in order: for
 * the tests *)
val categorize : string -> (string * Highlight_code.category) list

(* [src] cut into lines, ready to draw *)
val lines : string -> Highlight_code.span list array

(* the same, with where the names bound in the file are (arguments,
 * temporaries, the classes it defines), what it defines and the
 * classes it uses from elsewhere *)
val analyze : string -> Highlight_code.analysis
