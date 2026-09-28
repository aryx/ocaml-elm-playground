(* Highlight_ml: every token of an OCaml file given its category
   (Highlight_code's, shared by every language), the colour a code
   view draws it in.

   A category says more than a token's kind: a lowercase name is a
   function being defined, a parameter, another module's value, a type
   or a capability. From the tokens alone, for now: codemap's fallback
   when a file does not parse, looking at a token's neighbours
   (Highlight_ml.ml says which rule sees what). What they cannot tell
   -- a name shadowed, a local of a nested function -- waits for the
   parser (plan_tinybox_codemap.md, step 3).

   Worked example (the tests'):

     let move (p : point) ~dx = Point.add p dx

     let: Keyword         move: Def_function    p: Parameter
     point: Type          ~dx: Label            Point: Module
     add: Global          p, dx: Parameter *)

(* the tokens of a file, each with its category *)
val categorize : Token_ml.t list -> (Token_ml.t * Highlight_code.category) list

(* [src] lexed, categorized and cut into lines, ready to draw *)
val lines : string -> Highlight_code.span list array

(* claude: the same, and where the names bound in the file are (the
   parameters and locals, and the top-level values, types and
   constructors, the latest before a use: Highlight_code.occurrence),
   and for the other files what it defines at its top, the names it
   uses that are defined elsewhere (M.x, or not here) and its opens --
   from one parse (Highlight_code.analysis) *)
val analyze : string -> Highlight_code.analysis
