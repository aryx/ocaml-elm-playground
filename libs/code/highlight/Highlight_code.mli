(* Highlight_code: what a code view needs to colour a file, in any
   language.

   codemap's split (pfff's highlight_code/, 2010): the categories, their
   colours and a file as lines of coloured spans are the same for every
   language; only turning a language's tokens into categories is its own
   (Highlight_ml for OCaml, later Highlight_c...). So a view draws any
   language it is given the spans of.

   The categories are a subset of codemap's (Highlight_code.category,
   some 80), with its colours ("pad taste", on a dark background), and
   one of our own: [Capability], a Cap.* type or a caps argument, where a
   program's authority is (the repository's capabilities). *)

type category =
  | Comment
  | Comment_section (* a banner, (*****...*) or its title *)
  | Keyword
  | Keyword_control (* if, match, while, for... *)
  | Keyword_module (* module, struct, #include... *)
  | Def_function (* where a global function is defined *)
  | Def_value (* ... a global value *)
  | Def_type (* ... a type, an exception, a struct *)
  | Def_module (* ... a module *)
  | Parameter (* a function's *)
  | Local (* a name defined inside a function *)
  | Global (* M.x: another module's value *)
  | Module (* M in M.x *)
  | Constructor (* Some, true, an enum's value *)
  | Type
  | Type_var
  | Label
  | Capability
  | Number
  | String
  | Operator
  | Punctuation
  | Attribute (* [@...], a preprocessor's line *)
  | Normal
  | Error
  | Field (* a record's field, where it is declared and where it is read: p.x *)

val show : category -> string

(* every category, and a category's place in [all]: a byte per
   character is enough to remember a file's colours (codemap's
   overview, a picture of a whole file) *)
val all : category array
val index : category -> int

(* codemap's colours, as red, green, blue from 0 to 255, for a dark
   background ([background]) *)
val rgb : category -> int * int * int
val background : int * int * int

(* how much bigger codemap draws a category, far away: definitions
   stand out so that a whole file seen from afar shows what it defines
   (its semantic zoom, Style.size_font_multiplier_of_categ) *)
val emphasis : category -> float

(* A file ready to draw: line by line, each a list of spans. A span is a
   token's piece on one line (a comment over three lines is three
   spans), its column where it starts, in bytes. *)
type span = { col : int; text : string; category : category }

(* claude: a name bound in a function (a parameter, a local), where it
   is: its line (from 0), column and length, and where its binding is
   (line, column), its own place at the binding. The uses of a name are
   the occurrences of one binding (plan_codemap_naming.md, level 1: the
   language's scopes, exact) *)
type occurrence = { line : int; col : int; len : int; bound_at : int * int }

(* [occurrences tokens binds]: from a language's tokens (their line from
   1, column and text) and its resolver's [binds] (a name's token index
   to its binding's), the occurrences *)
val occurrences : (int * int * string) array -> (int, int) Hashtbl.t -> occurrence list

(* [lines src tokens]: [src]'s lines, from its tokens, each given as its
   first line (from 1), its column (from 0), its text and its category;
   what lies between the tokens (spaces) is not in a span. *)
val lines : string -> (int * int * string * category) list -> span list array
