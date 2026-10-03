(* Code_roles: a file's role in its project (its tests, its interfaces,
   its per-CPU code, its network code, its entry points...), the code
   map's roles layer (l), after codemap's Archi_code (the author:
   "hopefully you can come with better heuristics than in my
   Archi_code_lexer.mll"; not "architecture", the map's word for its
   layering of who uses whom).

   Codemap's matched substrings of the lowercased path ("core" inside any
   name, "parse" in "sparse", "screen" a UI); here a path is its words --
   each component split at its punctuation and its camelCase, a letter
   followed by a capital (TinyVisiCalc: tiny, visi, calc; x86_64: x86,
   64; the component whole too, amd64) -- and a word is matched whole.
   And a file is also judged by what the map knows of it that its name
   does not say:
   - generated: a .ml beside a .mll or .mly of its name, a .mli beside a
     .mly, a .c beside a .y or .l, y.tab.c;
   - an entry point: a file nothing uses but that uses others (the
     program's main, a game, a tool), from the links (Code_rank.links);
   - per-architecture: assembly (.s, .S, .asm);
   - parsing: .mll, .mly, .y;
   - an interface: .mli, .h.

   A file gets one category, the first of this order that it has
   evidence for, the most specific first: generated, test, example,
   third party, architecture, OS, then the topics (parsing, network,
   graphics, audio, storage, security), utilities, entry point,
   interface; else plain.

   Worked examples (the tests'): kernel/pc/l.s architecture; lib/
   Parser.ml generated beside Parser.mly; tests/Unit_rank.ml test;
   libs/networking/Http.ml network; games/TinyMario.ml, used by no one,
   an entry point; sparse/Matrix.ml plain (no "parse" in "sparse"). *)

type category =
  | Generated
  | Test
  | Example
  | Third_party
  | Architecture
  | Os
  | Parsing
  | Network
  | Graphics
  | Audio
  | Storage
  | Security
  | Utils
  | Entry
  | Interface
  | Plain

(* the order they are tried in, the most specific first *)
val all : category list
val name : category -> string
val colour : category -> int * int * int

(* a path's words, lowercased: "games/TinyVisiCalc.ml" is games, tiny,
   visi, calc, tinyvisicalc (a component whole too), ml *)
val words : string -> string list

(* [categories ~links paths]: each path's category, the paths all the
   map's files ([paths], for the generated ones' sources), [links] the
   files' uses ([(a, b, n)]: a using b), for the entry points *)
val categories : links:(string * string * int) list -> string list -> (string, category) Hashtbl.t
