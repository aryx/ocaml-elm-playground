(* tinybox's code visualizer, after codemap (Padioleau, 2010; pfff's)
   and SeeSoft (Eick et al., 1992): a program's code as a map
   (Code_map), a file of it read (Code_view). plan_tinybox_codemap.md.

   Which files a program's map shows: its source, and the modules it
   uses, and theirs, as far as they go -- a module named in the code (M.x,
   open M) found by its file's name among the repository's sources (the
   repository's libraries are unwrapped: a module's name is its file's),
   with its interface; a name that is several files' (Test) is left out, and
   so are the platforms (every program runs on one). So TinyMario's map is TinyMario.ml, its kits, the
   Playground and its libraries; w shows the whole repository. *)

type t

(* [make ~sources ~program ~path]: the map of [program], whose main file
   is [path], among [sources] (paths and contents) *)
val make : sources:(string * string) list -> program:string -> path:string -> t

(* None: closed (Escape on the map) *)
val update : Playground.computer -> pressed:(string -> bool) -> arrow:string option -> t -> t option

val view : Playground.computer -> t -> Playground.shape list
