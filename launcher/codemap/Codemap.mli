(* tinybox's code visualizer, after codemap (Padioleau, 2010; pfff's)
   and SeeSoft (Eick et al., 1992): a program's code as a map
   (Code_map), a file of it read (Code_view). plan_tinybox_codemap.md.

   Which files a program's map shows, w going from one to the next:
   - its code (the default): its source, and the modules it names that
     are its folder's or a kit's (gamekits/, appkits/), and theirs --
     TinyEmacs and appkits/editor's Tui_emacs, Emacs_editor, Emacs_simple,
     Gap_buffer -- each in its folder, for finding it later;
   - with what it uses: the same, into the Playground and libs/ too, as
     far as they go;
   - the whole repository.
   A module named in the code (M.x, open M) is found by its file's name
   among the repository's sources (the libraries are unwrapped: a
   module's name is its file's), with its interface; a name that is
   several files' (Test) is left out, and so are the platforms (every
   program runs on one). *)

type t

(* [make ~sources ~program ~path]: the map of [program], whose main file
   is [path], among [sources] (paths and contents) *)
val make : sources:(string * string) list -> program:string -> path:string -> t

(* None: closed (Escape on the map) *)
val update : Playground.computer -> pressed:(string -> bool) -> arrow:string option -> t -> t option

val view : Playground.computer -> t -> Playground.shape list
