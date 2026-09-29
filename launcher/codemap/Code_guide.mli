(* Code_guide: what the .codemapconfig files say of the code they sit
   beside (plan_codemap_v2.md) -- one per directory, in jsonnet, written
   by an LLM from what it read of the code and extended by hand -- and
   what the map asks of them: the project in a sentence, a directory's
   or a file's, the places to see first (capitals), the lines that
   matter, the tours, the colours.

   A directory's config speaks of the directory, its files (by their
   names) and its immediate subdirectories (dirs:, for those without a
   config of their own); the root's of the project too (title:). Paths
   in it are relative to it: a colour's, a tour's stop in another
   directory ('../../gamekits/shmup/Shots.ml:def:advance'). The fields,
   each checked (a misspelt one is a mistake, not ignored):

     { title, summary, generated: { by, on }, colors: { path: '#rrggbb' },
       dirs: { name: { summary } },
       files: { name: { summary, digest, capitals: [item], important: [item],
                        links: [{ from, to }], related: [path] } },
       tours: [{ name, stops: [item] }],
       skeletons: [{ name, bones: [{ at: path:anchor, role }], joints: [{ from, to, say }] }],
       views: [{ name, files: [path] } or { name, of: path, with: 'users' }],
       layers: [{ name, rules: [{ text or ref, color: '#rrggbb', say }] }] }

   an item being { at: anchor, say: words, weight: 1 to 3 }.

   claude: a layer (the map's l): the lines containing a rule's text
   (smart case, as the search's text search), each rule's in its colour, all
   lit at once at any level; the colours best named in jsonnet (the
   author), local fork_color = '#e05050', then { text: 'Cap.fork',
   color: fork_color }; or ref: 'Cap.fork', the lines whose code refers
   to Cap.fork (the lexer's references: not a comment's words). Semgrep-
   like patterns later.

   Anchors, the places a config points at, by what they are rather than
   by their line, so that editing the code does not break them:

     def:view       a function's or value's definition
     type:model     a type's (an exception's, a struct's)
     module:Make    a module's
     section:Model  a section's title, the (* Model *) between rules
     comment:"one alien per frame"   the first comment saying those words
     line:42        a line (from 1): discouraged, it moves

   and in a tour, or a link to another file, a path first: 'Shots.ml:def:advance'.

   The checker (check) says what does not hold: a file described that is
   not there, an anchor found nowhere, a related file missing; and, as a
   warning, a file changed since it was described (its digest, the
   first 12 hex digits of its MD5, given anew so that the describer can
   update it). *)

type rgb = int * int * int

type item = { at : string; say : string option; weight : int }

type file_note = {
  summary : string option;
  digest : string option;
  capitals : item list;
  important : item list;
  links : (string * string) list; (* from, to: anchors *)
  related : string list; (* paths, relative to the config *)
}

type tour = { name : string; stops : item list }

(* claude: a skeleton, as in biology: the structure the rest hangs on --
   a game's Model-View-Update, a compiler's lexer, parser, typer and
   code generator -- its bones (an anchor, its file first, relative to
   the config: 'TinyInvaders.ml:def:update', and the bone's role) and
   the joints between them (the bones' anchors, as written), spanning
   files if it must. Resolved: [bpath] the bone's file from the root,
   [banchor] the anchor in it *)
type bone = { bat : string; bpath : string; banchor : string; role : string }
type joint = { jfrom : string; jto : string; jsay : string option }

(* [sdir]: the directory of the config that says it, its level: a
   repository's skeleton at the root, a file's in its directory. A bone
   may be a whole file or directory, a path with no anchor ('games/',
   'Playground.mli': [banchor] ""), an architecture's parts seen from
   afar *)
type skeleton = { sname : string; sdir : string; bones : bone list; joints : joint list }
type view = { vname : string; files : string list; of_ : string option; with_ : string option }

(* claude: a layer's rule: the text of the lines it lights (or, [is_ref],
   the name their code refers to), their colour, what it means *)
type rule = { text : string; is_ref : bool; colour : rgb; rsay : string option }

(* a layer: its name, its config's directory, its rules *)
type layer = { lname : string; ldir : string; rules : rule list }

type dir_note = {
  dir : string; (* the config's directory, relative to the root ("": the root) *)
  title : string option;
  summary : string option;
  colors : (string * rgb) list; (* paths relative to the root *)
  subdirs : (string * string) list; (* an immediate subdirectory's name, its summary *)
  notes : (string * file_note) list; (* a file's name, what is said of it *)
  tours : tour list;
  skeletons : skeleton list;
  views : view list;
  layers : layer list;
}

type t

val empty : t

(* [of_json ~dir json]: a config's value, read and checked *)
val of_json : dir:string -> Json.t -> (dir_note, string) result

(* [load ~read paths]: the configs at [paths] (".codemapconfig",
   "games/shmup/.codemapconfig": relative to the root), evaluated as
   jsonnet, their imports read by [read] too (a path relative to the
   root); the guide, and each config's mistakes *)
val load : read:(string -> string option) -> string list -> t * string list

val dirs : t -> dir_note list

(* the project's sentence (the root's title) *)
val title : t -> string option

(* a directory's sentence: its own config's summary, else its parent's
   dirs:; a file's note, in its directory's config *)
val dir_summary : t -> string -> string option
val file_note : t -> string -> file_note option

(* every config's colours *)
val colours : t -> (string * rgb) list

(* claude: every config's layers *)
val layers : t -> layer list

(* claude: every config's views, their paths from the root (a
   directory's without its final slash); every config's tours, their
   stops' paths from the root ("games/shmup/TinyInvaders.ml:def:march") *)
val views : t -> view list
val tours : t -> tour list

(* the capitals: a file's path, and the item *)
val capitals : t -> (string * item) list

(* the skeletons with a bone in a file (its path from the root) *)
val skeletons_of : t -> string -> skeleton list

(* an anchor, read: its kind and what follows ("def", "view"); with a
   path first, split off ("Shots.ml:def:advance": Some "Shots.ml") *)
val split : string -> string option * string

(* [find f anchor]: the line (from 0) the anchor points at in [f] *)
val find : Code_file.t -> string -> (int, string) result

(* a text's digest, as a config writes it *)
val digest : string -> string

(* [check t ~file ~exists]: the mistakes (Error) and warnings (Ok) of
   the configs, [file] giving a source's lexed file and its text by its
   path, [exists] whether any file is there (a related one may not be a
   source) *)
val check : t -> file:(string -> (Code_file.t * string) option) -> exists:(string -> bool) -> (string, string) result list
