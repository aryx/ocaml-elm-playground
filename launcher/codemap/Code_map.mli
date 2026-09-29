(* The code map: files as a treemap (Treemap), directories nested,
   each file's rectangle the size of its lines and filled with its code
   in miniature (codemap's macro level: its lines in as many columns as
   make its characters about twice as high as wide, a pixel's colour
   its character's category).

   Where we go beyond codemap: its map is redrawn around a directory
   you click on, and you read a file in another window. Here the whole
   tree is laid out once, and a camera moves over it -- the wheel zooms
   at the mouse, a drag pans, a click flies to a directory or a file,
   eased, frame by frame; and the zoom is semantic all the way down:
   from afar, the names of what each file defines are written over it,
   large (a function's, a type's, a section's), and close enough, its
   code turns into text that can be read on the map itself.

   claude: read there, in its columns, not in another view: the name
   under the mouse has its binding and uses lit, and a click on it goes
   to its binding (Code_file.name_at, plan_codemap_naming.md); Enter opens
   the file view (Code_view, [Open]).

   The picture is painted into one image per camera, the code's letters
   included (the VGA's font, Vga_font: a file is thousands of characters,
   not shapes), the names and labels over it as shapes. *)

type entry = { path : string; nlines : int; file : Code_file.t Lazy.t }

type t = Code_map_base.t

(* [make ~area ~title ~marked entries]: the map of [entries] in [area],
   its top left corner (in the playground's coordinates) and its width and
   height in pixels; [marked] (paths) framed, the program's own files;
   [numbered] (default false), each file's tab numbered by its place in
   [entries], the order to read them in; [colours] (a directory's
   .codemapconfig's, Code_config) the colours of the parts it names, over
   ours, the roles' and the hashed hues; [roots] the projects' tops, for
   finding a name defined elsewhere (Code_names.find); claude: [guide]
   what the directories' .codemapconfig say (Code_guide) *)
val make :
  ?fan_in:(string, int) Hashtbl.t Lazy.t ->
  ?top_kept:bool ->
  ?numbered:bool ->
  ?colours:(string * (int * int * int)) list ->
  ?roots:string list ->
  ?guide:Code_guide.t ->
  ?beyond:entry list ->
  ?style:Code_map_base.style ->
  area:float * float * int * int ->
  title:string ->
  marked:string list ->
  entry list ->
  t

(* claude: the map framing a unit by its path, a directory's or a
   file's, at once (tinybox codemap <dir> focus=<path>) *)
(* claude: [morph_from ~old t ~now]: [t]'s rectangles animated from
   where [old] shows them (a folder laid out anew, or back) *)
val morph_from : old:t -> t -> now:float -> unit

(* claude: a unit flown to, a definition (its line) peeked at: back from
   the matrix *)
val go_back_to : t -> string -> int option -> t

(* claude: whether a directory or file (its path) is on the map *)
val has : t -> string -> bool

val focus_on : t -> string -> t

type action =
  | Stay
  | Open of Code_file.t * int (* its line, from 0 *)
  | Close
  | Select of string * string list (* claude: directories and files to see together (a search's name//, or shift+Enter; a folder flown into, laid out anew), and what to call them *)
  | Up (* claude: up from the map's top: back to the map it was taken from *)
  | Tied of string * string list * string list (* claude: a unit, its users, what it uses: shift+click's view *)
  | Graph of string * string list (* claude: a unit and the units tied to it, in codegraph's matrix: ctrl+click's, g's *)

(* keys as Code_view's; the mouse; Escape closes. claude: / opens the
   search (Map_v2's), which takes the keys while open *)
val update : Playground.computer -> pressed:(string -> bool) -> arrow:string option -> t -> t * action

(* the map, and with [chrome] (the default) the screen round it: its
   background, the title, what is under the mouse, the keys; without, the
   map alone, where the caller shows it (tinybox's panel) *)
val view : ?chrome:bool -> Playground.computer -> t -> Playground.shape list

(* claude: the files in the map's order (the reading order, Codemap's),
   a file's number in it if numbered, and the stops of Codemap's tour in
   a file (line from 0, what is there): its header, its sections, and the
   places saying Code_file.trick; lexes the file *)
val entries : t -> entry list
val number : t -> string -> int option
val stops : entry -> (int * string) list

(* the number of files shown, and of their lines; lines_text 12345 is
   "12,345 lines" *)
val files : t -> int
val lines : t -> int
val lines_of : entry list -> int
val lines_text : int -> string

(* A magnifying glass at the mouse, when it is over the map: the part
   under it painted again closer (enough for its code to be read: its
   lines about 16 units high, the VGA font's size), with a rim and a
   handle. Round, a glance at the code; or a reading glass wide enough for
   80 columns and some 16 lines, lined up with the start of the lines
   under the mouse, to read whole lines; or none, at first. o (in update, or
   cycle_glass) goes from one to the next, one setting for every map.
   Nothing when the mouse is elsewhere. *)
(* claude: [panel], the menu's panel's glass, its own setting (round at
   first), the map's being none at first *)
val glass : ?panel:bool -> Playground.computer -> t -> Playground.shape list

val cycle_glass : ?panel:bool -> unit -> unit

(* the glass now, for a hint: "round", "wide" or "none" *)
val glass_name : ?panel:bool -> unit -> string

(* claude: the map's style (Code_map_base.style: Map_classic, today's;
   Map_streets, plan_codemap_google_maps.md's; Map_atlas, the street
   map with the files' heat and the parts' roads), one setting for every
   map as the glass's: m (in update, or cycle_style) goes to the next;
   choose_style by its name (the flag style=, "classic", "streets" or
   "atlas");
   a map is made in the style chosen *)
val cycle_style : unit -> unit
val choose_style : string -> unit
val style_name : unit -> string

(* claude: the search box open, or a config's tour under way: the keys
   are theirs (Codemap's n, p, w too) *)
val searching : t -> bool
