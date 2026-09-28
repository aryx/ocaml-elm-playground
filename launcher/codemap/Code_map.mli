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

   The picture is painted into one image per camera, the code's letters
   included (the VGA's font, Vga_font: a file is thousands of characters,
   not shapes), the names and labels over it as shapes. *)

type entry = { path : string; nlines : int; file : Code_file.t Lazy.t }

type t

(* [make ~area ~title ~marked entries]: the map of [entries] in [area],
   its top left corner (in the playground's coordinates) and its width and
   height in pixels; [marked] (paths) framed, the program's own files;
   [numbered] (default false), each file's tab numbered by its place in
   [entries], the order to read them in; [colours] (a directory's
   .codemapconfig's, Code_config) the colours of the parts it names, over
   ours, the roles' and the hashed hues *)
val make :
  ?numbered:bool -> ?colours:(string * (int * int * int)) list -> area:float * float * int * int -> title:string -> marked:string list -> entry list -> t

type action = Stay | Open of Code_file.t * int (* its line, from 0 *) | Close

(* keys as Code_view's; the mouse; Escape closes *)
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
   under the mouse, to read whole lines; or none. o (in update, or
   cycle_glass) goes from one to the next, one setting for every map.
   Nothing when the mouse is elsewhere. *)
val glass : Playground.computer -> t -> Playground.shape list

val cycle_glass : unit -> unit

(* the glass now, for a hint: "round", "wide" or "none" *)
val glass_name : unit -> string
