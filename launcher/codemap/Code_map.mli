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

(* [make ~title ~marked entries]: the map of [entries]; [marked] (paths)
   framed, the program's own files *)
val make : title:string -> marked:string list -> entry list -> t

type action = Stay | Open of Code_file.t * int (* its line, from 0 *) | Close

(* keys as Code_view's; the mouse; Escape closes *)
val update : Playground.computer -> pressed:(string -> bool) -> arrow:string option -> t -> t * action

val view : Playground.computer -> t -> Playground.shape list
