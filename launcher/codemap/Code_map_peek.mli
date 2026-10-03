(* Code_map_peek: what a peek shows, whatever style draws it. A click
   on the map read up close does not leave it: the definition clicked
   is shown over the map (Map_peek, the atlas's), and a click inside it
   on another name peeks at that one over the first, a stack.

   Here, the questions before the drawing: which definition a click
   means, which lines of its file are its own, and the stack kept in
   the map (t.peek, t.peek_stack, t.peek_scroll). *)

(* [peek_where t f path line col]: the definition a click at a line and
   column of [path]'s file [f] peeks at, as a file and a line: the
   binding of the name there, or, if it is defined elsewhere, where,
   found among the sources (the map's, and those beyond it); else the
   line's own definition *)
val peek_where : Code_map_base.t -> Code_file.t -> string -> int -> int -> (string * int) option

(* [peek_extent f path line]: the lines a peek at [line] of [path]'s
   file shows, first and last: a top-level comment whole, else the
   definition with the comment just above it *)
val peek_extent : Code_file.t -> string -> int -> int * int

(* a section's lines: from its title's to the one before the next
   section's banner *)
val section_extent : Code_file.t -> int -> int * int

(* [open_peek t file_of (path, line)]: that definition peeked at, on top
   of the one open if any (four at most, each keeping its scroll);
   [file_of], a path's file, lexed. Nothing if the path has none *)
val open_peek : Code_map_base.t -> (string -> Code_file.t option) -> string * int -> unit

(* the top peek closed: the one under it back, or none *)
val close_peek : Code_map_base.t -> unit
