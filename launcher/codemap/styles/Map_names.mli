(* Map_names: the names over Map_atlas's picture, what makes it a map.

   A directory is its name, the programmer's own choice: centred on its
   block, as large as fits (a region's up to 64 pixels, deeper ones
   smaller), standing up when the block is tall and narrow. A file big
   enough on the screen has its name on a tab, and its table of
   contents, its sections' titles where they are. The capitals, the few
   definitions the configs name (Code_guide.capitals), are a dot where
   they are and their name.

   All of them are candidates, each with a rank; they are placed
   greedily, the most important first, none over another
   (Code_map_base.place's way). What is kept is the list [names] gives,
   each with its box: the picture's labels, and what the mouse is over
   (unit_at, the hover cards of Map_cards). *)

(* a name on the map: the unit it names (its index in t.placed), its box
   in the map's pixels (left, top, right, bottom), its rank among the
   candidates, the shape that draws it; [said], the lines of a card when
   the name carries its own (a capital's); [sect], a section's title, its
   file and line; [cap], a capital, its file, line and name *)
type name = {
  node : int;
  nbox : float * float * float * float;
  nrank : float;
  draw : Playground.shape;
  said : string list option;
  sect : (string * int) option;
  cap : (string * int * string) option;
}

(* the names kept for this camera, the most important first *)
val names : Code_map_base.t -> Code_map_base.camera -> name list

(* the directory or file whose name is under a pixel of the map ([q],
   px, py): its index in t.placed. A section's title is not a unit *)
val unit_at : Code_map_base.t -> Code_map_base.camera -> float -> float -> float -> int option

(* the line of an anchor ("def:march", Code_guide.find) in a file, found
   once; None when the file has no such thing *)
val capital_line : Code_map_base.entry -> string -> int option

(* What the cards and boxes share *)

(* words cut into lines of at most [width] characters *)
val wrap : int -> string -> string list

(* [within (x0, y0, x1, y1) x y]: the point in the box, its right and
   bottom edges out *)
val within : float * float * float * float -> float -> float -> bool
