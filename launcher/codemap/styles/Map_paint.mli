(* Map_paint: Map_atlas's picture, the pixels under the names, and the
   three levels it is painted at.

   From afar (the earth, a region), a file is its columns: a strip per
   column of 80 characters, the last as long as the lines left, so a
   file's length is seen at a glance; no code, it is clutter. When the
   unit looked at is a file and the camera is on it, the ground
   (Code_ground): the whole map is that file, each line as high as it
   matters. There, a, the street (Code_street): the file on the left,
   the files it uses (or its users, or both) in panels beside it.

   The other parts of Map_atlas draw over this picture, and ask this
   module where they are: at_ground says whether the map is a file's,
   ground_of and street_of give the layout painted, the same one (kept
   from a frame to the next), so that a note, a road or a glow falls on
   the line it is about. *)

(* the map as an image: the columns from afar, the ground or the street
   when at_ground; [aa] the letters antialiased *)
val paint : aa:bool -> Code_map_base.t -> Code_map_base.camera -> Rgba_image.t

(* a unit outside the one looked at (Code_units, t.focus): drawn in the
   shade, its names faint; never at the earth, where nothing is looked
   at *)
val outside : Code_map_base.t -> Code_map_base.entry Treemap.placed -> bool

(* The ground *)

(* the file the map is, if the unit looked at is a file and the camera
   has arrived on it (during the flight there, None: the treemap still) *)
val at_ground : Code_map_base.t -> Code_map_base.camera -> Code_map_base.entry option

(* what the config calls important in a file, found in it: each line, its
   weight (1 to 3; a capital 3) and its words if any. Kept by file: an
   anchor found is a search of the file's lines *)
val important : Code_map_base.t -> Code_map_base.entry -> (int * int * string option) list

(* the file's layout at the ground, in the map's pixels: the one painted *)
val ground_of : Code_map_base.t -> Code_map_base.entry -> Code_ground.t

(* The street *)

(* t.street_mode as Code_street's: 1 the uses, 2 the users, 3 both *)
val street_mode : Code_map_base.t -> Code_street.mode

(* the street of a file, in the map's pixels: its own layout and its
   panels', the one painted *)
val street_of : Code_map_base.t -> Code_map_base.entry -> Code_street.t

(* the street mode that fits the file looked at: 3 its uses and users,
   1 its uses only, 2 its users only (a's first press) *)
val best_street_mode : Code_map_base.t -> int

(* at the street, its panels' files (not the focus); else none *)
val street_files : Code_map_base.t -> string list
