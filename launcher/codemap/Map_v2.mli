(* Map_v2: the code map redesigned (plan_codemap_v2.md; the name to be
   found), the default of tinybox codemap <dir>.

   The earth and region levels (the plan's step 1): no code from afar,
   it is clutter. A file is its columns -- a strip per column of 80
   characters, the last one as long as its lines, so a file's length is
   seen at a glance (codemap's column hint) -- and a directory is its
   name, the programmer's own choice: large, a region's over its
   subdirectories', standing up when its block is tall and narrow,
   clickable (the camera flies to it), and hovering it gives its card.
   A card says what the directories' .codemapconfig say of it
   (Code_guide, written by an LLM, extended by hand), and the counts.
   Near enough to be read, a file's code, as Map_classic's; and when the
   unit looked at is a file, the ground level (Code_ground): the whole
   map is that file, each line as high as it matters, the config's
   important lines marked in the margin, their words as notes after
   them (the plan's step 5). There, a: the street level (Code_street),
   the file on the left, the files it uses on the right, its own code's
   first (its kits), roads from each use to its definition (step 6);
   tinybox codemap <dir> focus=<path> opens the map on a unit.

   At the region level (a directory looked at), a file big enough shows
   its name on a tab, its card (what the configs say of it) and its table
   of contents (its sections' titles where they are). At the ground and
   the street, a click shows a definition's body readable
   over the map (a peek: the name's under the mouse, found elsewhere if
   defined elsewhere; else the line's own definition), a click or
   Escape closing it.

   x, the X-ray, at every level: the configs' skeletons (Code_guide), the
   rest in the shade -- from afar, a dot per file (a file whose bones are
   close together one dot, its skeleton's name) and the joints between
   them across the map; at the ground and the street, the bones'
   definitions lit, their roles, the joints between them, a bone in a
   panel reached across files, one off the map a stub naming it. In the
   X-ray, 1 to 6 the anatomy's plates (Code_anatomy): the skeleton, the
   blood running along its joints, the muscles, the nerves, the lungs,
   the skin; a legend, the atlas's key.

   The camera moves a unit at a time (the plan's step 2, Code_units, in
   Code_map.update): the wheel or a click one level in, the wheel back,
   a right click or - one level out, the arrows to a neighbour. The unit
   looked at and those holding it are named on a breadcrumb at the top
   left (clickable too), not over the map; outside it, the map is in the
   shade, the neighbours' names faint, their files' gone. *)

(* the picture, and the names (Code_map_base.style's fields) *)
val paint : aa:bool -> Code_map_base.t -> Code_map_base.camera -> Rgba_image.t
val labels : Code_map_base.t -> Code_map_base.camera -> float -> Playground.shape list

(* the directory or file whose name is under a pixel of the map ([q],
   px, py): its index in [placed] *)
val unit_at : Code_map_base.t -> Code_map_base.camera -> float -> float -> float -> int option

(* claude: the search (/, Code_search): its hits now (the files shown
   only if asked), and the directories a query ending in // takes
   together *)
val search_hits : Code_map_base.t -> Code_search.hit list
val search_named : Code_map_base.t -> string list

(* claude: all a search found, its directories and files (else its
   definitions' files), to see together (shift+Enter) *)
val search_set : Code_map_base.t -> string list

(* claude: the groups of layers l cycles through: those kept (ctrl+Enter
   in the search), then each config's, their names *)
val layer_groups : Code_map_base.t -> (string * Code_map_base.layer list) list

(* claude: the match (a search's, a layer's) under the mouse, if any:
   its hit, colour and meaning; a click on it peeks at its definition *)
val hovered_match : Code_map_base.t -> Code_map_base.camera -> (Code_search.hit * Playground.color * string option) option

(* claude: the street mode that fits the file looked at: 3 its uses and
   users, 1 its uses only, 2 its users only (a's first press) *)
val best_street_mode : Code_map_base.t -> int

(* claude: the unit whose name is under the mouse, and the units tied
   to it, its users and what it uses: shift+click's view *)
val unit_with_ties : Code_map_base.t -> Code_map_base.camera -> (string * string list * string list) option

(* claude: at the street, the panel whose name is under a pixel *)
val street_title_at : Code_map_base.t -> Code_map_base.camera -> float -> float -> string option

(* claude: the bone under the mouse, in the X-ray: a click peeks at it *)
val hovered_bone : Code_map_base.t -> Code_map_base.camera -> Code_guide.bone option

(* claude: the line of an anchor ("def:march") in a file of the map *)
val anchor_line : Code_map_base.t -> string -> string -> int option

(* claude: the layers' colours, one each, in turn *)
val layer_colours : (int * int * int) list

val style : Code_map_base.style
