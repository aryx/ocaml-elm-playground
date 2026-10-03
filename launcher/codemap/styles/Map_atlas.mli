(* Map_atlas: the code map as an atlas (plan_codemap_v2.md), the default
   style of tinybox codemap <dir>.

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
   shade, the neighbours' names faint, their files' gone.

   claude: a module per concern, each drawing its part and answering the
   mouse's questions about it; this one puts them together (labels: the
   order they are drawn in, what hides what) and is the style Code_map
   chooses:

     Map_paint      the picture: the columns, the ground, the street
     Map_names      the names over it, placed; the unit under a pixel
     Map_cards      the hover card, a unit's ties; notes, previews
     Map_skeleton   the X-ray: bones and joints; where a line is now
     Map_anatomy    the X-ray's other plates, the legend
     Map_peek       a definition read over the map
     Map_search     the search and the marks
     Map_layers     the map coloured by a measure

   Each uses only those above it in this list. *)

(* the style: Map_paint.paint, [labels], Map_names.unit_at, and
   the line under a pixel at the ground or in a peek *)
val style : Code_map_base.style

(* all that is drawn over the picture, back to front: the layer's tints,
   the notes or the street's titles, the names, the X-ray, a unit's ties
   and its card, the peeks, the marks and the search, the cards under
   the mouse, the layer's key, the tour's banner *)
val labels : Code_map_base.t -> Code_map_base.camera -> float -> Playground.shape list

(* claude: What is under the mouse, across the parts *)

(* claude: the match (a search's, a mark's) under the mouse, if any:
   its hit, colour and meaning; a click on it peeks at its definition *)
val hovered_match : Code_map_base.t -> Code_map_base.camera -> (Code_search.hit * Playground.color * string option) option

(* claude: the bone under the mouse, in the X-ray: a click peeks at it *)
val hovered_bone : Code_map_base.t -> Code_map_base.camera -> Code_guide.bone option

(* claude: at the street, the panel whose name is under a pixel *)
val street_title_at : Code_map_base.t -> Code_map_base.camera -> float -> float -> string option

(* claude: the line of an anchor ("def:march") in a file of the map *)
val anchor_line : Code_map_base.t -> string -> string -> int option
