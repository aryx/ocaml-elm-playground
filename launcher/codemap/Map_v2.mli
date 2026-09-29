(* Map_v2: the code map redesigned (plan_codemap_v2.md; the name to be
   found), the default of tinybox codemap <dir>.

   The earth and region levels (the plan's step 1): no code from afar,
   it is clutter. A file is its columns -- a strip per column of 80
   characters, the last one as long as its lines, so a file's length is
   seen at a glance (codemap's column hint) -- and a directory is its
   name, the programmer's own choice: large, a region's over its
   subdirectories', standing up when its block is tall and narrow,
   clickable (the camera flies to it), and hovering it gives its card.
   The cards say the counts for now; their words will come from the
   directories' .codemapconfig, written by an LLM (the plan's step 4).
   Near enough to be read, a file's code, as Map_classic's.

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

val style : Code_map_base.style
