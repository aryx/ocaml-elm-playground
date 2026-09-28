(* Map_atlas: the code map as an atlas, a style beside Map_classic and
   Map_streets (m goes from one to the next): the street map, with what
   the code says about itself over it, so that a newcomer sees how the
   parts hang together before reading any of them.

   - The heat: from afar (until SeeSoft's colours come in), a file's
     colour is how many other files use its definitions (Code_rank.links),
     dark if none, through yellow, to red for the most used on the map:
     where the foundations are.
   - The roads: the links between the parts seen at this zoom -- the
     countries from the whole map, the regions from 3 times closer, then
     the files (the street map's zooms) -- bundled along the tree of
     directories (Holten's hierarchical edge bundles, 2006, shown by him
     over a squarified treemap). All of them, the busiest 160, when the
     mouse is on no part; the hovered part's alone, framed, when it is
     on one. The direction without arrows: green at the user, red at the
     used, and a taper (Holten and van Wijk, 2009).

   Its map is laid out in layers (Code_layers, Code_map's laid_out): in
   each directory the users above the used, so that the roads run
   downhill and one going up stands out. The names are the street
   map's. *)

(* the part a file is in at a zoom's depth (1 the countries, 2 the
   regions, max_int the files), a road's control points and the
   B-spline through them, exposed for the tests *)
val depth_at : float -> int
val bspline : ?per:int -> (float * float) array -> (float * float) list

val paint : aa:bool -> Code_map_base.t -> Code_map_base.camera -> Rgba_image.t
val labels : Code_map_base.t -> Code_map_base.camera -> float -> Playground.shape list
val style : Code_map_base.style
