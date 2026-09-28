(* Code_layers: a map laid out by who uses whom, the users above the
   used, so that its roads (Map_atlas) run downhill and one going up
   stands out.

   In each directory, its children (directories and files) are layered
   by the links between them (Code_rank.links, a link counted in the
   directory where its two files part): a child nobody among its
   siblings uses on top, each other one a layer below the lowest of its
   users (the longest path, Sugiyama's layering); when two children use
   each other, only the heavier way counts. The layers are then squeezed
   into at most 4 bands (Treemap.layout's), a child with no link among
   its siblings in the top one.

   Worked example (the tests'): games/ uses playground/ and libs/,
   playground/ uses libs/: games/ in band 0, playground/ in 1, libs/ in
   2. *)

(* the bands of a tree's nodes, by their paths, from its files' links
   ([(a, b, n)], a using b n times) *)
val compute : (string * string * int) list -> 'a Treemap.tree -> string -> int
