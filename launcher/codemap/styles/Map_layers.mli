(* Map_layers: l, the layers, codemap's: Map_atlas's whole map coloured
   by a measure, at two levels. From afar a file is one colour (the
   macro level); nearer, each definition's lines have their own (the
   micro level), so that zooming in shows which definition made the
   file red. A measure is ranked, each file (each definition) by its
   place among all, a percentile, so that the whole gradient is used
   (Map_skeleton.heat: green the top, what uses; red the bottom, what
   is used).

   The layers, in l's order:

     1  used vs using   the parts of the unit looked at, each by the uses
                        coming into it over those and the uses going out
     2  the call depth  a definition by its depth over its depth and
                        height in the call graph; a file its
                        definitions' mean
     3  roles           a file's role (Code_roles)
     4  tested          the files a test reaches
     5  described       the files a config says something of

   The first four need the uses counted (Code_rank, t.rank), which takes
   a moment: until then, [counting]. *)

(* how many layers; t.layer is 0 for none, then 1 to layer_count *)
val layer_count : int

(* the layer on (t.layer): its tints, to draw under the names, and its
   key, to draw over them; both empty when none is on *)
val layer_shapes : Code_map_base.t -> Code_map_base.camera -> Playground.shape list * Playground.shape list

(* "counting the uses", said in the middle of the map while a key waits
   for them *)
val counting : Code_map_base.camera -> Playground.shape list
