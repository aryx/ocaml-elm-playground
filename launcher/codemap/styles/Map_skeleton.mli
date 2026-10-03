(* Map_skeleton: x, the X-ray's first plate, the configs' skeletons
   (Code_guide.skeleton) drawn on Map_atlas's picture, the rest in the
   shade.

   A skeleton is bones, each a definition (or a whole file, or a
   directory) with its role, and joints between them. At every level:
   from afar, a bone is a dot at its line in its file's columns, a file
   whose bones are close together one dot with the skeleton's name; at
   the ground and the street, the bone's definition is lit in the shaded
   file, its role beside it. The joints are ivory roads between the
   bones, their direction the road's taper, the order of the bones a
   gradient ([heat]); an end off the map is a stub to the map's edge
   naming it.

   Where no config gives a skeleton, one is derived from the code: a
   file's from its capitals, its important lines and its definitions the
   most used within it; a folder's from its parts the most tied.

   So this module is also where the others learn where something is on
   the map now, whatever the level: [spot], a line of a file; [grounds],
   the layouts painted; [entry_of], a file by its path. *)

(* the X-ray's bones, roles and joints for this camera; also notes the
   bones drawn ([drawn_bones]) *)
val skeleton_shapes : Code_map_base.t -> Code_map_base.camera -> Playground.shape list

(* the bones [skeleton_shapes] drew this frame: each with its dot's
   place and its role's width, for a hover and a click *)
val drawn_bones : (Code_guide.bone * float * float * float) list ref

(* of boxes (left, top, width, height) where a panel of the X-ray may
   go, the one over the fewest bones (the last frame's), the first as
   good: a legend is not put over what it is the legend of *)
val least_bones : (float * float * float * float) list -> float * float * float * float

(* a bone's line in its file; a whole file's or directory's has none *)
val bone_line : Code_map_base.t -> Code_guide.bone -> int option

(* Where things are on the map *)

(* a file of the map, or beyond it, by its path (a table kept, under
   Opti) *)
val entry_of : Code_map_base.t -> string -> Code_map_base.entry option

(* at the ground, the layouts on the map and their files' paths: the
   focus's and, at the street, the panels' *)
val grounds : Code_map_base.t -> Code_map_base.entry -> (string * Code_ground.t) list

(* [spot t c path line]: where a file's line is on the map now, in
   pixels: its left, its middle's height, the end of its text (at the
   ground; from afar, its left again). None when the file is not on the
   screen *)
val spot : Code_map_base.t -> Code_map_base.camera -> string -> int -> (float * float * float) option

(* a definition's lines, first and last: from its header to the line
   before the next top-level definition *)
val extent : Code_file.t -> int -> int * int

(* Colours *)

(* the joints' *)
val ivory : int * int * int

(* the gradient of an order, the skeleton's and the layers': 0 green
   (the top, the start), 0.5 yellow, 1 red (the bottom, the end) *)
val heat : float -> int * int * int
