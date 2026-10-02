(* Map_classic: the first code map, codemap's own look, as a style beside
   Map_v2 (y): SeeSoft's picture of every file at every zoom, turning into
   letters up close (Code_map_base.paint_code), the directories dark in
   their part's colour; the names over it: the directories' big and faint
   and their paths on tabs, the files' on tabs, the tricks marked, and,
   from afar, what each file defines, bigger for a function than a
   local (Highlight_code.emphasis), placed greedily each frame.

   Kept beside Map_v2 because it paints every file's content from the
   start, at any zoom the wheel gives: what v2's names and columns hide
   from afar (a directory of boilerplate, a file unlike its
   neighbours) shows here. *)

(* the picture, and the names (Code_map_base.style's fields) *)
val paint : aa:bool -> Code_map_base.t -> Code_map_base.camera -> Rgba_image.t
val labels : Code_map_base.t -> Code_map_base.camera -> float -> Playground.shape list

val style : Code_map_base.style
