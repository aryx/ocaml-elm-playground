(* Map_anatomy: the X-ray's other plates (Code_anatomy), 1 to 6, over
   the skeleton of Map_skeleton: the blood running along its joints,
   the muscles, the nerves, the lungs, the skin; and the atlas's key, a
   legend whose rows turn each plate on and off.

   A plate shows a file's facts (Code_anatomy.facts: which of its lines
   are muscle, nerve, lung, skin), found from its code and the configs'
   anatomy rules. From afar, a few files a frame, so that the whole
   repository's X-ray opens at once and fills in; at the ground and the
   street, the lines themselves tinted. *)

(* a file's facts, found once and kept; None if not found yet and the
   clock (Sys.time) is past [until]: a frame's budget, the rest the
   next frames' *)
val facts_of : Code_map_base.t -> ?until:float -> Code_map_base.entry -> Code_anatomy.facts option

(* the plates that are on (Code_anatomy.shown), drawn for this camera *)
val anatomy_shapes : Code_map_base.t -> Code_map_base.camera -> Playground.shape list

(* the legend: a row per plate, those shown bright, and under [pointer]
   (the mouse, in pixels) what the hovered plate shows. In the corner
   where it hides the fewest bones *)
val legend : ?pointer:float * float -> Code_map_base.camera -> Playground.shape list

(* the legend's row under a pixel, with the X-ray on: a click toggles
   its plate *)
val legend_row_at : Code_map_base.t -> Code_map_base.camera -> float -> float -> Code_anatomy.system option
