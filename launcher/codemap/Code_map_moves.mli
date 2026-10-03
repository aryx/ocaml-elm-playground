(* Code_map_moves: where the camera goes. Code_map.update reads the
   keys, the wheel and the clicks; each of the functions here answers
   one of its questions with a camera, a unit or a place, and changes
   nothing but, for some, the peek they open. The camera itself is
   moved by update, a step a frame ([ease]) towards its target.

   Two ways of moving. Freely (Map_classic): the wheel zooms at the
   mouse, a drag pans, [up] goes out. By units (Map_atlas, a style's
   [units]): a directory or a file at a time, in, out or beside
   ([unit_move], over Code_units). And, in both, to a place named: a
   name's definition ([go_to]), a search's hit, a tour's stop, a unit
   come back to. *)

(* The camera *)

(* the zoom kept within what a map can show *)
val clamp_cam : Code_map_base.camera -> Code_map_base.camera

(* [ease c target]: the camera a step nearer its target; the zoom eased
   in its logarithm, so that going in 100 times feels as steady as
   going in 2; the target itself once near enough *)
val ease : Code_map_base.camera -> Code_map_base.camera -> Code_map_base.camera

(* where going up goes: the directory round the view, a size bigger;
   the whole map from the top *)
val up : Code_map_base.t -> Code_map_base.camera

(* Names, on the map read up close *)

(* the file under a point of the layout readable where the camera is:
   its code read on the map itself *)
val readable_at : Code_map_base.t -> float -> float -> bool

(* the name under a point of the layout, bound in its own file
   (Code_file.name_at): the file's index in t.placed and the
   occurrence; None if the file is not lexed yet *)
val name_under : Code_map_base.t -> float -> float -> (int * Highlight_code.occurrence) option

(* the name under a point defined elsewhere (Code_file.ref_at): the
   file's index, its path, and the reference *)
val ref_under : Code_map_base.t -> float -> float -> (int * string * Highlight_code.reference) option

(* [found t i path r]: where a reference goes among the map's files
   (Code_names.find_in), the best first, and whether the first is alone
   at its rank; the last one asked is kept *)
val found : Code_map_base.t -> int -> string -> Highlight_code.reference -> Code_names.candidate list * bool

(* [go_to t target c]: the camera at a candidate's place, its code a
   readable size (10 units a line at least), its name lit; where the
   map was is kept, for b *)
val go_to : Code_map_base.t -> Code_map_base.camera -> Code_names.candidate -> Code_map_base.camera

(* Units *)

(* [unit_move computer ~pressed ~arrow t ~clicked mpx mpy]: the unit a
   key, the wheel or a click takes the map to, its index in t.placed,
   or None. A click on a name goes to it; on a block, a level down at
   most. The wheel steps once a gesture: its notches add up to one,
   then it rests until still a moment (a trackpad's flick is many
   events) *)
val unit_move :
  Playground.computer -> pressed:(string -> bool) -> arrow:string option -> Code_map_base.t -> clicked:bool -> float -> float -> int option

(* Places named *)

(* a search's hit gone to: a directory or a file framed; a definition's
   or a line's file framed and the definition peeked at. None for a hit
   beyond the map (a program's map, the repository behind it): the
   camera stays, the definition is peeked at where one is *)
val search_go : Code_map_base.t -> Code_search.hit -> Code_map_base.camera option

(* [tour_go t tour k]: a config's tour at its stop [k], the file flown
   to and the anchor's definition peeked at *)
val tour_go : Code_map_base.t -> Code_guide.tour -> int -> Code_map_base.camera option

(* a config's view as paths: its files, or a file and those it uses or
   that use it (Code_rank.links) *)
val view_set : Code_map_base.t -> Code_guide.view -> string list

(* back from the matrix (Map_graph): a unit flown to, and with a line,
   its definition peeked at *)
val go_back_to : Code_map_base.t -> string -> int option -> Code_map_base.t

(* a file's stops, for Codemap's tour: its header, its sections and the
   places saying "the trick of this game", each a line and a name;
   lexes the file *)
val stops : Code_map_base.entry -> (int * string) list
