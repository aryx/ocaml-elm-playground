(* Code_map_glass: a magnifying glass over the map, the part under the
   mouse closer. Not a zoom of the map's picture, which would only
   enlarge its pixels, blurred: the part under the glass is painted
   again, by the style's own paint, with a camera [power] times closer,
   at the window's resolution. The power is chosen for the file under
   the mouse, so that its lines come out about 16 units high, the VGA
   font's size: readable whatever the file's size.

   Two glasses. A round one, a glance at the code. A reading glass, the
   rectangular kind laid over a page: 80 columns by some 16 lines, lined
   up with the start of the lines under the mouse, whole lines rather
   than a keyhole of them. And none, the way to be rid of it.

   The glass's shape is its picture's own pixels made transparent, with
   a soft edge: the Playground has no clipping. Painted again only when
   the mouse moves, and not at all while the map moves under it (its
   picture would cost as much again as the map's, every frame); it
   comes back the frame the camera stops. *)

(* the glass at the mouse, with its rim and handle; nothing when there
   is none, when the map is moving, in a style that moves by units (its
   previews and peeks show the code), or over code already readable on
   the map. [panel], the menu's panel's glass, a setting of its own *)
val glass : ?panel:bool -> Playground.computer -> Code_map_base.t -> Playground.shape list

(* o: round, then the reading glass, then none; one setting for every
   map, none at first (the panel's: round at first) *)
val cycle_glass : ?panel:bool -> unit -> unit

(* "round", "wide" or "none", for the help line *)
val glass_name : ?panel:bool -> unit -> string
