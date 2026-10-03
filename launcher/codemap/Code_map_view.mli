(* Code_map_view: a frame of the map, whatever its style. The picture
   and its names are the style's (t.style: Map_atlas, Map_classic); what
   is here is what every style shares, round them: when the picture is
   painted and how sharp, the names lit under the mouse, the screen's
   chrome (the background, the title, the status line, the keys, the
   version), and the morph from one layout to the next.

   The picture is painted at the window's resolution, anti-aliased,
   when the camera is still, and kept. While the camera moves, a quick
   one, half the resolution, one sample a pixel: a zoom repaints every
   frame, and the sharp picture (7 million pixels at 4K, four samples
   each) would make it stutter; the sharp one comes the frame after the
   camera stops. *)

(* the map, and with [chrome] (the default) the screen round it: its
   background, the title, what is under the mouse, the keys; without,
   the map alone, where the caller shows it (tinybox's panel) *)
val view : ?chrome:bool -> Playground.computer -> Code_map_base.t -> Playground.shape list

(* The morph *)

(* [morph_from ~old t ~now]: [t]'s rectangles animated from where [old]
   shows them (a folder laid out anew, or back), over 0.45 s: each from
   where it was on the screen to where it is now (Transition), a unit
   only in [t] growing from its centre *)
val morph_from : old:Code_map_base.t -> Code_map_base.t -> now:float -> unit

(* the map as it is at [now], its rectangles on their way; [t] itself
   once the morph is over *)
val morphed : Code_map_base.t -> now:float -> Code_map_base.t

(* the code map's version, shown at the bottom right, raised by hand at
   each publish of a change to the map: to see at once whether a page
   runs the latest *)
val version : string
