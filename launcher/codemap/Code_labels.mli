(* Code_labels: which names a street map shows at each zoom, decided once
   (plan_codemap_google_maps.md, step 3), as a street map places its
   labels: a label's minimum zoom, its [minz], and the last it stays, its
   [maxz], found by placing them all zoom by zoom, from the whole map in,
   each zoom starting with the labels the zoom before kept (in their
   places: zooming in only spreads them apart), then the new ones, the
   most important first, each only where it overlaps none, a margin round
   it. So a label never jumps while zooming, nothing is placed each
   frame, and the screen holds about as many at every zoom.

   A label is sized in the screen's pixels, whatever the zoom (a street
   map's names do not grow with the map), and anchored at a point of the
   layout (its file's corner, a line of it). Its levels ([from_level] to
   [to_level], continuous, 0 the whole map to 4 the code readable) say
   when it may be shown at all; a file's tab, only when its file is wide
   and high enough to hold it; a definition, only when its file is at
   least 40 pixels wide.

   Worked example (the tests'): two labels at the same point, the more
   important placed from the start, the other never; a third far away
   placed too; and a label placed at a zoom is placed at every closer one
   while its levels allow it. *)

type kind =
  | Tab (* a file's name, on a tab at its top left corner *)
  | Landmark (* a "trick of this game" mark *)
  | Capital (* one of the map's most used definitions *)
  | City (* one of a directory's most used definitions *)
  | Def (* a definition *)
  | Section (* a section's title *)

type label = {
  kind : kind;
  text : string;
  x : float; (* its anchor in the layout's units *)
  y : float;
  left : bool; (* its box starts at the anchor (else centred on it); a tab's hangs below it *)
  px : float; (* its size, in the screen's pixels *)
  rank : float; (* the more, the earlier *)
  from_level : float;
  to_level : float;
  fw : float; (* its file's rectangle, in the layout's units *)
  fh : float;
  color : int * int * int;
  mutable minz : float; (* infinity: never placed *)
  mutable maxz : float;
}

val label :
  kind -> string -> x:float -> y:float -> ?left:bool -> px:float -> rank:float -> from_level:float -> to_level:float -> fw:float -> fh:float -> int * int * int -> label

(* a label's box, in the screen's pixels: width and height *)
val size : label -> float * float

(* [place ~level ~zmin ~zmax labels]: every label's minz and maxz, for
   zooms from [zmin] to [zmax] (pixels a unit), [level z] the map's level
   at zoom z *)
val place : level:(float -> float) -> zmin:float -> zmax:float -> label array -> unit

(* how much a label shows at zoom z: 0 outside its zooms, 1 inside,
   fading over a sixth of a zoom step either side *)
val alpha : label -> float -> float
