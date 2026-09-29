(* Code_units: the tree of the map's units -- its directories and files
   as placed (Treemap.placed, a directory before its children, the root
   first, at index 0) -- for moving from one to the next (Map_v2's way,
   plan_codemap_v2.md, step 2): the camera goes a whole directory or file
   at a time, in, out, or beside, never stopping halfway. Pure.

   Worked example (the tests'): with kernel/a.ml, kernel/b.ml and
   lib/c.ml, kernel's parent is the root, the root's child toward a point
   of a.ml is kernel (a level at a time), kernel's sibling to its right
   is lib. *)

(* the directory holding a unit (None: the root) *)
(* claude: a directory's children, one level deeper *)
val children : 'a Treemap.placed array -> int -> int list

val parent : 'a Treemap.placed array -> int -> int option

(* [child_toward placed i u v]: the child of [i] under the layout's
   point (u, v), if [i] is a directory and the point in one of them *)
val child_toward : 'a Treemap.placed array -> int -> float -> float -> int option

(* [toward placed i u v]: where a click at (u, v) goes when [i] is looked
   at: the unit under the point one level below [i] at most -- a child
   of [i], or a unit beside it, not the file deep inside *)
val toward : 'a Treemap.placed array -> int -> float -> float -> int option

type side = Left | Right | Up | Down

(* the sibling of a unit on that side, the nearest (its centre's
   distance, the side's axis weighing less) *)
val sibling : 'a Treemap.placed array -> int -> side -> int option

(* [ancestors placed i]: the units holding [i], the root first, [i] last *)
val ancestors : 'a Treemap.placed array -> int -> int list

(* the deepest unit [ok] holds for (0, the root, if none): after the
   camera was moved some other way (a search, a tour), the unit it frames *)
val deepest : 'a Treemap.placed array -> ('a Treemap.placed -> bool) -> int
