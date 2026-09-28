(* Treemap: a tree of sizes as nested rectangles, each the area of its
   size (Shneiderman, 1992), the layout of codemap's map.

   Three algorithms, side by side:

   - Ordered (Shneiderman and Wattenberg, "Ordered treemap layouts",
     2001, pivot by middle), codemap's default and ours: the children
     sorted by name keep their order, read left to right and top to
     bottom, so that a directory is found where the alphabet puts it.
     The middle child is the pivot; those before it make a strip along
     the longer side, some of those after it go beside the pivot
     (however many make its rectangle squarest), the rest a last strip;
     each part is laid out the same way. Nearly as square as squarified:

       +------+------+-----+
       |      |  c   |     |
       |  a   +------+  e  |
       |  b   |  d   |  f  |
       +------+------+-----+   a b before the pivot c, d beside it, e f after

   - Slice and dice (Shneiderman, "Tree visualization with tree-maps",
     1992): a directory's children cut its rectangle in strips, across
     at one depth and down at the next. Simple, keeps the order, and its
     rectangles are long and thin, hard to see and to point at.

   - Squarified (Bruls, Huizing and van Wijk, "Squarified treemaps",
     2000): the children, biggest first, put in rows
     along the shorter side, a row closed when adding the next child
     would make its worst aspect ratio (a rectangle's long side over its
     short one) grow. Near squares, each a file's code can fill.

   Worked example (the paper's, and the tests'): sizes 6 6 4 3 2 2 1 in
   a 6 by 4 rectangle. The two 6s make the first row, a column on the
   left (3 by 2 each, aspect 1.5; one 6 alone would have been 1.5 by 4,
   aspect 2.67, and adding the 4 makes it 4); then 4 and 3 in a row on
   top of what is left; then 2, 2 and 1:

     +-----------+-------+-----+
     |           |       |     |
     |     6     |   4   |  3  |
     |           |       |     |
     +-----------+---+---+-+---+
     |           |   |   | |
     |     6     | 2 | 2 |1|
     |           |   |   | |
     +-----------+---+---+-+

   Nesting: a directory's children go inside its rectangle less a
   border, so the directory shows round them (codemap paints it dark
   and lays its children out in it shrunk); and a file's rectangle is a
   little less than its share, a gap between neighbouring files. *)

type rect = { x : float; y : float; w : float; h : float } (* y downwards *)

type 'a tree = Dir of string * 'a tree list | File of string * float * 'a

type algo = Ordered | Squarified | Slice_and_dice

val size : 'a tree -> float

(* [of_paths files]: the tree of [files], each given as its path
   ("libs/core/Color.ml"), its size and its data *)
val of_paths : (string * float * 'a) list -> 'a tree

(* a directory with only a directory in it merged with it: "libs/core"
   rather than "libs" holding "core" (codemap's remove_singleton_subdirs) *)
val fold_singletons : 'a tree -> 'a tree

(* [squarify sizes r]: the rectangles of [sizes] in [r], in their order *)
val squarify : float list -> rect -> rect list
val slice : horizontal:bool -> float list -> rect -> rect list
val ordered_layout : float list -> rect -> rect list

(* a node placed: a directory's before its children's (so drawn first) *)
type 'a placed = { rect : rect; depth : int; path : string; node : 'a tree }

(* [layout algo r tree]: every node of [tree] placed in [r], the root's
   children at depth 1 *)
val layout : algo -> rect -> 'a tree -> 'a placed list
