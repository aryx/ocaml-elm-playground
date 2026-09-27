(* Flex_layout: flexbox's arithmetic -- how long each item is along the
   line, and where it goes.

   (notes_css_engine.md section 8.) A flex container (display: flex)
   lays its children, the **items**, along a main axis -- a row, or a
   column -- instead of stacking them. It is how pages now put things
   side by side: Google's links in a row, GitHub's header, Wikipedia's
   tabs. Box_layout lays each item out; this module is what is done
   with their sizes, numbers in and numbers out (as Table_layout is for
   a table's columns):

   1. each item's **base size**: its flex-basis, else its width (its
      height in a column), else its content's -- measured by
      Box_layout, which also gives its minimum (its widest word: an
      item does not shrink below its content, CSS's min-width: auto)
      and maximum;
   2. with flex-wrap: wrap, the items cut into **lines**, each as many
      as fit ([lines]);
   3. the room left on a line -- its length less the items' and the
      gaps -- **shared** ([resolve], CSS Flexbox section 9.7): given in
      proportion to flex-grow if there is room to spare, taken in
      proportion to flex-shrink times the base size if there is too
      little; an item clamped by its minimum or maximum is frozen at
      it, and the rest shared again among the others;
   4. the items **placed** along the line ([place]): what room remains
      goes to the auto margins if an item has one (margin-left: auto,
      the navigation bar's links pushed right), else as
      justify-content says -- at the start, the end, the centre, or
      between them (space-between, -around, -evenly);
   5. and across it ([cross]): align-items (or the item's align-self)
      at the start, the end, the centre, or stretched to the line.

     a row 600 wide, gap 10, three items of base 100, grow 0, 1, 2:
       room 600 - 300 - 20 = 280, shared 0 : 1 : 2 -> 0, 93.3, 186.7
       sizes 100, 193.3, 286.7 at 0, 110, 313.3

     the same, grow 0, shrink 1, in a row 250 wide:
       room 250 - 300 - 20 = -70, taken in proportion to 100 each:
       76.7 each

   Sizes here are outer: an item's margins, borders and paddings in it
   (Box_layout adds and takes them away). Not done: order, baseline
   alignment (as start), align-content (lines packed at the start).

   Reference: W3C, CSS Flexible Box Layout Level 1, sections 9.2 (the
   base size), 9.3 (lines), 9.7 (resolving flexible lengths), 8
   (alignment); notes_css_engine.md section 8. *)

(* an item, along the main axis *)
type item = {
  base : float; (* its outer base size *)
  grow : float;
  shrink : float;
  min_size : float; (* outer; 0 if none *)
  max_size : float; (* outer; infinity if none *)
  auto_before : bool; (* margin auto before it: what room remains pushes it along *)
  auto_after : bool;
}

(* the items cut into lines along [room], [gap] between two: all on
 * one without [wrap]; each line its first and last index *)
val lines : wrap:bool -> room:float -> gap:float -> item array -> (int * int) list

(* each item's outer size along a line [room] long, [gap] between two *)
val resolve : room:float -> gap:float -> item array -> float array

(* each item's start along the line, its [sizes] given: the room left
 * to the auto margins, or as justify-content says *)
val place : justify:Computed.align -> room:float -> gap:float -> item array -> float array -> float array

(* an item's offset across a line [line] thick, its own [size] (outer),
 * and its size if stretched: align-items or align-self *)
val cross : align:Computed.align -> line:float -> size:float -> float * float
