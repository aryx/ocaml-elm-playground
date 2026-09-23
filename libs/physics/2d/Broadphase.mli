(* The broad phase: which pairs of bodies are worth testing (see
 * notes_2d_physics.md section 9).
 *
 * n bodies make n (n - 1) / 2 pairs: 4,950 for 100 balls, 44,850 for
 * 300, most of them far apart. Before the exact tests of Collide (the
 * narrow phase), a cheap pass keeps only the pairs whose bounding boxes
 * overlap. Three ways to find them, from the obvious to the clever; all
 * three find the same pairs, they differ in how many box tests it
 * takes:
 *
 * - All pairs: test every one. Fine for a dozen bodies.
 *
 * - A uniform grid: each box goes in the cells it covers; only boxes
 *   sharing a cell are tested. Great when the bodies have about the
 *   same size (the cells then as big as the biggest body).
 *
 * - Sort and sweep (or sweep and prune): sort the boxes by their left
 *   edge, then sweep from left to right keeping the "active" boxes,
 *   those whose x range is still open; a new box is tested only against
 *   those. From one frame to the next the order barely changes, so an
 *   insertion sort of the previous order is nearly free (not done
 *   here: an exercise).
 *
 * Example, four boxes:
 *
 *      y
 *      6 |    +--+
 *        |    |D |              A [0,2] x [0,2]    B [1,3] x [1,3]
 *      5 |    +--+              C [5,6] x [0,1]    D [1.5,2.5] x [5,6]
 *        |
 *      3 |  +----+              the only overlap: A and B
 *        |  |  B |
 *      2 +--|-+  |
 *        | A+-|--+
 *        |    |          +-+
 *      0 +----+----------+-+-- x
 *        0  1 2  3       5 6
 *
 *   all pairs: 6 tests. Sort and sweep: A, B, D, C by left edges; B
 *   tested against A (overlap), D against A and B (their x ranges are
 *   still open), C against none (A, B and D all end before 5): 3 tests.
 *   Grid, cells of 2 (the largest side): only A and B share cells, 1
 *   test.
 *
 * Trees (quadtrees, bounding volume hierarchies) are the next step, for
 * big worlds with bodies of very different sizes: not here.
 *
 * References: David Baraff, "Dynamic Simulation of Non-Penetrating
 * Rigid Bodies", PhD thesis, Cornell, 1992 (sort and sweep); Cohen,
 * Lin, Manocha, Ponamgi, "I-COLLIDE", Symposium on Interactive 3D
 * Graphics, 1995; Christer Ericson, Real-Time Collision Detection,
 * 2005, chapter 7 (grids). *)

(* a bounding box: (min corner, max corner), as Shape.bounds *)
type box = Vec2.t * Vec2.t

type method_ = All_pairs | Grid | Sort_and_sweep

(* in the order above *)
val methods : method_ list
val name : method_ -> string

type result = {
  (* the pairs of indices (i, j), i < j, whose boxes overlap, sorted *)
  pairs : (int * int) list;
  (* how many pairs of boxes were tested to find them *)
  tests : int;
}

val all_pairs : box array -> result

(* [grid ?cell boxes]: cells of [cell] by [cell], by default
 * [cell_size boxes] *)
val grid : ?cell:float -> box array -> result

(* the largest width or height of the boxes: every box then covers at
 * most 2 x 2 cells *)
val cell_size : box array -> float

val sort_and_sweep : box array -> result

(* one of the three *)
val pairs : method_ -> box array -> result
