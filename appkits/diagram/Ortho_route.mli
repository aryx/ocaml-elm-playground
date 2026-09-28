(* Ortho_route: a connector's route, in right angles, around the
 * shapes between its ends.
 *
 * A diagram's connectors are drawn in horizontal and vertical pieces,
 * and they go around the boxes rather than through them. Where can
 * such a route turn? Only on a few lines: those along the edges of the
 * boxes (kept a margin away), and those through the two ends. So:
 *
 *   1. the **orthogonal visibility graph** (Wybrow, Marriott and
 *      Stuckey, "Orthogonal Connector Routing", 2009): every crossing
 *      of those vertical and horizontal lines that is outside all the
 *      boxes is a node, joined to its neighbours along the line when
 *      the piece between them crosses no box;
 *
 *        x1   x2        x3   x4          the lines, from the boxes'
 *    y1 --+----+--------+----+--          edges; a route is made of
 *         |####|        |####|            their pieces
 *    y2 --+----+--------+----+--
 *
 *   2. the shortest path on it (Dijkstra, 1959), where the cost is the
 *      length plus a price for each turn, so that of two routes as
 *      long the one with fewer bends wins -- the node is a place *and*
 *      the direction it was reached in, since a turn is a change of
 *      it.
 *
 * A glued end leaves its shape straight out of the side its
 * connection point is on, for a short stub, before routing starts.
 *
 * Worked example, checked by the tests: from the right side of a box
 * to the left side of a box straight across, one straight piece; with
 * a third box in the way, a route around it with four bends, none of
 * its pieces crossing the box. *)

type point = float * float

(* the way out of a connection point: its side of the shape *)
type dir = Left | Right | Up | Down

(* a box: its left, bottom, right, top *)
type box = float * float * float * float

(* [route ~boxes ~margin (start, dir) (goal, dir)]: the route's
 * corners, from [start] to [goal], its pieces horizontal or vertical,
 * kept [margin] away from the boxes. A box that holds an end not glued
 * (no direction) is ignored. Never fails: with no way round, a route
 * of three pieces through whatever is there. *)
val route : boxes:box list -> margin:float -> point * dir option -> point * dir option -> point list

(* how many times a route turns *)
val bends : point list -> int
