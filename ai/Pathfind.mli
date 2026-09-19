(* Finding a way: breadth-first search, Dijkstra and A*, the same
 * search with a different queue.
 *
 * All three grow a frontier from the start until they reach the goal,
 * remembering which node each was reached from, so the path can be read
 * backwards at the end. What changes is the order the frontier is taken
 * from:
 *
 *   breadth-first  the oldest node first (a queue): rings around the
 *                  start, so the goal is reached in the fewest steps;
 *                  every step costs the same
 *   Dijkstra       the cheapest node so far (g, the cost from the
 *                  start): the shortest way when steps cost different
 *                  amounts -- mud, hills, water
 *   A*             the cheapest once a guess of what's left is added
 *                  (g + h, h the [estimate]): the search leans toward
 *                  the goal instead of spreading everywhere
 *
 * On an open grid, from S to G, the cells each one takes out of its
 * frontier (o):
 *
 *   breadth-first / Dijkstra        A*
 *   . . o o o o o o o . .       . . . . . . . . . . .
 *   . o o o o o o o o o .       . . . . . . . . . . .
 *   o o o o o o o o o o o       . . . . . . . . . . .
 *   o o S o o o o o G o o       . . S o o o o o G . .
 *   o o o o o o o o o o o       . . . . . . . . . . .
 *   . o o o o o o o o o .       . . . . . . . . . . .
 *   . . o o o o o o o . .       . . . . . . . . . . .
 *
 *   rings around the start,      straight at the goal, because the
 *   until one reaches the goal   guess grows as one steps away from it
 *
 * Both find a shortest path; A* looks at far fewer cells. It stays
 * right as long as the estimate never overshoots the true remaining
 * cost ("admissible"): on a grid where one moves in the four
 * directions, the Manhattan distance ([manhattan]) is the classic one
 * -- it's exactly what's left when nothing is in the way, and less
 * otherwise. Overshoot and A* gets faster but can miss the shortest
 * path. With [estimate] always 0, A* *is* Dijkstra.
 *
 * Worked example: an empty 13x9 grid, from (1, 4) to (11, 4). All three
 * find a way of 10 steps; breadth-first and Dijkstra look at 80 cells
 * each, A* at 11 -- it walks nearly straight there.
 *
 * Now a patch of mud two cells wide between them (x = 6 and 7, y = 2 to
 * 6), each step in it costing 5. Breadth-first still takes its 10 steps
 * and wades through: a cost of 18. Dijkstra goes around, 16 steps but a
 * cost of 16, looking at 115 cells; A* finds the same way looking at
 * 75. The shortest way and the cheapest one are not the same, and the
 * guess helps less when the cheap way isn't the straight one.
 *
 * The frontier here is a list kept in order; a real implementation uses
 * a priority queue (a binary heap), which matters once the grid is big.
 *
 * References: Edsger Dijkstra, "A Note on Two Problems in Connexion
 * with Graphs", 1959; Peter Hart, Nils Nilsson and Bertram Raphael, "A
 * Formal Basis for the Heuristic Determination of Minimum Cost Paths",
 * 1968, which gave A* its name; Stuart Russell and Peter Norvig, "Artificial Intelligence:
 * A Modern Approach", chapter 3; Amit Patel's "Red Blob Games" pages on
 * pathfinding, the clearest pictures of all this. *)

type 'node problem = {
  (* where one can go from here, and what each step costs (1 for a
   * plain grid; more for mud, a hill, deep water) *)
  neighbors : 'node -> ('node * float) list;
  goal : 'node -> bool;
  (* a guess at the cost left to the goal, never more than the truth;
   * 0 turns A* into Dijkstra *)
  estimate : 'node -> float;
}

type 'node result = {
  path : 'node list; (* from the start to the goal, both included; [] if there's no way *)
  cost : float; (* the path's, the steps' costs added up *)
  visited : 'node list; (* every node taken from the frontier, in order: the work done *)
}

(* [breadth_first problem start]: the fewest steps, every step counting
 * the same, whatever [neighbors] says they cost *)
val breadth_first : 'node problem -> 'node -> 'node result

(* [dijkstra problem start]: the cheapest path *)
val dijkstra : 'node problem -> 'node -> 'node result

(* [astar problem start]: the cheapest path, guided by [estimate] *)
val astar : 'node problem -> 'node -> 'node result

(* [manhattan (x1, y1) (x2, y2)]: |x1 - x2| + |y1 - y2|, the number of
 * steps on a grid with no diagonals and nothing in the way *)
val manhattan : int * int -> int * int -> float
