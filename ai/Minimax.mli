(* Minimax and alpha-beta: how a computer plays a board game, by
 * thinking a few moves ahead.
 *
 * A two-player game where both players see everything (tic-tac-toe,
 * Othello, chess) is a tree: the positions are its nodes, the moves its
 * edges. One player, MAX, wants the final score high; the other, MIN,
 * wants it low. Looking [depth] moves ahead, the leaves are scored by
 * an evaluation function (a guess: who's better off here?), and the
 * scores go back up the tree: at a MAX node the largest of its
 * children's, at a MIN node the smallest -- each player assuming the
 * other plays their best. The textbook example (Russell and Norvig's):
 *
 *                      MAX                 3
 *            ___________|___________
 *           |           |           |
 *          MIN         MIN         MIN     3     2     2
 *         / | \       / | \       / | \
 *        3 12  8     2  4  6    14  5  2
 *
 * MIN picks 3 in the left branch, 2 in the others; MAX picks the left
 * move, worth 3. [minimax] visits all 13 nodes.
 *
 * Alpha-beta gets the same answer visiting fewer. After the left
 * branch, MAX knows it can get 3 (alpha, MAX's best so far). In the
 * middle branch, the first leaf is 2: MIN can get at most 2 there,
 * already worse for MAX than the 3 it has, so the 4 and the 6 don't
 * matter -- cut, not visited. (beta is the same for MIN: the best MIN
 * can get elsewhere.) [alphabeta] visits 11 nodes here; in the right
 * branch the 2 comes last, too late to cut. With the best moves tried
 * first, alpha-beta searches about b^(d/2) nodes instead of b^d, for b
 * moves in each position and d moves ahead: twice as deep in the same
 * time. Tic-tac-toe searched to the end (a draw): minimax visits the
 * whole tree, 549,946 positions; alpha-beta, 18,297, 3% of them.
 *
 * The same move is found. The values of the moves other than the best
 * are not: a cut branch's value is only a bound ("at most 2"). That's
 * why [result]'s [children] are exact only from [minimax].
 *
 * The players needn't take turns: [max_to_play] says who moves, so a
 * player can pass, or play twice. (Negamax, the usual way
 * of writing it, has one function instead of a MAX and a MIN case, the
 * scores negated at each level; the same algorithm, harder to read.)
 *
 * References: John von Neumann, "Zur Theorie der Gesellschaftsspiele",
 * 1928 (the minimax theorem); Claude Shannon, "Programming a Computer
 * for Playing Chess", 1950; Donald Knuth and Ronald Moore, "An Analysis
 * of Alpha-Beta Pruning", 1975; Stuart Russell and Peter Norvig,
 * "Artificial Intelligence: A Modern Approach", chapter 5 (Adversarial
 * Search), whose figure is the tree above. *)

type ('state, 'move) game = {
  (* the legal moves; none: the game is over *)
  moves : 'state -> 'move list;
  play : 'state -> 'move -> 'state;
  (* the evaluation function: the higher, the better for MAX; of an
   * ended game too, which alpha-beta scores without asking [moves] when
   * it is a leaf *)
  score : 'state -> float;
  max_to_play : 'state -> bool;
}

type 'move result = {
  value : float; (* the position's, [depth] moves ahead *)
  best : 'move option; (* None when the game is over *)
  nodes : int; (* the positions visited, this one included *)
  children : ('move * float) list;
      (* each move's value, in the order of [moves]: exact from
       * [minimax], bounds for the moves [alphabeta] cut *)
}

(* [minimax game ~depth state]: every position [depth] moves ahead
 * visited. E.g. the tree above: value 3, the left move, 13 nodes. *)
val minimax : ('state, 'move) game -> depth:int -> 'state -> 'move result

(* [alphabeta game ~depth state]: the same value and best move as
 * [minimax], without the branches that can't change them. E.g. the
 * tree above: 11 nodes.
 *
 * [leaf], if given, scores the positions [depth] moves ahead instead
 * of [score], told the window there: whatever it answers below [alpha]
 * or above [beta] only needs to stay below or above it. A game that
 * searches on at its leaves -- chess's quiescence, the captures played
 * out until the board is quiet (games/AiChess.ml) -- can then cut
 * that search too, where [score] would have to search with no window
 * at all. *)
val alphabeta :
  ?leaf:('state -> alpha:float -> beta:float -> float) ->
  ('state, 'move) game -> depth:int -> 'state -> 'move result
