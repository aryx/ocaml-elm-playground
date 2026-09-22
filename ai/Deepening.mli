(* Searching deeper: the tricks that buy depth (see notes_ai.md section
 * 9).
 *
 * [Minimax.alphabeta] searches to a fixed depth, in the order the game
 * lists its moves. Three things turn that into what an engine does:
 *
 * {2 Iterative deepening}
 *
 * Search 1 move ahead, then 2, then 3, keeping the last answer that
 * finished. It sounds wasteful and is not: a tree with [b] moves a
 * position multiplies by [b] at each level, so every depth before the
 * last one costs about 1 / (b - 1) of it -- a third at b = 4, a tenth
 * at b = 11. And it pays for that many times over, because the best
 * move of depth d is the first move to try at depth d + 1, and
 * alpha-beta's cuts depend entirely on trying a good move first.
 *
 * It also changes the question a game can ask. "Think 6 moves ahead"
 * takes as long as it takes; "think for a fiftieth of a second"
 * ([budget], in nodes, or [think] below) is what a game running at 60
 * frames a second actually wants, and iterative deepening answers it:
 * whatever depth finished is a complete answer.
 *
 * {2 Move ordering}
 *
 * Alpha-beta cuts a branch when it is already worse than one seen
 * before, so the earlier the good moves come, the more it cuts. In the
 * best order a search costs about the square root of what it costs in
 * the worst one (Knuth and Moore, 1975): the difference between
 * looking 4 moves ahead and 8, for the same work. Here: the best move
 * of the previous depth first (from [table] below, or the root's own
 * last answer), then whatever order [order] gives (a game's own hint:
 * in Connect 4, the middle columns, which win more).
 *
 * {2 The transposition table}
 *
 * The same position reached another way is the same position
 * (Zobrist.mli): if it was searched at least as deep, its value is
 * known and the subtree need not be searched at all; and if it is
 * known only as a bound, that bound can still narrow the window.
 *
 * Example, measured on Connect 4's opening (the empty 7x6 board,
 * searched 7 moves ahead: AiConnect4.ml, the numbers checked in
 * tests/games):
 *
 *     alpha-beta, the columns left to right    65,724 nodes
 *     the middle columns first                  9,449
 *     + iterative deepening, 1 to 7            12,818
 *     + the transposition table                 7,742
 *
 * Read the third line twice: iterative deepening *cost* a third more
 * here. It is not free -- the shallower searches are real work -- and
 * it pays only through the ordering it hands the deeper ones. This
 * game already hands out a good order (the middle columns), so there
 * was little left to buy, and the shallow passes were the price. Where
 * a game has no such hint, the ordering ID gives is often worth several
 * times what it costs; and what it always buys, hint or no hint, is the
 * ability to stop whenever asked, with a complete answer. The table
 * then makes the repeats cheap, which is why the last two lines belong
 * together: 7,742 with both, against 12,818 with deepening alone.
 *
 * References: David Slate and Larry Atkin, "Chess 4.5 -- The
 * Northwestern University Chess Program", 1977 (iterative deepening in
 * the form everyone copied); Donald Knuth, Ronald Moore, "An Analysis
 * of Alpha-Beta Pruning", 1975 (what ordering is worth); Albert
 * Zobrist, 1970 (the table's keys). *)

(* what a search came back with *)
type 'move plan = {
  value : float; (* the position's, as far as it got *)
  best : 'move option;
  depth : int; (* the deepest search that finished *)
  nodes : int; (* the positions visited, every depth together *)
}

(* [search ?budget ?order ?key ?table game ~depth state]: iterative
 * deepening from 1 to [depth].
 *
 * [order state moves] is the game's own hint, the moves it thinks
 * likeliest first; the previous depth's best move is tried before
 * them whatever it says. [key] gives a position's Zobrist key and
 * [table] holds what was learned about it (both or neither: a key
 * with no table does nothing). [budget] is a ceiling on nodes: the
 * depth being searched when it runs out is abandoned, and the last one
 * that finished is the answer -- so a plan is always complete to its
 * own [depth]. *)
val search :
  ?budget:int ->
  ?order:('state -> 'move list -> 'move list) ->
  ?key:('state -> int64) ->
  ?table:'move Zobrist.table ->
  ('state, 'move) Minimax.game ->
  depth:int ->
  'state ->
  'move plan

(* {1 Thinking a frame at a time}

   A game at 60 frames a second cannot stop for a search. [start] sets
   one up, [think] gives it another [nodes] of thought (one depth at
   most), [plan] is the best answer so far -- always a finished depth,
   usable from the first frame -- and [done_] says there is no more
   depth to find. Between two [think]s the game goes on drawing.

   A depth that does not fit in [nodes] is abandoned and begun again
   next time, with twice as much allowed, and again until it fits:
   otherwise a deep search would never finish in a frame's worth. The
   work thrown away is the price of stopping at a node boundary rather
   than keeping the search's stack; iterative deepening makes it a
   fraction of the whole, and a transposition table ([table]) makes the
   second attempt cheaper still. *)

type ('state, 'move) thinking

val start :
  ?order:('state -> 'move list -> 'move list) ->
  ?key:('state -> int64) ->
  ?table:'move Zobrist.table ->
  ('state, 'move) Minimax.game ->
  depth:int ->
  'state ->
  ('state, 'move) thinking

val think : nodes:int -> ('state, 'move) thinking -> ('state, 'move) thinking
val plan : ('state, 'move) thinking -> 'move plan
val done_ : ('state, 'move) thinking -> bool
