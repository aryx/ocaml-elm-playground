(* Searching without an evaluation function: Monte Carlo tree search
 * (see notes_ai.md section 10).
 *
 * Everything in Minimax.mli rests on [score], a guess at what a
 * position is worth. Chess has one (material, and where the pieces
 * stand); Go has none anybody could write -- whether a position is
 * good depends on whether groups eventually live, which is as hard as
 * playing. From 1970 to 2005 that kept Go programs weak.
 *
 * The 2006 answer was to give up on knowledge: to judge a position,
 * *play it out at random to the end*, many times, and count the wins.
 * One random game says nothing; ten thousand of them say who is
 * better. And done inside a tree, it says which move:
 *
 *     select     from the root, take the child with the best UCB score
 *                until reaching a node with a move never tried
 *     expand     try it: one new child
 *     simulate   play on at random from there to the end
 *     backup     add the result to every node on the way back up
 *
 * The selection rule is the whole trick (UCT, Kocsis and Szepesvari,
 * 2006):
 *
 *                  wins(c)              ln N(parent)
 *     score(c) =  ---------  +  C sqrt  ------------
 *                   N(c)                    N(c)
 *
 * the left term for what has been winning, the right for what has
 * barely been tried -- it grows as the parent is visited and the child
 * is not, so nothing is written off on a small sample. [exploration] is
 * C, sqrt 2 by default (the value UCB1 is proved with for rewards in
 * [0, 1], which is what [score] is turned into here).
 *
 * Three things make it worth its own module: it needs nothing of a game
 * but its rules, it is *anytime* (stop whenever and take the most
 * visited child: [think] below is a frame's worth), and it does not
 * care how many moves a position has -- Go's 250 no worse than chess's
 * 35.
 *
 * The playouts are uniformly random by default, which is what makes it
 * knowledge-free; a game that knows a little ([playout]) plays out
 * better and needs fewer of them. In Go, one rule -- do not fill your
 * own eyes -- is the difference between playouts that end sensibly and
 * playouts that fill the board.
 *
 * Example (Unit_mcts): tic-tac-toe, where nothing in the code knows
 * what a line of three is worth. With 2000 playouts a move it opens in
 * the centre, blocks a threat and takes a win when it has one -- and
 * blocks 20 times out of 20 over 20 seeds; with 10 playouts, 5 times
 * out of 20, which is what a guess looks like.
 *
 * References: Rémi Coulom, "Efficient Selectivity and Backup Operators
 * in Monte-Carlo Tree Search", 2006 (Crazy Stone); Levente Kocsis,
 * Csaba Szepesvari, "Bandit based Monte-Carlo Planning", 2006 (UCT);
 * Peter Auer, Nicolo Cesa-Bianchi, Paul Fischer, "Finite-time Analysis
 * of the Multiarmed Bandit Problem", 2002 (UCB1); Cameron Browne et
 * al., "A Survey of Monte Carlo Tree Search Methods", 2012. *)

(* what the search came back with *)
type 'move result = {
  best : 'move option; (* the most visited move: MCTS's answer *)
  tried : ('move * int * float) list; (* each root move: its visits, and its wins per visit *)
  playouts : int; (* the games played at random, in all *)
  nodes : int; (* the positions in the tree it grew *)
}

(* [search ?exploration ?seed ?playout game ~playouts state]: [playouts]
 * iterations of the four steps above.
 *
 * [seed] makes a run repeatable (0 by default: a game replays the
 * same). [playout] is how a simulation is played out from a position
 * to its end, the game's own knowledge if it has any; uniformly random
 * by default. The game's [Minimax.score] is used only at the end of a
 * playout, and only its sign matters: above 0 MAX won, below 0 MIN
 * won, 0 a draw. *)
val search :
  ?exploration:float ->
  ?seed:int ->
  ?playout:(Random.State.t -> ('state, 'move) Minimax.game -> 'state -> 'state) ->
  ('state, 'move) Minimax.game ->
  playouts:int ->
  'state ->
  'move result

(* {1 Thinking a frame at a time}

   MCTS is anytime: its tree is an answer at every moment, better the
   longer it is grown. [start] plants it, [think] grows it by so many
   playouts, [plan] reads the answer so far. Unlike a depth-first
   search there is nothing to abandon -- a playout either happened or
   did not.

   The tree is grown in place: [think] returns the same tree, not a
   copy, so a game keeps one value in its model and replaces it with
   what [think] gives back. (A copy would be the whole tree, and the
   tree is the point.) *)

type ('state, 'move) thinking

val start :
  ?exploration:float ->
  ?seed:int ->
  ?playout:(Random.State.t -> ('state, 'move) Minimax.game -> 'state -> 'state) ->
  ('state, 'move) Minimax.game ->
  'state ->
  ('state, 'move) thinking

val think : playouts:int -> ('state, 'move) thinking -> ('state, 'move) thinking
val plan : ('state, 'move) thinking -> 'move result
