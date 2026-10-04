(* Two players, so many games: which is the better
 * (notes_ai_learning.md section 16).
 *
 * A loss going down says the network agrees more with its own search;
 * it does not say it plays better. The only measure of a player is
 * games: against a fixed opponent whose strength is known (one
 * playing at random, the search without a network, a perfect player
 * where there is one), and against its own earlier self.
 *
 * The players take turns starting, since in most games moving first
 * is worth something, and each game has its own seed, so that two
 * players who draw from chance do not replay one game twenty times.
 *
 * Two players who draw *nothing* from chance are another matter, and
 * a trap: a search guided by a network against alpha-beta is the same
 * game every time, so ten games each way are two results counted ten
 * times, and the scores come out 10-0-0, 5-0-5 or 0-0-10 and nothing
 * between -- which is how to recognise it. The cure is the caller's:
 * start each game from a few moves made at random
 * (scripts/train/train_connect4's [opening]). Measured there on a
 * network that knew nothing: 5-0-5 against alpha-beta at depth 3
 * became 3-0-17, the true figure
 * (notes_ai_dark_arts.md). *)

(* a player: the move it makes in a position where there is one *)
type ('state, 'move) player = seed:int -> 'state -> 'move

(* the first player's games *)
type score = { won : int; drawn : int; lost : int }

(* [play game start ~a ~b ~games]: [a] against [b], [a] moving first
 * in the even games; [a]'s score *)
val play :
  ('state, 'move) Minimax.game -> 'state -> a:('state, 'move) player -> b:('state, 'move) player -> games:int -> score

(* one game, and MAX's share at its end: 1 won, 0 lost, a half drawn *)
val game :
  ('state, 'move) Minimax.game ->
  'state ->
  max:('state, 'move) player ->
  min:('state, 'move) player ->
  seed:int ->
  float

(* any legal move, drawn evenly: the opponent that knows nothing *)
val random : ('state, 'move) Minimax.game -> ('state, 'move) player
