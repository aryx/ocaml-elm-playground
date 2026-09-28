(* Learning to act from rewards alone: temporal difference, and
 * Q-learning (notes_ai_learning.md section 8).
 *
 * Everything in Backprop.mli needs the right answer for each example.
 * A game has none: only a result at the end, long after the move that
 * caused it. Which move gets the credit is *the* question (credit
 * assignment), and the answer is to learn not the right move but the
 * **value** of what you can do:
 *
 *     Q(s, a)   how well it tends to end, if I do a in s
 *
 * and then act by taking the best one. The learning rule is one line,
 * and it is the whole subject (Watkins, 1989):
 *
 *     Q(s,a) <- Q(s,a) + rate * ( r + discount * max Q(s',a') - Q(s,a) )
 *                               \___________________________________/
 *                                what I think now, minus what I thought:
 *                                the temporal difference (Sutton, 1988)
 *
 * Read it as: I did [a], got [r], and now I am in [s'], where the best
 * I can see is [max Q(s',a')]. If that is better than I expected,
 * raise my estimate a little. Nothing waits for the end of the game;
 * the value seeps backwards from the reward one step per visit, which
 * is why the first thousand episodes of a long corridor look like
 * nothing is happening.
 *
 * Two knobs decide what is learned:
 *
 *   discount   what a reward later is worth against one now (0.9). It
 *              is also what keeps the numbers finite when a game has
 *              no end.
 *   explore    how often to act at random instead of greedily (0.1).
 *              Zero and it never finds anything it does not already
 *              do; one and it learns a lot and plays terribly. This
 *              is the explore/exploit trade, in one number
 *              ([Mcts.mli]'s UCB is the other answer to it).
 *
 * Except that the second knob has a trapdoor in it, and it is worth
 * knowing which way round it opens. An action never tried is worth 0
 * here. In a world where every step *costs* something -- the cliff of
 * Unit_qlearn, one point a step -- nought is better than anything it
 * has tried, so a purely greedy learner tries everything once anyway
 * and finds the shortest way with [explore] at zero. That is optimism
 * in the initial values (Sutton and Barto, 2.6), switched on by
 * accident by the sign of the rewards. Take the costs away, pay only
 * at the goal, and the optimism goes with them: every action looks
 * exactly as good as every other for ever, and the same learner does
 * the same thing five hundred episodes running -- measured, four
 * state-action pairs in its whole table, and it never reaches the
 * goal at all.
 *
 * Off-policy, and why that matters: the rule uses [max Q(s',a')], the
 * best it *could* do next, not what it actually does next. So it goes
 * on learning the greedy policy while behaving at random, which is
 * what makes exploring safe. (Learning about the policy actually
 * followed is SARSA, one symbol's difference and a visibly more
 * cowardly player -- it learns to keep away from the cliff edge
 * because it knows it will sometimes step at random.)
 *
 * The table is a table: one number per state and action, which works
 * up to a few hundred thousand pairs and no further. Everything past
 * that replaces the table with a network that guesses Q -- the same
 * rule, the same line, with Backprop in the middle of it.
 *
 * References: Richard Sutton, "Learning to Predict by the Methods of
 * Temporal Differences", 1988; Christopher Watkins, "Learning from
 * Delayed Rewards", 1989; Arthur Samuel, "Some Studies in Machine
 * Learning Using the Game of Checkers", 1959 (the first program that
 * learned a game, by playing itself); Gerald Tesauro, "Temporal
 * Difference Learning and TD-Gammon", 1995; Sutton and Barto,
 * *Reinforcement Learning: An Introduction*, 2018, chapter 6. *)

type ('state, 'action) t

(* [make ?rate ?discount ?explore ?seed ()]: an empty table. [rate] is
 * how far each step moves the estimate (0.2), and [seed] makes the
 * exploring repeatable. *)
val make : ?rate:float -> ?discount:float -> ?explore:float -> ?seed:int -> unit -> ('state, 'action) t

(* [value q s a]: what it thinks of doing [a] in [s], 0 if it has
 * never tried *)
val value : ('state, 'action) t -> 'state -> 'action -> float

(* [best q s actions]: the action it thinks best (the first of the
 * ties) *)
val best : ('state, 'action) t -> 'state -> 'action list -> 'action option

(* [choose q s actions]: the same, except [explore] of the time, when
 * it takes any of them at random. What it does while learning. *)
val choose : ('state, 'action) t -> 'state -> 'action list -> 'action option

(* [learn q ~state ~action ~reward ~next ~next_actions]: the rule
 * above, once. [next_actions] is what can be done in [next], and []
 * means the episode ended there -- then there is no future to
 * discount and the estimate moves towards [reward] alone. *)
val learn :
  ('state, 'action) t ->
  state:'state ->
  action:'action ->
  reward:float ->
  next:'state ->
  next_actions:'action list ->
  unit

(* A world to learn in: what can be done, what doing it leads to and
 * pays, and where it ends. *)
type ('state, 'action) world = {
  actions : 'state -> 'action list;
  step : 'state -> 'action -> 'state * float; (* where it lands, and the reward *)
  over : 'state -> bool;
}

(* [episode ?limit q world start]: play once from [start], learning at
 * every step, and return what it collected. [limit] (1000) stops a
 * wanderer that has not found the end. *)
val episode : ?limit:int -> ('state, 'action) t -> ('state, 'action) world -> 'state -> float

(* [greedy_run ?limit q world start]: play once *without* learning or
 * exploring -- what it has actually learned, which is not the same as
 * how it behaves while learning *)
val greedy_run : ?limit:int -> ('state, 'action) t -> ('state, 'action) world -> 'state -> float * 'state list

(* how many state-action pairs it has an opinion about: the table's
 * size, and the number that says why this does not scale *)
val known : ('state, 'action) t -> int

(* every action's value in a state, for drawing (examples/AiQlearn.ml
 * puts them in the four corners of each cell) *)
val values : ('state, 'action) t -> 'state -> 'action list -> ('action * float) list
