(* Utility AI: deciding by scoring every option and taking the best,
 * instead of by rules.
 *
 * The Sims (Will Wright, Maxis, 2000) is the famous case: a Sim's
 * needs -- hunger, fun, sleep -- each a number, every object in the
 * house advertising how much it would satisfy which, and the Sim going
 * for the best offer. No state says "hungry": hunger just grows until
 * the fridge outscores the television. Dave Mark's "Behavioral
 * Mathematics for Game AI" (2009) made it a method: each option a
 * score between 0 and 1, from a few *curves* of the situation.
 *
 * The guard dog of Fsm.mli and Behavior.mli, scored:
 *
 *   flee     (1 - health)^2        the more hurt, the much more urgent
 *   attack   closeness * health     near, and fit enough to fight
 *   patrol   0.2                    the default, when nothing presses
 *
 * What the rules can't do, and this does: *degrees*. A dog at 40%
 * health with the intruder close (0.9) is torn -- flee 0.36, attack 0.36 --
 * and the answer moves smoothly with the numbers, where a rule has a
 * threshold. What it costs: the behaviour is in the numbers, so it is
 * found by tuning, and can surprise; and two options scoring nearly
 * the same flip back and forth, frame after frame. The cure is
 * [inertia]: a bonus to what the agent is already doing -- hysteresis
 * again, as Fsm.mli's guards need, in a different place.
 *
 * Worked example: health 1, the intruder at closeness 0.8: flee 0,
 * attack 0.8, patrol 0.2 -- attack. Health 0.3, closeness 0.8: flee
 * 0.49, attack 0.24 -- flee. Health 0.4, closeness 0.95: flee 0.36,
 * attack 0.38 -- attack; but a dog already fleeing, with an inertia of
 * 0.1, keeps fleeing (0.46 against 0.38).
 *
 * References: Dave Mark, "Behavioral Mathematics for Game AI" (2009),
 * and his GDC talks with Kevin Dill ("Improving AI Decision Modeling
 * Through Utility Theory", 2010); the Sims' "advertisements". *)

type ('context, 'action) option_ = {
  action : 'action;
  label : string;
  score : 'context -> float; (* between 0 and 1, by convention *)
}

(* every option's score, in order *)
val scores : ('context, 'action) option_ list -> 'context -> ('action * float) list

(* [choose ?current ~inertia options c]: the best-scoring option's
 * action, [current]'s score raised by [inertia] (0 by default) --
 * ties to the first; None when there are no options *)
val choose : ?current:'action -> ?inertia:float -> ('context, 'action) option_ list -> 'context -> 'action option

(* curves: [clamp01 x] into [0, 1]; [linear ~lo ~hi x]: 0 at [lo], 1 at
 * [hi], straight in between, clamped *)
val clamp01 : float -> float
val linear : lo:float -> hi:float -> float -> float
