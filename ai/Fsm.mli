(* Finite state machines: a character that is always in one of a few
 * states, each with its own behaviour, and rules for going from one to
 * another.
 *
 * The oldest game AI and still the commonest. Pac-Man's ghosts (Toru
 * Iwatani, Namco, 1980) are machines, and the Pac-Man Dossier (Jamey
 * Pittman, 2009) writes them down to the frame:
 *
 *              released                  a power pellet
 *   Waiting ------------> Leaving ----> Hunting ---------> Frightened
 *                            ^      out    ^   time's up      |
 *                            |             +------------------+ eaten
 *                            +---- home ---- Eyes <-----------+
 *
 * In OCaml a state machine is already a variant and a [match] -- which
 * is how games/TinyPacman.ml writes its ghosts by default -- so a
 * library has to earn its place, and this one is small because it only
 * adds three things the [match] doesn't give:
 *
 *  - the rules as data: a machine is a list of transitions, each from a
 *    state, when a guard holds, to a state, with a label. So the machine
 *    can be drawn (examples/AiGhosts.ml draws the ghost's, the state it
 *    is in lit up) and checked (every state reachable, none a dead end);
 *  - the time spent in the state, counted for you, which nearly every
 *    rule wants: "after 420 frames", "released after 3 seconds"
 *    ([after]); Pac-Man's scatter/chase waves are nothing else;
 *  - the transition that fired this step ([fired]), for what happens
 *    on entering a state -- Pac-Man's ghosts turn round when their mode
 *    changes -- without a second machine to track the first.
 *
 * One transition per step, the first rule (in the list's order) out of
 * the current state whose guard holds: the order of the rules is their
 * priority, so "eaten" is written before "time's up".
 *
 * The lesson of the guards, from games3d/TinyBoomerangFu.ml's computer:
 * two rules out of each other's states on the same threshold ("close"
 * and "not close") make an agent flip between them every frame and go
 * nowhere, when it sits on the threshold. The cure is hysteresis --
 * leave at a different threshold than you enter, like a thermostat --
 * written in the guards, not in the library.
 *
 * Worked example: a traffic light, green for 3 steps, orange 1, red 2.
 * Started green: steps 1-2 stay green (since 1, 2), step 3 fires
 * "change" to orange (since 0), step 4 to red, and step 6 back to
 * green.
 *
 * References: the Pac-Man Dossier; Mat Buckland, "Programming Game AI by
 * Example" (2005), chapter 2; and, for when states multiply, David Harel's
 * statecharts ("Statecharts: A Visual Formalism for Complex Systems",
 * 1987), nested states, which this does not do. *)

type ('state, 'context) rule = {
  from : 'state;
  label : string; (* what happens, for drawing it *)
  guard : 'context -> int -> bool; (* the context, and the steps spent in [from] *)
  target : 'state;
}

type ('state, 'context) machine = ('state, 'context) rule list

type 'state run = {
  state : 'state;
  since : int; (* the steps spent in it: 0 the step it was entered *)
  fired : string option; (* the label of the transition this step took, if one *)
}

val start : 'state -> 'state run

(* [step machine context run]: the first rule out of [run.state] whose
 * guard holds, taken; or the same state, a step older *)
val step : ('state, 'context) machine -> 'context -> 'state run -> 'state run

(* [after n]: a guard holding once [n] steps are spent in the state *)
val after : int -> 'context -> int -> bool

(* the states a machine names, in the order they first appear *)
val states : ('state, 'context) machine -> 'state list
