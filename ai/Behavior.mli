(* Behavior trees: deciding by trying things in order, as a tree of
 * fallbacks, instead of by states and transitions.
 *
 * Halo 2 (Damian Isla, Bungie, GDC 2005) made them the industry's
 * answer to state machines grown too big: in a machine every state
 * must know every other it can go to, so adding a behaviour touches
 * the rest; in a tree, a behaviour is a branch, added where its
 * priority is. Three kinds of node do nearly everything:
 *
 *   Selector   try the children in order: the first that succeeds wins
 *              ("or": the fallbacks, the priorities)
 *   Sequence   do the children in order: fail as soon as one fails
 *              ("and": a condition, then what to do)
 *   Condition  a question about the world, succeeding or failing
 *
 * plus the leaves that decide, [Action]s, and [Not]. The guard dog --
 * flee when hurt, attack an intruder who comes near, else patrol --
 * the same one ai/Fsm.mli and ai/Utility.mli write their way:
 *
 *   Selector
 *   |-- Sequence -- hurt?     -- Flee
 *   |-- Sequence -- intruder? -- Attack
 *   +-- Patrol
 *
 * Read top to bottom, it is the dog's priorities; a new behaviour
 * (bark when the intruder is far) is one more branch, touching nothing
 * else.
 *
 * This tree is pure: it *decides* -- [decide] answers the action to take
 * -- and the game does it, the Elm architecture's way, where the
 * behaviour trees of engines run their actions, which take frames and
 * so answer "running" as well as success and failure, and keep a
 * blackboard of what they remember between frames. Both are left out:
 * a game here keeps its memory in its model, and asks again each frame.
 *
 * Worked example: the dog, hurt and with an intruder near, flees (the
 * first branch); healthy with the intruder near, attacks; healthy and
 * alone, patrols -- and [path] says which nodes each answer went
 * through: "hurt?" failed, "intruder?" held, "Attack".
 *
 * References: Damian Isla, "Handling Complexity in the Halo 2 AI" (GDC
 * 2005); Alex Champandard's AiGameDev articles (2007-); Michele
 * Colledanchise and Petter Ögren, "Behavior Trees in Robotics and AI"
 * (2018). *)

type ('context, 'action) t =
  | Action of string * 'action (* succeeds, deciding [action] *)
  | Condition of string * ('context -> bool)
  | Sequence of ('context, 'action) t list
  | Selector of ('context, 'action) t list
  | Not of ('context, 'action) t

(* the action the tree decides, if it succeeds: the last action along
 * the way that succeeded *)
val decide : ('context, 'action) t -> 'context -> 'action option

(* the labels of the nodes [decide] visited, in order, each with
 * whether it succeeded: what the tree was thinking *)
val path : ('context, 'action) t -> 'context -> (string * bool) list
