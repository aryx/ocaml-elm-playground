(* What a bot is allowed to know (see notes_ai.md section 6).
 *
 * A bot that reads the world's state directly plays a different game
 * from the player: it knows where you are through a wall, the moment
 * you get there. That is the cheap way to make it hard, and the way
 * that makes it unfun -- the player can tell, and what they feel is
 * not "that was clever" but "that was unfair".
 *
 * So a bot's input is not the world but this: one [target] per thing it
 * might care about, holding only what a player in its place could know.
 *
 *     the world  -->  Sense  -->  the mind  -->  intent
 *     (the model)     here        Bot.mli       the keys' record
 *
 * The geometry stays in the game: only it knows what a wall is, whether
 * a shape is in the way, how far its units are. So [update] is told
 * [distance] and whether the line between them is [clear] -- one swept
 * test in a 2D game (Physics.went_through), one ray in a 3D one -- and
 * keeps what follows from them over time:
 *
 *   visible    clear, and within [sight]
 *   audible    within [hearing] (a shorter range, through walls: steps,
 *              a shot, an engine)
 *   position   where the target is, while visible; where it was last
 *              seen, after that -- which is what makes a bot look as if
 *              it were searching for you rather than tracking you
 *              through the wall
 *   age        frames since [position] was true: 0 while visible
 *   seen_for   frames it has been visible without a break, which is
 *              what an aim settles on (Bot.aim_error)
 *
 * Example: a target 300 away and hidden, with sight 600 and hearing
 * 200: not visible, not audible, no position. It steps out: visible,
 * position its own, age 0, seen_for counting up. It hides again: not
 * visible, the position stays where it was, age counting up. A bot that
 * runs at [position] while [age] is small and gives up when it is large
 * looks like it is hunting.
 *
 * The type is the point: a bot written as [senses -> intent] cannot
 * peek at the world, because it hasn't got it. It is a *type*, not a
 * promise.
 *
 * References: Mat Buckland, "Programming Game AI by Example", 2005,
 * chapter 9 (a sensory memory per agent); Steve Rabin (ed.), "Game AI
 * Pro", 2013, on perception and on bots that lose rather than cheat. *)

(* what a bot knows about one target; ['v] is however the game says
 * where something is (a pair in 2D, a triple in 3D: this module never
 * looks inside it) *)
type 'v target = {
  visible : bool;
  audible : bool;
  position : 'v option; (* where it is, or was last seen *)
  age : int; (* frames since [position] was true; 0 while visible *)
  seen_for : int; (* frames visible without a break *)
}

(* nothing known yet *)
val unknown : 'v target

(* [update ?sight ?hearing ~distance ~clear ~position t]: [t] one frame
 * later, given what the game measured this frame. [sight] is infinite
 * by default, [hearing] 0 (deaf); [clear] is the game's line of sight
 * (it alone knows the walls) *)
val update : ?sight:float -> ?hearing:float -> distance:float -> clear:bool -> position:'v -> 'v target -> 'v target

(* [lost t]: not visible, and nothing remembered either (never seen, or
 * [forget]ten) *)
val lost : 'v target -> bool

(* [forget ~after t]: the remembered position dropped once [age] passes
 * [after] frames: the bot gives up the hunt *)
val forget : after:int -> 'v target -> 'v target

(* [nearest targets]: the visible one that is nearest, by the distances
 * last given to [update] -- the game pairs each target with its
 * distance *)
val nearest : (float * 'v target) list -> 'v target option
