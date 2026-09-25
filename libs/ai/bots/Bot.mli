(* A bot: something that plays the game the way a player does (see
 * notes_ai.md section 6).
 *
 * The other modules here are an opponent's mind (Minimax: it moves
 * pieces) or an agent's motion (Steering, Pathfind: it is pushed
 * around). A bot is neither: it stands where a player stands, and the
 * game must not be able to tell. So it fills the same record the keys
 * fill --
 *
 *     the world --> Sense --> decide --> intent --> the game's update
 *                   what a    steering,  the same record a player's
 *                   player    fsm, aim   keys fill, so update doesn't
 *                   knows                know which is which
 *
 * -- and this module is the two middle arrows: [sense] then [decide],
 * with the knobs that make a bot *fair* rather than strong.
 *
 * {2 Difficulty without cheating}
 *
 * The lazy way to make a bot hard is to give it more: more speed, more
 * damage, your position through a wall. The honest knobs are these,
 * and each is a number here:
 *
 *   what it knows   Sense.mli: its input is not the world (a type, not
 *                   a promise)
 *   delay           it acts on the senses of [delay] frames ago; a
 *                   human's reaction is about 250 ms, 15 frames at 60
 *                   a second
 *   rate            it may change its mind every [rate] frames only,
 *                   and repeats its last intent in between: a bot that
 *                   fires on the exact frame a target appears is
 *                   recognisably a machine
 *   aim error       [aim_error], an offset that decays the longer the
 *                   target stays visible, so the bot settles on you
 *                   like a hand does instead of snapping
 *
 * {2 Reflexes}
 *
 * The delay is fair for what a bot perceives of the others, and absurd
 * for its own body: a player reacts late to an enemy stepping out, not
 * to the edge of the cliff under their feet. So a bot may have a
 * [reflex], run every frame on the world as it is *now*, which adjusts
 * the intent the (late, repeated) decision gave: the feet that do not
 * step into the water, the way to a chosen place found from where the
 * bot stands rather than from where it stood a tenth of a second ago.
 * The decision is what is repeated between two changes of mind, never
 * what the reflex made of it.
 *
 * Without it, [delay] turns a bot that walks towards a point into an
 * oscillator: it corrects from where it was [delay] frames ago,
 * overshoots, corrects back (TinyBoomerangFu.ml met exactly that,
 * walking onto a path's lane). A reflex is not a cheat, since it knows
 * nothing of the enemies -- the game's [reflex] should look at the
 * bot's own body and the ground only.
 *
 * Example: [delay = 15], [rate = 6]. The target steps out at frame
 * 100. The bot's senses see it at frame 115, and it may act at the
 * next multiple of 6: frame 120, a third of a second after a player
 * would have seen it. Turn both to 0 and it is a machine again, which
 * is the demonstration ([examples/AiBots.ml]).
 *
 * The mind is [decide], and it is the game's (or a genre kit's): a
 * racing bot aims at the next waypoint, a shooter's keeps its
 * distance. What is here is only the loop around it -- which is why
 * this module knows nothing of pixels, of what a wall is, or of how
 * many dimensions the game has: 2D and 3D bots differ in their senses
 * and their intent, both of them type parameters.
 *
 * References: Mat Buckland, "Programming Game AI by Example", 2005;
 * Steve Rabin (ed.), "Game AI Pro", 2013 (bots that lose gracefully);
 * Quake III Arena's bots (Jan Paul van Waveren, "The Quake III Arena
 * Bot", 2001): the same split of senses, mind and intent. *)

(* a bot: what it may know of the world, and what it does with it.
 * [sense] is given what it knew last frame, because part of what a bot
 * senses is what it remembers (Sense.mli's [target] ages inside the
 * senses record): the game keeps no memory of its own *)
type ('world, 'senses, 'intent) t = {
  sense : 'senses option -> 'world -> 'senses;
  decide : 'senses -> 'intent;
  delay : int; (* frames between what it sees and what it acts on *)
  rate : int; (* frames between two changes of mind (1: every frame) *)
  reflex : 'world -> 'intent -> 'intent; (* every frame, on the world now *)
}

(* [make ?delay ?rate ?reflex ~sense ~decide ()]: [delay] 0 and [rate]
 * 1 by default, i.e. a machine; no reflex (the intent as decided) *)
val make :
  ?delay:int ->
  ?rate:int ->
  ?reflex:('world -> 'intent -> 'intent) ->
  sense:('senses option -> 'world -> 'senses) ->
  decide:('senses -> 'intent) ->
  unit ->
  ('world, 'senses, 'intent) t

(* what the bot carries between frames: the senses it has seen but not
 * yet acted on, and the intent it is repeating *)
type ('senses, 'intent) running

(* [start intent]: doing that until it first decides otherwise *)
val start : 'intent -> ('senses, 'intent) running

(* [step bot world r]: the intent this frame, and the state for the
 * next: the world sensed now (from what was sensed last frame), the
 * senses of [delay] frames ago decided upon if this frame is one of
 * the [rate]th, the last decision repeated otherwise -- and that
 * through the [reflex], on [world] *)
val step : ('world, 'senses, 'intent) t -> 'world -> ('senses, 'intent) running -> 'intent * ('senses, 'intent) running

(* [last_senses r]: what it sensed on the last [step] (not what it has
 * acted on: that is [delay] frames older) -- for a game's tests, and
 * for drawing what a bot knows *)
val last_senses : ('senses, 'intent) running -> 'senses option

(* [aim_error ~spread ~settle ~seen_for ~seed ()]: how far off a bot's
 * aim is, in whatever unit the game aims in (degrees, say): [spread]
 * at the moment a target appears, decaying by half every [settle]
 * frames it stays visible ([seen_for], Sense.mli). Deterministic: a
 * smooth wobble of [seen_for] and [seed] (a bot's number), not
 * Random, so a game replays the same *)
val aim_error : spread:float -> settle:float -> seen_for:int -> seed:int -> unit -> float
