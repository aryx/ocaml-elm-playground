(* The Rhodes Stage 73's panel, with its Suitcase, as a part
 * (Component.mli; Part_hammond.mli says why a part, and how it closes
 * over its voice).
 *
 * The knobs: the model (the Rhodes, the Wurlitzer, the Clavinet); the
 * voicing (how far the tine sits off its pickup's centre, a
 * screwdriver's job on the real one); the hammer's hardness; the decay;
 * the Suitcase's vibrato, rate and depth; the volume. Beside them what
 * can't be seen on the real one: the pickup's curve, the flux against
 * the tip's position (the Rhodes' bell, the Wurlitzer's capacitance),
 * and on it the span the last note's tip swings across now; and the
 * Suitcase's two speakers, lit as the tremolo moves the sound between
 * them. *)

val kind : string

(* 960 x 460 *)
val natural : float * float
val make : Voice_rhodes.t -> Component.part
val load : Voice_rhodes.t -> string -> Component.part
