(* The rack's modulation slot: a chorus, a flanger or a phaser, one at
 * a time, behind the same knobs (see notes_synth.md sections 8.3 and
 * 8.4).
 *
 * The three are the modulation effects, a copy of the sound moved by an
 * LFO and added back (Modulated_delay.mli, Phaser.mli), and a pedal
 * board has one of them where the rack has one slot: the same four
 * knobs, rate, depth, feedback and mix, meaning for each what its own
 * numbers are --
 *
 *     depth       chorus               flanger              phaser
 *     0.5         15 ms +- 3 ms        2.5 ms +- 2 ms       200 Hz to 3.2 kHz
 *     in general  +- 6 ms x depth      +- 4 ms x depth      200 Hz x 2^(8 depth)
 *
 * so switching kinds keeps the knobs where they are and changes what
 * moves. *)

type kind = Chorus | Flanger | Phaser

val kinds : kind list
val name : kind -> string

(* kind (chorus, flanger, phaser: the chorus), rate (0.05 to 5 Hz: 0.5),
 * depth (0 to 1: 0.5), feedback (0 to 0.9: 0), mix (0 to 1: 0.5) *)
val knobs : Effect.knob list

(* [effect ()]: "modulation" (not "mod": a synthesizer's own panel has
 * a modulation section, the Minimoog's "mod.mix") *)
val effect : unit -> Effect.t
