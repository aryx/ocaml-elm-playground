(* The EQ: the three bands of a hi-fi's tone controls, the bass, the
 * middle and the treble (see notes_synth.md section 8.2).
 *
 * Filter's cookbook EQ, one after the other, a pair of memories per
 * channel:
 *
 *     x --> low shelf 200 Hz --> bell 1 kHz, Q 0.7 --> high shelf 4 kHz --> y
 *
 * each from -12 to +12 dB; at 0 dB a band is exactly no filter (its
 * numerator and denominator the same), so a flat EQ changes nothing.
 * Its curve, the three multiplied, is Filter.response's. A gain turned
 * is ramped over the next block as Effect.mli's knobs are, but a
 * filter's coefficients can't be multiplied in: they are recomputed
 * every 32 samples (0.7 ms), each time the gains a step nearer. *)

type t

val create : unit -> t

(* [process t ~bass ~middle ~treble s]: [s] in place, the gains in dB *)
val process : t -> bass:float -> middle:float -> treble:float -> Signal.stereo -> unit

(*****************************************************************************)
(* {1 As an effect} *)
(*****************************************************************************)

(* bass, middle, treble: -12 to 12 dB, 0 at first *)
val knobs : Effect.knob list

(* [effect ()]: "eq" *)
val effect : unit -> Effect.t
