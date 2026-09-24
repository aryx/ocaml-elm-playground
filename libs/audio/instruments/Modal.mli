(* A mode: a resonator struck, ringing as a decaying sine (see
 * notes_synth.md section 10; plan_synth_teaching.md, TinyRhodes, R1).
 *
 * Anything struck -- a tine, a bar, a bell, a drum -- vibrates in its
 * *modes*: each a sine at its own frequency, dying at its own rate, the
 * sound their sum. Modal synthesis builds a struck object from them,
 * as additive synthesis (TinyHammond) builds an organ from sines, but
 * each mode is a *resonator* rather than an oscillator: it rings only
 * when struck, keeps ringing on its own, and can be damped while it
 * does -- a Rhodes' felt coming back on the tine, a hand on a cymbal.
 *
 * The resonator is two poles, the smallest filter that rings:
 *
 *     y[n] = 2 r cos w  y[n-1]  -  r^2  y[n-2]  +  x[n]
 *
 * w the frequency (2 pi f / rate), r how much of its amplitude a sample
 * keeps (r^n the decay: -60 dB after t60 seconds when r = 10^(-3 / (t60
 * rate))). Struck by an impulse of [a sin w], it answers
 *
 *     y[n] = a r^n sin (w (n + 1))
 *
 * a sine starting from rest -- a hammer gives the tine a velocity, not a
 * displacement -- and dying exactly as r^n. Its cost is two multiplies
 * a sample, no sine computed: why modal synthesis ran on 1990s chips.
 * Damped, r changes and the ringing goes on from where it is, faster
 * to silence. The TR-808's drums are the same idea in analogue: a
 * bridged-T filter on the edge of oscillation, struck by a pulse.
 *
 * Worked example (Unit_modal): 440 Hz, t60 1 s, struck at 0.5: the
 * peak 0.498 in the first period (its crest between two samples), 440
 * Hz by its spectrum, -60 dB after 44,100 samples to 0.1 dB; damped to
 * t60 0.1 s a quarter second in, -60 dB 0.1 s later; struck again while
 * ringing: the two added, to 1e-12 (a linear system). *)

type t

(* [create ~frequency ~t60]: at rest *)
val create : frequency:float -> t60:float -> t

(* [strike t a]: struck: [a] more of amplitude, the ringing so far kept *)
val strike : t -> float -> unit

(* [damp t ~t60]: its decay from now on, its motion kept *)
val damp : t -> t60:float -> unit

(* [next t]: the next sample *)
val next : t -> float

(* [level t]: its amplitude now, a r^n for each strike summed at most:
 * below a threshold, silent *)
val level : t -> float
