(* The diode ladder: the TB-303's filter, and why it squelches (see
 * notes_synth.md; plan_synth_teaching.md, TinyTB303, B1).
 *
 * Moog's ladder (Moog_ladder.mli) is four one-pole filters in a row,
 * each *buffered* by its transistors: a pole doesn't feel the next
 * one, and the four together are exactly the product of four equal
 * one-poles. Roland's TB-303 (1981) builds its ladder of diodes
 * instead, and a diode ladder's stages load each other: each capacitor
 * is charged from the one before and drained by the one after,
 *
 *         u --[2w]-- v1 --[w]-- v2 --[w]-- v3 --[w]-- v4 --> out
 *                     |          |          |          |
 *                    C/2         C          C          C
 *
 *     v1' = 2w ((u - v1) - (v1 - v2))       u = x - k v4 (the feedback)
 *     v2' =  w ((v1 - v2) - (v2 - v3))
 *     v3' =  w ((v2 - v3) - (v3 - v4))
 *     v4' =  w  (v3 - v4)
 *
 * (w = 2 pi fc; the first capacitor half the others', the 303's: its
 * pole an octave up). The coupling spreads the four poles apart
 * instead of stacking them: the slope just above the cutoff is gentler
 * than the Moog's 24 dB an octave, reaching it only far above -- the
 * "18 dB an octave" the 303 is said to have, measured here; and the
 * resonance, the feedback k, peaks less sharply and needs more feedback
 * to sing: the rubbery, vocal squelch of acid, where the Moog whistles.
 *
 * Solved as the Moog ladder's zero-delay model is (Zavalishin), but
 * written out: the four equations integrated by the trapezoidal rule,
 * with the cutoff prewarped (w from tan (pi fc / rate), so that the
 * digital filter's frequencies are the analog one's at fc), which makes
 * each sample a 4 x 4 linear system in the new v1..v4, solved exactly --
 * no delay in the loop, the resonance where it belongs. A tanh on the
 * input (the diodes bend loud signals) keeps it gritty and bounded.
 *
 * Worked example (Unit_diode_ladder), measured on sines against
 * Moog_ladder's zero-delay model at the same fc = 500 Hz, no feedback:
 *
 *                     at fc     1 -> 2 kHz   2 -> 4 kHz   4 -> 8 kHz
 *     diode ladder   -21.3 dB   -15.8 dB     -20.7 dB     -25.8 dB
 *     Moog ladder    -12.0      -21.4        -24.0        -27.0
 *
 * the spread poles: the gain at fc far lower (the 303's cutoff knob
 * sets the poles' scale, not a -3 dB point), then a slope of about 16
 * dB an octave just above -- the "18 dB" -- reaching the Moog's only
 * far above (both steeper near Nyquist, the trapezoid's zero there).
 * A constant through it 1 / (1 + k), as the Moog's; the feedback where
 * it starts to ring on its own 22.1 (the Moog's 4: the coupling spends
 * the loop's gain); at 80% of that, its peak at 667 Hz, above the
 * cutoff, where the Moog's sits at it.
 *
 * References: Tim Stinchcombe, "Analysis of the Moog Transistor Ladder
 * and Derivative Filters", 2008, and his TB-303 diode ladder model,
 * https://www.timstinchcombe.co.uk/index.php?pge=diode2; Vadim
 * Zavalishin, The Art of VA Filter Design, 2012 (rev. 2018), chapter 5
 * (the diode ladder); Robin Whittle's "303 unique" page. *)

(* the four capacitors' voltages *)
type t

val create : unit -> t
val reset : t -> unit

(* [process t ~cutoff ~resonance samples]: [samples] filtered in place,
 * [cutoff] in Hz one per sample (kept within 10 Hz and 20 kHz),
 * [resonance] the feedback k *)
val process : t -> cutoff:Signal.t -> resonance:float -> Signal.t -> unit
