(* Filters: shaping the spectrum (see notes_audio.md section 7).
 *
 * A filter lets some frequencies through and weakens others. Its
 * memory is what does it: a filter's output depends on the last few
 * samples, and a slow wave looks the same from one sample to the next,
 * a fast one doesn't.
 *
 * The simplest, the one-pole low-pass: a running average following
 * the input slowly, one line,
 *
 *     y := y + a (x - y)          a = 1 - exp (-2 pi cutoff / rate)
 *
 * a fraction [a] of the way towards the input at each sample: a slow
 * wave is followed, a fast one averaged away. For a 1000 Hz cutoff at
 * 44,100, a = 0.133. Its gain falls by half (-6 dB) for each octave
 * above the cutoff (-3 dB at the cutoff itself): gentle. The input less
 * the low-pass is the high-pass, the fast part.
 *
 * The biquad: two samples of memory of the input and two of the
 * output, five coefficients (b0 b1 b2 over a1 a2, the numerator and
 * the denominator of its transfer function: "bi-quadratic"),
 *
 *     y[n] = b0 x[n] + b1 x[n-1] + b2 x[n-2] - a1 y[n-1] - a2 y[n-2]
 *
 * falls twice as fast (-12 dB an octave), and has a resonance, Q: a
 * boost at the cutoff, from nothing (Q = 0.707, "Butterworth", the
 * flattest) to a whistle (Q = 10: +20 dB, the filter ringing at its
 * own frequency). Robert Bristow-Johnson's cookbook gives the five
 * numbers for every classic filter from the cutoff and Q; the low-pass,
 * with w = 2 pi cutoff / rate and alpha = sin w / (2 Q):
 *
 *     b0 = b2 = (1 - cos w) / 2,  b1 = 1 - cos w,
 *     a0 = 1 + alpha,  a1 = -2 cos w,  a2 = 1 - alpha   (all over a0)
 *
 *     a low-pass at 1 kHz, its gain (dB) at     1 kHz   2 kHz   4 kHz
 *     the one-pole                               -3.0    -7.0   -12.2
 *     the biquad, Q = 0.707 (Butterworth)        -3.0   -12.4   -24.6
 *     the biquad, Q = 10                        +20.0    -9.7   -24.0
 *
 * (Unit_filter checks them, on sines actually filtered.)
 *
 * [response] computes the gain at a frequency from the coefficients
 * (the transfer function on the unit circle, H(e^iw)), which the tests
 * compare with a sine actually filtered.
 *
 * What they're for: subtractive synthesis, the analog synthesizers'
 * way (Moog, 1964): a wave rich in harmonics (a sawtooth, a square,
 * noise) with some taken away. A low-pass on noise is a rumble, an
 * engine, an explosion; a high-pass a hiss, a cymbal. A resonant
 * low-pass whose cutoff moves is the "wah" of every synthesizer since
 * ([sweep]): the resonance's boost sweeping through the harmonics.
 *
 * References: Julius O. Smith III, Introduction to Digital Filters,
 * 2007, https://ccrma.stanford.edu/~jos/filters/; Robert
 * Bristow-Johnson, "Cookbook formulae for audio EQ biquad filter
 * coefficients" (the Audio EQ Cookbook), 1998; Robert Moog, "Voltage-
 * Controlled Electronic Music Modules", Journal of the Audio
 * Engineering Society, 1965. *)

(*****************************************************************************)
(* {1 The one-pole} *)
(*****************************************************************************)

(* [one_pole_coefficient cutoff]: a above, 0.133 for 1000 Hz *)
val one_pole_coefficient : float -> float

(* [low_pass ~cutoff s], [high_pass ~cutoff s]: one-pole *)
val low_pass : cutoff:float -> Signal.t -> Signal.t
val high_pass : cutoff:float -> Signal.t -> Signal.t

(*****************************************************************************)
(* {1 The biquad} *)
(*****************************************************************************)

type kind = Low_pass | High_pass | Band_pass

(* the five coefficients, a0 divided out *)
type biquad = { b0 : float; b1 : float; b2 : float; a1 : float; a2 : float }

(* [biquad kind ~cutoff ~q]: the cookbook's (the band-pass's peak at 0
 * dB) *)
val biquad : kind -> cutoff:float -> q:float -> biquad

(* [response f frequency]: its gain at [frequency], 1 = unchanged *)
val response : biquad -> float -> float

(* [run f s]: [s] through [f], from silence *)
val run : biquad -> Signal.t -> Signal.t

(* a biquad's memory, the last two inputs and outputs: what lets a
 * continuous sound go through a filter a block at a time, as the mixer
 * pulls it, without a click where the blocks meet *)
type memory

val silence : unit -> memory

(* [step f m x]: one sample through [f], [m] updated *)
val step : biquad -> memory -> float -> float

(* [sweep kind ~q ~from ~to_ s]: through a biquad whose cutoff moves
 * from [from] to [to_] over [s], evenly in octaves (as the ear hears
 * it), its coefficients recomputed at each sample *)
val sweep : kind -> q:float -> from:float -> to_:float -> Signal.t -> Signal.t
