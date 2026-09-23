(* The phaser: notches without a delay line (see notes_synth.md section
 * 8.4).
 *
 * An all-pass filter lets every frequency through at the same level --
 * and delays each by a different *phase*: the first-order one,
 *
 *     y[n] = a x[n] + x[n-1] - a y[n-1]      a = (tan (pi fc / rate) - 1)
 *                                                / (tan (pi fc / rate) + 1)
 *
 * turns a frequency f by 2 atan (f / fc): nothing far below its break
 * frequency fc, 90 degrees at fc, 180 far above. Four in a row turn up
 * to 720 degrees; added to the dry sound, the frequencies turned by 180
 * degrees cancel, and those by 540 again:
 *
 *     8 atan (f / fc) = 180 deg  ->  f = fc tan (22.5 deg) = 0.414 fc
 *     8 atan (f / fc) = 540 deg  ->  f = fc tan (67.5 deg) = 2.414 fc
 *
 *     gain  ^
 *        1  |----.         .--------------.
 *           |     \       /                \      /-------
 *           |      \     /                  \    /
 *        0  +-------'---'--------------------'--'-----------> f
 *                   0.414 fc                  2.414 fc
 *
 * Two notches, *not* evenly spaced: 5.8 times apart, where a flanger's
 * are at 1, 3, 5, 7 times the first (Modulated_delay.mli) -- the
 * difference heard, a softer, vocal sweep, and seen on the spectrum. An
 * LFO moves fc (evenly in octaves, between [low] and [high]), the
 * notches with it; feedback (the output back into the first all-pass)
 * deepens the peaks between them. The MXR Phase 90 (1974) is four
 * stages, the Uni-Vibe (1968) four with lamps and photocells.
 *
 * Worked example (Unit_phaser): fc held at 1 kHz (low = high), no
 * feedback, mix 1: the gain 2.000 at 1 kHz, between the notches (where
 * the four have turned 360 degrees: back in phase, 2 = +6 dB), and the
 * notches found at 415 Hz and 2395 Hz, 1 Hz apart, their gains 0.0014
 * and 0.0002. The analog formula's 414 and 2414 are off by 1 and 19
 * Hz: the digital all-pass is the analog one with its frequencies
 * warped by the tan, exact at fc, farther from it the farther the
 * frequency.
 *
 * References: Julius O. Smith III, Physical Audio Signal Processing,
 * "Phasing with First-Order Allpass Filters",
 * https://ccrma.stanford.edu/~jos/pasp/; Will Pirkle, Designing Audio
 * Effect Plugins in C++, 2nd ed. 2019, chapter 15. *)

type settings = {
  low : float; (* the sweep's lowest break frequency, Hz *)
  high : float; (* its highest *)
  rate : float; (* the LFO, Hz *)
  feedback : float; (* -0.9 to 0.9 *)
  mix : float; (* the phased copy's level, 0 to 1, the dry sound kept *)
}

(* 200 Hz to 3 kHz at 0.3 Hz, feedback 0.3, mix 1 *)
val initial : settings

(* the number of all-pass stages: 4 *)
val stages : int

type t

val create : unit -> t

(* [process t settings s]: [s] in place, both channels *)
val process : t -> settings -> Signal.stereo -> unit
