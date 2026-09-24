(* Chorus and flanger: a delay whose length moves (see notes_synth.md
 * section 8.3).
 *
 * One idea, two effects. A delay line is read at a length an LFO moves
 * up and down, and the copy is added to the sound:
 *
 *     x --+----------------------------------------( + )--> y
 *         |                                           ^
 *         '--> [ delay line, read d(t) ago ] -- mix --'
 *                  d(t) = center + depth sin (2 pi rate t)
 *
 * A delay that *shortens* is read faster than it's written: the copy's
 * pitch goes up, by the delay's rate of change -- the copy is heard at
 * a frequency f (1 - d'(t)). A lengthening delay, down. So the moving
 * delay is a vibrato on the copy, and the copy beside the dry sound:
 *
 *  - a *chorus*, the delay around 15 ms (too long to comb, too short to
 *    echo), no feedback: the dry note and a copy wobbling slightly out of
 *    tune, the "several players" sound (the Juno-60's, 1982, a
 *    bucket-brigade chip). Ours has the right channel's LFO a quarter
 *    turn ahead, so the two sides go out of tune differently: wide.
 *    The Juno's own is shorter (1.66 to 5.35 ms), a triangle, the right
 *    side's modulation inverted: Voice_juno.mli, from Andy Harman's
 *    measurements;
 *  - a *flanger*, the delay short (1 to 5 ms), with feedback: the sound
 *    plus itself d later cancels where d is half a period, at (2k + 1)
 *    / (2 d), notches *evenly spaced* in frequency, sweeping as d moves:
 *    the jet plane, first made by two tape machines playing the same
 *    tape, a thumb on one reel's flange (hence the name).
 *
 * The read falls between samples (15 ms is 661.5 samples, and moving):
 * the two neighbours mixed, Resample's linear interpolation, whose own
 * low-pass Delay.mli measured. (An all-pass interpolator keeps the
 * highs: an exercise.)
 *
 * Worked examples (Unit_modulated_delay):
 *
 *  - the chorus's detune: 15 ms +- 3 ms at 0.5 Hz; the delay changes by
 *    at most 2 pi x 0.5 x 0.003 = 0.0094 seconds per second, so the
 *    copy's pitch swings by 1200 log2 (1.0094) = +-16.2 cents. A 1 kHz
 *    sine's copy, measured from its zero crossings over 40 ms where
 *    the LFO is steepest: 1009.42 Hz (+16.2 cents, the delay
 *    shortening) and 990.58 Hz (-16.4, lengthening);
 *  - the flanger's notches: the delay held at 1 ms (no depth), no
 *    feedback, the copy as loud as the sound (mix 1): gain 0.0002 at
 *    500 Hz (the first notch), 1.999 at 1 kHz (a peak, 2 = +6 dB),
 *    0.002 at 1.5 kHz (the second notch). With feedback 0.7 the peak
 *    grows to 4.05 at 1 kHz (+12 dB): the comb's teeth sharper.
 *
 * References: Jon Dattorro, "Effect Design, Part 2: Delay-Line
 * Modulation and Chorus", JAES 45(10), 1997; Julius O. Smith III,
 * Physical Audio Signal Processing, "Flanging" and "Chorus Effect",
 * https://ccrma.stanford.edu/~jos/pasp/. *)

type settings = {
  center : float; (* the delay's middle, seconds *)
  depth : float; (* how far it moves either way, seconds *)
  rate : float; (* the LFO, Hz *)
  feedback : float; (* -0.95 to 0.95 *)
  mix : float; (* the copy's level, 0 to 1, the dry sound kept *)
}

(* 15 ms +- 3 ms at 0.5 Hz, no feedback, mix 0.5 *)
val chorus : settings

(* 2.5 ms +- 2 ms at 0.2 Hz, feedback 0.5, mix 0.7 *)
val flanger : settings

(* the longest delay: 50 ms *)
val longest : float

type t

val create : unit -> t

(* [process t settings s]: [s] in place, both channels *)
val process : t -> settings -> Signal.stereo -> unit
