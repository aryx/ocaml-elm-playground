(* Seeing the sound: an oscilloscope and a spectrum drawn over the frame,
 * the magnifier of sound (docs/claude_notes/plan_audio_teaching.md,
 * "Debugging sound by looking at it"). For the backends' debug keys
 * (the software backend's "v", with -debug-keys), not for games.
 *
 * - The oscilloscope: the last samples played, as a wave, from a
 *   rising zero crossing (the trigger of a real oscilloscope: a steady
 *   tone then stands still instead of scrolling): a square looks
 *   square, a chord like a beat.
 * - The spectrum: the same samples' frequencies (Spectrum.mli, an
 *   FFT of 2048 samples, Hann-windowed), as bars on a logarithmic
 *   frequency axis from 20 Hz to 20 kHz (as the ear hears: an octave
 *   the same width anywhere), their heights in decibels from -80 to 0:
 *   a sine's one bar, a square's odd harmonics, noise's everything.
 *
 * Drawn as ordinary Playground shapes, so any backend could show them. *)

(* [record samples]: the samples just played (the platform's pull), the
 * last 2048 kept *)
val record : float array -> unit

type view = Off | Oscilloscope | Spectrum

(* off, oscilloscope, spectrum, off... *)
val next : view -> view
val name : view -> string

(* [shapes view screen]: the panel over the bottom of the screen *)
val shapes : view -> Playground.screen -> Playground.shape list
