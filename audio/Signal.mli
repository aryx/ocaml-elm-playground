(* Samples: what a digital sound is (see notes_audio.md sections 1-2).
 *
 * Sound is air pressure going up and down; a digital sound is that
 * pressure measured at regular instants, [rate] times a second, each
 * measure a float from -1. to 1. (a sample):
 *
 *     1 |    .  .                   a 440 Hz tone at 44,100 samples
 *       |  .      .                 a second: one period every
 *     0 +-.---------.---------.--   44,100 / 440 = 100.23 samples
 *       |             .     .
 *    -1 |                .  .
 *
 * A sample rate of F can represent the frequencies below F / 2, the
 * Nyquist frequency, and nothing above (Harry Nyquist, 1928; Claude
 * Shannon, 1949): at 44,100, up to 22,050 Hz, just above human hearing
 * (20 Hz to 20,000 Hz) -- why CDs chose it. A frequency above it isn't
 * lost but folded back below it, an alias: a 30,000 Hz tone sampled at
 * 44,100 gives the samples of a 14,100 Hz one (Oscillator.mli, and
 * notes_audio.md section 2).
 *
 * References: H. Nyquist, "Certain Topics in Telegraph Transmission
 * Theory", 1928; C. E. Shannon, "Communication in the Presence of
 * Noise", 1949. *)

(* samples, one per 1 / rate second *)
type t = float array

(* 44,100 samples a second, the CD's rate *)
val rate : int

(* the highest frequency [rate] can represent, rate / 2: 22,050 Hz *)
val nyquist : float

(* [samples seconds]: how many samples last [seconds] (rounded down):
 * 0.5 s is 22,050 *)
val samples : float -> int

(* [period_in_samples frequency]: rate / frequency, 100.23 for 440 Hz *)
val period_in_samples : float -> float

(* [alias frequency]: the frequency it's heard at once sampled: itself
 * below Nyquist, folded back above it (30,000 Hz: 14,100 Hz) *)
val alias : float -> float

(* [of_function seconds f]: f sampled at each sample's time, t = i /
 * rate, for [seconds]: the simplest synthesizer *)
val of_function : float -> (float -> float) -> t

(* [to_int16 x]: a sample as a 16-bit integer, what files and sound
 * cards want: x times 32,767, rounded, clipped to [-32768, 32767] *)
val to_int16 : float -> int
