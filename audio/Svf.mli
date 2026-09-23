(* The state-variable filter: a filter whose cutoff can move at every
 * sample (see notes_synth.md section 7).
 *
 * Filter.mli's biquad is designed for one cutoff: its five coefficients
 * come from it, and recomputed at every sample for a cutoff that moves,
 * they describe a sequence of filters, not a moving one -- whose memory
 * (the last inputs and outputs) belongs to the last filter. Moved
 * slowly, no one hears it; moved fast, by an envelope or an LFO at
 * audio rate, the biquad rings too loud, then blows up. A synthesizer
 * moves its cutoff all the time.
 *
 * The state-variable filter (Hal Chamberlin, Musical Applications of
 * Microprocessors, 1980, after the analog state-variable circuit) is two
 * integrators in a loop, and its parameters are the cutoff and Q
 * themselves; its states are the band-pass and low-pass outputs, real
 * signals, which mean the same whatever the cutoff. And it gives four
 * filters at once, from the same two states:
 *
 *            .------------------- -1/Q ---------------.
 *            v                                        |
 *     x -->( + )--> high -->[ integrate ]--> band --+-'--[ integrate ]--> low
 *            ^                                                           |
 *            '------------------------- -1 -----------------------------'
 *
 *     notch = high + low (everything but the cutoff)
 *
 * [Chamberlin]: the integrators as running sums, low += f band, band +=
 * f high, f = 2 sin (pi fc / rate), each reading the other's value from
 * this sample or the last. Cheap, and right at low cutoffs, but the
 * delays make it unstable high up: measured, at Q = 0.707 stable up to
 * 7,637 Hz (about a sixth of the rate, the classic warning), at Q = 5 up
 * to 15.9 kHz, at Q = 20 up to 18.9 kHz; above, it blows up.
 *
 * [Zero_delay]: the same loop solved within the sample (Zavalishin's
 * topology-preserving transform, as Moog_ladder.mli does; this form,
 * Andrew Simper's, 2013): with g = tan (pi fc / rate) and r = 1/Q,
 *
 *     high = (x - (r + g) s1 - s2) / (1 + r g + g^2)
 *
 * then two trapezoidal integrators give the band-pass and the low-pass
 * and update their states. Measured: -3.01 dB at the cutoff in all three
 * of low, band and high at Q = 0.707, the notch deeper than -219 dB, at
 * 1, 5, 8 and 12 kHz alike (Chamberlin's: the same up to 5 kHz, gone
 * from 8).
 *
 * Worked example, the cutoff swept at audio rate, 1000 x 2^(3 sin (2 pi
 * fm t)) (125 Hz to 8 kHz), a 110 Hz sawtooth low-passed
 * (Unit_moog_ladder), the loudest output sample: at Q = 5, swept 500
 * times a second, 2.40 through the zero-delay SVF, 6.66 through the
 * biquad recomputed at every sample; swept 3,000 times a second, 2.33,
 * and the biquad blown up. The same sweep at 5 Hz gives 2.38 through
 * both: slowly, the biquad is fine.
 *
 * References: Hal Chamberlin, Musical Applications of Microprocessors,
 * Hayden, 1980 (2nd ed. 1985), chapter 14; Andrew Simper, "Linear
 * Trapezoidal Integrated State Variable Filter", Cytomic, 2013; Vadim
 * Zavalishin, The Art of VA Filter Design, 2012, chapter 4. *)

type model = Chamberlin | Zero_delay
type mode = Low_pass | Band_pass | High_pass | Notch

val modes : mode list

(* the two integrators' memories *)
type t

val create : unit -> t

(* [process t model mode ~cutoff ~q samples]: [samples] filtered in
 * place, [cutoff] in Hz one per sample (kept within 10 Hz and 20 kHz) *)
val process : t -> model -> mode -> cutoff:Signal.t -> q:float -> Signal.t -> unit
