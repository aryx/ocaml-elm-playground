(* The Moog ladder filter: four poles, the output fed back (see
 * notes_synth.md section 6).
 *
 * Robert Moog's low-pass (1965, patented 1969), the sound people mean by
 * "Moog": four one-pole low-passes in a row -- transistor pairs stacked
 * like the rungs of a ladder -- and the fourth's output fed back to the
 * input, inverted, times k, the "emphasis" (resonance):
 *
 *     x --(+)--> [pole] --> [pole] --> [pole] --> [pole] --+--> y
 *          ^-                                              |
 *          '----------------------- k --------------------'
 *
 * Each pole loses 3 dB at the cutoff and 6 dB an octave above it: the
 * four, 12 dB at the cutoff and 24 dB an octave -- steep, the brightness
 * taken away fast (measured: -12.04 dB at the cutoff; with the cutoff at
 * 100 Hz, -23.3 then -23.8 dB an octave from 400 Hz to 1.6 kHz, going
 * to 24).
 *
 * {1 The resonance, worked out}
 *
 * At the cutoff each pole also turns the phase by 45 degrees: the four,
 * by 180. The inverted feedback then arrives *in* phase and reinforces
 * the frequencies around the cutoff: the resonant peak. The loop's gain
 * there is k (1 / sqrt 2)^4 = k / 4, so at k = 4 the loop sustains
 * itself: the filter oscillates, a sine at its cutoff, playable by
 * moving the cutoff with the keyboard. Below 4, the analog peak lies
 * a little under the cutoff, 59 cents under and 9.25 dB up at k = 3.5,
 * 11 cents at k = 3.9 (from the transfer function 1 / ((1 + s)^4 + k)).
 * The price: at 0 Hz the loop *subtracts*, the gain 1 / (1 + k), a
 * quarter (-12.04 dB) at k = 3, a fifth at 4: the bass thins as the
 * resonance rises. [~compensation] (0 to 1) adds c k of the input back,
 * (1 + c k) / (1 + k): at 1, the bass kept, as some later ladders do.
 *
 * {1 Three versions}
 *
 * [Naive]: each pole y += g (x - y), g = 1 - e^(-2 pi fc / rate), and the
 * feedback from the *previous* sample -- the loop can't be computed
 * within the sample, the fourth pole's output isn't known yet when the
 * first needs it. That one-sample delay adds its own phase turn (8.2
 * degrees at 1 kHz: 360 x 1000 / 44,100), different at every cutoff, so
 * the filter is out of tune with itself (Stilson and Smith, 1996).
 * Measured: at k = 3.5 its peak 36 cents under the cutoff at 440 Hz,
 * 10 under at 1 kHz, 129 *over* at 5 kHz; oscillating from k = 4.06 at
 * 110 Hz, 4.26 at 440, 4.64 at 1 kHz, and not at all, up to k = 8, at
 * 5 kHz: the resonance knob means something else at every note.
 *
 * [Zero_delay]: the loop solved instead of delayed (Zavalishin's
 * topology-preserving transform). Each pole is a trapezoidal integrator,
 * whose output is G (its input) + (its state) / (1 + g), with g = tan (pi
 * fc / rate) and G = g / (1 + g): known but for the input. Four in a row
 * give y = G^4 u + sigma, sigma from the four states; the loop says u = x
 * - k y; so
 *
 *     y = (G^4 x + sigma) / (1 + k G^4)
 *
 * computed first, then u, then the poles run. Measured: the analog
 * filter's numbers -- the peak at k = 3.5 59 cents under (+9.3 dB) at
 * 440 Hz and 1 kHz, 55 at 5 kHz (tan pre-warps the cutoff into place;
 * the frequencies around it still shift a little, more near Nyquist),
 * oscillation from k = 4.000 at 110 Hz, 440, 1 kHz, 5 kHz. (Near
 * Nyquist it cuts *more* than the analog one, the trapezoid's zero at
 * 22,050 Hz: -26 then -42 dB an octave above 1 kHz's cutoff, where the
 * naive one, folding, manages only -22 and -18.)
 *
 * [Nonlinear]: the zero-delay loop with a tanh at the loop's input and
 * each pole's: the transistors saturating (Huovilainen, 2004, whose own
 * model runs on the naive loop; here the linear solution predicts the
 * loop's input, then the saturation bends it). Quiet sounds go through as
 * through [Zero_delay]; loud ones are bent, gaining harmonics: a 108 Hz
 * sine's 3rd harmonic 50 dB under the fundamental at an amplitude of 0.1,
 * 24 dB at 0.5, 17 dB at 1, 11 dB at 4 -- driven harder, it gets thicker,
 * not just louder. And past k = 4 it oscillates without running away,
 * the tanh holding the level (0.08 at k = 4.2, 0.12 at 4.5), in tune
 * within 1 cent of the cutoff: the Minimoog's self-oscillating sine.
 *
 * References: Robert A. Moog, "A Voltage-Controlled Low-Pass High-Pass
 * Filter for Audio Signal Processing", AES convention, 1965, and US
 * patent 3,475,623, 1969; Tim Stilson, Julius O. Smith, "Analyzing the
 * Moog VCF with Considerations for Digital Implementation", ICMC 1996;
 * Antti Huovilainen, "Non-Linear Digital Implementation of the Moog
 * Ladder Filter", DAFx 2004; Vadim Zavalishin, The Art of VA Filter
 * Design, 2012 (rev. 2018), chapters 3 and 5. *)

type model = Naive | Zero_delay | Nonlinear

val models : model list
val name : model -> string

(* the four poles' memories *)
type t

val create : unit -> t
val reset : t -> unit

(* [process ?compensation t model ~cutoff ~resonance samples]: [samples]
 * filtered in place, [cutoff] in Hz one per sample (kept within 10 Hz
 * and 20 kHz), [resonance] k (4: oscillating) *)
val process : ?compensation:float -> t -> model -> cutoff:Signal.t -> resonance:float -> Signal.t -> unit
