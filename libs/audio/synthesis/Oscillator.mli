(* Oscillators: the periodic waves every synthesizer starts from (see
 * notes_audio.md section 3).
 *
 * The heart of one is the phase accumulator: a number going from 0 to
 * 1 over a period, advanced by frequency / rate at each sample and
 * wrapped back to 0; the waveform is then a function of the phase:
 *
 *     sine      sin (2 pi phase)                 smooth, a flute
 *     square    1 below 0.5, -1 above            hollow, NES pulses
 *     triangle  0 up to 1, down to -1, back      soft, the NES's bass
 *     sawtooth  2 phase - 1                      buzzy, brass, strings
 *
 *      sine        square       triangle      sawtooth
 *     .-.         .--.           /\             /|  /|
 *    /   \   .    |  |  |       /  \  /        / | / |
 *         '-'        '--'           \/        /  |/  |
 *
 * Example, at phase 0.25 (a quarter period in): the sine, the square
 * and the triangle are at 1, the sawtooth at -0.5. A 440 Hz oscillator
 * at 44,100 samples a second advances 440 / 44,100 = 0.00998 per
 * sample: back to 0 every 100.23 samples.
 *
 * Why they sound different at the same pitch: a periodic wave is a sum
 * of sines at multiples of its frequency, its harmonics (Fourier,
 * 1822): the sine only the first; the square the odd ones, at 1, 1/3,
 * 1/5...; the sawtooth all, at 1/n; the triangle the odd ones, fading
 * fast (1/n^2): nearly a sine. That recipe is the timbre.
 *
 * These are the naive ones, a formula of the phase: their sudden jumps
 * (the square's, the sawtooth's) have harmonics going on forever,
 * past the Nyquist frequency, where they fold back as aliases
 * (Signal.alias): a 1000 Hz square's 23rd, 25th, 27th harmonics (23,
 * 25, 27 kHz) are heard at 21.1, 19.1, 17.1 kHz, not harmonics of 1000
 * Hz: a harsh whistle over high notes.
 *
 * The band-limited versions, [wave_band_limited], next to these: the
 * same formula, with each jump smoothed over the samples around it by
 * a small polynomial, PolyBLEP (a "band-limited step"). An ideal jump
 * sampled has harmonics forever; a jump band-limited to Nyquist is a
 * smeared one, rising over a sample or two with a small overshoot; the
 * polynomial is a cheap stand-in for the difference between the two,
 * added where the jump is:
 *
 *     naive square               band-limited (PolyBLEP)
 *       1 . . .                   1 . . .
 *                |                        '
 *                |                         .       the sample just
 *      -1        . . . .         -1          . . .  before and just
 *              ^ jump                               after the jump pulled
 *                                                   towards the middle
 *
 * The correction for a jump up by 2 at phase 0, t the phase and dt =
 * frequency / rate the phase step (a sample's worth of phase):
 *
 *     t < dt      (just after):  u = t / dt,        2u - u^2 - 1
 *     t > 1 - dt  (just before): u = (t - 1) / dt,  u^2 + 2u + 1
 *     otherwise 0
 *
 * -1 right at the jump (the naive 1 becomes 0, the middle), back to 0
 * one sample away: only two samples per jump change, so it costs
 * almost nothing. It's the audio twin of antialiasing a polygon's edge
 * (graphics/2d's coverage): the pixels on the edge get in-between
 * values, the samples on the jump too. The square has two jumps (up at
 * 0, down at 0.5), the sawtooth one (down by 2 at 0, so the correction
 * is subtracted). The sine has no jump. The triangle has no jump
 * either, only corners, where its slope turns (+4 to -4 a period, at
 * 0.25): its harmonics fall as 1/n^2 instead of 1/n, its aliases about
 * 30 dB quieter than the square's to begin with. A corner is a jump of
 * the slope, so its correction is the jump's integrated, PolyBLAMP (a
 * band-limited ramp): (u+1)^3 / 6 just before, (1-u)^3 / 6 just after,
 * times the slope's change in a sample (8 dt).
 *
 * Example, a 1001 Hz square (Unit_oscillator): its loudest alias below
 * 5 kHz, where one would be out of tune among the harmonics, drops from
 * -30 dB to -72 dB; the loudest anywhere only from -25 to -34 dB: those
 * just under Nyquist, folded from just above it, where a two-sample
 * correction can't tell them apart from the harmonics it must keep.
 * The price: the top harmonics dulled a little, the 5th (5 kHz) by 0.4
 * dB, the 9th by 1.2 dB. The triangle's loudest alias below 5 kHz: from
 * -66 dB to -110 dB.
 *
 * References: Joseph Fourier, Théorie analytique de la chaleur, 1822;
 * Curtis Roads, The Computer Music Tutorial, 1996, chapter 4; Tim
 * Stilson, Julius Smith, "Alias-Free Digital Synthesis of Classic
 * Analog Waveforms", ICMC 1996 (the band-limited step, BLEP); Vesa
 * Välimäki, Antti Huovilainen, "Antialiasing Oscillators in
 * Subtractive Synthesis", IEEE Signal Processing Magazine, 2007
 * (PolyBLEP, the polynomial one); Fabián Esqueda, Vesa Välimäki,
 * Stefan Bilbao, "Rounding Corners with BLAMP", DAFx 2016 (PolyBLAMP). *)

type waveform = Sine | Square | Triangle | Sawtooth

val waveforms : waveform list
val name : waveform -> string

(* [wave w phase]: the waveform at [phase], in [0, 1) *)
val wave : waveform -> float -> float

(* [polyblep ~dt t]: the correction above, for a jump up by 2 at phase
 * 0 *)
val polyblep : dt:float -> float -> float

(* [polyblamp ~dt t]: the triangle's, for a corner at phase 0 whose
 * slope goes up by one a sample *)
val polyblamp : dt:float -> float -> float

(* [wave_band_limited w ~dt phase]: [wave w phase] with its jumps
 * smoothed, [dt] the phase step, frequency / rate *)
val wave_band_limited : waveform -> dt:float -> float -> float

(*****************************************************************************)
(* {1 The pulse, and its width} *)
(*****************************************************************************)
(* A pulse is high for a fraction [width] of its period, low for the
 * rest; the square is the pulse of width 0.5:
 *
 *     width 0.5 (the square)       width 1/3             width 0.1
 *      .--.  .--.                 .-.   .-.             .   .
 *      |  |  |  |                 | |   | |             |   |
 *      '  '--'  '--               ' '---' '---          '---'---
 *
 * Its harmonic k is (4 / (pi k)) |sin (pi k width)| loud (from the
 * Fourier series of a pulse of amplitude 1): the width *is* the timbre.
 * At 0.5, sin (pi k / 2) is 0 for every even k: the square's odd
 * harmonics. At 1/3, 0 for k = 3, 6, 9...: harmonic 1 at 1.103,
 * harmonic 2 at 0.551, harmonic 3 missing, harmonic 4 at 0.276. The
 * narrower, the more alike the harmonics, thin and nasal: at 0.1 they
 * go 0.39, 0.37, 0.34, ... slowly down to 0 at the 10th, the
 * fundamental no longer standing out.
 *
 * Its average is width - (1 - width) = 2 width - 1, not 0 (a pulse of
 * width 0.1 is -1 most of the time): taken away here, as an analog
 * oscillator's output capacitor takes it away, so that moving the width
 * (pulse-width modulation, Vco.mli) doesn't move the whole wave up and
 * down, a thump at the modulation's rate. The square's average is 0: it
 * doesn't change. Band-limited, a pulse is two PolyBLEPs: up at phase 0,
 * down at [width]. *)

(* [pulse ~width phase]: the pulse, its average taken away *)
val pulse : width:float -> float -> float

(* [pulse_band_limited ~width ~dt phase]: with its two jumps smoothed *)
val pulse_band_limited : width:float -> dt:float -> float -> float

(* an oscillator: its waveform, its frequency (Hz), where it is in its
 * period *)
type t = { waveform : waveform; frequency : float; phase : float }

(* [make w frequency]: at phase 0 *)
val make : waveform -> float -> t

(* [next o]: its current sample (the naive one unless [band_limited]),
 * and the oscillator one sample later (the phase advanced by
 * frequency / rate, wrapped) *)
val next : ?band_limited:bool -> t -> float * t

(* [render w ~frequency seconds]: [seconds] of it, from phase 0, the
 * naive one unless [band_limited] *)
val render : ?band_limited:bool -> waveform -> frequency:float -> float -> Signal.t
