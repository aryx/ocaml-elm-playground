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
 * Hz: a harsh whistle over high notes. The band-limited versions come
 * later in the plan (PolyBLEP, phase 6), next to these, switchable.
 *
 * References: Joseph Fourier, Théorie analytique de la chaleur, 1822;
 * Curtis Roads, The Computer Music Tutorial, 1996, chapter 4. *)

type waveform = Sine | Square | Triangle | Sawtooth

val waveforms : waveform list
val name : waveform -> string

(* [wave w phase]: the waveform at [phase], in [0, 1) *)
val wave : waveform -> float -> float

(* an oscillator: its waveform, its frequency (Hz), where it is in its
 * period *)
type t = { waveform : waveform; frequency : float; phase : float }

(* [make w frequency]: at phase 0 *)
val make : waveform -> float -> t

(* [next o]: its current sample, and the oscillator one sample later
 * (the phase advanced by frequency / rate, wrapped) *)
val next : t -> float * t

(* [render w ~frequency seconds]: [seconds] of it, from phase 0 *)
val render : waveform -> frequency:float -> float -> Signal.t
