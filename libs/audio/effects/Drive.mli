(* Drive: a sound pushed into a curve that flattens its loud parts, the
 * distortion of an overdriven amplifier (see notes_synth.md section
 * 8.1).
 *
 * Every sample goes through a *waveshaper*, a curve y = f (g x), [g]
 * the drive (the gain before it, in dB): quiet samples on the curve's
 * straight middle come out as they went in, loud ones are flattened.
 *
 *     y                     hard   the corners: a square from a sine
 *     1 |       ______      tanh   a smooth knee, the classic
 *       |     /             cubic  x - x^3 / 3, smoother still,
 *       |    /                     flat from 1 on
 *     --+---/-------> g x   asym   tanh with a bias, flattening one
 *       |  /                       side before the other: the tube's
 *    -1 |_/                        and the diode's even harmonics
 *
 * A sine flattened is a sine plus its harmonics: a symmetric curve
 * adds the odd ones (3, 5, 7, ...), the asymmetric one the even ones
 * too (2, 4, ...: an octave above, "warmer"). Adding harmonics is the
 * point -- and the trouble: they don't stop at Nyquist. A 5 kHz sine
 * through tanh gets its 3rd harmonic at 15 kHz, its 5th at 25 kHz
 * (folded to 19.1 kHz), its 7th at 35 (to 9.1 kHz), and its 9th at 45
 * kHz, folded to 0.9 kHz: a tone *under* the note, a harmonic of
 * nothing, the fizz of cheap distortion (notes_audio.md section 2's
 * aliasing, made by the effect itself).
 *
 * The cure is *oversampling*: do the shaping at a higher rate, where
 * the new harmonics fit, then filter them out before coming back down:
 *
 *     44.1 kHz --> x L (zeros in between) --> low-pass --> shape
 *              --> low-pass --> every L-th sample --> 44.1 kHz
 *
 * The first low-pass fills the zeros in (the images of the spectrum
 * the zeros make, removed: an interpolation), the second removes what
 * the shaping put above 20 kHz before it can fold. Both are 8th-order
 * Butterworths (four biquads) at 18 kHz, run at L x 44,100.
 *
 * Worked example (Unit_drive): a 5 kHz sine through tanh at +12 dB,
 * the loudest thing below 4 kHz (only aliases can be there: the first
 * harmonic is the note, at 5 kHz), under the note:
 *
 *     L = 1 (none)   -32.6 dB, at 904 Hz: the 9th, folded
 *     L = 2          -58.6 dB, at 3198 Hz
 *     L = 4          -81.7 dB (at 904 Hz, the 9th's remains)
 *
 * x2 already stops the 5th to the 9th (25 to 45 kHz fit under 44.1
 * kHz, the new Nyquist); the 17th at 85 kHz still folds there, to 3.2
 * kHz, what x2 leaves. (Measured on 4096 samples, Hann-windowed:
 * Spectrum.of_signal.) The price: L times the work, and the filters' delay (a few
 * samples).
 *
 * The asymmetric curve leaves a constant offset (its bias), which a
 * one-pole high-pass at 10 Hz takes away, as the capacitor after a
 * tube stage does.
 *
 * References: Udo Zolzer (ed.), DAFX: Digital Audio Effects, 2nd ed.
 * 2011, chapter 4 (nonlinear processing); Will Pirkle, Designing Audio
 * Effect Plugins in C++, 2nd ed. 2019, chapter 19 (waveshapers and
 * oversampling). *)

type shape = Hard | Tanh | Cubic | Asymmetric

val shapes : shape list
val name : shape -> string

(* [curve shape x]: the curve, between -1 and 1 (the asymmetric one
 * tanh (x + 0.3) - tanh 0.3: from -1.29 to 0.71) *)
val curve : shape -> float -> float

(* the oversampling filters' state, and the DC blocker's *)
type t

(* [create ~oversampling ()]: [oversampling] 1, 2 or 4 *)
val create : oversampling:int -> unit -> t

(* [process t shape ~drive ~mix s]: [s] in place, [drive] in dB before
 * the curve, [mix] the shaped part (0: dry, 1: all driven) *)
val process : t -> shape -> drive:float -> mix:float -> Signal.t -> unit

(* {1 As an effect} *)

(* shape (hard, tanh, cubic, asymmetric: tanh at first), gain (the
 * drive, 0 to 36 dB: 12), oversampling (a switch: x4 or none, on) *)
val knobs : Effect.knob list

(* [effect ()]: "drive", both channels, the dry sound gone (mix 1) *)
val effect : unit -> Effect.t
