(* Noise: random samples, for wind, explosions, drums (see
 * notes_audio.md section 3).
 *
 * A computer's randomness is a pseudo-random generator; the NES's noise
 * channel (the 2A03's APU, 1983) used a 15-bit linear-feedback shift
 * register (LFSR): at each step, the XOR of two of its bits is fed
 * back in at the top, as the others shift down one place; the lowest
 * bit is the output.
 *
 *     bit 14 ...          bit 1  bit 0 --> out
 *       ^                   |      |
 *       |                   +-XOR--+       (long mode: bits 0 and 1)
 *       +-------------------------'        (short mode: 0 and 6)
 *
 * Cheap, and deterministic: the same noise every time, so it can be
 * tested and replayed. From 1, the long mode goes through all 32,767
 * non-zero 15-bit values before coming back (a maximal-length LFSR):
 * hiss. The short mode loops after 93 steps: a pitched, metallic buzz,
 * the "short noise" of 8-bit games. The first steps from 1: 16384,
 * 8192, 4096, ... (the fed-back 1 entering at bit 14, then sliding
 * down).
 *
 * A step per sample is the highest-pitched noise; the NES stepped it
 * at one of 16 rates, its 1.79 MHz clock divided by 4 to 4068: from
 * 447 kHz down to 440 Hz, the noise's "pitch": [render ~rate] holds each value for 44,100 / rate samples.
 *
 * White noise has all frequencies equally (these); pink noise less of
 * the high ones (-3 dB an octave: rain rather than hiss): with the
 * filters, later in the plan.
 *
 * References: Solomon Golomb, Shift Register Sequences, 1967; the NES
 * APU's noise channel, https://www.nesdev.org/wiki/APU_Noise *)

type mode = Long | Short

(* [step mode register]: the register one step later *)
val step : mode -> int -> int

(* [period mode]: how many steps from 1 before it comes back to 1:
 * 32,767 (long), 93 (short) *)
val period : mode -> int

(* [render ?mode ~rate seconds]: [seconds] of noise, the register
 * stepped [rate] times a second (each value held in between), from 1;
 * the lowest bit as 1. or -1. *)
val render : ?mode:mode -> rate:float -> float -> Signal.t
