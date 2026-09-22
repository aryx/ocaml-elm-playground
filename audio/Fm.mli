(* FM synthesis: a rich, changing spectrum from two sines (see
 * notes_audio.md section 7).
 *
 * John Chowning (Stanford, 1967, published 1973) let one sine wobble
 * the phase of another, at an audio rate:
 *
 *     out(t) = sin (2 pi fc t + I sin (2 pi fm t))
 *
 * fc the carrier (the pitch heard), fm the modulator, I the index (how
 * far it wobbles). Slow, it's a vibrato; as fast as the carrier, it's a
 * new timbre: the spectrum has the carrier and sidebands on each side,
 * at fc +- k fm, the k-th of amplitude J_k(I), a Bessel function:
 *
 *     amplitude          fc = 400, fm = 100, I = 1
 *        |                   |
 *        |                   |          J0(1) = 0.77 at fc
 *        |              |    |    |     J1(1) = 0.44 at fc +- fm
 *        |              |    |    |     J2(1) = 0.11 at fc +- 2 fm
 *        |         |    |    |    |    |    J3(1) = 0.02
 *        +----+----+----+----+----+----+----> Hz
 *            100  200  300  400  500  600
 *
 * Two numbers then choose the sound: the ratio fm / fc, which sets
 * where the sidebands fall -- a whole number, on the harmonics (1: all
 * of them, brass; 2: the odd ones, a clarinet), otherwise between them,
 * inharmonic (1.4: a bell, a gong) -- and the index, how many sidebands
 * are loud (about I + 1 on each side): the brightness. Chowning's
 * insight was to make the index follow the envelope: a note bright
 * when struck, darker as it dies away, like every real instrument --
 * which Synth does for a fading FM voice. Sidebands falling below 0 Hz
 * fold back, inverted, onto the positive ones: part of the character.
 *
 * Yamaha licensed it from Stanford: the DX7 (1983), six "operators"
 * wobbling each other, and the sound chips of the Sega Genesis and the
 * PC's AdLib and Sound Blaster (the OPL2, two operators per voice):
 * the sound of 1980s and early 1990s games after the NES's squares.
 *
 * (Strictly this is phase modulation, the formula every "FM"
 * synthesizer since the DX7 computes: the same spectrum as modulating
 * the frequency itself, for a sine modulator.)
 *
 * References: John Chowning, "The Synthesis of Complex Audio Spectra by
 * Means of Frequency Modulation", Journal of the Audio Engineering
 * Society 21(7), 1973; Curtis Roads, The Computer Music Tutorial,
 * 1996, chapter 6. *)

(* [wave ~index carrier modulator]: the formula above, the two phases
 * in [0, 1) *)
val wave : index:float -> float -> float -> float

(* [render ~carrier ~ratio ~index seconds]: the modulator at [ratio]
 * times the carrier, both from phase 0 *)
val render : carrier:float -> ratio:float -> index:float -> float -> Signal.t
