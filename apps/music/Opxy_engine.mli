(* The OP-XY's own engines, four of its nine, behind the same four knobs
 * as the OP-1's (Op1_engine.mli, whose type they share; see
 * notes_synth.md; plan_synth_teaching.md, TinyOpxy).
 *
 * Teenage Engineering's guide, "synth engines", the knobs dark grey,
 * mid grey, light grey, white:
 *
 *     engine     dark grey   mid grey    light grey       white
 *     wavetable  table       position    warp             drift
 *     organ      type        bass        tremolo amount   tremolo speed
 *     hardsync   freq        sub         noise            lowcut
 *     simple     shape       pw          noise            stereo
 *
 * a lesson each:
 *
 *  - *wavetable*, "waveforms arranged one after the other in a look up
 *    table": a table of eight periods, the position crossfading between
 *    two neighbours -- and a crossfade of two waves is a crossfade of
 *    their spectra, harmonic by harmonic, since both are sums (the PPG
 *    Wave's idea, 1981). Its nine tables are ours as four: a sawtooth
 *    growing its harmonics, a formant moving up (a vowel), a pulse
 *    narrowing, a square turning into a saw (its even harmonics
 *    coming in). Warp bends the reading's phase; drift is ours, a
 *    second reader at an inharmonic ratio;
 *  - *hardsync*: a second oscillator restarted each time the first
 *    completes a period, whatever its own frequency -- the sound's
 *    period stays the first's, the second's frequency moving only its
 *    harmonics (the formant sweep of sync leads); a sub-oscillator an
 *    octave down, noise, a low cut;
 *  - *organ*: sines added at the drawbars' footages (TinyHammond's
 *    additive idea, Voice_hammond.mli), four registrations of ours as
 *    its "type" -- the last a transistor organ's squares, its dividers'
 *    sound -- the 16' as the bass, a tremolo;
 *  - *simple*: one oscillator morphing through sine, triangle, saw and
 *    square, the pulse's width, noise; "stereo" a second copy detuned
 *    (the engines are mono here: ours).
 *
 * The oscillators are naive where they are not sums of sines (aliasing
 * at high notes: Oscillator.mli's PolyBLEP the exercise); the tables are
 * sums of 32 harmonics at most, made once, when first played.
 *
 * Worked example (Unit_opxy): simple's shape 0 a sine (its harmonics 2
 * to 5 under -100 dB); hardsync at 441 Hz, its second oscillator at
 * 3.59 times, periodic every 100 samples, the first's period; the
 * organ's jazz registration (16', 5 1/3', 8') with nothing at 4' (twice
 * the note), the full one with it; the wavetable half-way from its
 * first wave (a sine) to its second, the third harmonic at half its
 * level there (-6.02 dB): the spectra crossfaded. A golden WAV each. *)

val wavetable : Op1_engine.t
val organ : Op1_engine.t
val hardsync : Op1_engine.t
val simple : Op1_engine.t

(* the four *)
val all : Op1_engine.t list

(* the wavetable's tables' names, and the organ's registrations *)
val tables : string list
val registrations : (string * string) list
