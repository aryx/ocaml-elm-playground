(* Notes and music (see notes_audio.md section 9): the notes'
 * frequencies, and tunes (Abc.mli) played by a small band of NES-like
 * instruments. MIDI comes next in the plan (phase 8).
 *
 * Western music's twelve-tone equal temperament: an octave doubles the
 * frequency, and is split into 12 semitones, each multiplying it by the
 * same ratio, 2^(1/12) = 1.0595; the reference is A4 = 440 Hz (the A
 * above middle C, ISO 16, 1975). A note n semitones from A4 is
 * 440 * 2^(n/12):
 *
 *     C4 (middle C)  -9 semitones   261.63 Hz
 *     A4              0             440
 *     E5             +7             659.26     (a fifth above: nearly 3/2)
 *     A5            +12             880        (an octave: exactly 2)
 *
 * MIDI numbers the same notes from 0, A4 being 69, middle C 60
 * (notes_audio_midi.md): frequency = 440 * 2^((n - 69) / 12).
 *
 * References: Marin Mersenne, Harmonie universelle, 1636 (the ratio
 * 2^(1/12)); the MIDI 1.0 specification, 1983; Paul Hudak and Donovan
 * Quick, The Haskell School of Music, 2018, chapter 2 (pitches as
 * numbers, "absPitch"). *)

(* [midi_frequency n]: MIDI note [n]'s frequency, 440 for 69 *)
val midi_frequency : int -> float

(* [midi_number name]: "A4" -> 69, "C4" -> 60, "F#5" -> 78, "Bb3" ->
 * 58 (a letter, an optional # or b, an octave from -1 to 9); None if
 * it isn't a note *)
val midi_number : string -> int option

(* [frequency name]: "A4" -> 440.; 0. if it isn't a note *)
val frequency : string -> float

(*****************************************************************************)
(* {1 Playing tunes} *)
(*****************************************************************************)
(* A tune's voices played by the NES's band (the 2A03 chip, 1983: two
 * square channels and a triangle): the first voice on a square (the
 * melody), the last on the triangle (the bass), those in between on a
 * softer square. Each note sounds for 90% of its length, then a short
 * silence, so that two notes in a row are heard as two (a tracker's
 * "note off"); rests are silences. *)

(* [instrument ~voice ~voices]: the sound of voice number [voice] (from
 * 0) among [voices]: its waveform and volume *)
val instrument : voice:int -> voices:int -> Oscillator.waveform * float

(* [to_sound tune]: its voices together, each a sound after another *)
val to_sound : Abc.tune -> Synth.t

(*****************************************************************************)
(* {1 Playing MIDI scores} *)
(*****************************************************************************)
(* A MIDI file's notes overlap freely (a chord, a sustained bass under a
 * melody), which a tune's one-after-another voices can't hold: a score
 * is rendered straight into one buffer, each note added at its start.
 * General MIDI's instrument families (the program / 8, 1991) mapped to
 * our few waveforms, NES-like:
 *
 *   pianos, chromatic percussion (0-15)   square, fading
 *   organs (16-23)                        square, held
 *   guitars (24-31)                       sawtooth, fading
 *   basses (32-39)                        triangle
 *   strings, ensembles (40-55)            sawtooth, softer
 *   brass (56-63)                         sawtooth
 *   reeds, pipes (64-79)                  square
 *   synth leads (80-87)                   square
 *   synth pads (88-95)                    triangle
 *   the rest                              square
 *
 * and channel 10 (9 from 0), the drums, by key: 35-36 a bass drum (a
 * triangle falling from 150 to 50 Hz), 38 and 40 a snare (noise), 42
 * to 46 hi-hats (short, bright noise), the others a short noise. The
 * velocity is the volume (127: 0.25, so that chords don't clip). *)

(* [render_score score]: its samples *)
val render_score : Midi.score -> Signal.t
