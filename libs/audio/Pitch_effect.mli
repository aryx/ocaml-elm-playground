(* Pitch effects: a voice's pitch changed over time, wobbling or
 * jumping (see notes_audio.md section 8).
 *
 * They multiply a voice's frequency by a factor that
 * changes with time t, the seconds since the voice started:
 *
 *   - a vibrato, the pitch wobbling up and down, [depth] semitones either
 *     way, [rate] times a second: 2^(depth sin (2 pi rate t) / 12). A
 *     singer's or a violinist's (5 to 7 a second, a quarter of a
 *     semitone); faster and deeper, a siren, a UFO.
 *   - a jump, [semitones] up (or down) once, at [at] seconds: a coin's
 *     two notes in one sound (sfxr's "change").
 *   - an arpeggio, the notes of a chord one after the other, [step]
 *     seconds each, around and around:
 *
 *         semitones  0  4  7  0  4  7  0 ...     C E G C E G: a C major
 *         pitch      ___                          chord, played by a
 *                       ___                       single voice
 *                          ___
 *
 *     the chiptune trick: the NES had two square channels, the C64
 *     three voices, too few to play chords; cycled every frame (1/60 s),
 *     the notes blur into one warbling chord. (Arpeggio machines and
 *     trackers' "0xy" effect: the note, x semitones up, y up, every
 *     tick.)
 *
 * The echo and the reverb, the other effects of a rendered sound, are
 * Synth's; the live ones, a sound going through them block after block,
 * audio/effects/'s.
 *
 * Reference: Tomas Pettersson, sfxr, 2007 (vibrato and "change" among
 * its parameters). *)

type t =
  | Vibrato of { rate : float; depth : float } (* Hz, semitones *)
  | Jump of { semitones : float; at : float } (* at, in seconds *)
  | Arpeggio of { semitones : float list; step : float } (* each [step] s *)

(* [factor p t]: the frequency multiplier at [t] seconds in: 1 for no
 * change, 2 an octave up *)
val factor : t -> float -> float

