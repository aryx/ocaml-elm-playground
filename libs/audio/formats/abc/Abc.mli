(* ABC notation: tunes as text (see notes_audio.md section 9).
 *
 * ABC (Chris Walshaw, 1991) writes music in plain text, the way it's
 * sung: the letters are the notes, a number how long, a bar line where
 * the bar ends. Folk musicians have typed tens of thousands of tunes
 * in it; a tune here is a few lines, not a file of numbers:
 *
 *     X:1                   the tune's number (a file can hold many)
 *     T:Frere Jacques       its title
 *     L:1/8                 the unit length: a note is an eighth
 *     Q:1/4=120             the tempo: 120 quarter notes a minute
 *     K:C                   the key: C major, no sharps or flats
 *     C2 D2 E2 C2 | C2 D2 E2 C2 | E2 F2 G4 |
 *
 * The subset read here:
 *
 * - notes: C D E F G A B from middle C (C4, MIDI 60) up, c d e f g a b
 *   an octave higher; each ' an octave up, each , down: C, is C3, c' is
 *   C6;
 * - accidentals before the note: ^ sharp, _ flat, = natural (^^ and __
 *   twice); they last until the end of the bar, for that note, as in
 *   sheet music;
 * - lengths after it, in units of L: C2 twice, C/2 or C/ half, C3/2
 *   one and a half; z a rest, with a length too;
 * - [CEG] a chord; (3CDE a triplet, three notes in the time of two;
 *   C>D a dotted pair (C longer by half, D half as long), C<D the
 *   other way;
 * - the key signature K: (major keys C G D A E B F# C#, F Bb Eb Ab Db
 *   Gb Cb, and their relative minors, Am Em ... Dm Gm ...): K:G makes
 *   every F an F#, unless marked =F;
 * - voices, V:1, V:2 ... each its own part, played together (a melody
 *   and a bass); fields inline too, [K:D], [L:1/16], [V:2];
 * - a percussion voice, "V:4 clef=perc" (ABC 2.1's own marking): its
 *   notes are drums, not pitches -- each note's MIDI number is read as a
 *   General MIDI drum key, so C,, (36) is the bass drum, D,, (38) the
 *   snare, ^F,, (42) the closed hi-hat, ^A,, (46) the open one, and
 *   Music.to_sound plays them with the drum sounds of its MIDI player;
 * - skipped: % comments, "Am" chord names, !trill! and +fermata+
 *   decorations, {grace notes}, the ~ . H ... ornaments, ties (the note
 *   played again), repeats and endings (a tune loops as a whole).
 *
 * Example: with L:1/8, Q:1/4=120 and K:G, "F2 =F2 c/2" is an F#4 for
 * 0.5 s (a quarter at 120 a minute), an F4 for 0.5 s, then a C5 for
 * 0.125 s.
 *
 * References: Chris Walshaw, "The abc music standard 2.1", 2011,
 * https://abcnotation.com/wiki/abc:standard:v2.1 ; the tune collections
 * at https://abcnotation.com and https://thesession.org *)

(* one note, or a rest: when, how long (seconds), which (MIDI numbers,
 * several for a chord, none for a rest) *)
type event = { start : float; length : float; notes : int list }

type tune = {
  title : string;
  (* each voice's events, in time order *)
  voices : event list list;
  (* for each voice, whether it is percussion (clef=perc) *)
  drums : bool list;
}

(* [parse text]: the first tune of [text] (up to the next X:), or why
 * not *)
val parse : string -> (tune, string) result

(* when the last voice ends *)
val duration : tune -> float
