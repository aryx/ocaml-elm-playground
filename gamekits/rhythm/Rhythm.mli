(* Rhythm: the machinery every rhythm game has under its pictures --
   what time it is, how close a press was, and a chart played through.

   {1 What time it is}

   Not computer.time. The player is listening to the music, and the
   music runs on the sound card's clock, which never waits for a late
   frame (playground/Audio.mli draws the two clocks). So a rhythm game
   asks Audio.position how far into the song the card has been fed,
   and takes off the machine's latency -- the gap between feeding the
   card and hearing it, ~50 ms natively, ~100 ms in a browser -- which
   only the player can measure, by playing ([average_error]):

     song_time = position - offset

   {1 How close a press was}

   Each note has a time. A press in its lane is judged by how far from
   that time it came, early or late alike:

        early                        on the beat                         late
     -135     -100     -60     -30        0        30      60      100      135 ms
       | ALMOST | GOOD  | GREAT | PERFECT  | PERFECT | GREAT | GOOD  | ALMOST |
     not a press at all                                          once a note is
     (too early to be this note)                                 this far gone: MISS

   The windows are this repository's own, near DDR's (whose exact ones
   changed from version to version). An ALMOST counts but breaks the
   combo, as a MISS does.

   {1 A chart played through}

   A chart is notes -- when, in which lane, how long -- and a
   performance is the chart with, for each note, how it went. Each
   frame, [play] takes the song's time and the lanes pressed in that
   frame: a press takes the nearest unjudged note in its lane, if one is
   close enough; the notes gone by unpressed become misses. The lanes
   are the game's own type (DDR's four arrows, a guitar's five frets),
   compared with [=].

   {1 Charts from tunes}

   A chart need not be typed in: [sounding] gives the notes of one voice
   of a tune (audio/Abc.mli), each with its start, its length and its
   pitches, and a game turns those into lanes by its own rule -- DDR by
   the melody's shape, a guitar by pitch. The arrows then land exactly
   on the notes, because they are the notes.

   Part of the rhythm kit (gamekits/rhythm/); used by TinyDDR,
   TinyGuitarHero and TinyRockBand. *)

(* {1 Judging} *)

type judgement = Perfect | Great | Good | Almost | Miss

(* [window j]: how far from the note, in seconds, a press may come and
 * still be graded [j] (Miss: any distance) *)
val window : judgement -> float

(* [judge error]: the grade of a press [error] seconds from its note
 * (negative early, positive late), or None if it is too far to be a
 * press of that note at all. E.g. 0.02 is Perfect, -0.05 Great, 0.2
 * nothing *)
val judge : float -> judgement option

val points : judgement -> int
val name : judgement -> string

(* {1 The clock} *)

(* [song_time ~position ~offset]: the song's time as the player hears
 * it: how far into it the card has been fed, less the latency *)
val song_time : position:float -> offset:float -> float

(* {1 Charts, and a performance of one} *)

(* a note to play: when (seconds into the song), where, how long *)
type 'lane note = { at : float; lane : 'lane; length : float }

type 'lane performance = {
  (* each note, and how it went: None until it is judged *)
  judged : ('lane note * judgement option) list;
  (* the signed errors of the notes hit, early < 0 < late *)
  errors : float list;
  combo : int;
  best_combo : int;
  score : int;
  (* the last judgement and when, for a game to flash it *)
  last : (judgement * float) option;
  (* the calibration, seconds: the latency taken off the clock *)
  offset : float;
  (* the song's time at the last frame *)
  now : float;
  (* when the performance began by the frame clock (computer.time),
   * for a game that wants to compare the two clocks *)
  started : float;
}

(* [start ~offset ~started notes]: [notes] not yet played *)
val start : offset:float -> started:float -> 'lane note list -> 'lane performance

(* [press now lane p]: a press in [lane] at song time [now]: the nearest
 * unjudged note in that lane, judged if it is close enough *)
val press : float -> 'lane -> 'lane performance -> 'lane performance

(* [misses now p]: the notes gone by unpressed, judged Miss *)
val misses : float -> 'lane performance -> 'lane performance

(* [play now pressed p]: one frame: the presses, then the misses *)
val play : float -> 'lane list -> 'lane performance -> 'lane performance

(* the mean of the signed errors: for a player dancing steadily to what
 * they hear, the machine's latency -- the calibration to set *)
val average_error : 'lane performance -> float option

(* {1 Charts from tunes} *)

(* [sounding tune v]: voice [v]'s notes, rests left out: (start, length,
 * pitches as MIDI numbers, several for a chord) *)
val sounding : Abc.tune -> int -> (float * float * int list) list

(* [on_frets tune v]: voice [v] charted onto five frets, 0 to 4: each
 * note's fret is where its pitch sits in that voice's range, low to
 * high, and a chord is one note per fret it covers. How a guitar game
 * charts a part it was not written for -- the frets follow the tune up
 * and down, even though they are not its notes. *)
val on_frets : Abc.tune -> int -> int note list

(* {1 Difficulty: the same chart, reduced}

   A rhythm game's difficulty levels are not different songs. The
   hardest is the chart as the music has it, and the others are the
   same chart with things taken away -- the frets folded together and
   the chords thinned -- so that an easy part is still recognisably the
   same part:

     level    frets   notes of a chord
     Easy       3     1, its lowest
     Medium     4     1
     Hard       5     2, its outline: the lowest and the highest
     Expert     5     all of them: the chart itself *)

type difficulty = Easy | Medium | Hard | Expert

val difficulties : difficulty list
val difficulty_name : difficulty -> string
val frets_at : difficulty -> int
val chord_at : difficulty -> int

(* [reduce level chart]: the chart at that level (on five frets in, on
 * [frets_at level] out) *)
val reduce : difficulty -> int note list -> int note list

(* {1 Instruments: ways of pressing} *)

(* [strummed ~strum ~held]: a guitar's notes this frame. A note is two
 * hands -- the fret held, then the strum -- so the frets down *at the
 * strum* are the notes played, and a fret alone plays nothing. (Rock
 * Band wants the exact frets and counts an extra one held as a wrong
 * chord; here every fret held is played, the forgiving version.) *)
val strummed : strum:bool -> held:'lane list -> 'lane list

(* {1 Sustains} *)

(* how long a note must be to be a sustain, in seconds: shorter, it is
 * a tap. 0.75: a half note is one at any tempo up to 160 beats a
 * minute, and a quarter note never is below 80 *)
val sustain_min : float

(* [sustaining now held p]: how many notes are being sustained at song
 * time [now] -- hit, long enough, not over yet, and their lane still
 * held. A game scores them for every frame they last. *)
val sustaining : float -> 'lane list -> 'lane performance -> int

(* {2 Hearing what is played}

   In the games the player's part is not in the song: a note sounds
   when it is hit, and a note missed is silence -- the most direct
   judgement there is. So the song plays with the part muted, and each
   hit plays that note alone, in the part's own sound:

     the song      [muted v tune]: voice [v] all rests, the other voices
                   (and their sounds, which depend on how many voices
                   there are) untouched
     a hit         [newly_hit before after]: the notes hit between two
                   moments, by their start in the song
     its sound     [struck tune v at]: voice [v]'s note (or chord)
                   starting at [at], as a tune of its own

   The whole written chord sounds even on an easy chart: the part is
   reduced, never the music. *)
val muted : int -> Abc.tune -> Abc.tune
val newly_hit : 'lane performance -> 'lane performance -> float list
val struck : Abc.tune -> int -> float -> Abc.tune
