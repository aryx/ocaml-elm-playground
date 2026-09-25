(* The OP-XY: a groovebox, eight tracks on one clock, each a step
 * sequencer and an instrument (see notes_synth.md;
 * plan_synth_teaching.md, TinyOpxy, X3).
 *
 * Teenage Engineering's OP-XY (2024) is the OP-1's other half: where
 * the OP-1 records what is played on a tape, the OP-XY sequences it,
 * eight *tracks* stepping together, each a pattern of 16 steps playing
 * an instrument -- the drum machine's way (TinyTR808) given to every
 * sound. Its ideas, each of them here:
 *
 *  - a *step* holds any number of notes (a chord; a drum track's
 *    several pads at once), its velocity, and parameter locks
 *    (Sequencer.mli: the OP-XY's points, smoothed): the knobs'
 *    values moved by the pattern itself;
 *  - a *scene* is a pattern chosen per track and the tracks muted:
 *    the song's parts (the verse, the chorus), switched while it plays
 *    -- at the bar's end, so the change lands on the beat ("scenes are
 *    as long as their longest pattern": ours are all a bar);
 *  - the *brain* transposes the tracks linked to it: into a key, and
 *    into a *scale*, degree by degree -- a pattern written in C minor,
 *    C Eb G Bb, is D F A C in D dorian and D F# A C# in D major. Not
 *    a transposition, which keeps the intervals, but a re-reading of
 *    the same degrees in another mode:
 *
 *        written (C minor)   C   Eb  G   Bb     degrees 1 3 5 7
 *        D dorian            D   F   A   C      the same degrees
 *        D major             D   F#  A   C#
 *
 * The tracks' instruments are this repository's: an OP-1 sound
 * (Studio_op1.voice: an Op1_engine and its envelope, the OP-XY's
 * own nine engines left for later), a drum kit (Sampler.mli's, its
 * pads sampled from our TR-808, Voice_tr808.mli, at their General
 * MIDI keys), or keys (Sampler.mli's one-shot sampler over a note of
 * our Rhodes, Voice_rhodes.mli: a sampled electric piano, the way
 * the Emulator made one in 1981). Each track then a low-pass (the
 * OP-XY's filter module, without its envelope: ours), its volume and
 * its pan. A note's gate is half a step (Sequencer.mli; the OP-XY's
 * is a knob). The sequencers are the clock, a tick each step; each track
 * plays its pattern from the ticks, which is what lets a step change
 * the track's timing: the *step components* (below), a ratchet's
 * triggers inside a step, a step repeated or held while the clock goes
 * on. The other ten components, the LFOs, the effects sends and the
 * auxiliary tracks are exercises.
 *
 * Worked example (Unit_opxy): the brain's figure; a scene asked for
 * in the middle of a bar heard at the next bar's first sample (88200
 * at 120 BPM: 16 steps of 5512.5); a chord step, three voices; a
 * lock on a track's cutoff changing its sound; a golden WAV of our
 * song, a bar of each of its first two scenes. The components at 120
 * BPM (a step 5512.5 samples): multiply 4 on the first step, four
 * triggers at samples 0, 1379, 2757, 4135; pulse 3, the step struck at
 * the first three ticks and the pattern's next note three ticks late;
 * hold 3, struck once, the next note three ticks late; skip 2, the
 * step played in the first and third bars of four. *)

(*****************************************************************************)
(* The brain *)
(*****************************************************************************)

(* the scales, their degrees in semitones from the key *)
val scales : (string * int list) list

(* [brain ~from ~key ~scale note]: [note], written in C in the scale
 * [from] (an index in [scales]), read in [key] (0 C to 11 B) and
 * [scale]: its degree kept, its octave kept (a note not in [from]
 * taken as the degree under it); into a scale of another size, the
 * nearest of its notes (the pentatonic has no second: D is Eb, and G
 * stays G) *)
val brain : from:int -> key:int -> scale:int -> int -> int

(*****************************************************************************)
(* The tracks *)
(*****************************************************************************)

type kind = Synth of Studio_op1.sound | Drums | Keys

(* the step components (four of the OP-XY's fourteen, TE's guide's
 * words): [Multiply n] "multiply the number of triggers in a step,
 * creating a ratchet effect"; [Pulse n] "repeat a step a defined
 * number of times without progressing the sequence"; [Hold n] "hold a
 * step a defined number of steps without progressing the sequence";
 * [Skip n] "play only one in every defined number of repetitions".
 * Pulse and hold make the track's step lag the clock's -- the track
 * drifting from the others until the scene changes, which rewinds them
 * all *)
type component = Multiply of int | Pulse of int | Hold of int | Skip of int

(* a step: its notes (MIDI; a drum track's, the pads' General MIDI
 * keys), none a rest *)
type step = { notes : int list; velocity : float; locks : (string * float) list; components : component list }

type track = {
  name : string;
  kind : kind;
  patterns : step array array; (* 4, of 16 steps *)
  cutoff : float; (* 0 to 1 *)
  resonance : float;
  volume : float;
  pan : float; (* -1 to 1 *)
  linked : bool; (* to the brain *)
  smoothing : float; (* between its locks: Sequencer's Points *)
}

type scene = { chosen : int array; (* a pattern per track *) mutes : bool array }

type patch = {
  tracks : track array; (* 8 *)
  scenes : scene array; (* 4 *)
  scene : int;
  tempo : float;
  written : int; (* the scale the patterns are written in, in C *)
  key : int;
  scale : int;
  volume : float;
}

val rest : step
val note : ?velocity:float -> int list -> step

(* a step's components' numbers, 1 without *)
val multiply : step -> int
val pulse : step -> int
val hold : step -> int
val skip : step -> int

(* the knobs a step can lock: the engine's four ("p1" to "p4"),
 * "cutoff", "resonance", "volume", "pan" *)
val lockable : string list

(* a track's value of a lockable knob, and the track with it *)
val get : track -> string -> float
val put : track -> string -> float -> track

(* our song: drums, bass, chords, a lead, and four tracks to fill; its
 * four scenes *)
val initial : patch

(* the drum kit's pads: the TR-808's instruments at their General MIDI
 * keys (36 BD, 38 SD, 42 CH, 46 OH, ...), the hats in a mute group *)
val pads : (int * string) list

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t

val create : patch -> t
val patch : t -> patch
val set_patch : t -> patch -> unit

(* the clock: the eight sequencers started together *)
val run : t -> bool -> unit
val running : t -> bool

(* the step sounding, and the scene playing (the patch's, once its bar
 * is over) *)
val step : t -> int
val playing : t -> int

(* a track's own step (the clock's, unless a pulse or a hold made it
 * lag), and the samples its notes were pressed at, the last first (the
 * last 64) *)
val position : t -> int -> int
val triggers : t -> int -> int list

(* the track the keys play live *)
val select : t -> int -> unit

(* a track's voices sounding, its last 1024 samples' loudness (rms) *)
val voices : t -> int -> int
val level : t -> int -> float

(* the last 2048 samples out, for the screen *)
val recent : t -> Signal.t
val instrument : t -> Instrument.t
