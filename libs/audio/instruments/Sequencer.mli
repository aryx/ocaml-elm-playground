(* The step sequencer: a pattern played in the audio clock (see
 * notes_synth.md section 1; plan_synth_teaching.md, TinyTB303, B1).
 *
 * An instrument hears its keys between blocks (Instrument.mli): at a
 * frame's granularity, 16.7 ms, fine for a finger, whose own timing is
 * no better. A sequencer is not a finger: a 16th note at 120 BPM lasts
 * 5512.5 samples, and heard on the frame clock its steps would land on
 * multiples of 735 -- the second at 5880 instead of 5512.5, 8.3 ms
 * late, the next ones early or late by different amounts: a groove
 * gone wrong, which the ear catches. So the sequencer runs inside the
 * voice, in the samples it computes: asked for the next block, it says
 * at which sample of it each event falls, the voice rendering the block
 * in pieces between them.
 *
 *     steps (16ths)   |  C    |  C    |  rest |  Eb   |
 *     gate            |##     |#######|       |##     |    half a step,
 *                          ^ slide: the gate held,          or held into
 *                            the next note glides in        a slide
 *
 * A step is a note or a rest, and a note may be *accented* (the voice
 * plays it louder and brighter) or *slide* into the next: the gate held
 * open through the next note, which the voice glides to instead of
 * starting again -- the TB-303's two gestures, and the whole of acid's
 * phrasing. A note's gate is open for half its step (commonly quoted
 * for the 303: ours, to check), unless it slides into a note.
 *
 * An event falls at the first sample at or after its exact time (step k
 * at k x 60 / (bpm x 4) seconds), counted from the start, never rounded
 * to a block's edge: the same samples whatever the blocks' size.
 *
 * A step may also carry *parameter locks*: a knob's value stored in
 * the step, so that a pattern is a sequence of sounds and not only of
 * notes -- Elektron's idea (the Machinedrum, 2001), the whole of their
 * machines' way of playing. What a lock means between the steps is a
 * choice, and two machines made the two:
 *
 *     cutoff locked at 0.2 on step 1, 0.8 on step 9 (of 16):
 *
 *     step            1   3   5   7   9   11  13  15  1
 *     Per_step        .2  -   -   -   .8  -   -   -   .2    - : the knob's own
 *     Points 0        .2  .2  .2  .2  .8  .8  .8  .8  .2    held till the next
 *     Points 1        .2  .35 .5  .65 .8  .65 .5  .35 .2    a line to the next
 *
 * Elektron's (Per_step): the lock is the step's only, the knob's own
 * value back after it. The OP-XY's (Teenage Engineering, 2024; Points
 * with its smoothing): "lock points are treated as vectors" -- a lock
 * is a point the value goes through, held till the next lock
 * (smoothing 0) or gliding to it (smoothing 1: the whole way; 0.5: the
 * last half of the way), round the pattern's end to its first; a
 * parameter with no lock anywhere is the knob's own. Elektron's
 * behaviour on the OP-XY takes locking the knob's own value on the
 * steps after -- which says what each makes easy: a single accent of
 * sound, or automation.
 *
 * Worked example (Unit_sequencer): at 120 BPM a step is 5512.5
 * samples; the first four steps begin at samples 0, 5513, 11025 and
 * 16538, and their gates close at 2757, 8269, 13782, 19294, whether the
 * blocks are 735 samples (a frame's), 500 or 1. The figure's locks,
 * read as steps 1, 5, 7 and 15 begin: Per_step 0.2, none, none, none;
 * Points 0 0.2, 0.2, 0.2, 0.8; Points 1 0.2, 0.5, 0.65, 0.35; Points
 * 0.5 0.2, 0.2, 0.5, 0.5. *)

(* a step: [note] a MIDI note, or None for a rest; [locks] its knobs'
 * values, by name *)
type step = { note : int option; accent : bool; slide : bool; locks : (string * float) list }

(* a rest, a note, a note accented, a note sliding *)
val rest : step
val note : ?accent:bool -> ?slide:bool -> int -> step

(* [lock s name v]: the step with [name] locked to [v] (replacing its
 * lock), [unlock s]: with none *)
val lock : step -> string -> float -> step
val unlock : step -> step

type event =
  (* a note begins; [glide]: the gate stays open and the voice glides
   * to it from the note sounding (the step before slid into it) *)
  | Note_on of { note : int; accent : bool; glide : bool }
  | Note_off

type t

val create : ?bpm:float -> step array -> t

(* the pattern changed and the tempo turned, heard from the next step *)
val set_pattern : t -> step array -> unit
val set_bpm : t -> float -> unit

(* [start t]: from the pattern's first step, at the next sample;
 * [stop t]: no more events (the voice lets its note go) *)
val start : t -> unit
val stop : t -> unit
val running : t -> bool

(* the step sounding (for a panel to light), from 0 *)
val step : t -> int

(* [samples_per_step bpm]: 60 / (bpm x 4) seconds in samples *)
val samples_per_step : float -> float

(* [advance t n f]: the next [n] samples: [f offset event] for each event
 * falling in them, in order, [offset] from the block's first sample *)
val advance : t -> int -> (int -> event -> unit) -> unit

(*****************************************************************************)
(* {1 Parameter locks} *)
(*****************************************************************************)

(* what a lock means between steps: the step's only (Elektron's), or a
 * point the value goes through, with a smoothing from 0 to 1 (the
 * OP-XY's) *)
type locks = Per_step | Points of float

(* [lock_value locks pattern name position]: [name]'s value at
 * [position], in steps from the pattern's start (fractional, wrapping
 * round), None where the knob's own value holds *)
val lock_value : locks -> step array -> string -> float -> float option

(* [position t offset]: where the pattern is, in steps, [offset]
 * samples into the block last advanced; None before its first step or
 * stopped *)
val position : t -> int -> float option

(* [locked t locks name offset]: [lock_value] at [position] *)
val locked : t -> locks -> string -> int -> float option
