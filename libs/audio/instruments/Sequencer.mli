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
 * Worked example (Unit_sequencer): at 120 BPM a step is 5512.5
 * samples; the first four steps begin at samples 0, 5513, 11025 and
 * 16538, and their gates close at 2757, 8269, 13782, 19294, whether the
 * blocks are 735 samples (a frame's), 500 or 1. *)

(* a step: [note] a MIDI note, or None for a rest *)
type step = { note : int option; accent : bool; slide : bool }

(* a rest, a note, a note accented, a note sliding *)
val rest : step
val note : ?accent:bool -> ?slide:bool -> int -> step

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
