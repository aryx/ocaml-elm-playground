(* Reason's main sequencer's data: a track of notes per instrument, the
 * song looping over its bars (TinyReason's sequencer pane edits it,
 * Studio_reason plays it).
 *
 * Time is counted in sixteenths from the loop's start, a note a start,
 * a length, a pitch (MIDI's) and a velocity:
 *
 *   sixteenth  0   4   8   12  16
 *   C4         [=======]              a note from 0, 8 long
 *   E4                 [===]          from 8, 4 long
 *
 * What a stretch of time plays is its *events*: the notes starting in
 * it, the notes ending in it, the loop wrapped round. The rack asks for
 * them a chunk at a time, and plays them at the chunk's start: a note
 * up to a chunk (1.4 ms) early (Unit_reason: the note at 22050 samples
 * on at 22016), where the Matrix, reading its step at the chunk's
 * start, is up to a chunk late.
 *
 * Ours, and said so: one loop for the whole song (Reason's has a song
 * of any length, a loop inside it), no quantize, no velocity lane (a
 * note's velocity 0.8 as drawn). *)

type note = { start : float; length : float; pitch : int; velocity : float }

(* the tracks by the id of the device they play *)
type t = { bars : int; tracks : (int * note list) list }

val empty : t

(* the loop's length, in sixteenths *)
val length : t -> float
val notes : t -> int -> note list
val set_notes : t -> int -> note list -> t

(* [add t track note]: the note put in, over any note of the same pitch
 * it overlaps; [remove t track n]: that note taken out *)
val add : t -> int -> note -> t
val remove : t -> int -> note -> t

(* the note at a time and a pitch, if one sounds there *)
val note_at : t -> int -> float -> int -> note option

type event = On of int * float | Off of int

(* [events t ~from ~until]: what plays from [from] up to [until] (in
 * sixteenths, [from] in the loop, [until] past it wrapping round), by
 * track, in order *)
val events : t -> from:float -> until:float -> (int * event) list
