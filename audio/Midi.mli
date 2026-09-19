(* MIDI files: music as messages with their times (see
 * notes_audio_midi.md, the whole story).
 *
 * MIDI (1983) doesn't carry sound, only what a player does: "key 60
 * pressed, this hard", "released", "switch to the trumpet". A message
 * is a status byte (high bit 1: what, and on which of 16 channels) and
 * one or two data bytes (high bit 0, 0 to 127):
 *
 *   9n kk vv    note on, channel n, key kk (60: middle C), velocity vv
 *   8n kk vv    note off (and a note on with velocity 0 is one too)
 *   Cn pp       program change: the instrument (General MIDI's list)
 *
 * Running status: several messages in a row with the same status send
 * it once -- 90 3C 64 40 64 43 64 is three notes on, C E G, a chord, in
 * 7 bytes instead of 9.
 *
 * A Standard MIDI File (1988) stores them with their times: chunks
 * (a 4-letter tag, a 32-bit big-endian length), an MThd header (the
 * format, the tracks, the division: ticks per quarter note), then MTrk
 * tracks, each event after a delta time in ticks, written as a
 * variable-length quantity, 7 bits a byte, the high bit set on every
 * byte but the last:
 *
 *        0 -> 00     127 -> 7F     128 -> 81 00     200 -> 81 48
 *      480 -> 83 60      16383 -> FF 7F       16384 -> 81 80 00
 *
 * Ticks become seconds through the tempo, a meta event (FF 51 03) in
 * microseconds per quarter note: at 480 ticks per quarter and 500,000
 * microseconds (120 beats a minute), a tick is 1.042 ms, 960 ticks a
 * second; the tempo can change mid-song, so the conversion walks the
 * tempo map, each stretch at its own tempo.
 *
 * MIDI's cousins, the trackers' module files (MOD, S3M, XM, IT: the
 * Amiga and the demoscene), carry their instruments' samples inside
 * where MIDI carries only what to play: notes_audio_midi.md section 9
 * (not read here).
 *
 * References: the MIDI 1.0 Detailed Specification, and the Standard
 * MIDI Files 1.0 specification (the MIDI Manufacturers Association,
 * 1983 and 1988), https://midi.org/specifications ; General MIDI 1
 * (1991); notes_audio_midi.md. *)

(* a note of a score: when it starts, how long it lasts (seconds), its
 * key (MIDI number), velocity (0-127), channel (0-15, 9 for drums),
 * and program (the instrument, 0-127) at the time *)
type note = { start : float; length : float; key : int; velocity : int; channel : int; program : int }

type score = { notes : note list; (* by start time *) duration : float }

(* {1 Variable-length quantities} *)

(* [vlq n]: its bytes (the table above) *)
val vlq : int -> string

(* [read_vlq s i]: the number at [i], and the position after it *)
val read_vlq : string -> int -> int * int

(* {1 Files} *)

(* [parse bytes]: a Standard MIDI File's notes (formats 0 and 1: every
 * track's notes merged), or why not *)
val parse : string -> (score, string) result

(* [of_tune ?program tune]: a Standard MIDI File (format 1, 480 ticks a
 * quarter, 120 beats a minute, a track per voice, each on its channel)
 * playing an Abc.tune: ABC (or solfège) to MIDI *)
val of_tune : ?program:int -> Abc.tune -> string
