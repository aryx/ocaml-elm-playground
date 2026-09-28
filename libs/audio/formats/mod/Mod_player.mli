(* A MOD player: the song's patterns played on Paula's channels (see
 * notes_audio_formats.md section 1).
 *
 * Time is ticks: 2.5 / tempo seconds each (the tempo 125 by default: a
 * tick every 20 ms, 882 of our samples -- the PAL screen's 50 Hz, which
 * the first trackers played on), a row every [speed] ticks (6: a row
 * every 120 ms). At a row's first tick each channel reads its cell: a
 * new instrument sets the volume, a new period restarts the sample (at
 * its finetune); then the effects act, some once on that tick, most on
 * every other tick of the row:
 *
 *     tick   0      1      2      3      4      5      0 (next row)
 *            cell   slides, vibrato, volume slides, arpeggio's notes...
 *
 * The effects played: 0xy arpeggio (the note, +x, +y semitones, a tick
 * each), 1xx and 2xx the slides (the period down or up by xx a tick),
 * 3xx the slide to the cell's note, 4xy vibrato (ProTracker's sine table,
 * speed x, depth y), 5xy and 6xy (3 and 4 going on, with a volume
 * slide), 9xx the sample from byte xx x 256, Axy the volume slide, Bxx
 * the jump to position xx, Cxx the volume, Dxy the break to row 10x + y
 * of the next position, Fxx the speed (under 32) or the tempo, and E1y,
 * E2y (fine slides), EAy, EBy (fine volume slides), ECy (the note cut
 * at tick y). The others (the pattern loop E6, the delays ED and EE,
 * the filter E0...) are read and ignored.
 *
 * The four channels are Paula's, panned as the Amiga's: 1 and 4 on the
 * left, 2 and 3 on the right, hard ([separation] 1); a separation of 0
 * mixes them all in the middle, 0.5 halfway, what players offer for
 * headphones.
 *
 * Worked example, the row's length: at speed 6 and tempo 125, 6 x 882
 * = 5,292 samples, 120 ms; F03 (speed 3), 2,646; F7D (tempo 125) is the
 * default, FA0 (160) makes a tick 689.06 samples.
 *
 * References: the ProTracker effects as documented with the players
 * (MikMod, libxmp, OpenMPT's wiki); ProTracker's vibrato table. *)

type t

(* [create ?loop song]: at the song's start; at its end, back to the
 * first position if [loop] (true by default), else silent *)
val create : ?loop:bool -> Mod.song -> t

(* [fill p out]: the next samples, both channels *)
val fill : t -> Signal.stereo -> unit

(* how to read the samples (Paula.mli), Hold by default *)
val set_reading : t -> Paula.reading -> unit

(* 1 hard (the Amiga's), 0 all in the middle *)
val set_separation : t -> float -> unit

(* where it is: the position in the order list, the row *)
val position : t -> int * int

(* [seek p ~position ~row]: from there, at its first tick *)
val seek : t -> position:int -> row:int -> unit

(* [set_song p song]: [song] from now on, where it is -- a tracker's edits
 * heard as they are made (its instruments' samples read again) *)
val set_song : t -> Mod.song -> unit

(* the song it plays *)
val song : t -> Mod.song

(* the song played to its end (never, looping) *)
val finished : t -> bool

(* for tests and a display: channel [c]'s period, volume *)
val channel_period : t -> int -> int
val channel_volume : t -> int -> int

(* the speed and the tempo now *)
val speed : t -> int
val tempo : t -> int

(* the samples a tick lasts at [tempo]: 44,100 x 2.5 / tempo *)
val tick_samples : int -> float
