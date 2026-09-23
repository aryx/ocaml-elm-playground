(* MOD: a song that carries its own instruments (see
 * notes_audio_formats.md section 1).
 *
 * Karsten Obarski's Ultimate Soundtracker (Amiga, 1987) wrote the first
 * modules, and the tracker to make them: short recordings (samples) and
 * the notes that play them, in one file, so the song sounds the same
 * everywhere -- unlike a MIDI file, which leaves the sound to whatever
 * reads it. NoiseTracker and ProTracker (1989-90) made the form
 * everyone used, big-endian like the Amiga's 68000:
 *
 *   offset  size
 *        0    20   the title
 *       20  30x31  the instruments: name (22), length in words (2),
 *                  finetune (1), volume 0-64 (1), loop start in words
 *                  (2), loop length in words (2; 1: no loop)
 *      950     1   the song's length: the positions that play
 *      951     1   (restart: kept, not used)
 *      952   128   the order list: a pattern per position
 *     1080     4   "M.K." -- 1084 bytes of header
 *     1084  1024x  the patterns: 64 rows x 4 channels x 4-byte cells
 *                  (as many as the order list's highest number + 1)
 *                  then the samples, 8-bit signed, one after the other
 *
 * The tag says the channels: "M.K.", "M!K!", "FLT4", "4CHN" four,
 * "6CHN", "8CHN" (FastTracker's) six and eight, "FLT8" and "OCTA" eight.
 * No tag at all is Ultimate Soundtracker's own form: 15 instruments, a
 * 600-byte header (20 + 15 x 30 + 1 + 1 + 128), four channels, and its
 * loop start in bytes rather than words.
 *
 * {1 A cell}
 *
 *     byte 0      byte 1      byte 2      byte 3
 *     iiii pppp   pppp pppp   iiii eeee   xxxx xxxx
 *
 * the instrument's high and low nibbles (0: none), a 12-bit period (0:
 * no new note), the effect (0-F) and its parameter. Worked example:
 * instrument 1 at C-2 (period 428 = 0x1AC), effect C (the volume) at
 * 0x20: 01 AC 1C 20; instrument 17 (0x11), the same: 11 AC 1C 20.
 *
 * {1 Periods}
 *
 * A cell has no note: it has what the Amiga's Paula chip counts down
 * from, at 3,546,895 Hz (a PAL machine's), between two bytes of the
 * sample -- a period is a sample rate, 3,546,895 / period: C-2's 428 is
 * 8,287 Hz. ProTracker's table ([periods]) covers three octaves, C-1
 * (856) to B-3 (113), each semitone the last divided by 2^(1/12) and
 * rounded; the finetune (-8 to 7 eighths of a semitone, a signed nibble)
 * shifts an instrument's. What pitch a period *sounds* is the sample's
 * business: a recording of a C played at C-2 sounds C.
 *
 * References: Ultimate Soundtracker (Karsten Obarski, EAS Computer
 * Technik, 1987); the format as documented with the players that read it
 * (MikMod, libxmp, OpenMPT's wiki); ProTracker's period table. *)

(*****************************************************************************)
(* {1 The song} *)
(*****************************************************************************)

type instrument = {
  name : string; (* up to 22 characters *)
  finetune : int; (* -8 to 7 *)
  volume : int; (* 0 to 64 *)
  loop_start : int; (* in bytes *)
  loop_length : int; (* in bytes; 0: no loop *)
  data : string; (* the sample, 8-bit signed, as in the file *)
}

type cell = {
  instrument : int; (* 1 to 31; 0: none *)
  period : int; (* 0: no new note *)
  effect : int; (* 0 to 15 *)
  param : int; (* 0 to 255 *)
}

type song = {
  title : string;
  instruments : instrument array; (* 31, or 15 for Ultimate Soundtracker's *)
  restart : int;
  positions : int array; (* the order list's played part: a pattern each *)
  patterns : cell array array array; (* pattern, row (64), channel *)
  tag : string; (* "M.K.", ...; "" for Ultimate Soundtracker's *)
}

(* the channels a tag says: 4 for "M.K.", 8 for "8CHN"...; None if it
 * isn't a tag *)
val channels_of_tag : string -> int option
val channels : song -> int

(* [of_string bytes]: a module, or why not *)
val of_string : string -> (song, string) result

(* [to_string song]: the file; the order list padded to 128 with 0,
 * patterns as many as [song.patterns] *)
val to_string : song -> string

(* the header's size: 1084 (31 instruments), 600 (15) *)
val header_size : song -> int

(*****************************************************************************)
(* {1 Cells} *)
(*****************************************************************************)

val empty_cell : cell

(* [cell_of_bytes s i], [cell_to_bytes c]: the 4 bytes at [i], and back *)
val cell_of_bytes : string -> int -> cell
val cell_to_bytes : cell -> string

(*****************************************************************************)
(* {1 Periods and notes} *)
(*****************************************************************************)

(* ProTracker's periods at finetune 0, C-1 to B-3: 856 ... 428 (C-2) ...
 * 113 *)
val periods : int array

(* [note_name period]: "C-2", "F#1", the nearest in [periods]; "---" for
 * 0 *)
val note_name : int -> string

(* [period_of_name "C-2"]: 428; None if it isn't one *)
val period_of_name : string -> int option

(* [rate period]: the sample rate Paula plays it at, 3,546,895 / period
 * (8,287.1 Hz for 428) *)
val rate : int -> float

(* the Amiga's clock for Paula, PAL: 3,546,895 Hz *)
val paula_clock : float

(*****************************************************************************)
(* {1 Samples} *)
(*****************************************************************************)

(* [sample i k]: instrument [i]'s byte [k] as a number from -1 to 1 *)
val sample : instrument -> int -> float

(* [data_of_floats a]: numbers from -1 to 1 as 8-bit signed bytes, an
 * instrument's [data] (clamped; an odd length padded with a 0, since
 * lengths are in words) *)
val data_of_floats : float array -> string
