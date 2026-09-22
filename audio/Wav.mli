(* WAV files: samples on disk, the PPM of sound (see notes_audio.md
 * section 2): what the golden tests compare, and what a sound can be
 * listened to with, in any player.
 *
 * A WAV file (Microsoft and IBM's RIFF format, 1991) is a 44-byte
 * header, then the samples, here 16-bit integers (little-endian), one
 * channel:
 *
 *   offset  bytes  what                      for 1 s at 44,100 Hz
 *     0      4     "RIFF"
 *     4      4     36 + the data's size      88,236
 *     8      4     "WAVE"
 *    12      4     "fmt "
 *    16      4     16 (the format's size)
 *    20      2     1 (PCM: plain samples)
 *    22      2     1 channel
 *    24      4     the sample rate           44,100
 *    28      4     bytes a second            88,200
 *    32      2     bytes per sample frame    2
 *    34      2     bits per sample           16
 *    36      4     "data"
 *    40      4     the data's size           88,200
 *    44     ...    the samples
 *
 * References: the RIFF/WAVE format, "Multimedia Programming Interface
 * and Data Specifications 1.0", IBM and Microsoft, 1991;
 * http://soundfile.sapp.org/doc/WaveFormat/ *)

(* [to_string samples]: the whole file, header and samples (each as
 * Signal.to_int16) *)
val to_string : Signal.t -> string

(* [of_string s]: the samples of a file [to_string] wrote (16-bit mono
 * PCM at Signal.rate), as floats (x / 32,767); Error for anything else *)
val of_string : string -> (Signal.t, string) result

(* [write path samples], [read path] *)
val write : string -> Signal.t -> unit
val read : string -> (Signal.t, string) result

(* [write_stereo path s]: two channels, the header's channels 2, the
 * bytes a second and per sample frame doubled, and the samples
 * interleaved, left then right: L0 R0 L1 R1 ... (what a game's
 * -dump-audio writes) *)
val write_stereo : string -> Signal.stereo -> unit
