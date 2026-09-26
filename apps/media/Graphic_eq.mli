(* A graphic equalizer: Winamp's ten sliders, a band each.
 *
 * A hi-fi's tone controls are three knobs (Eq.mli: a shelf for the
 * bass, a bell for the middle, a shelf for the treble); a graphic EQ
 * (the studio's, then the car stereo's of the 1980s) is a row of bells
 * at fixed frequencies, each a slider, so that the sliders together
 * draw the curve they make -- hence "graphic". Winamp 2's (1998) ten:
 *
 *     60  170  310  600  1k  3k  6k  12k  14k  16k   Hz
 *
 * roughly an octave apart up to 1 kHz, then not at all: three bands
 * crowded between 12 and 16 kHz, where a song has little (and an MP3 at
 * 128 kbps nothing: its encoder cut it, Layer3.mli). A studio's ISO
 * graphic EQ spaces them evenly on a log scale instead (31, 63, 125,
 * ..., 16k: an octave each).
 *
 * Each band is Filter's cookbook peaking bell, Q 1.4 (an octave wide),
 * -12 to +12 dB, one after another, and the preamp a gain before them
 * all, to take back the level the boosts add (six bands up 6 dB
 * overlapping clip: the preamp down 6 dB makes room). At 0 dB a bell
 * is no filter, so a flat EQ changes nothing, and the curve drawn over
 * the sliders ([response]) is the bells' gains multiplied, in dB added:
 * what the sound really goes through, not a line joining the knobs.
 *
 * The gains are read each block the mixer pulls; a band moved gets its
 * new coefficients at the next block (none of Eq.mli's ramp: a slider
 * is dragged a step at a time, small enough not to click). *)

(* the ten bands' centre frequencies, in Hz *)
val frequencies : float array

(* the sliders' range, dB: -12 to 12 *)
val range : float

type t

val create : unit -> t

(* [process t ~preamp ~gains s]: [s] in place through the preamp and
 * the ten bells, gains in dB (an array of ten) *)
val process : t -> preamp:float -> gains:float array -> Signal.stereo -> unit

(* [response ~preamp ~gains f]: the gain at [f] Hz, in dB *)
val response : preamp:float -> gains:float array -> float -> float

(* the presets, named after Winamp's (the shapes their names say; the
 * values ours): Flat first *)
val presets : (string * float array) list
