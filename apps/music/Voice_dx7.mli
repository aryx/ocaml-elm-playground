(* The DX7's voice: six operators through an algorithm, a patch as the
 * DX7's own bytes (see notes_synth.md section 10; plan_synth_teaching.md,
 * TinyDX7, D2).
 *
 * The Yamaha DX7 (1983) sold more than any synthesizer before it, and
 * its sound -- the electric piano of every ballad, the bass of every
 * hit, the bells -- is the 1980s'. It was also famously hard to
 * program: 145 parameters a voice, edited one at a time with a single
 * data slider, most people using the presets. Here the voice is built
 * from audio/'s blocks, each a lesson of its own:
 *
 *     key, velocity
 *       |
 *       v                                 Fm_algorithm (the wiring)
 *     for each operator:                  +------------------------+
 *       frequency: the key's, times a     |  6 -> 5 -> 4 -> 3 -+   |
 *         ratio (or a fixed one),         |  2 -> 1 -----------+-> out
 *         + pitch envelope + LFO          +------------------------+
 *       level: Dx_envelope, from its          ^
 *         output level, less the          each operator a sine, its
 *         keyboard's scaling and the      amplitude 2^(steps/256 - 14)
 *         velocity's, less the LFO        (Dx_envelope.gain)
 *
 * The patch is the DX7's parameters as the DX7 has them, each an
 * integer in its own range, so a voice from a cartridge is read as it
 * is and plays as it did. Their meanings, and the formulas, are Dexed's
 * (msfa's, measured on the hardware), said at each field below;
 * where Dexed's own is a guess (the amplitude modulation's curve), ours
 * is simpler and said so.
 *
 * The frequencies: a *ratio* operator plays the key's frequency times
 * its coarse ratio (0 is 0.5, then 1 to 31) times 1 + fine / 100, and
 * detuned by a few cents (detune 0 to 14, 7 none: about a cent a step
 * at A4, Dexed's 0.0209 e^(-0.396 log2 f) / 7 octave per step and
 * octave); a *fixed* one plays 10^(coarse mod 4 + fine / 100) Hz
 * whatever the key (1, 10, 100, 1000 Hz and between: a drum's
 * clank, a bell's partial that doesn't follow the keyboard).
 *
 * The levels: the output level 0-99 (Dx_envelope's scaled 0-127, a
 * step 0.75 dB), plus the keyboard level scaling -- a break point, and
 * on each side a depth and a curve (linear or exponential, down or up,
 * a step every 3 keys away) so a patch can be quieter at the top, as a
 * piano's high strings are -- times 32, plus the velocity's
 * (sensitivity 0 to 7: 0 ignores it). The envelope's rates are faster
 * up the keyboard by the rate scaling (0 to 7), as a high string dies
 * faster.
 *
 * The pitch envelope: four rates and levels too, 50 no change, up to
 * 4 octaves either way (Dexed's table); the LFO, one for the whole
 * instrument (triangle, saws down and up, square, sine, sample and
 * hold), its speed 0.06 to 49 Hz, its delay ramping it in after the
 * key, into the pitch (depth times sensitivity, up to an octave) and
 * the level (depth times each operator's sensitivity: ours, linear in
 * dB, to silence at full).
 *
 * The bytes: a voice is 128 bytes packed (op 6 first, 17 bytes each,
 * then 26 for the pitch envelope, the algorithm, the LFO, the name); a
 * cartridge 32 voices after a 6-byte header, a checksum and F7, 4104
 * bytes: the files people share as .syx. Yamaha's own voices are
 * Yamaha's: bring your own cartridge; ours below are our own.
 *
 * Worked example (Unit_dx7): the initial voice (the DX7's INIT VOICE,
 * one sine) at A4 is 440 Hz; a ratio of 0 an octave down, coarse 3 fine
 * 50 is 4.5 times; detune 14 at A4 +6.8 cents, at A1 +14.7 (nearly
 * even in Hz, so a detuned pair beats at about the same rate all along
 * the keyboard); fixed coarse 2 fine 0 100 Hz at every key. The level
 * scaling at depth 99, three octaves from the break point: -lin 95
 * steps of 0-127 (71 dB), +exp only 15 (the exponential curves start
 * gently, 3 steps an octave away). Velocity 127 at sensitivity 7 is 224
 * steps up (5.3 dB), 64 is 448 down (10.5 dB). A patch's 128 bytes and
 * its text read back to it; a cartridge's checksum catching a changed
 * byte. The presets on a riff of four-note chords peak from 0.28 (the
 * marimba) to 0.69 (the brass); their brightness moving as FM's does,
 * measured as the spectrum's centroid: the brass's from 367 Hz to 2568
 * Hz in its first 0.1 s (the modulator's slower attack: the swell), the
 * bass's from 504 to 141 (the pluck), the organ's steady. *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type operator = {
  rates : int array; (* 4, 0-99 *)
  levels : int array; (* 4, 0-99 *)
  break_point : int; (* 0-99, 0 the A below the piano's lowest, 39 middle C *)
  left_depth : int; (* 0-99 *)
  right_depth : int;
  left_curve : int; (* 0-3: -lin, -exp, +exp, +lin *)
  right_curve : int;
  rate_scaling : int; (* 0-7 *)
  amp_sensitivity : int; (* 0-3 *)
  velocity : int; (* 0-7 *)
  level : int; (* 0-99, the output level *)
  fixed : bool;
  coarse : int; (* 0-31 *)
  fine : int; (* 0-99 *)
  detune : int; (* 0-14, 7 none *)
}

type patch = {
  name : string; (* 10 characters *)
  operators : operator array; (* op 1 first *)
  pitch_rates : int array;
  pitch_levels : int array; (* 50 the note *)
  algorithm : int; (* 1-32 *)
  feedback : int; (* 0-7 *)
  key_sync : bool; (* the oscillators restarted at each key *)
  lfo_speed : int;
  lfo_delay : int;
  lfo_pitch_depth : int;
  lfo_amp_depth : int;
  lfo_sync : bool; (* the LFO restarted at each key *)
  lfo_wave : int; (* an index in [waves] *)
  pitch_sensitivity : int; (* 0-7 *)
  transpose : int; (* 0-48, 24 none *)
}

val waves : string list
val curves : string list

(* the DX7's INIT VOICE: algorithm 1, op 1 alone at 99, a sine *)
val initial : patch

(* [of_packed bytes]: a voice from its 128 packed bytes (7 bits each,
 * out-of-range values clamped, as Dexed does) *)
val of_packed : string -> patch
val to_packed : patch -> string

(* [cartridge bytes]: the 32 voices of a .syx bulk dump; the header,
 * the length and the checksum checked *)
val cartridge : string -> (patch array, string) result

(* [to_cartridge voices]: a .syx, filled up to 32 with [initial] *)
val to_cartridge : patch array -> string

(* the patch as text, "op1.rate1 = 99" lines, the name first *)
type knob = patch Patch_text.knob

val knobs : knob list
val to_string : patch -> string
val of_string : string -> (patch, string) result

(* our own: an electric piano, a brass, a bass, bells, a marimba, an
 * organ *)
val presets : (string * patch) list

(*****************************************************************************)
(* The formulas *)
(*****************************************************************************)

(* [frequency op note]: the operator's frequency at MIDI [note] (the
 * transpose applied by the caller) *)
val frequency : operator -> int -> float

(* [level_scaling op note]: the keyboard's scaling, in steps of 0-127 *)
val level_scaling : operator -> int -> int

(* [output_level op ~note ~velocity]: the envelope's output level, in
 * steps (Dx_envelope.create), [velocity] 0-127 *)
val output_level : operator -> note:int -> velocity:int -> int

(* [rate_scaling op note]: added to the envelope's qrates *)
val rate_scaling : operator -> int -> int

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t

(* [create ?voices patch]: 16 voices by default, the DX7's *)
val create : ?voices:int -> patch -> t

val patch : t -> patch
val set_patch : t -> patch -> unit
val voices : t -> int

(* the last 2048 samples played, for a scope *)
val recent : t -> Signal.t

(* [levels t]: each operator's amplitude (cycles, 0 to 2: its
 * envelope), the loudest of the voices, at the last block's end, op 1
 * first, for a panel's meters *)
val levels : t -> float array

(* note_on, note_off, one knob "volume" (0 to 1) besides the patch's *)
val instrument : t -> Instrument.t
