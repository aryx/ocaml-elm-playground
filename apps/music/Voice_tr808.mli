(* The Roland TR-808's voices and its sequencer: drums synthesized, not
 * sampled (see notes_synth.md; plan_synth_teaching.md, TinyTR808, T1).
 *
 * The TR-808 Rhythm Composer (1980) was a commercial failure -- its
 * drums sounded nothing like drums -- and then the sound of electro,
 * hip hop, house and trap, because they sounded like nothing else. Each
 * of its eleven instruments is a small analogue circuit struck by a 1
 * ms pulse from the sequencer, the pulse's height the accent:
 *
 *  - the *bass drum*: a bridged-T network, a band-pass on the edge of
 *    ringing, struck -- a decaying sine near 50 Hz. For its first 6 ms
 *    the circuit raises its frequency more than an octave (the punch:
 *    too short to hear as a pitch, heard as the attack's click), and all
 *    along a leakage makes the pitch follow the level (the "sigh", the
 *    note sinking as it dies). Ours: a sine whose frequency the model
 *    moves so (Modal.mli's resonators can't change theirs), its tone a
 *    low-pass on the click, its decay the sine's;
 *  - the *snare*: two tones struck (bridged-Ts) and noise through a
 *    high-pass, the "snappy" its level;
 *  - the *toms*: a bridged-T each, sighing like the kick;
 *  - the *rim shot*: two short tones; the *clap*: noise through a
 *    band-pass in three bursts 10 ms apart then a tail, several hands;
 *  - the *metal*: six square-wave oscillators (205.3, 304.4, 369.6,
 *    522.7, 540 and 800 Hz, running all the time) summed, through two
 *    band-passes (about 3440 and 7100 Hz) that keep their overtones and
 *    hide their fundamentals -- inharmonic, a metal's clang. The closed
 *    and open hi-hats the upper band through a high-pass, short or long
 *    (a closed hat stops an open one: the *choke*, a foot on the
 *    pedal); the cymbal three bands with their own envelopes; the
 *    cowbell the 540 and 800 Hz squares alone through their own
 *    band-pass.
 *
 *     trigger (accent) -> bridged-T ring --------------> kick, toms
 *                      -> 2 tones + noise HP ----------> snare
 *     6 squares -> BP 3440, BP 7100 -> envelope, HP -> hats, cymbal
 *
 * The sequencer: 16 steps, a track per instrument, an accent track;
 * our Sequencer.mli as its clock, a step a sixteenth, each hit at its
 * own sample whatever the blocks.
 *
 * Facts from Kurt Werner, Jonathan Abel and Julius Smith's papers on
 * the 808's bass drum (DAFx 2014) and cymbal (ICMC 2014). Ours, and said
 * so: the snare's (180 and 330 Hz), toms' (90, 130, 190 Hz), rim shot's,
 * clap's and cowbell's filter frequencies, all the decay times, the
 * sigh's depth (12%), the punch's octave and a bit (2.2 times).
 *
 * Worked example (Unit_tr808): the kick at 108.9 Hz its first 6 ms (the
 * punch), then 55.4 Hz at full level and 50.1 at a tenth; measured on
 * a long kick, 52.7 Hz in its first quarter second and 49.6 near its
 * end (the sigh); the closed hat's energy 90% above 5 kHz, under 0.1%
 * below 1 kHz (the squares' fundamentals hidden); an open hat still
 * sounding a quarter second in, stopped by a closed one; a pattern
 * rendered in blocks of 735 and of 100, the same samples; the patterns
 * peaking from 0.47 to 0.79, a golden WAV each. The same samples
 * whatever the blocks took three things: the squares running all the
 * time (as the 808's), each hit's own noise, and a hit's end decided
 * at its sample -- each, first done per block, made two renderings
 * differ. The busiest pattern costs 6% of a CPU natively, 12% in
 * JavaScript. *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type instrument = BD | SD | LT | MT | HT | RS | CP | CB | CY | OH | CH

val instruments : instrument list
val name : instrument -> string (* "BD", ... *)
val index : instrument -> int

(* an instrument's knobs, 0 to 1 (each uses its own: the kick level,
 * tone and decay; the snare level, tone, snappy; a tom level, tuning;
 * the cymbal level, tone, decay; the open hat level, decay; ...) *)
type drum = { level : float; tone : float; decay : float; tuning : float; snappy : float }

type patch = {
  drums : drum array; (* by [index] *)
  tracks : bool array array; (* by [index], 16 steps each *)
  accents : bool array; (* 16 steps *)
  accent : float; (* how much louder an accented step, 0 to 1 *)
  tempo : float; (* BPM *)
  volume : float;
}

val initial : patch

type knob = patch Patch_text.knob

(* the knobs; the tracks written "BD.steps = x...x...x...x..." *)
val knobs : knob list
val to_string : patch -> string
val of_string : string -> (patch, string) result

(* ours: electro, house, hip hop, latin *)
val presets : (string * patch) list

(* [drum_frequency ~f0 ~sigh ~punch ~age level]: the kick's and toms'
 * frequency [age] seconds in, at [level] (0 to 1): [punch] f0 the first
 * 6 ms, then f0 (1 + [sigh] level) *)
val drum_frequency : f0:float -> sigh:float -> punch:float -> age:float -> float -> float

(* the General MIDI drum keys: 36 BD, 38 SD, ... *)
val key : instrument -> int
val of_key : int -> instrument option

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t

val create : patch -> t
val patch : t -> patch
val set_patch : t -> patch -> unit

(* the sequencer: run or stop, the step sounding *)
val run : t -> bool -> unit
val running : t -> bool
val step : t -> int

(* [hit t i ~accent]: an instrument struck now (at the next sample) *)
val hit : t -> instrument -> accent:bool -> unit

(* the hits still sounding *)
val sounding : t -> int
val recent : t -> Signal.t

(* note_on a General MIDI drum key: that instrument struck, velocity
 * above 0.8 an accent; note_off nothing (a drum is struck, not held) *)
val instrument : t -> Instrument.t
