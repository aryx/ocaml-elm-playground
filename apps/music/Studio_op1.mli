(* The OP-1: a synthesizer made of four modules, eight sounds, and a
 * four-track tape (see notes_synth.md; plan_synth_teaching.md, TinyOp1,
 * O3).
 *
 * A *studio*, as Studio_rebirth.mli is: several instruments and what
 * goes between them. The OP-1 (Teenage Engineering, 2011) makes a
 * sound of four modules, each on a soft key and its four parameters
 * on the four coloured encoders:
 *
 *     T1 engine    how the sound is made     Op1_engine.mli, six of them
 *     T2 envelope  attack decay sustain release, and the play mode
 *     T3 effect    delay, spring, punch, nitro
 *     T4 LFO       tremolo, value
 *
 * eight such sounds a key away, and instead of a sequencer's song a
 * *tape* (Tape.mli): four tracks recorded from what is played, while
 * what is already there plays back -- the studio of 1967, in a
 * keyboard:
 *
 *     keys --> sound (engine, envelope, effect, LFO) --+--> out
 *                                                      |
 *                              tape <--- recording ----+
 *                                |
 *                                +--- playing back ----> out
 *
 * The effects, from the manual's four words each, over this
 * repository's blocks (ours where the words don't say more): delay
 * ("solid state delay": size, speed, feedback, mix -- Delay.mli, the
 * speed its echoes' tone), spring ("mathematic reverb": tone, turns,
 * damping, send -- Reverb.mli's Freeverb, the turns its length), punch
 * ("hard hitting low pass filter": frequency, punch, rounds, power --
 * a resonant low-pass, then a drive), nitro ("dual resonant turbo
 * filter": frequency, follow, resonance, frequency -- two resonant
 * low-passes, the first following the envelope). The LFOs: tremolo
 * (speed, pitch, volume, envelope: a vibrato and a tremolo, fading in
 * or out with the note) and value (amount, speed, destination,
 * parameter: one encoder of one module moved by a sine).
 *
 * The tape is 30 seconds (the OP-1's six minutes of four tracks would
 * be half a gigabyte of floats: ours, and said so).
 *
 * Worked example (Unit_op1): a sine's level through its envelope,
 * -18.4 dB while rising, -14.3 at the top, -18.1 sustained, -40.8 30 ms
 * after the key is let go; the effects each changing the sound; the
 * tremolo's quietest 20 ms 0.32 of its loudest; a phrase recorded on
 * track 1, played back from the tape alone, the same samples at the
 * track's level (0.8, to 1e-9); a golden WAV of each of the eight
 * sounds. *)

(*****************************************************************************)
(* The sound *)
(*****************************************************************************)

val effects : string list (* "delay", "spring", "punch", "nitro" *)
val effect_encoders : int -> string array
val lfos : string list (* "tremolo", "value" *)
val lfo_encoders : int -> string array
val envelope_encoders : string array (* attack, decay, sustain, release *)
val play_modes : string list (* "poly", "mono" *)

type sound = {
  engine : int; (* an index in Op1_engine.all *)
  engine_params : float array; (* its four encoders, 0 to 1 *)
  envelope : float array; (* attack, decay, sustain, release, 0 to 1 *)
  play_mode : int;
  effect : int; (* an index in [effects] *)
  effect_params : float array;
  effect_on : bool;
  lfo : int; (* an index in [lfos] *)
  lfo_params : float array;
  lfo_on : bool;
  octave : int; (* -2 to 2 *)
}

type patch = {
  sounds : sound array; (* 8 *)
  current : int; (* the sound played, 0 to 7 *)
  levels : float array; (* the tape's four tracks *)
  volume : float;
}

val initial : patch

(* the knobs' curves: [seconds k], 1 ms to 10 s, the envelope's *)
val seconds : float -> float

(* [voice sound params key velocity]: a note of [sound]'s engine and
 * envelope (its octave, not its effect or LFO), the engine reading
 * [params] each block (for Studio_opxy's tracks too) *)
val voice : sound -> float array -> int -> float -> Polyphony.voice

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t

val create : patch -> t
val patch : t -> patch
val set_patch : t -> patch -> unit

(* the tape, 4 tracks of 30 s; [record t k] its transport armed on
 * track k (while it plays the others back), [play t], [stop t] *)
val tape : t -> Tape.t
val record : t -> int -> unit
val play : t -> unit
val stop : t -> unit

(* the notes sounding *)
val voices : t -> int

(* the last 2048 samples out, for the screen *)
val recent : t -> Signal.t
val instrument : t -> Instrument.t
