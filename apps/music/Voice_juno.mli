(* The Roland Juno-106's voice: one oscillator that stays in tune, one
 * envelope, and the chorus (see notes_synth.md; plan_synth_teaching.md,
 * TinyJuno, J1).
 *
 * The Juno-60 (1982) and 106 (1984) made polyphony cheap: six voices,
 * each only one oscillator, one filter, one envelope for both the
 * filter and the amplifier -- a Minimoog's voice cut down to what
 * matters, and then made to sound big by what comes after it:
 *
 *     key -> DCO: saw + pulse + sub + noise -> VCF (4-pole) -> VCA --+
 *              ^ LFO (delayed)      ^ ENV, LFO, key    ^ ENV or gate |
 *                                                                   v
 *            the six voices summed -> HPF (one, 4 positions) -> chorus -> L, R
 *
 * The *DCO*: the voice's waveforms are analogue (a capacitor charged
 * and reset makes the sawtooth, a comparator on it the pulse, a
 * flip-flop the sub-oscillator's square an octave down), but *when* it
 * resets is counted from a digital clock. A VCO's pitch drifts with
 * its temperature (Drift.mli, the Minimoog's); a DCO's is the clock's,
 * divided: six voices exactly in tune for ever, and a little less alive
 * -- which the chorus gives back.
 *
 * The *chorus*: the sound into two bucket-brigade delay lines (a chip
 * passing samples along a chain of capacitors, an analogue delay), their
 * delay moved by one triangle LFO, the right line's inverted: while the
 * left copy's pitch rises, the right one's falls, each side the dry
 * sound plus its copy. Chorus I at 0.513 Hz, II at 0.863 Hz, the delay
 * between 1.66 and 5.35 ms; I and II together at 9.75 Hz between 3.3
 * and 3.7 ms, the same on both sides: a vibrato, "a Leslie". So much of
 * the Juno's sound is this that Roland sold it alone (the Dimension D).
 *
 * The *HPF* is one for all six voices, after them: four positions,
 * the 106's a +6 dB shelf below 65 Hz, flat, 6 dB/octave below 225 Hz,
 * below 720 Hz.
 *
 * Facts from Andy Harman's measurements of a Juno-60 (its envelope's
 * times and curves, its chorus, its high-pass; github.com/
 * pendragon-andyh/Juno60), the 106's the same design. Ours, and said
 * so: the filter as Moog_ladder's (the Juno's IR3109 chip is a
 * four-pole OTA ladder too), the knobs' curves beyond the envelope's,
 * the envelope's times given to Envelope.mli as its -60 dB times (the
 * Juno's "durations" are nearer -40 dB: ours dies a little sooner), no
 * low-pass before the chorus's lines (the Juno's 12 dB one).
 *
 * Worked example (Unit_juno): the attack slider at 0, 5 and 10 (of 10)
 * 0.001, 0.248 and 3.251 s by Harman's curve (he measured 0.001, 0.24,
 * 3.25), the decay's 0.002, 1.043, 17.46 (measured 0.002, 0.984, 19.78:
 * his fit short at the top); the sub-oscillator's A4 at 220 Hz; the
 * high-pass at 30 Hz and 1 kHz: position 0 1.86 and 1.01 (the shelf),
 * 1 flat, 2 0.13 and 0.96, 3 0.04 and 0.77; chorus I's delays 1.66 and
 * 5.35 ms at its start, both 3.505 a quarter of its cycle in, I+II's
 * the same on both sides; the presets on a phrase peaking from 0.34 to
 * 0.66, a golden WAV each. Six voices at once: 0.12 s of CPU a second
 * of sound natively, 0.30 s in JavaScript. *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

(* the 106's panel, its sliders 0 to 1 *)
type patch = {
  lfo_rate : float; (* 0.1 to 20 Hz *)
  lfo_delay : float; (* 0 to 3 s, the LFO faded in after the key *)
  range : int; (* an index in [ranges]: 16', 8', 4' *)
  dco_lfo : float; (* the vibrato, up to a semitone *)
  pwm : float; (* the pulse's width, 50% to 95% *)
  pwm_lfo : bool; (* the width moved by the LFO, else set by hand *)
  pulse : bool;
  saw : bool;
  sub : float;
  noise : float;
  hpf : int; (* 0 to 3 *)
  cutoff : float; (* 20 Hz to 20 kHz *)
  resonance : float; (* self-oscillating at 1 *)
  env : float; (* the envelope into the cutoff, up to 7 octaves *)
  env_invert : bool;
  vcf_lfo : float; (* up to 2 octaves *)
  key_follow : float; (* 1: the cutoff follows the key octave for octave *)
  gate : bool; (* the VCA a gate, else the envelope *)
  level : float;
  attack : float;
  decay : float;
  sustain : float;
  release : float;
  chorus : int; (* an index in [choruses]: off, I, II, I+II *)
  volume : float;
}

val ranges : string list
val choruses : string list
val initial : patch

type knob = patch Patch_text.knob

val knobs : knob list
val to_string : patch -> string
val of_string : string -> (patch, string) result

(* ours: brass, strings, bass, pad, lead *)
val presets : (string * patch) list

(* Harman's curves: [attack_seconds k] for the slider at [k] (0 to 1),
 * 0.001 + (e^5k - 1) / (e^5 - 1) x 3.25; [decay_seconds k], the decay's
 * and the release's, 0.002 + (e^4k - 1) / (e^4 - 1) x 17.46 k *)
val attack_seconds : float -> float
val decay_seconds : float -> float

(* [chorus_delays mode t]: the two lines' delays, seconds, [t] seconds
 * into the LFO's cycle, for [mode] 1 (I), 2 (II), 3 (I+II) *)
val chorus_delays : int -> float -> float * float

(* [high_pass position s]: [s] through the shared high-pass, set at
 * [position] (0 to 3), from rest *)
val high_pass : int -> Signal.t -> Signal.t

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t

(* 6 voices, the Juno's *)
val create : patch -> t
val patch : t -> patch
val set_patch : t -> patch -> unit
val voices : t -> int
val recent : t -> Signal.t
val instrument : t -> Instrument.t
