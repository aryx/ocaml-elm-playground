(* The TB-303's voice: a bass line playing itself (plan_synth_teaching.md,
 * TinyTB303, B2).
 *
 * Roland's Bass Line (1981) was sold to guitarists as a practice bass
 * and flopped; a few years later, in Chicago, its knobs turned while it
 * played became acid house. It is a small subtractive synthesizer -- one
 * oscillator, saw or square, into a filter, a volume -- and what makes
 * it squelch is how its few parts are wired:
 *
 *  - the diode ladder (Diode_ladder.mli), not a Moog's: gentler just
 *    above the cutoff, a rubbery resonance;
 *  - the filter's envelope, a decay only, the *main envelope*: its time
 *    the Decay knob (200 ms to 2 s) and its depth Env Mod (never quite
 *    none: ours, from Robin Whittle's remark);
 *  - the *accent*: an accented note's envelope decays in the shortest
 *    time whatever the knob, and charges a capacitor (the *accent
 *    sweep*: 47k into 1 uF, 47 ms; draining through 100k and 100k, 200
 *    ms) whose voltage opens the filter further and turns the note up.
 *    It hasn't drained when the next accent comes: a run of accents
 *    climbs, each wah higher than the last -- the circuit's accident,
 *    acid's signature;
 *  - the *slide*: a note sliding into the next keeps its gate open, and
 *    the pitch glides there (60 ms, commonly quoted: ours, to check);
 *  - the volume envelope, a sharp attack and a long fixed decay, closed
 *    at the gate's end (half a step: Sequencer.mli).
 *
 * The notes come from its own sequencer, running in the audio clock
 * inside the voice (Sequencer.mli), or from keys, a monophonic line
 * (the last key held sounds).
 *
 * Its patterns are text, a step a word: a note ("C2", "Eb2", "F#3") or
 * a rest (".", "-"), a note ending in "*" accented, in "~" sliding into
 * the next: "C2 C2~ C3* . Eb2".
 *
 * Worked example (Unit_tb303, at 120 BPM): the filter's envelope 200
 * ms into an accented note 60 dB down, into a normal one at Decay 0.5
 * (632 ms to -60 dB) -19 dB; three accents in a row, the sweep's peaks
 * 0.283, 0.374, 0.402 -- climbing, each starting where the last left
 * the capacitor, less each time as it nears its top; a slide from C2
 * to C3, the pitch 63% of the way 60 ms after; the gate's close (a 3 ms
 * time constant, ours), the note 60 dB down 25 ms later. *)

type patch = {
  tuning : float; (* -1 to 1: an octave down or up *)
  cutoff : float; (* 0 to 1 *)
  resonance : float; (* 0 to 1 *)
  env_mod : float; (* 0 to 1 *)
  decay : float; (* 0 to 1: 200 ms to 2 s *)
  accent : float; (* 0 to 1 *)
  square : bool; (* else the sawtooth *)
  bpm : float;
  volume : float;
  pattern : Sequencer.step array;
}

val initial : patch

(* the knobs' laws *)
val cutoff_hz : float -> float (* 100 x 25^k: 100 Hz to 2.5 kHz *)
val decay_seconds : float -> float (* 0.2 x 10^k *)
val resonance_k : float -> float (* 0 to 0.9 of the ladder's 22.1 *)
val env_octaves : float -> float (* 1 + 4 k: never none *)

(* the patterns as text *)
val pattern_to_string : Sequencer.step array -> string
val pattern_of_string : string -> (Sequencer.step array, string) result

(* the knobs, by name: "tuning", "cutoff", "resonance", "env.mod",
 * "decay", "accent", "waveform" (saw, square), "tempo" (60 to 200),
 * "volume"; the text the knobs' lines and a "pattern = ..." line *)
type knob = patch Patch_text.knob

val knobs : knob list
val to_string : patch -> string
val of_string : string -> (patch, string) result

(* our patterns: acid, bass, accents (three accents in a row, to hear
 * the sweep climb) *)
val presets : (string * patch) list

type t

val create : patch -> t
val patch : t -> patch
val set_patch : t -> patch -> unit

(* the sequencer: running, and its step sounding *)
val run : t -> bool -> unit
val running : t -> bool
val step : t -> int

(* for tests and a panel: the filter's envelope, the accent sweep's
 * voltage, the pitch (a MIDI number) and the cutoff (Hz) now *)
val envelope : t -> float
val sweep : t -> float
val pitch : t -> float
val cutoff_now : t -> float

(* the last 2048 samples it played, for a scope *)
val recent : t -> Signal.t

(* the voice as an instrument (Instrument.mli): keys by MIDI number (the
 * last held sounds), [set] by a knob's name and "run" (1 or 0) *)
val instrument : t -> Instrument.t
