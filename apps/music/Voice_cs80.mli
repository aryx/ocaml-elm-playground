(* The Yamaha CS-80's voice: two synthesizers per key, the touch and
 * the ribbon (see notes_synth.md; plan_synth_teaching.md, TinyCS80,
 * C1).
 *
 * The CS-80 (1977) weighs 100 kg and has 8 voices, each of them two
 * complete synthesizers, the panel's sections I and II, played
 * together: a VCO (a sawtooth, a pulse 50 to 90% wide with its own
 * width modulation, noise), a high-pass then a low-pass filter (12
 * dB/octave each, state-variable: Svf.mli), a filter envelope, a VCA
 * with its own ADSR mixing the filtered sound with a pure sine:
 *
 *     key --+--> I:  VCO -> HPF -> LPF -> VCA --+
 *           |              ^       ^      ^     +--> mix -> ring -> chorus
 *           +--> II: VCO -> HPF -> LPF -> VCA --+
 *                  (detuned)  filter env  ADSR
 *                        touch: velocity, pressure -> brilliance, level
 *
 * Two layers make one sound thicker than either (section II detuned
 * half a semitone at most: the beating), or two sounds at once (a
 * brass on I, strings on II), each with its own envelopes -- the brass
 * swelling while the strings are already there.
 *
 * Its lesson is the *touch*. Each key knows how fast it went down (the
 * velocity, "initial" touch) and how hard it is pressed while held (the
 * pressure, "after" touch) -- and the pressure is each key's own:
 * *polyphonic* aftertouch, rare to this day. Each section routes them
 * to its brilliance (the filters' cutoff) and its level. Pressing one
 * note of a chord harder opens that note alone: Vangelis's brass in
 * Blade Runner, each note swelling as his fingers push. And the
 * *ribbon*, a strip above the keys bending the held notes from wherever
 * it is first touched: slides, sirens, the pitch as a string's.
 *
 * The filter envelope's two levels are the CS-80's: an Initial Level,
 * where the cutoff starts, and an Attack Level, where the attack takes
 * it; then the decay to the cutoff set, held, and the release back to
 * the initial level (ours, from the panel tour; the manual's curves not
 * copied). The resonance can't make the filters oscillate: Yamaha
 * limited it, so the touch could never overdrive them.
 *
 * The sub-oscillator is an LFO (sine, saw, ramp, pulse, noise) into
 * the VCO (vibrato), the VCF (wah) and the VCA (tremolo); the ring
 * modulator an LFO multiplying the sound, its depth rising and falling
 * with its own attack and decay (bells, a robot's voice). A chorus
 * (Modulated_delay's, the CS-80's a bucket-brigade line) and a tremolo
 * after the mix.
 *
 * Ours, and said so: the touch's ranges (3 octaves of brilliance), the
 * knobs' curves (times 2 ms to 10 s, cutoffs 20 Hz to 20 kHz), the
 * sub-oscillator's and ring's ranges.
 *
 * Facts from Old Crow's panel tour (cs80.com) and the owner's manual.
 *
 * Worked example (Unit_cs80): the filter envelope from IL -0.5 to AL
 * 0.5 in 0.1 s, 0.25 halfway down its 0.2 s decay, back to IL 1 s
 * after the release; C E G held, E pressed at full: E's 5th harmonic
 * 31.0 dB up, C's and G's within a dB -- the pressure each key's own;
 * the ribbon bending a held A4 to B4; the resonance at its most, the
 * note let go: silent, no self-oscillation; the presets on a phrase
 * (a chord, its middle note pressed, the ribbon on a line) peaking from
 * 0.34 to 0.67, a golden WAV each.
 *
 * The cost: 8 voices at once, 0.17 s of CPU a second of sound natively,
 * 0.35 s in JavaScript, the filters' envelope and the pulse's width
 * computed once a chunk of 64 samples (0.33 and 0.84 s every sample). *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

(* a section, its knobs 0 to 1 unless said *)
type layer = {
  feet : int; (* an index in [feet]: 16', 8', 4' *)
  saw : float; (* its level *)
  pulse : float; (* its level *)
  width : float; (* 0: 50%, 1: 90% *)
  pwm : float; (* the width's modulation, its depth *)
  pwm_speed : float; (* 0.1 to 10 Hz *)
  noise : float;
  hpf : float; (* 20 Hz to 20 kHz *)
  hpf_res : float;
  lpf : float;
  lpf_res : float;
  il : float; (* -1 to 1: the filter envelope's initial level, 4 octaves each way *)
  al : float; (* -1 to 1: its attack level *)
  f_attack : float; (* 2 ms to 10 s *)
  f_decay : float;
  f_release : float;
  attack : float; (* the VCA's *)
  decay : float;
  sustain : float;
  release : float;
  sine : float; (* the pure sine mixed after the filters *)
  level : float;
  initial_brilliance : float; (* the velocity into the cutoff *)
  initial_level : float; (* into the level *)
  after_brilliance : float; (* the pressure into the cutoff *)
  after_level : float;
}

type patch = {
  layers : layer array; (* I and II *)
  mix : float; (* 0: I alone, 0.5: both, 1: II alone *)
  detune : float; (* -1 to 1: section II, half a semitone each way *)
  sub_wave : int; (* an index in [sub_waves] *)
  sub_speed : float; (* 0.1 to 20 Hz *)
  sub_vco : float; (* up to a semitone *)
  sub_vcf : float; (* up to 2 octaves *)
  sub_vca : float;
  ring_speed : float; (* 1 Hz to 1 kHz *)
  ring_depth : float;
  ring_attack : float;
  ring_decay : float; (* 1: held *)
  chorus : bool;
  tremolo : bool;
  volume : float;
}

val feet : string list
val sub_waves : string list

(* a section's defaults: a sawtooth at 8', the filter half open *)
val layer0 : layer

(* both sections [layer0], section II a little detuned *)
val initial : patch

type knob = patch Patch_text.knob

val knobs : knob list
val to_string : patch -> string
val of_string : string -> (patch, string) result

(* ours: brass (Blade Runner's), strings, pad, ring bells, lead *)
val presets : (string * patch) list

(* the knobs' curves *)
val seconds : float -> float (* 0.002 x 5000^k *)
val cutoff_hz : float -> float (* 20 x 1000^k *)

(* [filter_envelope layer ~held t]: the filter envelope's level (-1 to
 * 1) [t] seconds after the key, let go at [held] *)
val filter_envelope : layer -> held:float -> float -> float

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t

(* 8 voices, the CS-80's *)
val create : patch -> t
val patch : t -> patch
val set_patch : t -> patch -> unit
val voices : t -> int
val recent : t -> Signal.t

(* [pressure t key p]: the pressure on a held key, 0 to 1: that note's
 * brilliance and level, and no other's *)
val pressure : t -> int -> float -> unit

(* [bend t semitones]: the ribbon, every held note bent *)
val bend : t -> float -> unit

val instrument : t -> Instrument.t
