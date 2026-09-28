(* The OP-1's synth engines: six ways of making a sound, each behind
 * the same four coloured encoders (see notes_synth.md;
 * plan_synth_teaching.md, TinyOp1, O2).
 *
 * The OP-1 (Teenage Engineering, 2011) has one idea about control: four
 * encoders, blue, green, white and orange, and whatever is on the
 * screen, its four parameters drawn in those colours. An engine -- the
 * way a sound is made -- is then four numbers, whatever its method:
 *
 *     engine   blue          green          white          orange
 *     FM       FM amount     freq           topology       detune
 *     cluster  waves (0-6)   wave envelope  spread         unitor
 *     string   tension       impulse decay  detune         impulse type
 *     pulse    filter        amplitude      second pulse   mod
 *     phase    phase shift   distortion     phase filter   phase tilt
 *     digital  wave shaper   octave         detune & ring  digitalness
 *     dr wave  type & length filter         phase          chorus
 *     voltage  env crossfader waveform      envelope       cross mod
 *     d-synth  pitch         waveform       envelope       cross mod
 *     sampler  start         loop in        loop out       end
 *
 * (the manual's words, its reference chapter; its synthesizer chapter
 * says d-synth "dual oscillator" and voltage "multi oscillator electric
 * synthesis", the reference chapter d-synth a "teenage drum
 * synthesizer" and voltage a "multi envelope dual oscillator synth":
 * ours follow the reference, whose four encoders fit). Ten methods, a
 * lesson each, most of them already this repository's:
 *
 *  - *FM*, four operators (the OP-1's, not the DX7's six): four of
 *    Fm_algorithm.mli's six, the DX7's algorithms giving the four
 *    topologies -- a stack of four, two pairs, three onto one, four
 *    carriers (an organ);
 *  - *cluster*, up to six sawtooths detuned around the note: the
 *    supersaw (Roland's JP-8000, 1996), a chorus made of oscillators;
 *  - *string*, a waveguide: a delay line a period long, fed an impulse,
 *    its loop a low-pass losing a little each time round -- Karplus and
 *    Strong's plucked string (Pluck.mli renders one ahead; this one runs
 *    live, two strings detuned against each other);
 *  - *pulse*, two pulse trains, their widths moved, through a
 *    low-pass: the square waves of the chip music the OP-1 loves;
 *  - *phase*, Casio's phase distortion (the CZ-101, 1984): a cosine
 *    read through a bent phase, fast then slow, the bend the distortion
 *    -- no filter, the harmonics made by the reading alone;
 *  - *digital*, a sine folded by a wave shaper, ring modulated, then
 *    its samples held and its bits dropped: digital's own grit;
 *  - *dr wave*, "frequency domain synthesis": a period made as the sum
 *    of its harmonics (a sawtooth's, a square's, a triangle's, a
 *    formant's), as many as its length, a brick-wall filter removing
 *    those above it -- no phase shift, no ringing, what only the
 *    frequency domain allows -- and a phase moving each harmonic by a
 *    different amount: the waveform's shape changed, the sound not, the
 *    ear deaf to phase (Ohm's acoustic law, 1843);
 *  - *voltage*, two oscillators and two envelopes, one crossfading
 *    from the first to the second, one decaying the second's cross
 *    modulation of the first: a sound that changes its waveform as it
 *    lasts;
 *  - *d-synth*, a drum: a sine falling from a higher pitch (the punch,
 *    TinyTR808's kick's idea), mixed with noise, decaying, a second
 *    sine for metal;
 *  - the *sampler*, Sampler.mli's voice over a recording (a string
 *    plucked at C4, until Studio_op1 takes one from the tape), its
 *    start, loop and end on the encoders.
 *
 * What the manual names without saying more is ours, and said at each
 * engine in Op1_engine.ml: the ranges, the FM ratios, the cluster's
 * "wave envelope" and "unitor", the string's impulses, the pulse's
 * "second pulse" and "mod", phase's tilt and filter.
 *
 * Worked example (Unit_op1): FM at amount 0 a sine (its harmonics 2 to
 * 5 under -120 dB), at 0.6 in the stack 15.1 dB above the fundamental
 * (three modulators in a chain); the cluster's six waves on 4 bins of
 * the spectrum at spread 0, 10 at spread 1; the string plucked at 220
 * Hz by its autocorrelation (its zero crossings say 1572: they count
 * its bright harmonics, not its period); phase distortion's harmonics
 * 2 to 5 under -100 dB at amount 0 (a cosine), -11.7 at 0.5, -5.9 at 1;
 * digital's samples 2029 distinct values at 16 bits, 5 at 2 bits; a
 * golden WAV per engine. *)

type t = {
  name : string;
  kind : string; (* the manual's "type:" *)
  encoders : string array; (* blue, green, white, orange *)
  (* [start params ~frequency ~velocity]: a note, [params] the four
   * encoders' values (0 to 1); its next block, written over *)
  start : float array -> frequency:float -> velocity:float -> Signal.t -> unit;
}

val fm : t
val cluster : t
val string : t
val pulse : t
val phase : t
val digital : t
val dr_wave : t
val voltage : t
val d_synth : t
val sampler : t

(* the ten, the first six in the OP-1's browser's order *)
val all : t list

(* dr wave's period (2048 samples) for its encoders at a frequency *)
val dr_wave_table : float array -> float -> Signal.t

(* the sampler's four encoders as the start, loop in, loop out and end,
 * fractions of the recording, in order *)
val sampler_points : float array -> float array

(* the sampler's recording: set, and the one playing *)
val set_sample : Sampler.sample -> unit
val sample : unit -> Sampler.sample

(* FM's topologies, the white encoder's four positions *)
val topologies : string list

(* [phase_distortion ~amount p]: the bent phase, [p] 0 to 1: straight at
 * [amount] 0, the knee sliding towards the start as it grows *)
val phase_distortion : amount:float -> float -> float
