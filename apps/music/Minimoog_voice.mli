(* The Minimoog Model D's voice: audio/'s blocks wired as its panel wires
 * them (see notes_synth.md section 2, and plan_synth_teaching.md).
 *
 * The Model D (Moog Music, 1970) fixed the modular synthesizer's usual
 * patch behind a panel read left to right, and this module is that
 * panel's signal path, each box one of audio/'s modules:
 *
 *   CONTROLLERS        OSCILLATORS          MIXER        MODIFIERS           OUTPUT
 *
 *   keys --> Voicing --> pitch ---> osc 1 (Vco) --.
 *   (low note,  glide     |    +--> osc 2 (Vco) --+--> (+) --> Moog_ladder --> x --> volume
 *    legato)              |    '--> osc 3 (Vco) --+     ^    ^ cutoff        ^
 *                         |          |            |     |    |               |
 *   pitch wheel ----------'          |     noise -'     |  filter contour  loudness
 *                                    v       |          |  (Envelope)      contour
 *   mod wheel x modulation mix: (osc 3 <-> noise) ------+--> oscillators' pitch,
 *                                                            filter's cutoff
 *
 *  - the pitch: the lowest key held (low-note priority, the Model D's
 *    keyboard circuit), legato (a new key while one is held changes the
 *    pitch without restarting the contours: "single trigger"), glided
 *    (Voicing); the pitch wheel bends it a fifth up or down;
 *  - three oscillators, each with a range in organ feet (LO, 32', 16',
 *    8', 4', 2': octaves, 8' the key's own pitch) and six waveforms:
 *    triangle, "shark tooth" (a triangle with a quarter of a sawtooth
 *    mixed in: the original's selector switch mixes the two through a
 *    47k and a 10k resistor), sawtooth, square, and a wide and a narrow
 *    rectangle; oscillator 3 has a *reverse* sawtooth instead of the
 *    shark tooth, since it doubles as a modulator. Oscillators 2 and 3
 *    are detuned by their knob, 7 semitones either way; each drifts a
 *    little (Drift);
 *  - oscillator 3 can leave the keyboard: then it plays its range and
 *    knob alone, and in LO it is an LFO;
 *  - the mixer: the three oscillators' levels and the noise's, summed
 *    straight into the filter, which saturates (Moog_ladder's
 *    [Nonlinear]): three oscillators at 10 overdrive it, the Model D's
 *    famous fat sound, and the sum at a third of that goes through
 *    clean;
 *  - the filter: its cutoff knob over 10 octaves, 20 Hz to 20 kHz; the
 *    "emphasis" (resonance), oscillating at its top; the "amount of
 *    contour", up to 4 octaves of the filter contour added to the
 *    cutoff; and the keyboard control switches, the cutoff following the
 *    note by a third (1), two thirds (2), or all of it (both);
 *  - two contours (Envelope): attack (1 ms to 10 s), decay (4 ms to 35
 *    s), sustain; no release knob -- the "decay" switch makes the release
 *    the decay, off it's 4 ms: the note stops when the key comes up;
 *  - the modulation: the mod wheel sends the modulation mix, from all
 *    oscillator 3 to all noise, to the oscillators' pitch (12 semitones
 *    either way at the top: vibrato is a little wheel) and to the
 *    filter's cutoff (3 octaves either way), each by its switch.
 *
 * The knobs' ranges are the Model D's where its documentation gives them
 * (the contours' times, the glide from 1 ms to 10 s, the 4 octaves of
 * contour, the three trackings, 7 semitones of detune, a fifth of pitch
 * wheel); where it doesn't, ours: LO 6 octaves under 8' (C4's key at
 * 4.1 Hz, the knob's 7 semitones either way 2.7 to 6.1 Hz, a vibrato's
 * rates), the wide and narrow rectangles 30% and 10% wide, the
 * modulation's depths above, the tune knob a semitone either way.
 *
 * A patch is the panel's positions: [knobs] lists them with their
 * names, and a patch is written as "name = value" lines (a knob's
 * position from 0 to 1, or -1 to 1 for the tune and the detunes; a
 * switch on or off; a selector's choice by its label), what a Minimoog
 * player wrote on a paper patch chart. The presets are our own
 * settings, written in that text.
 *
 * Knob positions map to values the way the Model D's pots did, most of
 * them exponentially (each equal turn an equal ratio): worked example,
 * the cutoff knob at 0.5 is 20 x 1000^0.5 = 632 Hz; the attack at 0 is
 * 1 ms, at 0.5 100 ms, at 1 10 s.
 *
 * The teaching switches ([options]) are not the Model D's: which ladder
 * (naive, zero-delay, nonlinear), which envelope curve, drift or not,
 * oscillators band-limited or not -- the simple and the better versions
 * of notes_synth.md, heard on the same patch.
 *
 * References: Moog Music, Minimoog Model D owner's manual (the reissue's,
 * 2016, for the panel; its additions, a separate LFO, a choice of note
 * priority, the filter contour as a modulation source, are not here);
 * the Model D service manual (the modulation mix of oscillator 3 and
 * noise; the 3 1/2 octave keyboard). *)

(* {1 The panel} *)

type wave = Triangle | Shark_tooth | Reverse_sawtooth | Sawtooth | Square | Wide | Narrow

(* the six waveforms of oscillators 1 and 2, and of oscillator 3 *)
val waves : wave list
val waves3 : wave list
val wave_name : wave -> string

(* "LO", "32'", "16'", "8'", "4'", "2'" *)
val ranges : string list

type oscillator = {
  range : int; (* an index in [ranges] *)
  wave : int; (* an index in [waves] or [waves3] *)
  frequency : float; (* the detune knob, -1 to 1 (7 semitones); 0 for osc 1 *)
  on : bool;
  level : float; (* 0 to 1 *)
}

type contour = { attack : float; decay : float; sustain : float } (* knobs, 0 to 1 *)

type patch = {
  tune : float; (* -1 to 1: a semitone *)
  glide : float;
  glide_on : bool;
  decay_on : bool;
  modulation_mix : float; (* 0: oscillator 3, 1: noise *)
  oscillator_modulation : bool;
  filter_modulation : bool;
  osc1 : oscillator;
  osc2 : oscillator;
  osc3 : oscillator;
  osc3_keyboard : bool; (* off: an LFO *)
  noise : float;
  noise_on : bool;
  cutoff : float;
  emphasis : float;
  contour_amount : float;
  keyboard_1 : bool;
  keyboard_2 : bool;
  filter_contour : contour;
  loudness_contour : contour;
  volume : float;
}

(* the patch the panel starts with: osc 1 alone, a sawtooth at 8', the
 * filter half open *)
val initial : patch

type control = Knob of float * float (* from, to *) | Switch | Selector of string list

(* a control of the panel: its name, in the text and in [set] *)
type knob = { name : string; control : control; get : patch -> float; put : patch -> float -> patch }

val knobs : knob list

(* [to_string p], [of_string s]: a patch as "name = value" lines ('#'
 * starts a comment; a control not named keeps its [initial] position) *)
val to_string : patch -> string
val of_string : string -> (patch, string) result

(* our presets: bass, lead, brass, flute, whistle (the filter alone,
 * oscillating, played by the keyboard), wind *)
val presets : (string * patch) list

(* {1 The knobs' laws} *)

val cutoff_hz : float -> float (* 20 x 1000^k *)
val attack_seconds : float -> float (* 0.001 x 10000^k *)
val decay_seconds : float -> float (* 0.004 x 8750^k *)
val glide_seconds : float -> float (* 0.001 x 10000^k *)
val emphasis_k : float -> float (* 4.5 k: oscillating from 0.89 *)
val range_octaves : int -> float (* LO -6, 32' -2 ... 2' +2 *)

(* the tracking, from the two keyboard control switches: 0, 1/3, 2/3, 1 *)
val tracking : patch -> float

(* {1 Playing it} *)

type options = { ladder : Moog_ladder.model; curve : Envelope.curve; drift : bool; band_limited : bool }

(* nonlinear, exponential, drifting, band-limited: the Model D as close
 * as we get *)
val analog : options

type t

val create : ?options:options -> patch -> t
val patch : t -> patch

(* [set_patch v p]: the panel now [p], its knobs glided to over the next
 * block *)
val set_patch : t -> patch -> unit

val options : t -> options
val set_options : t -> options -> unit

(* the voice as an instrument (Instrument.mli): keys by MIDI numbers
 * (velocity ignored: the Model D has none), [set] by a knob's name, and
 * the two wheels: "pitch_wheel" (-1 to 1: a fifth), "mod_wheel" (0 to
 * 1) *)
val instrument : t -> Instrument.t

(* the pitch now, in semitones, and the cutoff last computed, in Hz (for
 * tests and a display) *)
val pitch : t -> float
val cutoff_now : t -> float

(* the last 2048 samples it played, oldest first: for a panel's
 * oscilloscope and spectrum, on any backend *)
val recent : t -> Signal.t
