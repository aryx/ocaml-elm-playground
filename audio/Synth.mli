(* Sounds as values: what playground/Audio's sounds are underneath, and
 * how they become samples (see notes_audio.md sections 3-5, 8).
 *
 * A sound is a tree, the way a picture is a tree of shapes: at the
 * leaves, voices (an oscillator or noise, at a frequency, for a
 * duration, at a volume, maybe sliding to another frequency, maybe
 * fading out); above them, sounds played together or one after the
 * other. The two combinators are those of Paul Hudak's Euterpea (the
 * library of The Haskell School of Music), whose music values are built
 * with (:=:), "together", and (:+:), "one after the other" -- here on
 * sounds rather than notes:
 *
 *     after [ voice A4 0.1; voice E5 0.2 ]        A4 then E5
 *     together [ voice C4 1; voice E4 1; voice G4 1 ]    a C major chord
 *
 *         together                  a sound's duration: a voice's own;
 *        /    |    \                together, the longest; after,
 *      C4    E4    G4               the sum
 *
 * Rendering: each voice its oscillator (Oscillator's phase accumulator,
 * the frequency moving from [frequency] to [slide] over the voice, for
 * a laser's or a jump's sweep; band-limited, unless [Naive]), its FM
 * pair (Fm.mli: a fading voice's index following its envelope, bright
 * when struck, darker as it dies, Chowning's way) or its noise (Noise's
 * LFSR, stepped [frequency] times a second: its "pitch"), times its
 * volume, times its
 * envelope: [fade], a percussive one, attack 5 ms then down to 0 over
 * the rest; otherwise 5 ms ramps in and out, so that even a plain voice
 * never clicks (Envelope.mli). Then Mix.add for together, Mix.then_ for
 * after, and a filter (Filter.mli) over a sound's samples for
 * [Filtered]: subtractive synthesis, a rich sound with some taken away.
 *
 * References: Paul Hudak, Donovan Quick, The Haskell School of Music:
 * From Signals to Symphonies, Cambridge University Press, 2018,
 * chapter 1 (Music values, (:+:) and (:=:)); Euterpea,
 * https://www.euterpea.com *)

type source =
  | Wave of Oscillator.waveform (* band-limited (Oscillator.mli) *)
  | Naive of Oscillator.waveform (* the formula of the phase, aliasing *)
  | Fm of { ratio : float; index : float } (* the modulator's ratio *)
  | Noise
  (* a plucked string (Pluck.mli): its slide and effects ignored; kept
   * playing, a triangle (a string can't be held) *)
  | Pluck

type voice = {
  source : source;
  frequency : float; (* Hz; for noise, the LFSR's steps per second *)
  slide : float option; (* the frequency reached at the end *)
  seconds : float;
  volume : float; (* 0 to 1 *)
  fade : bool; (* percussive: dies away over its duration *)
  effects : Effect.pitch list; (* vibrato, jumps, arpeggios, multiplied *)
  (* an ADSR instead of the 5 ms ramps (or [fade]), released at
   * [seconds] less its release: sfxr's attack, sustain, decay *)
  envelope : Envelope.t option;
}

type t =
  | Voice of voice
  | Together of t list
  | After of t list
  (* samples already computed: a MIDI file's rendering (Music.render_score),
   * untouched by the modifiers below *)
  | Samples of Signal.t
  (* the sound through a biquad (Filter.mli), its cutoff moving from
   * [cutoff] to [cutoff_to] over it (the same: fixed) *)
  | Filtered of filter * t
  (* the sound echoed (Effect.echo), [Effect.tail] longer *)
  | Echo of echo * t
  (* the sound in a room (Effect.reverb), its reverberation time in
   * seconds, the sound that much longer *)
  | Reverb of float * t
  (* the sound panned, -1 left to 1 right (Space.pan): only
   * [render_stereo] hears it, [render] mixes it down to one channel *)
  | Panned of float * t

and echo = { delay : float; feedback : float }

and filter = { kind : Filter.kind; cutoff : float; cutoff_to : float; q : float }

(* false: every Wave played as Naive, the band-limited oscillators off,
 * for the backends' debug key (the software backend's "l"): the sounds
 * rendered from then on (a loop already rendered keeps its own) and the
 * continuous voices at once; true by default *)
val band_limited : bool ref

(* [voice source frequency]: 0.3 s at volume 0.5, not sliding nor fading *)
val voice : source -> float -> t

(* the modifiers, on every voice of the tree: *)
val lasting : float -> t -> t
val fading : t -> t
val louder : float -> t -> t (* the volume multiplied *)
val sliding : float -> t -> t (* to that frequency *)
val naive : t -> t (* Wave to Naive *)
val with_effect : Effect.pitch -> t -> t (* one more *)

(* [faster k s]: [s] played [k] times as fast, every duration divided by
 * [k] (the voices', their effects' times, an echo's delay), the pitches
 * kept: a tune's tempo (Samples, already computed, untouched) *)
val faster : float -> t -> t

(* [pitched k s]: every frequency of [s] times [k] (slides too), the
 * durations kept -- but a recording, Samples, has no frequency: read
 * [k] times as fast (Resample, with !Resample.kind), it is higher and
 * shorter, pitch and time together. With [faster k] (which leaves
 * Samples alone), a Doppler shift, a sound squeezed or stretched as a
 * whole *)
val pitched : float -> t -> t

(* [duration s]: in seconds (see above) *)
val duration : t -> float

(* [render s]: its samples, [duration s] long, in one channel (pans
 * ignored) *)
val render : t -> Signal.t

(* [render_stereo s]: the same in two channels, each [Panned] subtree's
 * gains (Space.pan) applied, and its far ear delayed
 * (Space.interaural_delay, both channels then that much longer), pans
 * nested multiplied; a tree with no pan is [render] in both (the same
 * array, not copied) *)
val render_stereo : t -> Signal.stereo

(* a continuous voice's state, frame after frame: its oscillator's
 * phase (or its noise's register and clock), its last volume, its
 * time (for its pitch effects: a vibrato goes on from frame to frame) *)
type running

val start : unit -> running

(* [continue running v n]: the next [n] samples of [v] as a steady
 * sound (its duration, slide, fade and envelope ignored), its phase going on
 * from the last call, its volume moving smoothly from the last call's
 * to [v]'s (no zipper noise when a theremin's volume changes), and the
 * state for the next call *)
val continue : running -> voice -> int -> Signal.t * running

(* [release running n]: [n] samples of the voice fading from its last
 * volume to 0, when it stops *)
val release : running -> voice -> int -> Signal.t
