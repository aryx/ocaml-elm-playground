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
 * a laser's or a jump's sweep) or its noise (Noise's LFSR, stepped
 * [frequency] times a second: its "pitch"), times its volume, times its
 * envelope: [fade], a percussive one, attack 5 ms then down to 0 over
 * the rest; otherwise 5 ms ramps in and out, so that even a plain voice
 * never clicks (Envelope.mli). Then Mix.add for together, Mix.then_ for
 * after.
 *
 * References: Paul Hudak, Donovan Quick, The Haskell School of Music:
 * From Signals to Symphonies, Cambridge University Press, 2018,
 * chapter 1 (Music values, (:+:) and (:=:)); Euterpea,
 * https://www.euterpea.com *)

type source = Wave of Oscillator.waveform | Noise

type voice = {
  source : source;
  frequency : float; (* Hz; for noise, the LFSR's steps per second *)
  slide : float option; (* the frequency reached at the end *)
  seconds : float;
  volume : float; (* 0 to 1 *)
  fade : bool; (* percussive: dies away over its duration *)
}

type t =
  | Voice of voice
  | Together of t list
  | After of t list
  (* samples already computed: a MIDI file's rendering (Music.render_score),
   * untouched by the modifiers below *)
  | Samples of Signal.t

(* [voice source frequency]: 0.3 s at volume 0.5, not sliding nor fading *)
val voice : source -> float -> t

(* the modifiers, on every voice of the tree: *)
val lasting : float -> t -> t
val fading : t -> t
val louder : float -> t -> t (* the volume multiplied *)
val sliding : float -> t -> t (* to that frequency *)

(* [duration s]: in seconds (see above) *)
val duration : t -> float

(* [render s]: its samples, [duration s] long *)
val render : t -> Signal.t

(* a continuous voice's state, frame after frame: its oscillator's
 * phase (or its noise's register and clock), its last volume *)
type running

val start : unit -> running

(* [continue running v n]: the next [n] samples of [v] as a steady
 * sound (its duration, slide and fade ignored), its phase going on
 * from the last call, its volume moving smoothly from the last call's
 * to [v]'s (no zipper noise when a theremin's volume changes), and the
 * state for the next call *)
val continue : running -> voice -> int -> Signal.t * running

(* [release running n]: [n] samples of the voice fading from its last
 * volume to 0, when it stops *)
val release : running -> voice -> int -> Signal.t
