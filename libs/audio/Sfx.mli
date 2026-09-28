(* Game sounds from a few numbers: sfxr's idea, readable (see
 * notes_audio.md section 8).
 *
 * Tomas Pettersson ("DrPetter") wrote sfxr in 2007 for a 48-hour game
 * jam: nobody there had time to record or find sounds, and most 8-bit
 * game sounds turn out to be the same recipe with different numbers --
 * one oscillator, an envelope, a slide, maybe a vibrato, a jump, a
 * filter. His window was a column of sliders and buttons named after
 * what games need ("pickup/coin", "laser/shoot", "explosion",
 * "powerup", "hit/hurt", "jump", "blip/select"), each a preset or a
 * random pick in that category's ranges; bfxr, jsfxr, ChipTone and
 * PICO-8's sound editor all descend from it.
 *
 * A sound here is the record [t], each field one of those sliders, and
 * [to_sound] turns it into a Synth tree:
 *
 *     wave, frequency, slide     the voice, its pitch moving (Synth)
 *     attack, sustain, decay     its envelope: up, held, down
 *                                (Envelope.mli; an attack of at least
 *                                5 ms, the no-click ramp)
 *     vibrato, jump              pitch effects (Pitch_effect.mli)
 *     low_pass, high_pass        filters, the low-pass's cutoff moving
 *                                (Filter.mli): subtractive synthesis
 *     echo, reverb               a delay line, a room (Synth.mli)
 *
 *       level
 *         1 |   ________
 *           |  /        \          attack 0.01, sustain 0.1, decay 0.3:
 *           | /          \___      a sound of 0.41 s
 *         0 +---------------------> t
 *            a    s       d
 *
 * Example, the explosion: noise from 1500 steps a second sliding to 150
 * (rough to dull), 0.1 s held then 0.6 s dying away, through a low-pass
 * falling from 4000 Hz to 150 Hz: the burst, then the rumble; its eight
 * numbers are all of it.
 *
 * What these numbers buy over plain recipes (the ready-made sounds'
 * first generation), measured sound by sound: notes_audio.md section 8,
 * "The ready-made sounds, three generations".
 *
 * [vary] is sfxr's "mutate" button: every number nudged at random (a
 * seed, so the same seed gives the same sound): a family of lasers from
 * one, so that ten shots in a row don't sound like a machine.
 *
 * References: Tomas Pettersson, sfxr, 2007,
 * https://www.drpetter.se/project_sfxr.html; Stephen Lavelle (increpare),
 * bfxr, 2011. *)

(*****************************************************************************)
(* {1 A sound as a few numbers} *)
(*****************************************************************************)

type wave = Square | Sawtooth | Triangle | Sine | Noise

type t = {
  wave : wave;
  frequency : float; (* Hz at the start; noise: its steps a second *)
  slide : float; (* Hz at the end; the same: no slide *)
  attack : float; (* seconds *)
  sustain : float;
  decay : float;
  vibrato_rate : float; (* Hz *)
  vibrato_depth : float; (* semitones; 0: no vibrato *)
  jump : float; (* semitones; 0: no jump *)
  jump_at : float; (* seconds *)
  low_pass : float; (* cutoff, Hz; 0: no low-pass *)
  low_pass_to : float; (* the cutoff at the end; 0: fixed *)
  resonance : float; (* the low-pass's Q, 0.707: none *)
  high_pass : float; (* cutoff, Hz; 0: no high-pass *)
  echo : float; (* the delay, seconds; 0: no echo *)
  reverb : float; (* the room's reverberation time, seconds; 0: none *)
  volume : float; (* 0 to 1 *)
}

(* a plain beep: a square at 440 Hz, 0.1 s held, 0.1 s decay *)
val default : t

(* [to_sound s]: as a Synth tree; [duration s] long, with its echo *)
val to_sound : t -> Synth.t

(* [duration s]: attack + sustain + decay (+ the echo's tail) *)
val duration : t -> float

(*****************************************************************************)
(* {1 Presets} *)
(*****************************************************************************)

(* the presets, after sfxr's categories: our own numbers *)
val blip : t (* a menu, a ball on a paddle *)
val coin : t (* a pickup: a note, then a fifth up *)
val jump : t (* a quick rising square *)
val laser : t (* a sawtooth falling, darker as it falls *)
val hit : t (* a short burst of noise *)
val explosion : t (* noise, a burst then a rumble *)
val step : t (* a soft low tick: footsteps *)
val powerup : t (* a rising square, warbling *)

(* all of them, by name *)
val presets : (string * t) list

(*****************************************************************************)
(* {1 At random} *)
(*****************************************************************************)

(* [vary ~seed s]: every number of [s] nudged at random, the wave kept:
 * durations and cutoffs times 0.7 to 1.4, pitches up to 5 semitones up
 * or down; [seed] 0: [s] itself *)
val vary : seed:int -> t -> t

(* [random category ~seed]: sfxr's buttons, "random laser", "random
 * explosion": every number drawn afresh, within that category's
 * ranges, which are what makes it that kind of sound -- a laser always
 * slides down, a jump up, an explosion is noise getting duller, a coin
 * jumps up an interval, a powerup rises and warbles, a hit is short
 * and falls, a blip holds a note; the ranges are ours, after sfxr's.
 * [category] one of [presets]'s names but "step" (a blip for any other
 * name); the same seed, the same sound *)
val random : string -> seed:int -> t
