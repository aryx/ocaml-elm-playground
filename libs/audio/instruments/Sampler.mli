(* The sampler: a recording played as an instrument (see notes_synth.md;
 * plan_synth_teaching.md, TinyOpxy, X2).
 *
 * A synthesizer computes its sound; a sampler replays one, recorded --
 * the Fairlight CMI (1979), the Emulator (1981), the Akai MPC (1988),
 * and every phone's piano since. What it adds to a recording is the
 * little that makes it playable: a key plays it at a pitch (read faster
 * or slower, Resample.mli: pitch and length together, as on tape), a
 * *region* of it (a start and an end, so a breath or a silence is left
 * out), forwards or backwards, and a *loop* (a stretch read again and
 * again) so that a short recording holds as long as the key:
 *
 *     the recording  |~~~~~/\/\/\/\/\/\/\/\/\/\/\/~~~~|
 *                    start   loop start   loop end   end
 *     played         |~~~~~/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/~~~~|
 *                          the loop, while the key is held   then on
 *                                                            to the end
 *
 * The OP-XY's two samplers (Teenage Engineering's guide, "sample"), each
 * the same few settings:
 *
 *  - the *one shot synth sampler*, one recording across the keyboard:
 *    start, loop start, loop end, end, the direction, the tune, the
 *    loop's crossfade, the gain, and the loop "forever", "until
 *    release" or "off";
 *  - the *drum sampler*: 24 recordings, one per key, each with its play
 *    mode -- "key (play while held)", "oneshot (play whole sample)",
 *    "mute group (choke when another sample plays)", "loop" -- its
 *    tune, region, direction, pan and gain. The mute group is the
 *    hi-hat's: the closed one cuts the open one off, as on a real kit
 *    (and the TR-808's, Voice_tr808.mli).
 *
 * A loop has a seam: where it jumps back, the wave at the loop's end
 * rarely meets the one at its start, and the jump is a click each time
 * round. The *crossfade* hides it: over the loop's last stretch, the
 * sound is mixed more and more with what comes before the loop's start,
 * so that at the jump both are the same sample:
 *
 *     y(u) = (1 - a) s(u) + a s(u - loop length),  a from 0 to 1 over
 *                                                  the crossfade
 *
 * (linear, ours). It needs as much sound before the loop's start as it
 * is long, and is shortened to fit. What it can't hide: where the two
 * are in opposite phase, the mix goes through silence -- the click
 * traded for a dip (an equal-power curve keeps the loudness of two
 * sounds that don't correlate, not of two that cancel). The older
 * answer, from the days of the Fairlight: the loop points put on zero
 * crossings, where the wave meets itself -- which a sine allows, and a
 * rich sound rarely does.
 *
 * The recording is at Signal.rate (Resample.to_rate first, if not). The
 * voices read it with Resample's cubic, no low-pass first: a note far
 * above its root folds its highs (Resample.mli's exercise).
 *
 * Worked example (Unit_sampler): a 440 Hz sine recorded, its root A4,
 * played at A5 is 880 Hz (439 rising crossings in its half second) and
 * lasts half as long (0.5 s of 1); played backwards, a rising ramp
 * falls; a loop from a crest, 100.5 periods long, ends in a trough: it
 * jumps by 2 at each seam (the sine's whole swing), by 0.06 with a
 * crossfade of 0.2 -- the click gone, a step as small as the sine's own
 * (0.0627) -- but its quietest period then nearly silence (0.03, against
 * 0.71 without), the two halves cancelling; from 0.25 s itself, the seam on
 * zero crossings, no click and no crossfade; "until release" plays on
 * after the key, "forever" loops until its fade, silent 10 ms later;
 * the closed hat chokes the open one within a block; a pad panned left
 * in the left side only. *)

(*****************************************************************************)
(* {1 A recording, played} *)
(*****************************************************************************)

(* a recording and the key at which it sounds as recorded *)
type sample = { data : Signal.t; root : int }

type direction = Forward | Backward
type loop = Forever | Until_release | Off

(* the region and the loop as fractions of the recording (0 to 1),
 * [tune] in semitones, [crossfade] a fraction of the loop, [release]
 * the fade after the key is let go (None: let go, it plays on) *)
type settings = {
  start : float;
  loop_start : float;
  loop_end : float;
  end_ : float;
  direction : direction;
  tune : float;
  crossfade : float;
  gain : float;
  loop : loop;
  release : float option;
}

(* the whole recording, forwards, no loop, a 10 ms fade after the key *)
val default : settings

(* [voice sample settings ~key ~velocity]: a note; silent once past the
 * region's end, or faded out *)
val voice : sample -> settings -> key:int -> velocity:float -> Polyphony.voice

(* [seconds sample settings key]: how long [key] plays the region, once
 * through without its loop *)
val seconds : sample -> settings -> int -> float

(*****************************************************************************)
(* {1 The drum sampler} *)
(*****************************************************************************)

type play = Key | Oneshot | Mute_group | Loop

(* a pad: its recording (played at its root, [tune] moving it), its
 * settings' region, direction, tune and gain, its play mode, its pan
 * (-1 left to 1 right) *)
type pad = { sample : sample; settings : settings; play : play; pan : float }

val pad : ?settings:settings -> ?play:play -> ?pan:float -> sample -> pad

type kit

(* [kit ?base pads]: up to 24 pads, the first on key [base] (36, C2,
 * General MIDI's kick: ours; the OP-XY's keyboard starts where its
 * octave is) *)
val kit : ?base:int -> pad array -> kit

(* a key down: its pad sounding (a pad in the mute group choking the
 * others in it); a key up: a Key pad's fade, a Loop pad's *)
val press : kit -> int -> float -> unit
val release : kit -> int -> unit

(* [fill kit out]: the pads sounding, each at its pan, written over
 * [out] *)
val fill : kit -> Signal.stereo -> unit

(* the pads sounding *)
val sounding : kit -> int
