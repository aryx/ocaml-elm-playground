(* Dynamics: the compressor, the limiter and the noise gate, one machine
 * with three settings (see notes_synth.md section 8.7).
 *
 * The other effects change a sound's spectrum or its time; these
 * change its *loudness*, from its loudness: a knob turned by the sound
 * itself, faster than a hand could. Giannoulis, Massberg and Reiss's
 * design (2012), in three parts, all in decibels:
 *
 *     x --+-----------------------------------------(x gain)--> y
 *         |                                             ^
 *         '--> level (dB) --> gain computer --> smoothing --> + makeup
 *              |x|, or its RMS    the static curve   attack and release
 *
 * {1 The gain computer: the static curve}
 *
 * What the output level would be, held steady at an input level x, all
 * in dB, T the threshold, R the ratio, W the knee's width:
 *
 *     y = x                                 below T - W/2 (untouched)
 *     y = T + (x - T) / R                   above T + W/2
 *     y = x + (1/R - 1) (x - T + W/2)^2 / 2W    between: the soft knee
 *
 *     out (dB)                     ratio 4:1: above the threshold, 4 dB
 *        |            ____ ...     in give 1 dB out
 *        |       ____/
 *     T  +------/
 *        |    /                    Worked example: T = -20 dB, R = 4,
 *        |  /                      an input at -8 dB (12 over) comes out
 *        |/                        at -20 + 12 / 4 = -17 dB: 9 dB of
 *        +------+--------> in (dB) gain reduction.
 *               T
 *
 * The *limiter* is the ratio infinite (y = T above it: a ceiling); the
 * *noise gate* the curve turned the other way, below the threshold:
 * y = T + (x - T) R (an expander; R = 10 is practically silence), for
 * the hiss between the notes.
 *
 * {1 The smoothing: attack and release}
 *
 * The curve says how much to turn down, the gain reduction x - y; it
 * would be heard as distortion if applied at once (a sine's peaks
 * turned down, not its whole). So the reduction follows the curve's
 * through a one-pole, fast when it grows (the attack, a few ms: a
 * loud note caught quickly) and slow when it falls (the release, 50 to
 * 500 ms: the gain coming back smoothly). A compressor with a slow
 * attack lets a drum's first milliseconds through before clamping: the
 * "punch".
 *
 * The *limiter*, whose job is that nothing passes the ceiling, can't
 * wait even an attack: it looks ahead, the sound delayed by [lookahead]
 * while the detector hears the loudest sample of that window -- the
 * samples not out yet, the next one out among them -- and turns down
 * at once. Every sample's gain was then set knowing it: nothing passes.
 * (With a short attack in place of the window, a 0.5 ms one, a 441 Hz
 * sine's peaks came through at -5.24 dB against a -6 dB ceiling: a
 * one-pole can't follow a peak a third of a millisecond wide.)
 *
 * {1 The side-chain}
 *
 * The detector can listen to another sound, the *key*, than the one it
 * turns down: a kick drum's key on a bass, the bass ducked at every kick
 * and swelling back in the release -- "pumping", the sound of 2000s
 * dance music, and the radio's voice-over ducking its music.
 *
 * Worked examples (Unit_dynamics):
 *
 *  - the static curve: -8 dB in, -17 out (T -20, R 4, no knee); with a
 *    6 dB knee, at the threshold, -20.56;
 *  - the attack: a -40 dB square wave (its level the same at every
 *    sample, so the detector's own ripple is out of the way) jumping to
 *    -8 dB, the reduction reaching 63% of its 9 dB in 4.99 ms (set to
 *    5), and back down to 37% in 99.96 ms (set to 100) when it drops
 *    again;
 *  - the limiter at -6 dB (0.501), 5 ms of look-ahead: a 441 Hz burst
 *    at 0 dB comes out at 0.501, never over;
 *  - the gate at -40 dB, 10:1: a -50 dB hum comes out at -140 dB, 90 dB
 *    under (10 dB below the threshold is 100 below it out);
 *  - the side-chain: a 55 Hz bass at -12 dB, keyed by bursts of a 60
 *    Hz sine at 0 dB (T -20, R 4, a peak detector), comes out at -26
 *    dB during them and back at -12.3 between: ducked 13.7 dB.
 *
 * References: Dimitrios Giannoulis, Michael Massberg, Joshua D. Reiss,
 * "Digital Dynamic Range Compressor Design -- A Tutorial and Analysis",
 * JAES 60(6), 2012; Will Pirkle, Designing Audio Effect Plugins in C++,
 * 2nd ed. 2019, chapter 18. Mix.limit, the tanh at the end of the
 * mixer, is a *soft clip*, not a limiter: it bends every sample over
 * its knee at once (distortion), where this turns the whole sound down
 * (no new harmonics). *)

type mode = Compressor | Limiter | Gate

val modes : mode list
val name : mode -> string

type detector = Peak | Rms

type settings = {
  mode : mode;
  threshold : float; (* dB *)
  ratio : float; (* 1 to 20: 4 means 4:1; ignored by the limiter *)
  knee : float; (* dB, the soft knee's width *)
  attack : float; (* seconds *)
  release : float; (* seconds *)
  makeup : float; (* dB, the gain after *)
  detector : detector;
  lookahead : float; (* seconds; the limiter's *)
}

(* a compressor: -20 dB, 4:1, 6 dB knee, 5 ms, 100 ms, no makeup, RMS *)
val compressor : settings

(* a limiter: -1 dB, 0.5 ms, 50 ms, peak, 5 ms of look-ahead *)
val limiter : settings

(* a gate: -40 dB, 10:1 below, 1 ms, 100 ms, peak *)
val gate : settings

(* [curve settings x]: the static curve, x and the result in dB *)
val curve : settings -> float -> float

type t

val create : unit -> t

(* [process ?key t settings s]: [s] in place, both channels; the
 * detector hears [key] if given (the side-chain; as long as [s]),
 * else [s] itself, its two sides linked (the louder) *)
val process : ?key:Signal.stereo -> t -> settings -> Signal.stereo -> unit

(* the gain reduction at the last block's end, dB (0 or more) *)
val reduction : t -> float

(* {1 As an effect} *)

(* mode (compressor, limiter, gate: the compressor), threshold (-60 to
 * 0 dB: -20), ratio (1 to 20: 4), attack (0.5 to 100 ms: 5), release
 * (10 ms to 1 s: 100 ms), makeup (0 to 24 dB: 0); the knee, the
 * detector and the look-ahead the mode's ([compressor], [limiter],
 * [gate]) *)
val knobs : Effect.knob list

(* [effect ()]: "dynamics", its meter "reduction" (dB) *)
val effect : unit -> Effect.t
