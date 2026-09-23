(* Delay: the echo, live, the way a tape echo makes it (see
 * notes_synth.md section 8.5).
 *
 * Synth.echo's idea, a delay line whose output is fed back in, made a
 * streaming effect, in stereo, with what the hardware added:
 *
 *            .--------------- feedback x [low-pass] <------.
 *            v                                              |
 *     x --->( + )--> [ delay line, [time] seconds ] --------+--> wet
 *
 *  - the time in seconds, or in *beats* ([beats]): at 120 BPM a dotted
 *    eighth is 0.75 x 0.5 = 0.375 s, the echoes falling between the
 *    notes (the Edge's guitar);
 *  - the feedback through a one-pole low-pass, so each repeat is
 *    darker than the last: a tape loses its highs at each pass over
 *    the heads, a bucket-brigade chip at each of its thousand steps
 *    (the Roland Space Echo, 1974; the Boss DM-2, 1981). Only the
 *    feedback: the first echo comes back as it went in, the second
 *    filtered once, the third twice, ...
 *  - *ping-pong*: the dry sound (its two sides mixed) into the left
 *    line only, each line's output fed back into the other's, so the
 *    repeats alternate sides;
 *  - the time turned while it sounds moves the read position a little
 *    each sample (a one-pole towards the new time, in about 50 ms), not
 *    at once: the line is read *faster or slower* for a moment and its
 *    echoes bend in pitch, as a tape echo's do when its motor speed
 *    changes. The read falls between samples: the two neighbours mixed
 *    (Resample's linear interpolation).
 *
 * Worked example (Unit_delay): 120 BPM, a dotted eighth (0.375 s),
 * feedback 0.5, the low-pass at 3 kHz. A burst of a 200 Hz sine comes
 * back at 0.375, 0.75, 1.125 s, as loud as 1, 0.499, 0.249 of it (a
 * one-pole at 3 kHz keeps nearly all of 200 Hz); a burst at 8 kHz as
 * 0.842, 0.131, 0.033 -- darker at every pass, the one-pole's gain at
 * 8 kHz (0.37) with the feedback's 0.5. And the first echo's 0.842 is a
 * lesson of its own: 0.375 s is 16,537.5 samples, read half-way
 * between two, and the two neighbours averaged are a low-pass, cos (pi
 * 8000 / 44100) = 0.842 at 8 kHz -- the linear read's price
 * (Resample.mli), heard on every echo whose time falls between
 * samples.
 *
 * References: Will Pirkle, Designing Audio Effect Plugins in C++, 2nd
 * ed. 2019, chapter 14 (delay effects); Julius O. Smith III, Physical
 * Audio Signal Processing, "Delay Lines",
 * https://ccrma.stanford.edu/~jos/pasp/. *)

(* [beats ~bpm b]: [b] beats at [bpm], in seconds (0.75 a dotted eighth
 * when the beat is a quarter note) *)
val beats : bpm:float -> float -> float

(* the longest time: 2 s *)
val longest : float

type t

val create : unit -> t

type settings = {
  time : float; (* seconds, up to [longest] *)
  feedback : float; (* 0 to 0.95 *)
  tone : float; (* the feedback's low-pass, in Hz *)
  ping_pong : bool;
  mix : float; (* the echoes' level, 0 to 1, the dry sound kept *)
}

(* [process t settings s]: [s] in place, both channels *)
val process : t -> settings -> Signal.stereo -> unit

(* {1 As an effect} *)

(* time (0.05 to 2 s: a dotted eighth at 120 BPM, 0.375), feedback (0
 * to 0.95: 0.4), tone (300 Hz to 12 kHz: 3 kHz), pingpong (off), mix
 * (0 to 1: 0.3) *)
val knobs : Effect.knob list

(* [effect ()]: "delay" *)
val effect : unit -> Effect.t
