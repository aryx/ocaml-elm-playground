(* Envelopes: how a sound starts and stops (see notes_audio.md
 * section 4).
 *
 * A tone switched on or off at once clicks: its wave jumps, and a jump
 * is a burst of every frequency. No real instrument does that: a
 * piano's note starts sharply and dies away, a violin's swells. An
 * envelope is the volume over time, multiplied into the wave; the
 * classic one, from the 1960s analog synthesizers (Robert Moog's, ARP's),
 * is ADSR:
 *
 *    volume
 *     1 |   /\
 *       |  /  \
 *     S |_/____\_____________        A: attack, the time to full volume
 *       |/               |   \       D: decay, the time down to...
 *     0 +--A--D----------+-R--\-->   S: sustain, ...this level, held
 *       0                held    t   R: release, the time to silence,
 *                                       once the note is let go
 *
 * Example: A = 0.01 s, D = 0.1 s, S = 0.5, R = 0.2 s, the note held
 * 0.5 s: at 0.005 s, halfway up, 0.5; at 0.06 s, halfway down, 0.75; at
 * 0.3 s, sustaining, 0.5; at 0.6 s, halfway through the release, 0.25;
 * from 0.7 s on, 0. Let go before the sustain (a short note), the
 * release starts from wherever the volume was.
 *
 * A percussive sound (a blip, a drum, most game sounds) is a short
 * attack and a decay to 0, no sustain ([percussive]); an organ is all
 * sustain. The lines are straight here; analog synthesizers curve them
 * (exponentially: the ear hears ratios, see Mix.mli), as the live
 * envelope below can.
 *
 * References: the ADSR envelope generator, credited to Vladimir Ussachevsky
 * (1965) for Robert Moog's modular synthesizers; Curtis Roads, The
 * Computer Music Tutorial, 1996, chapter 4. *)

type t = {
  attack : float; (* seconds *)
  decay : float; (* seconds *)
  sustain : float; (* a level, 0 to 1 *)
  release : float; (* seconds *)
}

(* [percussive ~attack ~decay]: up, then down to 0, no sustain nor
 * release *)
val percussive : attack:float -> decay:float -> t

(* [level env ~held time]: the volume at [time] seconds after the start,
 * the note let go at [held] *)
val level : t -> held:float -> float -> float

(* [duration env ~held]: when it's silent for good: held + release *)
val duration : t -> held:float -> float

(* [apply env ~held samples]: each sample times the level at its time *)
val apply : t -> held:float -> Signal.t -> Signal.t

(*****************************************************************************)
(* {1 Live: an envelope driven by a gate} *)
(*****************************************************************************)
(* Above, the note's length is known ([held]) and the envelope is
 * computed whole. Played live (Instrument.mli), nobody knows when the
 * key will come up: the envelope is a state machine, driven by a
 * *gate*, on while the key is held (see notes_synth.md section 4):
 *
 *     gate on              gate off
 *        |                    |
 *   Idle --> Attack --> Decay --> Sustain --> Release --> Idle
 *               ^                                 |
 *               '--- gate on again: from the level where it is
 *
 * A new gate starts the attack from the current level, not from 0: a
 * note played again while the last one dies away rises from there,
 * without a jump. (Whether a new key re-opens the gate at all -- legato
 * -- is Voicing.mli's choice.) The ADSR's times and sustain are read at
 * each block, knobs turned while it runs.
 *
 * {2 Straight or exponential}
 *
 * [Linear]: the lines above, the same levels as [level] (the tests
 * compare the two on the example). [Exponential]: an analog envelope's
 * segments, a capacitor charging through a resistor towards a target,
 * each sample a one-pole step, level += (target - level) c, so the
 * level approaches its target by a fixed *ratio* per sample. Two
 * consequences:
 *
 *  - it never arrives. So the attack aims *past* 1, at 1.5, and stops
 *    when it gets to 1: 1.5 (1 - e^(-t / tau)) = 1 at t = tau ln 3, so
 *    tau = attack / ln 3 (0.91 attack) makes it arrive in its time;
 *    halfway through, it's already at 1.5 (1 - 1 / sqrt 3) = 0.634,
 *    not 0.5: a punchier start. The decay and the release aim at their
 *    targets for good, their time the time to get most of the way, to
 *    a thousandth of the distance: tau = time / ln 1000 (time / 6.91);
 *  - it is a straight line in decibels, the ear's scale (Mix.mli):
 *    the exponential release loses 60 dB over its time, the same number
 *    of dB every millisecond, a natural fade; the straight release
 *    loses little at first and plunges at the end, -6 dB at half its
 *    time, -20 dB at 90%, -40 dB at 99%, silent: it sounds cut off.
 *
 * Worked example, the exponential on the example's ADSR (A = 0.01,
 * D = 0.1, S = 0.5, R = 0.2): 0.634 at 5 ms, 1 at 10 ms (441
 * samples); 0.5158 at 0.06 s (the distance to S, 0.5, divided by
 * sqrt 1000 = 31.6 halfway through the decay), 0.5005 at 0.11 s; let
 * go from 0.5 at 0.5 s: 0.0158 at 0.6 s, 0.0005 at 0.7 s (-60 dB).
 * Idle below 0.00001 (-100 dB).
 *
 * References: the Minimoog's contour generators; Will Pirkle, Designing
 * Software Synthesizer Plug-Ins in C++, 2014, chapter 7 (the analog
 * envelope's overshooting targets). *)

type curve = Linear | Exponential
type stage = Idle | Attack | Decay | Sustain | Release

type running

(* idle, at 0 *)
val start : unit -> running

(* the key pressed (the attack, from the current level), let go (the
 * release, from the current level) *)
val gate_on : running -> unit
val gate_off : running -> unit

(* [fill curve env r out]: the next [Array.length out] levels *)
val fill : curve -> t -> running -> Signal.t -> unit

val stage : running -> stage
val current : running -> float
