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
 * sustain. The lines are straight here; real synthesizers often curve
 * them (exponentially: the ear hears ratios, see Mix.mli).
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
