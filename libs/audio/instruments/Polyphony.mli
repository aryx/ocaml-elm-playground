(* Polyphony: a voice for every key held, and for every key let go
 * whose sound hasn't died yet (see notes_synth.md section 5).
 *
 * A monophonic synthesizer has one voice and chooses which key it
 * follows (Voicing.mli). A piano, an organ, a Juno has a voice per key:
 * a chord is three voices sounding at once, each its own oscillator and
 * its own envelope, their samples added. Two things make it more than
 * "a voice per key held":
 *
 *  - a key let go doesn't free its voice: the voice *releases*, its
 *    envelope falling for the release's time, and only then, silent,
 *    is it freed. Freed at once, every release would be cut, a click;
 *    never freed, voices would pile up with every note (a leak);
 *  - a key pressed again while its last voice still releases gets a
 *    new voice, the old one fading beside it (as a piano's string
 *    struck again while it still rings: two sounds, briefly).
 *
 *     keys      C E G held         E let go            all let go
 *     voices    [C] [E] [G]        [C] [E~] [G]        [C~] [E] [G~]
 *                                  E releasing          ...then none
 *
 * A voice is whatever makes a sound for one key, as a record of
 * functions (Instrument.mli's pattern): released, filled a block at a
 * time, and silent once its release is over. So the same machinery
 * serves a sine and an envelope ([sine] below, an organ's single
 * drawbar), a Hammond's nine, a Rhodes tine.
 *
 * This is the first step (plan_synth_teaching.md, H1): as many voices
 * as keys. The second, a fixed number of voices and one *stolen* when
 * a key needs one and none is free (the oldest released first, then
 * the oldest), is a real synthesizer's (the Juno's six, the DX7's
 * sixteen), and comes with them.
 *
 * Worked example (Unit_polyphony): C, E and G pressed, sines with a 15
 * ms release: three voices, the block their sum; E let go: still three
 * while it releases -- still after the next block, 16.7 ms -- and two
 * after the second: silent means -100 dB, which an exponential release
 * reaches at 5/3 of its time (it is -60 dB at its time: Envelope.mli),
 * 25 ms; all let go: none, nothing left behind. *)

(* a voice, for one key *)
type voice = {
  (* its key let go: the release begins *)
  release : unit -> unit;
  (* its next block, written over the one given *)
  fill : Signal.t -> unit;
  (* its sound over, after its release: may be freed *)
  silent : unit -> bool;
}

type t

val create : unit -> t

(* [press t key voice]: [key] down, [voice] its sound *)
val press : t -> int -> voice -> unit

(* [release t key]: [key] up, its voice releasing (no voice: nothing) *)
val release : t -> int -> unit

(* [fill t out]: the voices' next blocks summed, written over [out];
 * the voices silent after it freed *)
val fill : t -> Signal.t -> unit

(* the voices sounding, releasing ones included *)
val voices : t -> int

(* the keys held, lowest first *)
val held : t -> int list

(* [sine ~adsr frequency velocity]: a sine at [frequency], its loudness
 * [velocity] times the envelope [adsr] (Envelope.mli, exponential) *)
val sine : adsr:Envelope.t -> float -> float -> voice
