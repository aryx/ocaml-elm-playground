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
 * By default, as many voices as keys (an organ's way: every tonewheel
 * always turning). A real synthesizer has a fixed number, each its
 * circuits or its share of the chip's time (the Juno's six, the DX7's
 * sixteen): a key pressed with all of them sounding *steals* one --
 * the oldest released, whose sound is fading anyway, and if none is,
 * the oldest held, the note played longest ago, the least missed:
 *
 *     2 voices   C down  E down  C up    G down         A down
 *                [C]     [C][E]  [C~][E] [G][E]         [A][G]
 *                                        C~ stolen:     none released:
 *                                        released       E, the oldest
 *
 * Stolen, a voice is cut, not faded: a click, which the DX7 has too
 * (a real synthesizer ramps it over a few milliseconds: an exercise).
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

(* [create ?voices ()]: at most [voices] sounding, unlimited by
 * default *)
val create : ?voices:int -> unit -> t

(* [press t key voice]: [key] down, [voice] its sound, a voice stolen
 * if they're all sounding *)
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
