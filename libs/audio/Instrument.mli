(* An instrument: a sound played live, its notes and its knobs arriving
 * while it sounds (see notes_synth.md section 1).
 *
 * The rest of audio/ renders ahead: a sound is a value (Synth.t), its
 * samples computed whole when it is played. A synthesizer can't be:
 * when a key goes down nobody knows yet when it comes up, and someone
 * may turn the filter's knob in between. So an instrument is a
 * *process*: a state, changed by events, asked for the next block of
 * samples.
 *
 *     update, 60 a second            the sound card, 44,100 a second
 *
 *     note_on 60 ----.
 *     set "volume" --+--> [ state ] --fill--> the next n samples --> Mixer
 *     note_off 60 ---'       ^  |                  (a pull: 735 a frame)
 *                            '--'
 *                     kept from block to block
 *
 * Every live block of plan_synth_teaching.md (the oscillators, the
 * ladder, the envelopes, the effects) follows the same pattern, shown
 * here on the smallest instrument there is, [sine]:
 *
 *  - its parameters and its memory in one record of mutable fields,
 *    hidden behind the .mli: the oscillator's phase, the gate's level,
 *    the knobs. Mutable, as the DSP books write them and as a sound
 *    card pulls them: a block computed in place, nothing allocated per
 *    sample, the state the process's own business;
 *  - an event (a key, a knob) changes the state *between* blocks, and
 *    is heard from the next one: an event's time is a block's
 *    boundary. With a pull per frame (a golden run, -dump-frame) a
 *    note played at frame k starts at sample 735 k; natively ~50 ms of
 *    queue lie ahead of it too, the latency of Audio.mli's two clocks;
 *  - [fill] computes a block, and a knob's new value is *ramped* over
 *    it rather than jumped to. A volume going from 0.2 to 0.8 at once
 *    is a step in the wave, heard as a click; a knob turned slowly is a
 *    step every block, a click 60 times a second, the "zipper noise"
 *    of the first digital synthesizers.
 *
 * Worked example, the ramp: the volume turned from 0.2 to 0.8, over a
 * block of 735 samples. Sample i (from 0) gets
 *
 *     0.2 + 0.6 (i + 1) / 735          0.20082, 0.20163, ..., 0.8
 *
 * neighbours 0.6 / 735 = 0.00082 apart, instead of 0.6 once. The gate
 * is ramped the same way, over 5 ms (220.5 samples: a key pressed,
 * 0 to 1 in 221 samples; released, back to 0), so a note never starts
 * or stops on a jump.
 *
 * [t] is a record of functions -- an object in all but name -- because
 * the mixer holds instruments of different kinds (this sine, the
 * Minimoog) and wants the same four things from each; the state
 * stays inside, in the closures.
 *
 * References: Will Pirkle, Designing Software Synthesizer Plug-Ins in
 * C++, 2014 (chapter 2: the plug-in's process of blocks, the
 * parameters smoothed); Steinberg's VST 2 SDK, 1999, whose
 * processReplacing (a block of samples in, a block out, in place) is
 * the shape every plug-in API has had since. *)

type t = {
  (* a key goes down: its number (MIDI's, 60 middle C, 69 the A at 440
   * Hz: Music.midi_frequency), its velocity, 0 to 1 *)
  note_on : int -> float -> unit;
  (* the key comes up *)
  note_off : int -> unit;
  (* a knob turned, by its name, to a value; names the instrument
   * doesn't have are ignored *)
  set : string -> float -> unit;
  (* the next block: both channels written over, their length the
   * block's *)
  fill : Signal.stereo -> unit;
}

(* [sine ()]: one sine wave, playing the last key pressed until that key
 * is let go (another key's release is ignored; a key pressed while one
 * sounds changes the pitch at once, the wave going on: legato); its
 * velocity the loudness, the gate ramped over 5 ms; one knob, "volume",
 * 0 to 1, 0.5 at first. *)
val sine : unit -> t

(* the gate's ramp, in samples: 5 ms *)
val gate_samples : float
