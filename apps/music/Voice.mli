(* What every synthesizer's voice gives its panel (see notes_synth.md;
 * plan_synth_teaching.md): Voice_minimoog, Voice_hammond, Voice_tb303,
 * Voice_dx7, Voice_rhodes, Voice_cs80, each a module of this type and
 * more.
 *
 * A voice is two things. A *patch*, a record of the instrument's own
 * settings, with its controls by name (Patch_text.mli: the panel's
 * knobs, the text a patch is saved as, Instrument.set's names) and
 * presets; and a *player*, [t], holding the patch playing, which the
 * mixer pulls as an Instrument.t, and the last samples it played, for
 * a scope. What the panels need beyond that stays each voice's own: the
 * Leslie's rotors, the TB-303's sequencer, the DX7's operator levels.
 * [create] too, each taking its own options.
 *
 *     patch ---- knobs ----> panel, text, Instrument.set
 *       |
 *       v
 *       t ----- instrument --> Mixer (note_on, note_off, fill)
 *         ----- recent ------> scope
 *
 * The voices are checked against it in their tests (Test.ml), so a
 * voice drifting from it is a compile error, not a surprise to the
 * next panel -- TinyOp1's, which swaps engines behind the same four
 * knobs. *)

module type S = sig
  type patch

  val initial : patch

  type knob = patch Patch_text.knob

  val knobs : knob list
  val to_string : patch -> string
  val of_string : string -> (patch, string) result
  val presets : (string * patch) list

  type t

  val patch : t -> patch
  val set_patch : t -> patch -> unit
  val recent : t -> Signal.t
  val instrument : t -> Instrument.t
end
