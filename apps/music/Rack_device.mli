(* A rack module's back and sound (plan_tiny_reason.md): its jacks, and
 * how it turns what arrives on its inputs into its outputs. Its front,
 * the panel, is a part (Rack_module.mli); this half is pure, no screen,
 * so the rack's sound is tested without one (Unit_reason).
 *
 * Two kinds of jack, as on Reason's back: *audio* (a stereo sound) and
 * *CV* (control voltage, one number per chunk: a note, a gate, a knob
 * moved by another device). A cable joins an output to an input of the
 * same kind (Studio_reason.mli).
 *
 * A device runs in *stages*, most in one: a stage reads some inputs and
 * writes some outputs. The mixer has two, so that a send and its
 * return is not a loop:
 *
 *     mixer.sends --Aux Send--> delay --> Aux Return--> mixer.master
 *
 * Every device runs every chunk, plugged or not: its clock goes on (a
 * sequencer keeps its place), as ReBirth's machines do. *)

(* the rack runs in chunks of 64 samples, 1.45 ms: the CVs' rate *)
val chunk : int

type signal = Audio | Cv
type dir = In | Out
type jack = { label : string; dir : dir; signal : signal }

(* what a device is, for the rack's automatic routing (Studio_reason.add) *)
type role = Hardware | Mixer | Instrument | Effect | Sequencer

(* a stage's view of the cables, for one chunk: an audio input's buffer
 * (silence if unplugged), a CV input's value (None if unplugged), an
 * audio output's buffer to write, a CV output's value to give *)
type io = {
  audio_in : int -> Signal.stereo;
  cv_in : int -> float option;
  audio_out : int -> Signal.stereo;
  cv_out : int -> float -> unit;
}

(* the jacks it reads and writes, by index *)
type stage = { reads : int list; writes : int list; run : io -> unit }

type t = {
  kind : string; (* "hammond" *)
  role : role;
  jacks : jack array;
  stages : stage list;
  (* its knobs by name, for the front and for CV; names it doesn't have
     are ignored, and read 0 *)
  set : string -> float -> unit;
  get : string -> float;
  (* the keys, if it plays notes *)
  note_on : int -> float -> unit;
  note_off : int -> unit;
  (* the transport: started, stopped, the tempo; the step sounding, if
     it has a sequencer *)
  run : bool -> unit;
  tempo : float -> unit;
  step : unit -> int option;
}

(* the jack of that label, by index *)
val jack : t -> string -> int option

(* [of_instrument ~kind inst]: a voice as a device. Its back: Audio Out
 * (0), Seq Note (1) and Seq Gate (2), CV from a sequencer -- the gate
 * going up plays the note (its value the velocity), going down lets it
 * go, the note changing while it is up glides into the next (legato);
 * then a CV input per [cv] (its label, the knob it moves, the knob's
 * range: the CV's 0 to 1 over it). [transport] for a voice with a
 * sequencer of its own (the 808): run, tempo, step. *)
val of_instrument :
  kind:string ->
  ?cv:(string * string * (float * float)) list ->
  ?transport:(bool -> unit) * (float -> unit) * (unit -> int option) ->
  Instrument.t ->
  t

(* [of_effect ~kind fx]: an effect as a device: Audio In (0), Audio
 * Out (1), the sound through it; [bypass] read each chunk *)
val of_effect : kind:string -> bypass:(unit -> bool) -> Effect.t -> t
