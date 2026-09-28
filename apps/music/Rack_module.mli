(* A rack module: what TinyReason's rack holds, and all it knows of one
 * (plan_tiny_reason.md). Two halves over the same sound:
 *
 *   front   its panel, a part (Component.mli, the office's idea):
 *           drawn into a box, scaled, given the mouse
 *   device  its back and its sound (Rack_device.mli): its jacks, its
 *           stages, its notes, its transport
 *
 * A host never names a module in particular: it draws fronts and
 * backs, plugs cables into jacks, and runs devices. Adding one to the
 * rack is a line in its catalogue, a name and a maker -- for an effect,
 * [of_effect], its panel drawn from its knobs; for a voice with a panel
 * of its own (Part_hammond.mli) a few lines pairing the two. The
 * VST plug-ins of Cubase (1996) and Reason's Rack Extensions (2012)
 * are the same idea, with a file format in the middle; here, a value. *)

type t = {
  name : string; (* "Juno-106", its front's label and the Create menu's *)
  color : Playground.color; (* its cables' shade, its selection *)
  front : Component.part;
  device : Rack_device.t;
}

(* the Create menu: a name and a maker *)
type catalogue = (string * (unit -> t)) list

(* an effect of libs/audio/effects: its panel its knobs and a bypass *)
val of_effect : kind:string -> name:string -> color:Playground.color -> Effect.t -> t

(* the Matrix, and the Mixer 14:2 ([peak k] channel k's level, 14 the
 * master's, for its meters) *)
val matrix : unit -> t
val mixer : peak:(Rack_device.t -> int -> float) -> t
