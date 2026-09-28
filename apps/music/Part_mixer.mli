(* The Mixer 14:2's panel as a part (Component.mli), over its device
 * (Rack_mixer.mli): a strip per channel -- aux send and pan knobs, a
 * mute, a fader and its level's meter -- and the master's fader. The
 * faders are dragged, the knobs turned, a mute clicked. *)

(* 880 x 240 *)
val natural : float * float

(* [make mixer ~peak]: the part over the mixer device; [peak k] the
 * channel k's level now (0 to 13, 14 the master), for the meters *)
val make : Rack_device.t -> peak:(int -> float) -> Component.part
