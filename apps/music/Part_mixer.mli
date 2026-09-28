(* The Mixer 14:2's panel as a part (Component.mli), over its device
 * (Rack_mixer.mli): a strip per channel -- aux send and pan knobs, a
 * mute, a fader and its level's meter -- and the master's fader. The
 * faders are dragged, the knobs turned, a mute clicked. *)

(* 880 x 240 *)
val natural : float * float

(* the part over the mixer device, its meters its "chN.peak" *)
val make : Rack_device.t -> Component.part
