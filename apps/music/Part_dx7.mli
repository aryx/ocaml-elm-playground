(* The Yamaha DX7's panel as a part (Component.mli; Part_hammond.mli
 * says why a part, and how it closes over its voice).
 *
 * The DX7 was edited as its front panel allowed: a two-line display, a
 * button to choose one parameter, a slider and two buttons (-1, +1) to
 * change it -- 145 parameters one at a time, with nothing to show how
 * they fit together. The left half of the panel is that: the LCD,
 * < PARAM > and < OP > to walk the parameters (the second jumping an
 * operator at a time), the data slider, -1 and +1. The right half is
 * what the DX7 hid: the algorithm drawn as the graph it is (the
 * carriers at the bottom, each modulator above what it modulates, the
 * fed-back one marked), each operator lit by how loud it is now, a click
 * on one choosing its output level; and the six envelopes drawn, their
 * four rates and levels as a shape. *)

val kind : string

(* 960 x 494 *)
val natural : float * float

(* [number]: the voice's number on the LCD's first line -- a host's,
   whose menu may hold a cartridge's voices; by default its place among
   Voice_dx7.presets *)
val make : ?number:(unit -> int) -> Voice_dx7.t -> Component.part
val load : Voice_dx7.t -> string -> Component.part
