(* The Minimoog Model D's panel as a part (Component.mli;
 * Part_hammond.mli says why a part, and how it closes over its voice).
 *
 * Read left to right -- CONTROLLERS, OSCILLATOR BANK, MIXER,
 * MODIFIERS, OUTPUT -- in black between two wooden cheeks. The knobs
 * turn by dragging them up or down, the rotary switches (the
 * oscillators' ranges and waveforms) by dragging or a click, the
 * rockers by a click. *)

val kind : string

(* 1000 x 470 *)
val natural : float * float

(* white on black, the lit half of a rocker the Model D's blue: for a
   host's widgets beside the panel (TinyMinimoog's effects rack) *)
val theme : Theme.t
val make : Voice_minimoog.t -> Component.part
val load : Voice_minimoog.t -> string -> Component.part
