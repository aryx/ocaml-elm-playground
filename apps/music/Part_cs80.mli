(* The Yamaha CS-80's panel as a part (Component.mli; Part_hammond.mli
 * says why a part, and how it closes over its voice).
 *
 * Section I's two rows of knobs, then section II's (the sound -- feet,
 * sawtooth, pulse, its width and modulation, noise, the high-pass and
 * the low-pass with their resonance, the pure sine; then the filter
 * envelope's IL, AL and times, the amplifier's ADSR, the level, the
 * touch: velocity and pressure into brilliance and level), then the
 * controls both share (the mix of I and II, II's detune, the
 * sub-oscillator, the ring modulator, the chorus and tremolo, the
 * volume). The ribbon and the keyboard that feels each key's pressure
 * are the player's, TinyCS80's. *)

val kind : string

(* 960 x 462 *)
val natural : float * float
val make : Voice_cs80.t -> Component.part
val load : Voice_cs80.t -> string -> Component.part
