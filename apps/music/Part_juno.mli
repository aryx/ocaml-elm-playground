(* The Roland Juno-106's panel as a part (Component.mli; Part_hammond.mli
 * says why a part, and how it closes over its voice).
 *
 * Vertical sliders in its sections, left to right -- LFO (rate,
 * delay), DCO (the LFO's vibrato, the pulse's width, the
 * sub-oscillator, the noise), HPF (four positions, the slider snapping
 * to them), VCF (cutoff, resonance, the envelope, the LFO, the
 * keyboard's tracking), VCA (level), ENV (A, D, S, R) -- and under them
 * its buttons, each with its light: the range (16', 8', 4'), the pulse
 * and the sawtooth, the width by the LFO or by hand, the envelope's
 * polarity into the filter, the VCA by the envelope or a gate, the
 * chorus (off, I, II, and I+II). A slider pressed follows the mouse
 * until let go; a button acts on a click. *)

val kind : string

(* 960 x 420 *)
val natural : float * float
val make : Voice_juno.t -> Component.part
val load : Voice_juno.t -> string -> Component.part
