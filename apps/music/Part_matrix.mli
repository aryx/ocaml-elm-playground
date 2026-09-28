(* The Matrix's panel as a part (Component.mli), over its device
 * (Rack_matrix.mli): its 16 steps as columns -- a two-octave key grid
 * from C2 (a click sets the step's note), a gate bar under it (a click
 * sets its height, the velocity; at the bottom, a rest) and a tie --
 * the step playing lit.
 *
 * Ours, and said so: two octaves (Reason's has an octave switch), the
 * curve not shown (the device's curve is a ramp). *)

(* 880 x 240 *)
val natural : float * float
val make : Rack_device.t -> Component.part
