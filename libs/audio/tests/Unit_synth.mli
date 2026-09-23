val tests : Testo.t list

(* [centroid x]: the spectrum's centre of mass, in Hz: a sound's
 * brightness *)
val centroid : Signal.t -> float
