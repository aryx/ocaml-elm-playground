(* The TR-808's panel, and the 909's, as a part (Component.mli;
 * Part_hammond.mli says why a part, and how it closes over its voice).
 *
 * A column per instrument, its knobs over its name (the kick's level,
 * tone and decay; the snare's level, tone and snappy; a tom's level and
 * tuning; ...); a click on the name strikes it and makes its track the
 * one the step buttons edit (AC the accents', FL the flams'). The 16
 * step buttons in the 808's colours, red, orange, yellow and cream, four
 * by four (a beat each), their lights the track's hits, the step
 * playing lit as it runs; start/stop, the tempo, the accent's level,
 * the shuffle, the flam, the volume; the 808 and 909 buttons switching
 * the machine. *)

val kind : string

(* 960 x 440 *)
val natural : float * float
val make : Voice_tr808.t -> Component.part
val load : Voice_tr808.t -> string -> Component.part

(* the pattern's tracks, for a host showing it whole: the instruments'
 * by their index, then the accents (11) and the flams (12); a step
 * toggled; the machine's colour for the accents and flams *)
val track : Voice_tr808.patch -> int -> bool array
val with_step : Voice_tr808.patch -> int -> int -> Voice_tr808.patch
val stripe : int -> Playground.color
