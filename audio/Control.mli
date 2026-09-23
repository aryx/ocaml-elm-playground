(* A control on a panel: what an instrument's or an effect's knob is,
 * so that a program can draw it, a patch can store it, and a text file
 * can say it.
 *
 * Every control's value is a float, whatever it looks like:
 *
 *     Knob (from, to)   turned between two values, in the control's own
 *                       units (0 to 1, dB, seconds, hertz)
 *     Switch            off (0) or on (1): a rocker
 *     Selector labels   a position (0, 1, 2, ...) of a rotary switch,
 *                       each named by its label
 *
 * so a panel is a list of named controls and a patch a list of named
 * numbers, and the text form writes a switch as on or off and a
 * selector by its label: "reverb.kind = plate", not "2". *)

type t = Knob of float * float (* from, to *) | Switch | Selector of string list

(* [on x]: a switch's value read, 0.5 and above on *)
val on : float -> bool
val of_bool : bool -> float

(* [index x]: a selector's value read, rounded *)
val index : float -> int

(* [to_string c x]: "0.375", "on", "plate" *)
val to_string : t -> float -> string

(* [of_string c s]: the value [s] says, a knob's kept within its range;
 * None if it isn't one *)
val of_string : t -> string -> float option
