(* A patch as a panel and as text: an instrument's controls, each named,
 * and the patch written and read as "name = value" lines (see
 * notes_synth.md section 9).
 *
 * A synthesizer's patch was a sheet of paper: the Minimoog's patch
 * charts, the panel drawn with each knob's position marked; a Hammond's
 * registration, nine digits for its nine drawbars (888000000). Here a
 * patch is a record of the instrument's own ('p), and its controls a
 * list, each a name, a Control.t (a knob, a switch, a selector), and
 * how to read it from the patch and put it back: the same list draws
 * the panel, turns its knobs by name (Instrument.set), and writes the
 * text --
 *
 *     # a comment
 *     filter.cutoff = 0.320
 *     osc2.on = on
 *     osc1.range = 32'
 *
 * a control not named keeping the position it has in the patch read
 * into. *)

type 'p knob = { name : string; control : Control.t; get : 'p -> float; put : 'p -> float -> 'p }

(* builders: a knob from 0 to 1, from -1 to 1, a switch (a bool in the
 * patch), a selector (an index in the patch) *)
val knob : string -> ('p -> float) -> ('p -> float -> 'p) -> 'p knob
val detune : string -> ('p -> float) -> ('p -> float -> 'p) -> 'p knob
val switch : string -> ('p -> bool) -> ('p -> bool -> 'p) -> 'p knob
val selector : string -> string list -> ('p -> int) -> ('p -> int -> 'p) -> 'p knob

(* [to_string knobs p]: every control's line *)
val to_string : 'p knob list -> 'p -> string

(* [of_string knobs ~initial text]: [initial] with the lines' values;
 * '#' starts a comment; an error names the line's trouble: "no such
 * control: ...", "...: not a value: ...", "not \"name = value\": ..." *)
val of_string : 'p knob list -> initial:'p -> string -> ('p, string) result
