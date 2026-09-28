(* Any voice's panel as a part, with no drawing of its own: its knobs
 * (Voice.mli: every voice gives them the same way, by name) in two rows
 * of up to nine, each drawn by its kind -- a knob, a rocker, a
 * selector as a knob in steps -- its label under it. The panel a voice
 * gets before it has one made for it (Part_hammond.mli), and the
 * third level of plan_tiny_reason.md's "a module in a few lines".
 *
 * A voice is given as a record of what the part needs from it, not as
 * a module (no first-class modules here, ocaml-light) -- the voice's
 * own functions, closed over the running voice. *)

type 'p voice = {
  knobs : 'p Patch_text.knob list;
  presets : (string * 'p) list;
  patch : unit -> 'p;
  set_patch : 'p -> unit;
  to_string : 'p -> string;
  of_string : string -> ('p, string) result;
}

(* 960 x 290 *)
val natural : float * float

(* [make ~kind ~color voice controls]: the part over [voice], its
   controls as (label, knob name), the knobs' pointers in [color];
   fails on a name the voice has no knob for *)
val make : kind:string -> color:Playground.color -> 'p voice -> (string * string) list -> Component.part
