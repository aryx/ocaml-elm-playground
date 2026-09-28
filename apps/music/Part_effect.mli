(* Any effect's panel as a part (Component.mli), a rack's 2 units: its
 * name, its knobs in a row (Effect.knobs: every effect gives them the
 * same way, by name), and a bypass rocker, as Reason's effects have --
 * the first level of plan_tiny_reason.md's "a module in a line". *)

(* 880 x 80 *)
val natural : float * float

(* [make ~kind ~name ~color fx bypass]: the part over [fx], its bypass
 * rocker setting [bypass] *)
val make : kind:string -> name:string -> color:Playground.color -> Effect.t -> bool ref -> Component.part
