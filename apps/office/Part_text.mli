(* A text with looks, as a part of a compound document (appkits/embed):
 * TinyWord's engine (Rich, Page, Stroke_text) behind the four
 * functions a document asks of a part. Active, it takes clicks, drags,
 * typing, and its Text menu; inactive, it is only drawn. *)

val kind : string

(* [make rich]: the part, showing [rich] *)
val make : Rich.t -> Component.part

(* read back from its save: the looks first, then the characters *)
val load : string -> Component.part
