(* The web backend's store of documents: the browser's localStorage,
 * which holds text, so the bytes are kept base64-encoded, each under
 * "elm-playground:" and its name (see Playground_platform.mli). *)

val store : string -> string -> unit
val fetch : string -> string option
val stored : unit -> string list

(* a download: an <a download> clicked, its href the bytes as a data:
 * URL *)
val export : string -> string -> unit
