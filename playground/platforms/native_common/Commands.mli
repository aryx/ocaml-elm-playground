(* The commands of a native program ([init]'s and [update]'s Cmd.t)
 * performed while the frames go on: each request an Http_request,
 * stepped once a frame by the loop (Native_loop_2d), its answer given
 * back as the message the command asked for; an https:// one fetched
 * by curl at once, blocking, until TLS is ours. Cmd.Msg's message is
 * given back at the next frame.
 *
 * With threads (the flag threads=on, TinyNetscape's), what still
 * blocks is done on a pool of threads (Worker): curl's fetches, and
 * the names Http_request resolves; the frames go on meanwhile. *)

(* the commands in flight *)
type 'msg t

val create : ?threads:bool -> unit -> 'msg t

(* start what [cmd] asks (nothing waits: a request only begins) *)
val perform : 'msg t -> 'msg Cmd.t -> unit

(* advance every request; the messages of the commands now finished,
 * in the order they were performed *)
val step : 'msg t -> 'msg list
