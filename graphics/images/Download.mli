(* Getting an image file, from the disk or over the network, for
 * Image_decode and Texture_decode (and the native platforms' sounds):
 * http:// by our own client (networking/unix/Http_client.mli),
 * https:// still by curl, until TLS is ours too
 * (plan_dependencies_remaining.md, section 2). *)

(* "http://..." or "https://..." *)
val is_url : string -> bool

(* [local_file ~prefix src]: a local file with [src]'s content: [src]
 * itself for a local path, else a temporary file (named after
 * [prefix]) where the URL was downloaded. Blocks while downloading;
 * raises Failure on a network error, or a status other than 2xx. *)
val local_file : prefix:string -> string -> string
