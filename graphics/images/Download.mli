(* Getting an image file, from the disk or over the network (with
 * curl), for Image_decode and Texture_decode. *)

(* "http://..." or "https://..." *)
val is_url : string -> bool

(* [local_file ~prefix src]: a local file with [src]'s content: [src]
 * itself for a local path, else a temporary file (named after
 * [prefix]) where the URL was downloaded. Blocks while downloading;
 * raises on a network error. *)
val local_file : prefix:string -> string -> string
