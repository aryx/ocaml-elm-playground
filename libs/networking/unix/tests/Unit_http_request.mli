(* networking/unix/Http_request: the non-blocking request gives the
 * same responses as the blocking Http_client, when the answer comes
 * at once and when it comes a byte at a time; a slow server doesn't
 * stop the frames; a silent one times out *)
val tests : < Cap.network ; .. > -> Testo.t list
