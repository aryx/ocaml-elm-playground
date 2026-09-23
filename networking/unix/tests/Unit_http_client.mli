(* networking/unix/Http_client: GETs against a server on localhost,
 * forked by the test, answering canned responses: a redirection
 * followed to a chunked body, a 404 given back, an https:// URL and a
 * closed port refused *)
val tests : Testo.t list
