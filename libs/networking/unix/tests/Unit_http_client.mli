(* networking/unix/Http_client: GETs against a server on localhost,
 * forked by the test, answering canned responses: a redirection
 * followed to a chunked body, a 404 given back, a redirection to
 * https:// followed into TLS (to a port where nobody listens), and a
 * closed port refused *)
val tests : < Cap.network ; .. > -> Testo.t list
