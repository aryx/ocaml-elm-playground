(* networking/unix/Http_server over localhost, in one process: three
 * requests of Http_request at once (a page, a redirection followed, a
 * 404), the server and the clients stepped in turn; garbage answered
 * 400 *)
val tests : < Cap.network ; .. > -> Testo.t list
