(* networking/unix/Universe_server over localhost, worlds as plain
 * transports (Relay_client): on_new greets the newcomer and tells the
 * others, on_msg answers the sender only, on_disconnect tells who is
 * left; and the universe's state follows *)
val tests : < Cap.network ; .. > -> Testo.t list
