(* networking/unix/Relay and Relay_client over localhost, in one
 * process: the seats (0, 1, and a third player refused), a packet
 * copied to the others only, and Lockstep through the relay for 300
 * ticks, both players computing the game alone's models *)
val tests : < Cap.network ; .. > -> Testo.t list
