(* The transport for a role (Transport.mli): UDP for a host or a
 * player joining it (Udp.mli), WebSocket through a relay (Relay_client.mli).
 * What the native platforms install for Multiplayer (set_connect). *)
val connect : < Cap.network ; .. > -> Transport.role -> (Transport.t, string) result
