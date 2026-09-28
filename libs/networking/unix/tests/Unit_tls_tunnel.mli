(* Tls_tunnel against a local openssl s_server: with a certificate
 * that does not check out (self-signed), the connection is refused --
 * nothing received, the tunnel closed -- rather than talked through *)
val tests : < Cap.network ; Cap.exec ; .. > -> Testo.t list
