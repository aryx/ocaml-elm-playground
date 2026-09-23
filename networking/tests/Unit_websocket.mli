(* networking/Websocket: RFC 6455's examples -- the handshake's
 * accept, "Hello" unmasked and masked, the two longer lengths -- and a
 * stream cut anywhere: incomplete until whole, then the frames in
 * order *)
val tests : Testo.t list
