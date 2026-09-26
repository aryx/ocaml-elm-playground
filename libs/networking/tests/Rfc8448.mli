(* RFC 8448's "simple 1-RTT handshake" (section 3), the values Unit_tls13
 * checks Tls13 against, in hexadecimal: the two ephemeral X25519 keys,
 * the messages, every secret of the key schedule, the keys and IVs, the
 * encrypted records *)

val client_private : string
val server_public : string
val client_hello : string
val server_hello : string
val handshake_secret : string
val c_hs_traffic : string
val s_hs_traffic : string
val master_secret : string
val server_hs_key : string
val server_hs_iv : string
val client_hs_key : string
val client_hs_iv : string
val encrypted_extensions : string
val certificate : string
val certificate_verify : string
val server_finished : string
val server_flight_payload : string
val server_flight_record : string
val c_ap_traffic : string
val s_ap_traffic : string
val server_ap_key : string
val server_ap_iv : string
val client_finished : string
val client_finished_record : string
val client_ap_key : string
val client_ap_iv : string
val client_data : string
val client_data_record : string
val server_data_record : string
val client_alert_record : string
val server_ticket_record : string
