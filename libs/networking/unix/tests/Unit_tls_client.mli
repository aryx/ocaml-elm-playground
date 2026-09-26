(* Tls_client against a local openssl s_server -tls1_3 -www: a page
 * asked and received over ChaCha20-Poly1305 and over AES-128-GCM, with
 * an ECDSA P-256 certificate and with an RSA one (the handshake then
 * signed with RSA-PSS); a server asking for a client certificate
 * answered with an empty one; refused when its self-signed certificate is not
 * among the roots, and for another host name *)
val tests : < Cap.network ; Cap.open_in ; Cap.exec ; .. > -> Testo.t list
