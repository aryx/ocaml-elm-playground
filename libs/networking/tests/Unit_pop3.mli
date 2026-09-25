(* Pop3: RFC 1939's own session (section 10) replayed against the
 * client, USER and PASS for APOP; leaving the mail on the server, UIDL
 * and the ids already known; a wrong password; an empty maildrop *)
val tests : Testo.t list
