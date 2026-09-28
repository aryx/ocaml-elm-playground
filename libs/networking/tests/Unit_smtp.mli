(* Smtp: RFC 5321's own session (appendix D.1) replayed against the
 * client, a recipient refused and the others kept; EHLO refused and
 * HELO instead; the dot stuffed and unstuffed; Bcc in the envelope and
 * not in the message; a message refused whole and the next one sent *)
val tests : Testo.t list
