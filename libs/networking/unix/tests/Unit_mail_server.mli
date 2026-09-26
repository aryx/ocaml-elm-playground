(* Mail_server over localhost, in one process, driven by the pure
 * clients (Smtp, Pop3): alice sends, bob checks, twice (the second time
 * nothing: deleted at QUIT); mail for elsewhere refused (not a relay);
 * a POP3 connection dropped after DELE, before QUIT, deletes nothing;
 * a wrong password refused; the telnet session, SMTP and POP3 typed
 * by hand over plain TCP *)
val tests : < Cap.network ; .. > -> Testo.t list
