(* Mail_server over localhost, in one process, driven by the pure
 * clients (Smtp, Pop3): alice sends, bob checks, twice (the second time
 * nothing: deleted at QUIT); mail for elsewhere refused (not a relay);
 * a POP3 connection dropped after DELE, before QUIT, deletes nothing;
 * a wrong password refused *)
val tests : < Cap.network ; .. > -> Testo.t list
