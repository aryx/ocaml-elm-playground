(* networking/unix/Irc_server over localhost, two clients: registered
 * (001), a nick taken (433), both in #ocaml (the JOIN announced, 353),
 * a line to the channel reaching the other only, a line to a nick, one
 * quitting (the other told) *)
val tests : < Cap.network ; .. > -> Testo.t list
