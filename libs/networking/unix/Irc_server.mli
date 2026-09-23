(* Irc_server: an IRC server small enough to read in one sitting.

   What an IRC server does (Irc.mli for the messages): it keeps who is
   connected, by nickname, and which channels exist, with who is in
   each; a line to a channel goes to everyone in it but the sender, a
   line to a nick goes to that user. Channels come into being when the
   first user joins, and go away with the last.

     users:     alice -> #ocaml #tiny      channels:  #ocaml -> alice bob
                bob   -> #ocaml                       #tiny  -> alice

   The commands understood: NICK (433 if taken), USER (with NICK, the
   registration: 001, the welcome), JOIN and PART (announced to the
   channel, then 353 and 366, the names in it), PRIVMSG to a channel or
   a nick (401 if nobody), NAMES, PING (PONG), QUIT (announced to
   whoever shared a channel), and 421 for the rest. No modes, no
   operators, no servers linked into a network (the "relay" of Internet
   Relay Chat: servers passing messages to each other, a spanning tree
   of them -- here one server is the whole network).

   Over WebSocket (Server.mli), a message a frame, as the IRCv3
   WebSocket specification has it, so that a browser can connect (a web
   page has no plain TCP); a usual IRC client (irssi, weechat) speaks
   plain TCP, and can't, yet. The port is IRC's, 6667.

   Reference: RFC 1459 (1993), sections 4 (the commands) and 6 (the
   numeric replies); RFC 2810, "Internet Relay Chat: Architecture"
   (2000), the spanning tree of servers. *)

type t

(* a server listening on [bind]:[port] (127.0.0.1:6667), and the port
 * it got *)
val create : < Cap.network ; .. > -> ?bind:string -> ?port:int -> unit -> t * int

(* the events of now, answered *)
val step : t -> unit

(* sleep until a client has something, or [timeout] seconds *)
val wait : t -> float -> unit

(* the nicks registered; the channels, each with its nicks *)
val nicks : t -> string list
val channels : t -> (string * string list) list
