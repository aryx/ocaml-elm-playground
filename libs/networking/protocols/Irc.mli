(* Irc: Internet Relay Chat's messages, a line of text each.

   IRC (Jarkko Oikarinen, University of Oulu, 1988; RFC 1459, 1993;
   RFC 2812, 2000) is chat as a protocol anyone can read: every message
   a line of text, which a person could type into a telnet session --
   and people did. A client connects to a server, says who it is, joins
   channels (#names), and sends lines to a channel or a nick; the server
   passes them on to everyone in the channel.

       client                               server
       NICK alice                  ----->
       USER alice 0 * :Alice
                                   <-----   :tiny 001 alice :Welcome to TinyIRC, alice
       JOIN #ocaml                 ----->
                                   <-----   :alice!alice@tiny JOIN #ocaml
                                            :tiny 353 alice = #ocaml :alice bob
       PRIVMSG #ocaml :hello       ----->   (to bob:) :alice!alice@tiny PRIVMSG #ocaml :hello

   A message: an optional prefix, who it is from (after a ':'); the
   command, a word (PRIVMSG) or three digits (a *numeric* reply, 001 the
   welcome, 433 "nickname in use"); up to 15 parameters, separated by
   spaces; and the last one may contain spaces if it starts with ':',
   the *trailing* parameter. At most 512 bytes, CR LF included.

       :alice!alice@tiny PRIVMSG #ocaml :hello, world\r\n
       \________________/ \_____/ \____/ \___________/
            prefix        command  param   trailing

   Worked examples (checked by the tests): that line parses to the prefix
   "alice!alice@tiny", the command "PRIVMSG", the parameters "#ocaml"
   and "hello, world", and prints back the same; "PING :tiny" to the
   command PING and one parameter; a line without a command is refused.

   References: RFC 1459, "Internet Relay Chat Protocol" (Jarkko
   Oikarinen and Darren Reed, 1993), its section 2.3.1 the grammar; RFC
   2812, "Internet Relay Chat: Client Protocol" (Christophe Kalt, 2000);
   the IRCv3 WebSocket specification (a message a WebSocket frame, as
   here). *)

type message = {
  prefix : string option; (* who it is from: "alice!alice@tiny", or a server's name *)
  command : string; (* "PRIVMSG", or three digits: "001" *)
  params : string list; (* the last one may hold spaces (it was the trailing one) *)
}

(* a line (without its CR LF, or with it), parsed; Error if it has no
 * command or is longer than 512 bytes *)
val parse : string -> (message, string) result

(* the message as a line, without CR LF: the last parameter written as
 * trailing (after ':') when it needs to be -- spaces, empty, or a ':' of
 * its own *)
val print : message -> string

(* [msg ?prefix command params] *)
val msg : ?prefix:string -> string -> string list -> message

(* the nick of a prefix: "alice" of "alice!alice@tiny" *)
val nick_of : string -> string
