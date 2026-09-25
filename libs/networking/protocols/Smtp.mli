(* Smtp: sending mail, the Simple Mail Transfer Protocol (Jonathan
   Postel, RFC 821, 1982; RFC 5321, 2008).

   A client connects to a server and they talk, a line each way: the
   client a command, the server a reply that starts with three digits,
   the first of which is all a client needs to read -- 2 done, 3 go
   on, 4 not now (try later), 5 never. RFC 5321's own example
   (appendix D.1), which the tests replay:

       S: 220 foo.com Simple Mail Transfer Service Ready
       C: EHLO bar.com
       S: 250-foo.com greets bar.com        <- "250-": more lines follow
       S: 250-8BITMIME
       S: 250-SIZE
       S: 250-DSN
       S: 250 HELP                          <- "250 ": the last one
       C: MAIL FROM:<Smith@bar.com>
       S: 250 OK
       C: RCPT TO:<Jones@foo.com>
       S: 250 OK
       C: RCPT TO:<Green@foo.com>
       S: 550 No such user here             <- one recipient refused,
       C: RCPT TO:<Brown@foo.com>              the others still take it
       S: 250 OK
       C: DATA
       S: 354 Start mail input; end with <CRLF>.<CRLF>
       C: Blah blah blah...
       C: ....etc. etc. etc.                <- the RFC's "...etc.", sent:
       C: .                                    its dot doubled (below);
                                               a line of a dot: the end
       S: 250 OK
       C: QUIT
       S: 221 foo.com Service closing transmission channel

   Two things to see in it. The *envelope* -- MAIL FROM and RCPT TO --
   is not the message's headers: the server delivers to the RCPT TOs,
   whatever To: says. That is how Bcc: works (its recipients are in the
   envelope and nowhere in the message: [envelope] takes the Bcc: out),
   and how a mailing list works; and it is also why anybody can send
   mail as anybody: nothing checks MAIL FROM, nor From:, against
   anything (SPF, DKIM and DMARC, thirty years later, are the patch).
   And the *dot*: the message ends at a line of a single ".", so a line
   of the message that starts with a dot is sent with one more
   ([stuff]) and the server takes it off ([unstuff]) -- the same trick
   as mbox's ">From" (Mbox.mli).

   The client is a state machine, pure: [step] is given each line the
   server sends and answers with the lines to send back, so it can be
   driven by a socket, a WebSocket, or a test replaying the RFC. It
   sends several messages in one connection -- Eudora's Send Queued
   Messages, connecting once -- each accepted or refused on its own.

   References: RFC 5321, "Simple Mail Transfer Protocol" (John
   Klensin, 2008): 4.1 the commands, 4.2 the replies, 4.5.2 the
   transparency (the dot), appendix D the examples; RFC 821 (Jonathan
   Postel, 1982). *)

(*****************************************************************************)
(* {1 Replies and commands} *)
(*****************************************************************************)

(* a reply line: its code, whether more lines follow ("250-"), its text *)
val reply_line : string -> (int * bool * string) option

(* a reply, as the lines a server sends: "250-a", "250 b" *)
val reply : int -> string list -> string list

type command =
  | Helo of string
  | Ehlo of string
  | Mail_from of string (* the address, without its <> *)
  | Rcpt_to of string
  | Data
  | Rset
  | Noop
  | Quit
  | Unknown of string

(* a command line, the verb in any case *)
val parse_command : string -> command
val command_to_string : command -> string

(*****************************************************************************)
(* {1 The message} *)
(*****************************************************************************)

(* who from, who to, and the message as it is sent *)
type envelope = { sender : string; recipients : string list; text : string }

(* [envelope ~sender mail]: the recipients are To:, Cc: and Bcc:'s
 * addresses; the text is the message less its Bcc: *)
val envelope : sender:string -> Mail.t -> envelope

(* the text as the lines after DATA: a dot doubled where a line starts
 * with one, and the "." that ends it *)
val stuff : string -> string list

(* the lines received after DATA, less the final ".", as a text (LF) *)
val unstuff : string list -> string

(*****************************************************************************)
(* {1 The client} *)
(*****************************************************************************)

type client

(* what became of a message: sent to that many recipients (the others
 * refused, their replies kept), or refused whole *)
type outcome = Sent of int * string list | Refused of string

(* [client ~hello envelopes]: the machine, before the server's greeting *)
val client : hello:string -> envelope list -> client

(* [step c line]: the server said [line]; the machine, and what to send *)
val step : client -> string -> client * string list

(* the outcomes, in the envelopes' order, once the server said goodbye;
 * Error when it never said hello, or the connection is over with
 * messages untried *)
val finished : client -> (outcome list, string) result option
