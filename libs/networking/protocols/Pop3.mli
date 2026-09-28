(* Pop3: fetching mail, the Post Office Protocol version 3 (John Myers
   and Marshall Rose, RFC 1939, 1996; POP, RFC 918, 1984).

   SMTP (Smtp.mli) brings mail to a server, which keeps each user's in
   a *maildrop*; a mail client that is not always on -- a Macintosh on
   a modem -- comes and takes it. As plain as SMTP: a command a line,
   and a reply that starts "+OK" or "-ERR". A reply of several lines
   (a list, a message) ends with a line of a single ".", and a line of
   the message starting with a dot has a second one, as in SMTP.
   RFC 1939's own example (section 10), which the tests replay -- with
   USER and PASS where the RFC has APOP, whose MD5 we do not have:

       S: +OK POP3 server ready <1896.697170952@dbc.mtview.ca.us>
       C: USER mrose
       S: +OK
       C: PASS tanstaaf                       <- in clear: why APOP (1993),
       S: +OK mrose's maildrop has 2 messages    and then TLS, had to come
       C: STAT
       S: +OK 2 320                           <- how many, how big
       C: LIST
       S: +OK 2 messages (320 octets)
       S: 1 120
       S: 2 200
       S: .
       C: RETR 1
       S: +OK 120 octets
       S: <the message>
       S: .
       C: DELE 1                              <- marked, not deleted yet
       S: +OK message 1 deleted
       C: RETR 2
       ...
       C: QUIT                                <- now: the *update state*
       S: +OK dewey POP3 server signing off

   The deletions happen only at QUIT, so a connection that drops in
   the middle loses nothing: the next session fetches the same
   messages again. And a client that leaves the mail on the server --
   Eudora's "Leave mail on server" checkbox -- sends no DELE, and asks
   UIDL for each message's unique id, to fetch only the ones it has
   not already got.

   The client is a state machine, pure, as Smtp's: [step] takes each
   line the server sends and answers the lines to send.

   References: RFC 1939, "Post Office Protocol - Version 3" (1996):
   section 10 the example, 7 UIDL; RFC 918 (Joyce Reynolds, 1984), the
   first POP. *)

(* "+OK text" is Ok "text", "-ERR text" Error "text" *)
val status : string -> (string, string) result option

(* a multi-line reply's lines, less the final "." and the added dots *)
val unstuff : string list -> string

(* the lines of a message sent by RETR, the dots added and the "." *)
val stuff : string -> string list

(*****************************************************************************)
(* {1 The client} *)
(*****************************************************************************)

type client

(* [client ~user ~pass ~leave ~known]: it fetches every message, or,
 * when [leave], the ones whose unique id is not in [known] and deletes
 * none; with [limit], only the last [limit] of them (the newest: a
 * maildrop is in the order mail arrived) -- Gmail's, the first time,
 * is years of mail *)
val client : user:string -> pass:string -> leave:bool -> known:string list -> ?limit:int -> unit -> client

(* [step c line]: the server said [line]; the machine, and what to send *)
val step : client -> string -> client * string list

(* once the server said goodbye: the messages fetched, each with its
 * unique id ("" when not asked), in the maildrop's order; Error when
 * the server refused (a wrong password: -ERR) *)
val finished : client -> ((string * string) list, string) result option
