(* Mail_server: a mail server small enough to read in one sitting --
   mail taken by SMTP (Smtp.mli), kept, and given by POP3 (Pop3.mli).

       alice's client --SMTP--> [ the server: a maildrop per user ] --POP3--> bob's client
                        MAIL FROM, RCPT TO,            bob -> [m1; m2]            USER, PASS,
                        DATA                           carol -> []                RETR, DELE

   The SMTP side is the post office's counter: it takes a message for
   anyone of its domain ("bob@tiny", or just "bob"), adds a Received:
   line at the top saying from whom and when (every server a message
   crosses adds one: read bottom up, they are its route), and puts it
   in the recipient's maildrop -- a user comes into being with the
   first mail they receive, as a channel does with its first user in
   Irc_server. Mail for another domain is refused, 550: this server is
   not a *relay*. A server that took mail for anywhere and passed it on
   was an *open relay*, which every server was in the 1980s, trusting
   the others, until the spam of the 1990s made them all close.

   The POP3 side is the letter box: a user's messages, as they were
   when the session logged in, listed, fetched, and marked for
   deletion -- deleted only at QUIT, the *update state*, so that a
   connection dropped in the middle loses nothing. Any password is
   accepted unless [passwords] says otherwise (a teaching server on
   127.0.0.1).

   Each side listens twice (Server.mli): over WebSocket, a line a
   frame, as Irc_server does, so that a TinyEudora in a browser can
   connect -- a web page has no plain TCP -- on 8025 (SMTP) and 8110
   (POP3); and over plain TCP, a line each way, on 2525 and 1100 (the
   real ports, 25 and 110, are for root), for telnet and for the mail
   clients of the world:

       $ telnet localhost 2525
       220 tiny ESMTP tiny_maild
       HELO me
       250 tiny
       MAIL FROM:<alice@tiny>
       ...

   The server keeps nothing on disk: [changed] is called with a user's
   maildrop each time it changes, for tiny_maild to write it as an mbox
   file (flag spool=dir), and [maildrops] starts it with what was
   written. *)

type t

(* the ports: SMTP and POP3 over WebSocket, and over plain TCP *)
type ports = { smtp : int; pop : int; smtp_plain : int; pop_plain : int }

(* [create caps ~domain ?passwords ?maildrops ?changed ()]: the four
 * servers listening (8025, 8110, 2525, 1100 on 127.0.0.1 unless said
 * otherwise, 0 for free ports), and the ports they got *)
val create :
  < Cap.network ; .. > ->
  ?bind:string ->
  ?ports:ports ->
  ?domain:string ->
  ?passwords:(string * string) list ->
  ?maildrops:(string * Mbox.entry list) list ->
  ?changed:(string -> Mbox.entry list -> unit) ->
  unit ->
  t * ports

(* the events of now, on both sides, answered; [now]: the seconds since
 * the epoch, for the Received: lines and the envelopes *)
val step : t -> now:float -> unit

(* sleep until a client has something, or [timeout] seconds *)
val wait : t -> float -> unit

(* a user's messages *)
val maildrop : t -> string -> Mbox.entry list
