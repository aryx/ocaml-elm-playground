(* Mbox: a mailbox, the messages one after the other in one file
   (Unix's mail, Version 6, 1975; still what mutt and Thunderbird read).

       From alice@tiny Fri Sep 25 12:00:00 2026      <- "From ", a space:
       From: Alice <alice@tiny>                         a new message
       Subject: lunch                                   starts (the colon
                                                        of "From:" makes
       Noon at the usual place?                         it a header)
                                                     <- an empty line
       From carol@tiny Thu Sep 24 17:40:00 2026         ends each message
       ...

   The "From " line is the *envelope's* sender (the one SMTP's MAIL
   FROM gave, Smtp.mli), not the From: header -- a forged message shows
   the difference -- and the date it arrived, in C's asctime format.

   So a body line starting with "From " would start a new message; it
   is written ">From " instead, the "From munging" everyone has seen in
   a quoted email ("the mailer put a > before my From"). The first
   mboxes (mboxo) munged only "From ", which cannot be undone: was
   ">From " in a file a "From " or a ">From "? This one is *mboxrd*
   (Rahul Dhesi, 1995): any number of ">" before "From " gets one more,
   and reading takes one away, so every body comes back as it was.

   Worked example (checked by the tests): a message whose body has the
   lines "From the desk of Alice" and ">From the desk of Bob" is written
   with ">From the desk of Alice" and ">>From the desk of Bob", read back
   as it was; the mailbox of the two messages above reads as two, and
   writes back as the same bytes.

   References: "mbox" in the Unix manuals (mail(1), 1975); RFC 4155,
   "The application/mbox Media Type" (2005); Jamie Zawinski, "mbox
   From_ lines" (the four variants, and the damage). *)

(* a message and its envelope: the "From " line's rest
 * ("alice@tiny Fri Sep 25 12:00:00 2026") *)
type entry = { envelope : string; mail : Mail.t }

val parse : string -> entry list
val to_string : entry list -> string

(* [envelope ~sender date]: "alice@tiny Fri Sep 25 12:00:00 2026" *)
val envelope : sender:string -> Mail.date -> string

(* the envelope's sender: its first word *)
val sender : entry -> string

(* mboxrd's quoting of a body, and its undoing *)
val escape : string -> string
val unescape : string -> string
