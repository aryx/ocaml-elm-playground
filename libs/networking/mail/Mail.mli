(* Mail: a message, as RFC 822 wrote it (David Crocker, 1982) and RFC
   5322 still does: header fields, an empty line, a body.

       From: Alice <alice@tiny>                 <- a field: a name, a colon,
       To: bob@tiny                                a value
       Subject: lunch
       Date: Fri, 25 Sep 2026 12:00:00 +0200
       Message-ID: <1@tiny>
       Received: from eudora by tiny;           <- a long value folded: a line
         Fri, 25 Sep 2026 12:00:01 +0200           starting with a space goes
                                                   on with the one before
                                                <- the empty line: headers end
       Noon at the usual place?                 <- the body, lines of text

   That is all a message is, and it has not changed since 1982: what
   came later (MIME, Mime.mli) put everything else *inside* this
   shape rather than change it. A field can appear twice (every server
   the message passed through adds a Received: at the top), so the
   fields are a list, in order, not a table; and their names are looked
   up without regard to case ("message-id" finds Message-ID).

   A field keeps its value as written, folding and all ([raw]), so that
   a message read and written again is the same bytes -- a mailbox
   rewritten after marking one message read must not change the others.
   [get] unfolds it: the line breaks removed, the space that followed
   kept (RFC 5322, 2.2.3 -- unlike iCalendar's folding, Ics.mli, where
   the space is removed too, which is why the two do not share code).

   Lines end in CR LF on the wire and in LF in a file; [parse] takes
   either and keeps LF, and the protocols (Smtp, Pop3) put the CRs back.

   Worked example (checked by the tests): the message above parses to
   six fields; [get m "subject"] is "lunch"; [get m "received"] is
   "from eudora by tiny; Fri, 25 Sep 2026 12:00:01 +0200"; the From:
   is the address { display = "Alice"; mailbox = "alice@tiny" }; the
   date is 2026-09-25 at 12:00:00, 120 minutes east of Greenwich, which
   is 10:00 in Greenwich; and [to_string] gives back the same bytes.

   References: RFC 822, "Standard for the format of ARPA Internet text
   messages" (1982); RFC 5322, "Internet Message Format" (2008), its
   section 3.4 the addresses and 3.3 the dates. *)

(*****************************************************************************)
(* {1 A message} *)
(*****************************************************************************)

(* a header field: its name as written, and everything after the colon
 * as written, folding kept: [{ name = "To"; raw = " bob@tiny" }] *)
type field = { name : string; raw : string }

type t = { fields : field list; body : string }

(* the text of a message, CR LF or LF; the headers end at the first
 * empty line, or at a line that is neither a field nor a continuation
 * (which is then the body's first line) *)
val parse : string -> t

(* the message written back, lines ending in LF *)
val to_string : t -> string

(* [get m name]: the first field of that name, any case, its value
 * unfolded and trimmed *)
val get : t -> string -> string option

(* every field of that name, in order *)
val get_all : t -> string -> string list

(* [set name value m]: the first field of that name given [value]
 * (the others removed), or a new one added at the end *)
val set : string -> string -> t -> t

(* [remove name m]: every field of that name gone *)
val remove : string -> t -> t

(* [make fields body]: a message from names and values *)
val make : (string * string) list -> string -> t

(* a value's line breaks removed: "a\n  b" is "a  b" *)
val unfold : string -> string

(* the wire's CR LF line ends as a file's LF *)
val lf : string -> string

(*****************************************************************************)
(* {1 Addresses} *)
(*****************************************************************************)

(* "Alice <alice@tiny>": the display name and the mailbox; "alice@tiny"
 * alone has an empty display name; the old form "alice@tiny (Alice)"
 * reads the same as the first *)
type address = { display : string; mailbox : string }

val address : string -> address option

(* a list of them, separated by commas -- not the commas inside quotes,
 * angle brackets or parentheses: "\"Smith, J\" <j@tiny>, bob@tiny" is
 * two *)
val addresses : string -> address list

(* the display name quoted when it has to be *)
val address_to_string : address -> string

(* the name to show: the display name, else the mailbox *)
val who : address -> string

(* "\"Smith, J\"" is "Smith, J" (trimmed), anything else as it is *)
val unquote : string -> string

(*****************************************************************************)
(* {1 Dates} *)
(*****************************************************************************)

(* "Fri, 25 Sep 2026 12:00:00 +0200": the day and time where it was
 * written, and that place's offset, in minutes east of Greenwich *)
type date = { day : Civil.date; time : Clock.time_of_day; offset : int }

(* the weekday optional, the seconds optional, two-digit years (49 is
 * 2049, 50 is 1950), and the old zone names (GMT, EST, PDT...) *)
val date : string -> date option

val date_to_string : date -> string

(* seconds since the epoch, to sort by *)
val seconds : date -> float

(*****************************************************************************)
(* {1 Message-IDs} *)
(*****************************************************************************)

(* the ids in a Message-ID, In-Reply-To or References value:
 * "<1@tiny> <2@tiny>" is ["1@tiny"; "2@tiny"] *)
val message_ids : string -> string list
