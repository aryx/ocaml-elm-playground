(* Ics: iCalendar files, read and written (the subset a calendar and a
   to-do list use).

   The format every calendar exchanges since 1998 (RFC 2445, now
   5545): text, a property per line, components between BEGIN and END,
   the same shape as vCard, the address books' format, from the same
   Versit consortium:

       BEGIN:VCALENDAR
       VERSION:2.0
       PRODID:-//ocaml-elm-playground//Tiny//EN
       BEGIN:VEVENT
       UID:1@tiny
       DTSTAMP:20260924T120000Z
       DTSTART:20260928T090000               <- floating: 9:00 wherever
       DTEND:20260928T100000
       RRULE:FREQ=WEEKLY;BYDAY=MO,TH         <- Recur.mli
       SUMMARY:Standup\, then coffee         <- a comma escaped
       END:VEVENT
       BEGIN:VTODO
       UID:2@tiny
       SUMMARY:Write the plan
       DUE;VALUE=DATE:20261001               <- a date, no time
       PRIORITY:1
       END:VTODO
       END:VCALENDAR

   Three layers, each in its function below:
   - lines: a line longer than 75 bytes is *folded*, cut and continued
     on the next line after a space ([fold], [unfold]), the line
     length limit of the e-mails the format was meant to travel in;
   - a content line: NAME;PARAM=value;...:value ([content_line]), text
     values escaping \ ; , and newlines ([escape]);
   - values: dates (20260928), date-times (20260928T090000, with a Z if
     UTC), and RRULE's rules ([rule_of_string]).

   Read leniently, as the RFC asks: unknown properties and components
   (VTIMEZONE, an event's VALARM) are skipped, a component missing
   what it needs (an event without DTSTART) is dropped. A TZID is read
   as floating time: this program has no tz database (plan_pim.md).

   Worked example: RFC 5545's first example (section 4), a conference
   with a description folded over three lines, in the tests. *)

(* a date, or a date and a time of day (seconds after midnight), UTC
 * or floating (the same clock time wherever you are) *)
type moment = { date : Civil.date; time : int option; utc : bool }

type event = {
  uid : string;
  summary : string;
  description : string;
  location : string;
  start : moment;
  end_ : moment option;
  rrule : Recur.rule option;
}

type todo = {
  uid : string;
  summary : string;
  due : moment option;
  priority : int; (* 1 the highest .. 9, 0 none *)
  completed : bool;
}

type calendar = { events : event list; todos : todo list }

(* the file's text, leniently (see above); never raises *)
val of_string : string -> calendar

(* [to_string ~stamp cal]: the file's text, CRLF line ends, folded.
 * [stamp] is when it is written (every VEVENT's required DTSTAMP): a
 * library reads no clock, the caller gives the time *)
val to_string : stamp:moment -> calendar -> string

(* {2 The layers} *)

(* the logical lines of a text: CRLF or LF ends, a line starting with
 * a space or a tab continuing the one before (without that character);
 * empty lines dropped *)
val unfold : string -> string list

(* a logical line cut into lines of at most 75 bytes, joined by CRLF
 * and a space, never inside a UTF-8 character *)
val fold : string -> string

(* [content_line "DTSTART;VALUE=DATE:20261001"]:
 * ("DTSTART", [("VALUE", "DATE")], "20261001"), the names uppercased,
 * a quoted parameter value's ':' and ';' not ending it *)
val content_line : string -> (string * (string * string) list * string) option

(* TEXT values: \\ \; \, and \n (or \N) for a newline *)
val escape : string -> string
val unescape : string -> string

val moment_of_string : string -> moment option
val moment_to_string : moment -> string

(* RRULE's value: "FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE,FR;UNTIL=19971224T000000Z";
 * None for what Recur.mli leaves out (an ordinal BYDAY, BYSETPOS, ...) *)
val rule_of_string : string -> Recur.rule option
val rule_to_string : Recur.rule -> string
