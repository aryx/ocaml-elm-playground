(* Vcard: address book cards, read and written (vCard 3.0, the subset
   an address book uses).

   The business card as a file (the Versit consortium, 1995; RFC 2426
   for 3.0, 1998), the format every address book and phone exchanges,
   iCalendar's sibling: the same lines, folded the same way, the same
   NAME;PARAM=value:value -- read here with Ics.mli's layers:

       BEGIN:VCARD
       VERSION:3.0
       N:Lovelace;Ada;;;                    <- family;given;additional;prefix;suffix
       FN:Ada Lovelace                      <- the name as shown
       ORG:Analytical Engine Society
       TEL;TYPE=home,voice:+44 20 7946 0000
       EMAIL;TYPE=internet:ada@example.org
       ADR;TYPE=home:;;12 St James's Square;London;;SW1Y 4JH;England
       BDAY:1815-12-10
       END:VCARD

   What's new next to iCalendar: *structured* values, N and ADR, whose
   parts are separated by ';' -- and a ';' inside a part is escaped,
   "\;", so a value is split on the unescaped ones first and each part
   unescaped after ([structured]); the other order would cut
   "Doe\;Smith" in two.

   Read leniently: unknown properties skipped (PHOTO, URL, X-...),
   vCard 2.1's bare parameters understood (TEL;HOME;VOICE:...), an ADR's
   post office box and extended address read and dropped. Not read:
   2.1's quoted-printable and other encodings, vCard 4.0's differences.

   Worked example: RFC 2426's own (section 7), Frank Dawson's and Tim
   Howes's cards, an address folded across two lines, in the tests. *)

(* N's parts *)
type name = { family : string; given : string; additional : string; prefix : string; suffix : string }

(* [kinds]: TYPE's values, lowercased: "home", "work", "cell", "fax",
 * "pref", ... *)
type phone = { number : string; kinds : string list }
type email = { address : string; kinds : string list }

(* ADR's parts, less the post office box and the extended address *)
type address = {
  street : string;
  locality : string; (* the city *)
  region : string; (* the state *)
  code : string;
  country : string;
  kinds : string list;
}

type card = {
  uid : string;
  name : name;
  full_name : string; (* FN *)
  org : string;
  title : string;
  phones : phone list;
  emails : email list;
  addresses : address list;
  birthday : Civil.date option;
  note : string;
}

(* a card with nothing in it but [full_name] *)
val make : string -> card

(* the cards of a text, in order; never raises *)
val of_string : string -> card list

(* the text of the cards, CRLF line ends, folded *)
val to_string : card list -> string

(* [structured "Doe\\;Smith;John;;;"]: ["Doe;Smith"; "John"; ""; ""; ""],
 * split on the unescaped ';' then each part unescaped, and trimmed
 * (RFC 2426's own example writes "; 94043") *)
val structured : string -> string list

(* the name to show: FN, or else the given and family names *)
val display_name : card -> string

(* the Palm's order: by family name, then given name, ignoring case;
 * a card with no N by its FN *)
val compare_by_name : card -> card -> int
