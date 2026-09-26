(* Mail_thread: a mailbox's messages as conversations, by Jamie
   Zawinski's algorithm ("message threading", 1997, written for
   Netscape Mail 2.0 and published as an essay; RFC 5256 took it for
   IMAP's THREAD=REFERENCES).

   A reply says what it answers: In-Reply-To: the one message,
   References: the whole chain back to the first (Mail.mli). So
   threading is building a tree from those links -- but the links are
   in the messages you *have*, and they name messages you may not:

       <1> the plan                    <1> (never received)
         <2> Re: the plan    --->      +-- <2> Re: the plan
           <3> Re: the plan              +-- <3> Re: the plan
         <4> Re: the plan              +-- <4> Re: the plan

   The algorithm, in the essay's steps:

   1. A *container* per Message-ID, found or made, holding its message
      if we have it, empty if we only heard of it. Each message's
      References link the containers in a chain, parent to child (a
      container keeps its first parent; no link that would make a
      loop), and the message's own container becomes the child of the
      last one.
   2. The containers with no parent are the roots.
   3. Empty containers pruned: with no children, gone; with children,
      replaced by them -- except at the top with several children,
      where the empty one stays to hold them together (a thread whose
      first message you never got).
   4. The roots grouped by subject, for the mailers that sent no
      references at all: "Re: lunch" goes under "lunch" (the subject
      less its "Re:"s, [base_subject]).
   5. Brothers sorted by date.

   Worked examples (checked by the tests): TinyEudora's thread of five
   (Our_mail) is one tree, <plan1> at the top, <plan2> under it, and
   under that <plan3> (with <plan5> under it) and <plan4>; two replies
   to a message never received hang from an empty container; "Re:
   lunch" with no references goes under "lunch"; a message that names
   itself in its References makes no loop.

   References: Jamie Zawinski, "message threading",
   https://www.jwz.org/doc/threading.html (1997, 2002); RFC 5256,
   "Internet Message Access Protocol - SORT and THREAD Extensions"
   (2008), section 2.2 the same algorithm. *)

(* a thread: a message (None: one we never received, known only by
 * the replies naming it) and its replies *)
type 'a tree = Node of 'a option * 'a tree list

(* [threads ~id ~references ~subject ~date messages]: the conversations,
 * oldest first. [references]: the ids the message names, oldest
 * first -- References:, then In-Reply-To:'s if not already there *)
val threads :
  id:('a -> string option) -> references:('a -> string list) -> subject:('a -> string) -> date:('a -> float) -> 'a list -> 'a tree list

(* the subject less its "Re:", "RE:", "Re[2]:", "Fwd:"s, trimmed, in
 * lower case: what a reply and its original share *)
val base_subject : string -> string

(* does the subject say it is a reply ("Re: ...")? *)
val is_reply : string -> bool

(* the trees flattened, depth first: each message with its depth *)
val flatten : 'a tree list -> ('a * int) list

(* the same for mail: the ids from Message-ID:, References: and
 * In-Reply-To:, the subject decoded, the date *)
val of_mail : ('a -> Mail.t) -> 'a list -> 'a tree list
