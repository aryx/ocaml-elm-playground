(* HyperTalk: the language of HyperCard (Bill Atkinson and Dan Winkler,
 * Apple, 1987), written to be read aloud by people who did not think of
 * themselves as programmers:
 *
 *   on mouseUp
 *     add 1 to field "Count"
 *     if field "Count" > 9 then answer "Ten clicks!"
 *   end mouseUp
 *
 * A script is a list of **handlers**, each answering one message
 * ("mouseUp", "openCard", or any word you make up). What HyperCard
 * adds to an ordinary language is where the messages go: a click on a
 * button sends "mouseUp" to the button, and if the button's script has
 * no handler for it -- or its handler says [pass mouseUp] -- it goes on
 * to the card, then the background, then the stack:
 *
 *   button  ->  card  ->  background  ->  stack  ->  (HyperCard: nothing)
 *
 * So a handler in the card's script answers every button on the card
 * that does not answer for itself, and one in the stack's script every
 * card: behaviour shared by being further up the path, which is
 * inheritance without classes. And a line that is just a word sends
 * that word as a message, from the object whose script is running, up
 * the same path -- which is how you make up your own commands.
 *
 * Every value is a string, as in the shell: "3" + 4 is 7, and 7 & "up"
 * is "7up". A number is shown without ".0" when it is whole. A word
 * that is not a variable yet stands for itself, so [put hello into
 * field "A"] puts "hello" -- HyperTalk's forgiveness, kept.
 *
 * The subset here: handlers; put (into, after, before), add, subtract,
 * go (next, prev, first, last, card by name or number), answer, beep,
 * if/then/else (on one line or as a block ending "end if"), repeat n
 * times, repeat with i = a to b, pass, and messages; expressions with
 * & and && (joined with a space), + - * / mod, = <> < > <= >= (and
 * "is", "is not", "contains"), and, or, not; field "Name", the number
 * of cards, the number of this card, the name of this card, return,
 * empty, space, quote, true, false. Comments start with "--". Words
 * are not case-sensitive. "card field" and "bg field" are both just
 * a field found by its name.
 *
 * It knows nothing of cards: what "field", "go" and "answer" do is the
 * caller's [world], a record of functions threaded through as values,
 * so that a test can run a script against three strings in a list. *)

type script

exception Error of string

(* the handlers of a script, or Error saying which line is wrong *)
val parse : string -> script

(* the messages a script has a handler for *)
val handlers : script -> string list

type card_ref = Next | Prev | First | Last | Named of string | Numbered of int

(* what the scripts can see and do *)
type 'w world = {
  get_field : 'w -> string -> string; (* Error if there is no such field *)
  set_field : 'w -> string -> string -> 'w;
  go : 'w -> card_ref -> 'w;
  answer : 'w -> string -> 'w;
  beep : 'w -> 'w;
  number_of_cards : 'w -> int;
  card_number : 'w -> int; (* from 1 *)
  card_name : 'w -> string;
}

(* [send world path message w]: the message sent along [path] -- the
 * scripts from the target up, e.g. [button; card; background; stack]
 * -- to the first one with a handler for it, and on up the path from
 * there if that handler passes it. Error on a mistake at run time
 * (a word where a number was needed, a missing field, a handler that
 * calls itself without end). *)
val send : 'w world -> script list -> string -> 'w -> 'w
