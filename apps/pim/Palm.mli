(* Palm: the device TinyPalmPilot's applications run on -- a 160 x 160
   screen in greys, the stylus, and the four databases they share.

   The Pilot's screen (1996) is 160 x 160 dots on a greenish LCD, drawn
   here 4 times larger, its coordinates the Palm's own: from the top
   left, y down, as its programs wrote them.

       (0,0) +--------------------------------+
             | Date Book |     S M T W T F S  |  <- the title tab, inverted
             |--------------------------------|
             |  8:00 Standup                  |
             |  9:00                          |  <- a line is 11 dots
             |  ...                           |
             | [New] [Details]                |  <- the buttons, y = 147
             +--------------------------------+ (160,160)

   Each application is a module over [data] (Pim_date_book,
   Pim_address, Pim_todo, Pim_memo) and draws with the functions here,
   in these coordinates; TinyPalmPilot places the screen on the case.

   The databases are the Versit formats' own types (appkits/pim): the
   Date Book's events and the To Do's items are Ics's, the addresses
   are Vcard's cards -- so each could be exported as a file any
   calendar or address book reads, which is what HotSync did. *)

(*****************************************************************************)
(* {1 The data} *)
(*****************************************************************************)

type data = {
  events : Ics.event list;
  cards : Vcard.card list;
  todos : Ics.todo list;
  memos : string list;
  next_id : int; (* for the next record's uid *)
}

(* a few records of each kind, dated around [today] (a day number) *)
val sample : int -> data

(* [uid data]: a fresh uid, and the data counting it *)
val uid : data -> string * data

(*****************************************************************************)
(* {1 The stylus and the keys} *)
(*****************************************************************************)

(* one frame's input, in screen dots: where the stylus came down (not
   where it is held), what was typed, and the keys pressed this frame *)
type input = {
  tap : (float * float) option;
  typed : string;
  enter : bool;
  backspace : bool;
  tab : bool;
  up : bool;
  down : bool;
  today : int; (* the local day number *)
}

(*****************************************************************************)
(* {1 The screen} *)
(*****************************************************************************)

val size : float (* 160 *)
val dots : float (* the screen's dots per Palm dot: 4 *)

(* the screen's center, in playground coordinates *)
val center : float * float

(* a Palm point, (x, y) from the top left, in playground coordinates *)
val to_screen : float * float -> float * float

(* the Palm point under a playground point, if on the screen *)
val of_screen : float * float -> (float * float) option

(* the LCD's greys, lightest first *)
val paper : Playground.color
val light : Playground.color
val mid : Playground.color
val ink : Playground.color

(* a box, (x, y, w, h), its top left in Palm dots *)
type box = float * float * float * float

val inside : box -> float * float -> bool

(* a rectangle filled *)
val rect : Playground.color -> box -> Playground.shape

(* [text ~x ~y s]: [s] from [x], the line's top at [y] (a line is 11
   dots); [bold] the Palm's bold font *)
val text : ?color:Playground.color -> ?bold:bool -> x:float -> y:float -> string -> Playground.shape

(* [s] from the right edge [x] *)
val text_right : ?color:Playground.color -> x:float -> y:float -> string -> Playground.shape

(* how wide [s] is, in dots; and [s] cut to fit [w] dots *)
val width : string -> float
val fit : float -> string -> string

(* the title tab (inverted, top left) and the line under it *)
val title : string -> Playground.shape list

(* the buttons along the bottom, from the left: each its box, to hit *)
val buttons : string list -> (string * box) list
val draw_buttons : (string * box) list -> Playground.shape list

(* which button was tapped, if any *)
val tapped : input -> (string * box) list -> string option

(* a check box at [(x, y)], ticked or not *)
val checkbox : x:float -> y:float -> bool -> Playground.shape list

(* the line's row under a tap: [(first, n)] rows of 11 dots from [y0] *)
val row_at : y0:float -> rows:int -> input -> int option

(* the text caret, blinking, after [s] drawn from [x] *)
val caret : time:float -> x:float -> y:float -> string -> Playground.shape list
