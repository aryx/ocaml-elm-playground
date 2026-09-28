(* Curses: a screen of characters drawn as a whole, and sent to the
   terminal as what changed.

   A full-screen program (Rogue, vi, top) thinks in screens: this frame
   the player is here, the monsters there. The terminal, at the end of a
   wire, only takes bytes, and at 9600 baud -- 960 characters a second
   -- redrawing all of an 80 by 24 screen took two seconds. Ken
   Arnold's curses (Berkeley, 1980), written for Rogue, answered with
   the idea every UI library still uses (React's virtual DOM is the
   same): keep the screen the terminal shows, draw the next one in
   memory, and send only the difference.

       program ---draws---> the next screen  (a Curses.t)
                                  |
          the screen shown -----diff-----> bytes: cursor moves, colours,
          (a Curses.t)                     characters  ---> the terminal

   So a [t] is a value (drawing returns a new one), and [refresh ~before
   after] is the bytes that make a terminal showing [before] show
   [after]:

   - an unchanged cell costs nothing;
   - a changed cell costs its character, and a cursor move to it
     (ESC [ row ; col H, 6 to 8 bytes) unless the cursor is already
     there -- after the previous change on the same row;
   - between two changes on a row, the unchanged cells are sent again
     when that is cheaper than the move: a gap of 4 cells costs 4
     bytes, ESC [ 1 ; 6 H costs 6. The decision is a count of bytes,
     which is all an optimizing curses is (the real one also knows
     the terminal's insert and delete, Vt's CSI @ P L M: an exercise);
   - colours (SGR) are sent when they change from the last cell sent;
   - writing the last column of a row leaves a VT100's cursor there,
     its wrap pending (Vt.mli), so the next change always moves.

   Worked example (checked by the tests), on a row that showed CAT HAT,
   now BAT HOT: the changes are at columns 1 and 6, the gap between
   them "AT H", 4 bytes, cheaper than a move (ESC [ 1 ; 6 H, 6 bytes):

       refresh ~before after = ESC [ 1 ; 1 H  B A T space H O
                               6 bytes      +  6 bytes

   12 bytes for 2 characters changed, against 6 + 1 + 6 + 1 = 14 with
   two moves, and a whole row redrawn, 6 + 7. (Each refresh starts and
   ends with plain colours, so a screen of plain text needs no SGR.)

   References: Kenneth C. R. C. Arnold, "Screen Updating and Cursor
   Movement Optimization: A Library Package" (Berkeley, 1980); the
   ncurses sources' tty_update.c, its descendant. *)

(*****************************************************************************)
(* {1 Screens} *)
(*****************************************************************************)

type t

(* blank, no cursor shown *)
val create : rows:int -> cols:int -> t

val rows : t -> int
val cols : t -> int

(* [put row col text t]: [text] from (row, col), from 0, a cell per
   UTF-8 character, cut at the right edge; outside the screen,
   nothing *)
val put : ?attrs:Vt.attrs -> int -> int -> string -> t -> t

(* [box top left height width t]: a frame of + - and |, [height] rows
   and [width] columns counted with it *)
val box : ?attrs:Vt.attrs -> int -> int -> int -> int -> t -> t

(* where the cursor is shown, or None for hidden *)
val cursor : (int * int) option -> t -> t

val cell : t -> int -> int -> Vt.cell

(* the rows as text, trailing spaces removed: what the tests compare *)
val text : t -> string list

(*****************************************************************************)
(* {1 To the terminal} *)
(*****************************************************************************)

(* the bytes that make a terminal showing [before] show [after] *)
val refresh : before:t -> t -> string

(* the bytes that draw [t] on a terminal showing anything: clear, then
   everything (what a program sends first, and after a Control-L) *)
val redraw : t -> string
