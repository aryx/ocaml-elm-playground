(* Vt: a terminal, the screen at the end of the wire.

   A program on Unix doesn't draw text: it writes bytes, and the
   terminal decides what they look like. Most bytes are characters,
   put at the cursor, which then moves right. A few are orders: CR
   sends the cursor back to the first column, LF down a line (and
   when it is already on the last line, the whole screen scrolls up),
   BS back a column, BEL rings. Everything else a program asks of the
   screen -- go to row 3, column 5; erase the line; write in red --
   is an *escape sequence*: ESC, then a few characters saying what to
   do.

   The sequences are the DEC VT100's (1978), the terminal that made
   them standard (ANSI X3.64, then ECMA-48): xterm, the macOS Terminal
   and every terminal emulator today still speak them, so what this
   module understands, `ls --color`, `top` or `vi` can use. The most
   common is the CSI ("control sequence introducer", ESC [), then
   numbers separated by semicolons, then one letter saying what to do
   with them:

       ESC [ 3 ; 5 H        the cursor to row 3, column 5 (from 1)
       ESC [ 2 J            erase the whole screen
       ESC [ 1 ; 3 1 m      from now on, bold and red
       ESC [ 0 m            back to plain

   Reading them is a state machine, one byte at a time, because a
   sequence can arrive cut in two (half in this read, half in the
   next); the states are Paul Williams' (vt100.net, "A parser for
   DEC's ANSI-compatible video terminals"), fewer of them:

                  ESC                  [
       GROUND ---------> ESCAPE -------------> CSI  0-9 ; ? collected
         ^  ^              |   ]                |
         |  |   7 8 D M E c|   +---> OSC        | a letter (@ to ~):
         |  +--------------+          | BEL,    | do it
         |    done at once            | ESC \   |
         +----------------------------+---------+

       In any state, a control character (CR, LF, BS...) is done at
       once without leaving it, CAN or SUB abandon the sequence, and
       ESC starts another.

   OSC ("operating system command", ESC ] ... BEL) is recognized only
   to be ignored: shells use it to set the window's title, and a
   terminal that doesn't know it must not print it. That is the rule
   of the whole module: a sequence it doesn't implement is read to its
   end and dropped, never shown as garbage.

   Two subtleties every terminal emulator ends up getting right:

   - **The deferred wrap.** Writing in the last column leaves the
     cursor *on* it, with a "wrap pending" flag; only the next
     character wraps to the next line. Otherwise a line of exactly 80
     characters followed by CR LF, which is what every program
     writes, would leave a blank line: the wrap, then the LF.

         4 columns, "abcd\r\nef":   abcd        not   abcd
                                    ef                (blank)
                                                      ef

   - **Characters are UTF-8.** A byte is not a character: "é" is two
     bytes, "─" (a box-drawing line) three. They are collected and put
     in one cell; a byte that can't be UTF-8 shows U+FFFD.

   Worked example (checked by the tests), on a screen of 4 rows of 16:

       ESC [ 2 J  ESC [ H                    clear, cursor home
       Hello, ESC [1;31m world ESC [0m !     "world" bold and red
       CR LF                                 next line
       ESC [ 3 ; 5 H  x                      row 3, column 5

         +----------------+
         |Hello, world!   |     "world": bold, red
         |                |
         |    x_          |     the cursor after the x:
         |                |     row 2, column 5, counted from 0
         +----------------+

   What is understood: the controls BEL BS HT LF VT FF CR; ESC 7 and
   ESC 8 (save and restore the cursor), ESC D, ESC M, ESC E (index,
   reverse index, next line), ESC c (reset); CSI A B C D E F G d H f
   (the cursor), J K X (erasing), L M @ P (inserting and deleting
   lines and characters, what vi and curses use to avoid redrawing),
   r (the scrolling region), s u (save, restore), m (SGR: bold,
   reverse, the 8 colours, 90-97 as bold), ?25h ?25l (show, hide the
   cursor). Left out: the alternate screen (?1049h, where vi and less
   draw so that quitting restores the shell's screen), underline and
   blink, 256 and 24-bit colours (read and ignored), double-width
   characters, tab stops other than every 8 columns, the character
   sets (ESC ( 0, the VT100's line-drawing set; UTF-8 has its lines),
   and the scrollback: a line scrolled off the top is gone.

   References: DEC, "VT100 User Guide" (EK-VT100-UG, 1978), chapter 3;
   ECMA-48 (5th edition, 1991); Paul Williams, "A parser for DEC's
   ANSI-compatible video terminals" (vt100.net); the xterm control
   sequences (Edward Moy, Stephen Gildea, Thomas Dickey, "ctlseqs"). *)

(*****************************************************************************)
(* {1 Cells} *)
(*****************************************************************************)

(* the 8 colours of SGR 30-37 and 40-47, and the terminal's own *)
type color = Default | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type attrs = { fg : color; bg : color; bold : bool; reverse : bool }

(* a character, as its UTF-8 bytes ("a", "é", "─"), and how it looks *)
type cell = { glyph : string; attrs : attrs }

val plain : attrs

(* a space, plain *)
val blank : cell

(*****************************************************************************)
(* {1 The screen} *)
(*****************************************************************************)

type t

(* blank, the cursor at the top left *)
val create : rows:int -> cols:int -> t

(* the screen after these bytes; [t] itself is unchanged *)
val feed : t -> string -> t

val rows : t -> int
val cols : t -> int

(* row and column from 0; [blank] outside the screen *)
val cell : t -> int -> int -> cell

(* row and column from 0 *)
val cursor : t -> int * int
val cursor_visible : t -> bool

(* how many times BEL rang since [create]: a view flashes when it grows *)
val bells : t -> int

(* the screen as text, a string per row, trailing spaces removed: what
   the tests compare *)
val text : t -> string list

(*****************************************************************************)
(* {1 The keyboard} *)
(*****************************************************************************)

(* The bytes a key sends, for the keys that aren't characters: [key]
   takes the browser's name of a key (the Playground's, "Enter",
   "ArrowUp", "Backspace") and whether Control is held. Enter sends
   CR, Backspace DEL (0x7F), an arrow ESC [ A to ESC [ D, Control and a
   letter the letter's control code (Control-C is 0x03, ETX: the
   "interrupt" of Line_discipline), Control and Space NUL and Control
   and / 0x1F, as xterm sends them; F5 to F12 as xterm numbers them (F9
   is ESC [ 20 ~), and with Control or [alt] held, a named key carries
   xterm's modifier (Control-F9 is ESC [ 20 ; 5 ~, Alt-F9 ESC [ 20 ; 3 ~,
   Alt-Backspace Escape then DEL). Printable characters are not keys
   here: they are what the keyboard typed. None for a key that sends
   nothing (Shift alone).

   The VT100 had a second set for the arrows (ESC O A, "application
   mode", asked for by programs like vi); this module always sends the
   first. *)
val key : ?alt:bool -> ctrl:bool -> string -> string option
