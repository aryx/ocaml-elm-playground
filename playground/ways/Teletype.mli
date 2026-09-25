(* Teletype: the programs of the teletype (Talk.mli), on the Playground.

   A Talk program is a conversation written down as a value -- print,
   read a line, carry on with the answer -- and a Talk.machine plays it
   on a Vt screen, its tty's line editing Line_discipline's. What the
   machine does not know is where its keys come from and where its
   screen goes: this module gives it the Playground's keyboard, draws
   its screen (a grid of characters in phosphor green, or black on a
   roll of paper), and makes the whole an app, [teletype program], with
   the flags seed=, baud= and paper.

   TinyTerminal steps its machine itself and draws it with [draw],
   inside a VT100; the Textmode way draws its own screens with
   [draw_screen] and reads [keyboard_bytes]. *)

(*****************************************************************************)
(* {1 The keyboard and the screen} *)
(*****************************************************************************)

(* the bytes a Playground frame typed: [computer.keyboard.typed], and
   the named keys that went down since the last frame (Enter,
   Backspace, the arrows, Control and a letter), in Vt's bytes; with
   Alt held, Escape before each (Meta, as terminals send it) *)
val keyboard_bytes : Playground.computer -> before:Playground.keyboard -> string

(* the width and height [draw]'s grid of characters takes, centered on
   (0, 0): what a case drawn around it needs *)
val size : Playground.computer -> Talk.machine -> Playground.number * Playground.number
val screen_size : Playground.computer -> Vt.t -> Playground.number * Playground.number

(* [draw ?paper ?capitals ?phosphor computer m]: the grid of
   characters, as large as the playground's screen lets it be, centered:
   a cell per character, in [phosphor] (green by default) on black, or
   with [paper] black on a roll of paper; in [capitals] (by default with
   [paper]) as a Teletype Model 33 printed and an Apple II showed them,
   neither having lower case *)
val draw :
  ?paper:bool -> ?capitals:bool -> ?phosphor:Playground.color -> Playground.computer -> Talk.machine -> Playground.shape list

(* [draw]'s grid for any screen, the cursor blinking where the screen
   has it if [cursor] (the Textmode way's, whose screen no machine
   feeds). With [pc], the IBM PC's colours instead of a phosphor's:
   the CGA's sixteen, bold the bright ones (Turbo Pascal's blue, grey
   and yellow). Box-drawing characters, ─ │ ┌ and their double
   twins ═ ║ ╔, are drawn as lines. *)
val draw_screen :
  ?paper:bool ->
  ?capitals:bool ->
  ?phosphor:Playground.color ->
  ?pc:bool ->
  cursor:bool ->
  Playground.computer ->
  Vt.t ->
  Playground.shape list

(*****************************************************************************)
(* {1 Applications} *)
(*****************************************************************************)

type state

(* [teletype program]: the program on an 80 by 24 screen; when it
   ends, Enter runs it again (with the seed where the last run left
   it: another game). Flags: seed=n, baud=n (110: the Teletype's 10
   characters a second), paper. [view] draws the machine instead of
   [draw] (TinyTerminal: a VT100 around the screen). *)
val teletype :
  ?rows:int ->
  ?cols:int ->
  ?view:(Playground.computer -> Talk.machine -> Playground.shape list) ->
  unit Talk.talk ->
  (state Playground.game, Playground.msg) Playground.app
