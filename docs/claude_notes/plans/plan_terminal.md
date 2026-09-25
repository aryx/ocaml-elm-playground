# Plan: terminals -- the Teletype way, TinyTerminal, and what connects them

## Context

A family of games is missing from the catalogue: the ones of the
teletype and the terminal. They include Hangman, Guess the Number,
Hunt the Wumpus (Gregory Yob, 1973), Animal, Bagels, Nim, Star Trek
(Mike Mayfield, 1971) and The Oregon Trail (1971), most of them printed
in David Ahl's *101 BASIC Computer Games* (1973). ELIZA (1966) belongs
here too, and later Rogue (1980), the full-screen game of the terminal.

They are written in a style that the playground doesn't allow. A BASIC
game says `PRINT`, then `INPUT A$`, and waits: the program keeps its
place in the code while the player types. The playground's `update`
runs once per frame and must return at once, so a game that asks has
to become a state machine ("which question am I on?"). TinyHamurabi
and TinyZork both did this by hand. Each also built its own terminal:
typed characters from `computer.keyboard.typed`, Enter and Backspace
through `Scene2d`'s pressed keys, lines placed column by column. That is
two copies of the same code, and it will be copied again with every
new text game.

The author's idea (2026-09-25): a way in `playground/ways/` for these
games, and a TinyTerminal in `apps/system/` (its placeholder `dune`
already lists "a terminal and a shell"), with the two connected.

## The design: split where Unix splits

A terminal knows bytes and a program knows text. Between them is the
tty, which turns keys into lines. Each of the three becomes one piece,
and every piece is useful on its own:

```
  Teletype programs         TinyTerminal's shell       (native) a real shell, over a pty
  (Hangman, Wumpus...)      (help, ls, hangman)        (web) a pty behind a WebSocket
          \                         |                         /
           +------------ bytes out / bytes in ---------------+
                                    |
                       Line_discipline (cooked mode)
                   echo, Backspace, a line on Enter; raw mode
                                    |
                                   Vt
            bytes -> a grid of cells, a cursor, scrolling,
            the VT100's escape sequences; keys -> bytes
                                    |
                 a view: the grid as shapes, a cell per character
```

## 1. `libs/terminal/`: the terminal, pure

A new folder of `libs/`, library `terminal` (private, package
`elm_playground`, pure OCaml, no Playground). Like `networking/`, it
works on bytes in and bytes out, and knows nothing about shapes or a
`computer` (the `libs/README.md` rule). It is kept pure so that ix's
emulator can use it later for its serial console
(`project_tiny_xix`).

- **`Vt`**: the DEC VT100 (1978), the terminal whose escape sequences
  became ANSI X3.64, which every terminal emulator still speaks.
  - A screen is a grid of cells (a character, a foreground colour, a
    background colour, bold or reverse), plus a cursor and a scrolling
    region.
  - `feed : t -> string -> t` runs the bytes through a state machine.
    Printable characters go into the cells. The control characters are
    CR, LF, BS, TAB and BEL. Escape sequences are parsed in the usual
    states: ground, escape, CSI with its parameters.
  - The sequences a terminal game or `ls --color` needs:
    - cursor movement (CUU/CUD/CUF/CUB, CUP);
    - erasing (ED, EL);
    - SGR colours and attributes (the 8 colours and bold);
    - the scrolling region (DECSTBM);
    - save and restore of the cursor.
    Everything else is recognized and ignored, so an unknown sequence
    never prints garbage.
  - Its header draws the parser's states as an ASCII diagram, after
    Paul Williams' DEC-compatible parser (vt100.net), and gives the
    list of sequences handled against the ones left out.
  - `key : Keyboard.key -> string`: what a key sends. Arrows send
    `ESC [ A`, and so on; Enter sends CR; Ctrl-letter sends the control
    byte.
  - Worked example: the bytes of `ls --color`'s output, and the grid
    they leave behind.
- **`Line_discipline`**: the tty driver's cooked mode, the part
  TinyZork and TinyHamurabi each reinvented. Characters typed are
  echoed and kept in a line buffer; Backspace erases one (and sends
  `BS SP BS` to the screen); Enter delivers the line to the program.
  Raw mode delivers each key at once, for Rogue.
  - Worked example: `h e l x BS l o CR` gives the line `hello`, and the
    screen shows what the user saw.
- Tests in `libs/terminal/tests/`: bytes in, grid out, compared as text
  (a grid printed as lines), one test per sequence.

## 2. `playground/ways/Teletype`: games that ask

A conversation as data, in the style of Karel: the program is a value
that the way steps through, so the game has no `update` and no `view`
of its own.

```ocaml
type 'a talk =
  | Done of 'a
  | Print of string * 'a talk                (* bytes, escapes allowed *)
  | Ask of string * (string -> 'a talk)      (* a prompt, then the line typed *)
  | Key of (string -> 'a talk)               (* raw mode: one key, no Enter *)

val print : string -> unit talk
val ask : string -> string talk
val key : unit -> string talk
val return : 'a -> 'a talk
val ( let* ) : 'a talk -> ('a -> 'b talk) -> 'b talk

val teletype : ?paper:bool -> ?baud:int -> unit talk -> (_, _) Playground.app
val run : 'a talk -> string list -> string        (* the transcript, for tests *)
```

- **The model** is the talk still to run, a `Vt`, and a
  `Line_discipline`. Each frame runs the `Print`s until the next `Ask`
  or `Key`, then waits. Enter hands the line to the continuation.
- **Why a data type, not direct style**: we are on OCaml 4.14, so there
  are no effects that could suspend a real `input ()`. Closures kept in
  the model work on native and in the browser without any setup. The
  header gives the history:
  - Haskell 1.0's dialogue I/O (1990): a program as a value that
    the runtime interprets, one request at a time;
  - the free monad that later generalized it;
  - the playground's own `Cmd`, which is the same idea, bounded.
  Effects are an exercise for OCaml 5: `ask` in direct style, with a
  handler storing the continuation. The header says what changes (the
  program looks like BASIC) and what is lost (one-shot continuations
  can't be replayed, so `run` on a list of answers gets harder).
- **`baud`**: the ASR-33 Teletype printed 10 characters a second
  (110 baud). With the flag `baud=110`, output comes out at that
  speed, so a player can see what waiting on a 1973 game felt like.
  `Print` is then drained a few characters per frame.
- **`paper`**: TinyHamurabi's look, lines that go up and are never
  erased, in capitals on a roll. The default is TinyZork's look, a
  screen. Both are views of the same `Vt`.
- **`run`** feeds a list of answers and returns what was printed. This
  is how each game is tested against its BASIC listing's sample run,
  which Ahl's book prints for every game.
- Demo: `examples/TeletypeHangman.ml`, a page long, the gallows drawn in
  characters, with its golden frame and web page like the other
  `ways/` demos.

## 3. `apps/system/TinyTerminal`: the terminal itself

Done (2026-09-25): the shell `tsh` is itself a `talk`, and running a
program is `Teletype.spawn`, added for it -- fork, exec and wait in one
request, the machine keeping the waiting parents on a stack, so that
Control-C interrupts only the innermost program. The programs are the
appkit `appkits/teletype` (`Tty_hangman`, moved from the example, and
`Tty_guess`), so one value runs alone and under the shell. The
VT100's white phosphor by default (`phosphor=green|amber`), the
keyboard's ON LINE and KBD LOCKED lights. What follows is the plan as
written before.

The first program of `apps/system/`, which starts that section of
`CATALOG.md`. It is after the VT100: the screen is 80 by 24, with the
green or amber phosphor as a flag.

- **The built-in shell**, which works everywhere and so in the browser:
  a prompt, `help`, `clear`, `echo`, `ls` (the programs), and the
  Teletype programs as commands (`hangman`, `wumpus`, `hamurabi` once
  it is ported). A command's `talk` runs on the terminal's `Vt` until
  `Done`, then the prompt comes back. This is where the two connect: a
  Teletype program doesn't know whether it runs alone (`teletype`) or
  under TinyTerminal's shell.
- **The shell's own lesson**, kept small: a line split into words, the
  first word looked up in a table of programs. It is the Bourne shell's
  loop without the language. Pipes and `$VAR` are exercises.
- Golden frames: the shell after `help`, and a game started from it.

## 4. Later: a real shell, and the screen games

- **A pty, natively**: TinyTerminal running `/bin/sh` through
  `posix_openpt` with a `Cap.exec` and a `Cap.tty`
  (`plan_caps.md`), the pty's bytes into `Vt`. From then on, the
  escape-sequence coverage is measured against real programs: `ls
  --color`, `top`, `vi`. What `Vt` gets wrong shows up here.
- **In the browser**: the same over a WebSocket to a small native pty
  server (`networking/unix/`'s `Server`, as `Relay` is built), in the
  spirit of the old telnet-in-a-browser pages.
- **Screen mode**: full-screen programs such as **TinyRogue**
  (`games/rpg/`: the dungeon in characters, a turn per key, the letters
  as the monsters) are not `talk`s but event loops. They get their own
  way, in section 6.
- **Porting TinyHamurabi and TinyZork** onto `Teletype`. This is
  optional: Hamurabi's rules would read closer to the BASIC listing,
  while Zork's world-as-model design is its lesson and can stay.

## The games it unlocks

Each is a page or two long, after its listing, with its sample run as
its test (the genre directory in parentheses):

- Hangman (`puzzle/`).
- Hunt the Wumpus (`adventure/`): a dodecahedron of caves.
- Animal (`puzzle/`): a program that learns, a binary tree of
  questions grown by each game it loses.
- Bagels and Guess the Number (`puzzle/`).
- Nim (`strategy/`): the winning move by XOR.
- Star Trek (`strategy/`): the galaxy as an 8x8 grid of 8x8 sectors.
- The Oregon Trail (`strategy/`).
- ELIZA (`adventure/`, or `apps/`): DOCTOR's script as data, patterns
  and reassembly rules.

## 5. BASIC: the listings themselves, run

Should the Ahl games be written in OCaml, or run as the BASIC the book
printed? Both, one after the other, because each answers a different
question.

- **In OCaml first**: the games of the catalogue are ports that
  explain. Their headers say what the listing does, for example
  Wumpus's cave as a dodecahedron or Animal's tree growing. A
  400-line BASIC listing full of `GOTO 350` explains nothing. The
  OCaml version is the one that teaches the game.
- **Then TinyBasic, an interpreter**: this teaches something else, what
  a 1970s home computer did. Its lesson is `Teletype` again, because an
  interpreter is itself a `talk`. `PRINT` is a `Print`, and `INPUT` is
  an `Ask` whose continuation is "the rest of the program, from this
  statement, with the variable set". So the interpreter never blocks,
  and it runs in the browser like the rest. It has:
  - a line-numbered program;
  - `LIST`, `RUN`, and editing by typing a line with its number (the
    whole programming environment of 1977);
  - the Microsoft BASIC dialect the book targets: `DIM`, strings and
    `LEFT$`/`MID$`, `GOSUB`, `ON ... GOTO`, `DEF FN`, `RND`, `INT`,
    `TAB`.
  - Where it goes (the author's choice, 2026-09-25): **TinyBasic in
    `apps/devtools/`**, the category's first program, where
    TinyTurboPascal will follow it (section 6): both are a language
    and its environment, not a system program. It is split in the
    same way as TinyTerminal's programs:
    - the interpreter is a `unit talk` in an appkit (with the
      Teletype programs in `appkits/teletype`, or in an
      `appkits/basic` of its own if it grows past a few modules:
      lexer, parser, interpreter);
    - `apps/devtools/TinyBasic.ml` is the machine it runs on: the
      screen of a 1977 home computer (the Apple II's 40 columns, or
      the PET's), `READY.` and a blinking cursor;
    - the same value is `basic` in TinyTerminal's shell, the way BASIC
      was one command among others on a timesharing system (Dartmouth,
      1964, where it began).
  - It comes after Karel in `plan_teaching_languages.md`'s list of
    ways of programming.
- **What the two together give: a differential test.** The same
  answers are fed to the OCaml port through `run`, and to the original
  listing through the interpreter, and the transcripts must match. Two
  things are needed for this. `RND` must be seeded and the same on
  both sides: the port takes the interpreter's generator, so its
  header can say "the dice as the listing throws them". And the
  column layout of `PRINT` (`;`, `,`, `TAB`) must be exact, which is
  most of a BASIC's fiddly part.
- **The listings**: the book is © Creative Computing (1973, 1978).
  Jeff Atwood's `coding-horror/basic-computer-games` repository
  republishes them with permission, as the base for its ports to modern
  languages. Before copying any listing into this repository, check its
  licence. The fallback is listings of our own in the same dialect,
  which the tests need anyway.

## 6. Text mode: full-screen programs, and Turbo Vision

Rogue, `vi`, Norton Commander and the Turbo Pascal IDE (Borland, 1983)
are a different kind of text program than the Ahl games. They don't
ask and wait. They wait for a key, redraw the screen and wait again,
which is an event loop. So Model-View-Update fits them as they are,
and the way for them is thin: `update` as usual, and a view that
returns **a grid of cells instead of shapes**.

- **`Curses`** (`libs/terminal/`, pure): a screen of cells, drawing
  into it (move, add a string, a box, reverse video), and `refresh` as
  the difference between the previous grid and the new one, turned
  into the fewest escape sequences. That difference was curses' whole
  point (Ken Arnold, BSD, 1980, written for Rogue): at 9600 baud,
  redrawing a whole screen took a second.
- **The way, `Textmode`**: `textmode ~view ~update init`, where `view`
  returns a `Curses` screen. It has two outputs:
  - on the playground, the grid drawn as shapes, a cell per character
    (the same view as TinyTerminal's);
  - natively, *in a real terminal*: `refresh`'s escape sequences
    written to stdout in raw mode, with keys read from stdin.
  The same program then runs in a window, in the browser, and in the
  user's xterm. The precedent is Bubble Tea (Charm, Go, 2020), which is
  exactly the Elm architecture for terminals; Brick (Haskell) is
  another. The header can quote both.
- **Turbo Vision** (Borland, 1990, the text-mode UI of Turbo Pascal 6
  and Borland C++): menus, windows with shadows, dialogs, all in
  characters. It isn't a new toolkit. `gui/` already separates paint
  from wiring (`Look.mli`: "the paint lives here, once, and each
  architecture is only its wiring"). What it needs is a **second Look
  whose paint is cells**: box-drawing characters for frames, a shadow
  drawn as a darker cell, the hot letter of a menu item highlighted,
  with `Layout` measuring in cells. Then `Widget`, `Focus` and `Mvu`
  work unchanged, and a comparison between the two Looks shows what a
  GUI toolkit owes to pixels and what it doesn't.
- **The colours it needs**: `Vt` knows the 8 colours of SGR 30-37 and
  40-47, reads 90-97 as a colour plus bold, and reads 256-colour and
  24-bit codes only to ignore them. That won't do for Turbo Vision.
  Its screens are the PC's text mode: 16 foreground colours, where
  yellow is "bright brown" and not bold brown, and bright backgrounds
  taken from the blink bit (Turbo Pascal's white on bright cyan). So:
  - `Vt.color` grows 8 bright colours, kept apart from bold, for
    SGR 90-97 and 100-107;
  - the 256-colour palette (`38;5;n`: the 16, a 6x6x6 cube, 24 greys)
    and 24-bit RGB (`38;2;r;g;b`) become colours too, instead of being
    skipped;
  - a view maps each colour to the CGA/VGA palette. It is a palette
    and not the terminal's choice: the same 16 colours on every PC was
    what gave Turbo Pascal its blue.

  None of this is needed before TinyTurboPascal: the Ahl games and
  Rogue are happy with 8 colours.
- **TinyTurboPascal** (`apps/devtools/`) comes last and brings all of
  this together: the blue editor (`gui/`'s `Text_edit` over the cell
  Look), the menu bar, F9 to compile, and a small Pascal whose `write`
  and `readln` run on `Teletype` in the lower window. Compile-and-run
  in one keystroke was the whole lesson of the original. It is in the
  same family as TinyBasic (section 5), and the language itself
  belongs in `plan_teaching_languages.md`.

## Order

1. `libs/terminal/`: `Vt` and `Line_discipline`, with their tests.
2. `Teletype` and `examples/TeletypeHangman.ml`.
3. TinyTerminal with its built-in shell running the Teletype programs,
   plus its CATALOG row, golden frame and web page.
4. A first real game on `Teletype` (Hunt the Wumpus), then the others
   one at a time. Wumpus done (2026-09-25): `Tty_wumpus` in
   `appkits/teletype`, `games/adventure/TinyWumpus.ml` its thin main, and
   `wumpus` in TinyTerminal's shell.
5. TinyBasic (`apps/devtools/`) on `Teletype`, a `basic` command in TinyTerminal, and a
   differential test of Wumpus against its listing.
6. `Curses` and the `Textmode` way (the playground and a real
   terminal), then TinyRogue.
7. Later: the pty (native, then WebSocket); the cell Look for `gui/`,
   then TinyTurboPascal.

## Open questions for the author

- The way's name: `Teletype` (the machine, the paper look, the baud
  rate) or `Console`. This plan says `Teletype`.
- Where `Vt` lives: `libs/terminal/`, its own folder, as proposed, or
  inside `gui/`.
- Whether the Ahl games get their own section of `CATALOG.md` (a genre
  "teletype") or go to their genres, as this plan proposes.
