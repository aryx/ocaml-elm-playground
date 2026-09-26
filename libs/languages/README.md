# libs/languages/: languages as text, parsed and run

A language here is text, read by a parser and run by an evaluator,
with nothing of the Playground: what a program can touch outside
itself is a **host**, a record of functions its caller gives (a sheet's
cells, a page's elements), so that a test runs it against a list of
strings and an app against its screen. One library per language, each
`.mli` with its grammar, a worked example and its references.

| folder | language | used by |
|---|---|---|
| `formula/` | the spreadsheet's formulas (VisiCalc, 1979): arithmetic over numbers and cells, SUM and its kin over ranges; recursive descent, the parser to know first | `appkits/sheet`, and so TinyVisiCalc and TinyExcel |
| `basic/` | BASIC as the home computers had it: Tiny BASIC (1976), Integer BASIC and Applesoft (1978); a line read by recursive descent, a program run as a Talk conversation (`libs/terminal`), the prompt and the floppy of listings | TinyBasic, and TinyTerminal's shell (`basic`) |
| `lisp/` | a small Emacs Lisp (McCarthy, 1958; Emacs Lisp, 1985): the reader, eval and apply with dynamic scope and the specpdl, macros, condition-case; the evaluator's state a value threaded through it, a host adding its functions (an editor's buffers) | TinyEmacs (`appkits/editor`) |
| `pascal/` | Pascal (Wirth, 1970) compiled to P-code and run on a P-machine, Pascal-P's scheme (1973) and UCSD Pascal's: a one-pass compiler, recursive descent emitting the code as it parses, no tree; the stack machine's frames and static links; the machine a Talk program, so readln waits, or paused by a debugger (`Pdebug`: the compiler's debug information, frames, watches, steps) | TinyTurboPascal (`appkits/editor`'s `Tui_turbo`) |
| `hypertalk/` | HyperCard's language (Atkinson and Winkler, 1987), written to be read aloud: handlers answering messages, every value a string, and the message path (button, card, background, stack) -- inheritance without classes; the cards are the host's | TinyHyperCard, and TinyMyst's stack of stills |
| `javascript/` | a small modern core of JavaScript, for TinyFirefox: read (`Js_lexer`, `Js_ast`, `Js_parse`: recursive descent and Pratt, and why not yacc) and run (`Js_value`, `Js_eval`, a tree walker with closures, `this` and the coercions; `Js_builtins`); the DOM is `appkits/browser`'s `Browser_script` | TinyFirefox (`plan_tiny_firefox.md`) |

What is not here, and why: the Playground's *ways* (`playground/ways/`:
Logo, Big Bang, PuzzleScript, Karel) are OCaml APIs that build an app,
not text. A language that talks with a person (BASIC's
INPUT) does so as a `Talk` value, and the Playground's `Teletype` way
puts it on a screen. The analysis is in `plan_tiny_firefox.md`,
section "libs/languages/".
