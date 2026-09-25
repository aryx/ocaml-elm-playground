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
| `javascript/` | (to come) a small modern core of JavaScript, for TinyFirefox | `plan_tiny_firefox.md` |

What is not here, and why: the Playground's *ways* (`playground/ways/`:
Logo, Big Bang, PuzzleScript, Karel) are OCaml APIs that build an app,
not text; HyperTalk stays in `appkits/hypertalk` beside HyperCard, the
one program that speaks it. A language that talks with a person (BASIC's
INPUT) does so as a `Talk` value, and the Playground's `Teletype` way
puts it on a screen. The analysis is in `plan_tiny_firefox.md`,
section "libs/languages/".
