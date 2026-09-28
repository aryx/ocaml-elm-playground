# languages/: languages as text, parsed and run

claude: at the top of the repository, beside `appkits/` and
`gamekits/`, not in `libs/`: each language is made for a program or a
few (the table's last column), as a kit is, so tinybox's code map shows
it as their code and it counts toward their budget of 5,000 lines
(README's "A budget"). Six programs are over it for their languages,
and allowed to be, listed in `tests/catalog/`: TinySmalltalk80
(Smalltalk-80, a whole system), TinyChrome, TinyFirefox and TinyNetscape
(JavaScript, with the browser's engine: HTML, CSS, the layout),
TinyMosaic (the engine), and TinyOffice (the formulas and HyperTalk).

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
| `sexpr/` | s-expressions, the syntax every Lisp shares: `Sexpr`, a tree whose parts keep their span in the text, and `Sexpr_read`, the reader, Emacs Lisp's dialect (`?a`, `#'`, keys in strings) and Scheme's (`#t`, `#\a`, brackets, vectors, quasiquote, block comments); each Lisp turns the tree into its own values | `lisp/`, `scheme/` |
| `scheme/` | a small Scheme (Steele and Sussman, 1975; R5RS, 1998) and How to Design Programs' Beginning Student: lexical scope and closures, `#f` apart from `'()`, immutable pairs as Racket's; the special forms checked and the derived ones rewritten (`Scheme_syntax`), DrScheme's error messages with their spans; run by a CESK machine (`Scheme_eval`: the continuation as data, so call/cc, tail calls and a budget of steps), `define-struct`, 2htdp/image's images as data (`Scheme_image`), big-bang handed to the host; the stepper, Beginning Student's evaluation as rewriting (`Scheme_step`) | TinyDrScheme |
| `lisp/` | a small Emacs Lisp (McCarthy, 1958; Emacs Lisp, 1985): the reader, eval and apply with dynamic scope and the specpdl, macros, condition-case; the evaluator's state a value threaded through it, a host adding its functions (an editor's buffers) | TinyEmacs (`appkits/editor`) |
| `pascal/` | Pascal (Wirth, 1970) compiled to P-code and run on a P-machine, Pascal-P's scheme (1973) and UCSD Pascal's: a one-pass compiler, recursive descent emitting the code as it parses, no tree; the stack machine's frames and static links; the machine a Talk program, so readln waits, or paused by a debugger (`Pdebug`: the compiler's debug information, frames, watches, steps) | TinyTurboPascal (`appkits/editor`'s `Tui_turbo`) |
| `hypertalk/` | HyperCard's language (Atkinson and Winkler, 1987), written to be read aloud: handlers answering messages, every value a string, and the message path (button, card, background, stack) -- inheritance without classes; the cards are the host's | TinyHyperCard, and TinyMyst's stack of stills |
| `smalltalk/` | Smalltalk-80 from the Blue Book (Goldberg and Robson, 1983): read (`St_lexer`, `St_parse`, message precedence by recursive descent), compiled to the Blue Book's bytecodes (`St_compile`, the inlined ifTrue: and whileTrue:), run by its interpreter over an object table (`St_memory`, `St_interp`: contexts as objects, the method cache, non-local return, become:), the kernel's classes written in Smalltalk and bootstrapped from their text (`kernel/*.st`, `St_boot`), the debugger's questions (`St_debug`), the image (`St_image`), BitBlt (`St_bitblt`); the host gives the Transcript, the clock and the mouse | TinySmalltalk80 (`plan_tiny_smalltalk.md`) |
| `scratch/` | Scratch (MIT, 2007) and Snap! (Berkeley, 2011): the blocks as one table of specs, as Scratch 3's opcodes are, Snap!'s custom blocks named by their template (`Scratch_blocks`); their text, the forums' scratchblocks notation, read by matching a line against the templates and printed back, a ring round a script on one line (`Scratch_text`); a project run, sprites on a stage, each script a green thread yielding at the end of a loop's turn, and Snap!'s heap of lists and cells, environments, closures, custom reporters run to their report (`Scratch_run`) -- a language whose programs are made with the mouse, and have a text all the same | TinyScratch and TinySnap (`appkits/blocks`, the editor's geometry) |
| `ocaml/` | OCaml read, not run, for a code visualizer (`plan_tinybox_codemap.md`): `Lexer_ml`, ocamllex after ocaml-light's (OCaml 1.07's) lexer, every token kept, comments included, with its place; `Parse_ml`, the parser, recursive descent (not ocamlyacc: `Parse_ml.mli` says why), building `Ast_ml`, the names and what they are; `Highlight_ml`, each token's category, guessed from its neighbours (codemap's token pass), then told by the tree (scopes, fields) | tinybox's code map (`launcher/codemap/`) |
| `c/` | C read, not run, the same way: `Lexer_c` (the preprocessor's lines kept, marked), `Parse_c` (recursive descent, no preprocessor: an #if's first branch read, heuristics for the types the headers would declare and for macros, `Parse_c.mli`) building `Ast_c`, `Highlight_c`. Reads 97% of Principia Softwarica's C files whole | tinybox's code map (`launcher/codemap/`) |
| `javascript/` | a small modern core of JavaScript, for TinyFirefox: read (`Js_lexer`, `Js_ast`, `Js_parse`: recursive descent and Pratt, and why not yacc) and run (`Js_value`, `Js_eval`, a tree walker with closures, `this` and the coercions; `Js_builtins`); the DOM is `appkits/browser`'s `Browser_script` | TinyFirefox (`plan_tiny_firefox.md`) |
| `html/` | HTML read, not run (from `libs/web/html`): the bytes to text (`Charset`), the entities, the tokens (`Html_lexer`, the WHATWG's state machine), the tree (`Dom`, `Dtd`, `Html_tree`, the stack of open elements), the tree as the Line Mode Browser showed it (`Line_mode`), the forms (`Forms`) | the browsers (`appkits/browser`), TinyMosaic to TinyChrome |
| `css/` | what a page's elements look like (from `libs/web/style`): Mosaic's fixed table (`Looks`), style sheets read (`Css_syntax`), `Selectors`, the cascade (`Css`), and TinyChrome's values, cascade and computed styles (`Css_values`, `Cascade`, `Computed`, over `ua.css`) | the same; laid out by `appkits/browser/layout` |

What is not here, and why: the Playground's *ways* (`playground/ways/`:
Logo, Big Bang, PuzzleScript, Karel) are OCaml APIs that build an app,
not text. A language that talks with a person (BASIC's
INPUT) does so as a `Talk` value, and the Playground's `Teletype` way
puts it on a screen. The analysis is in `plan_tiny_firefox.md`,
section "languages/".
