# A JavaScript engine, and a browser that runs it: a tutorial

TinyMosaic and TinyNetscape read a page; TinyFirefox also runs it.
This tutorial explains how, stage by stage, in the order the code is
written (`plan_tiny_firefox.md`): first a language engine, knowing
nothing of pages (`libs/languages/javascript/`), then what joins it to
the browser (`appkits/browser/`'s `Browser_script`), then the browser.
Each section ends with the worked example its tests check.

The thread through it: **a language engine is a pipeline of
translations too** -- as a browser is (`notes_browser.md`) -- text into
tokens, tokens into a tree, the tree into values by walking it:

```
"let n = 1 + 2 * 3;"
   |  Js_lexer         the characters grouped: keywords, names, numbers, operators
   v
[let] [n] [=] [1] [+] [2] [*] [3] [;]
   |  Js_parse         the tokens grouped: a tree, the operators' precedence decided
   v
Let (n, Binary (+, 1, Binary (*, 2, 3)))
   |  Js_eval          the tree walked: each node's value from its children's
   v
n = 7, in the current scope
```

And JavaScript's part in a browser is small once the engine exists:
a few objects (`document`, the elements) whose methods are OCaml
functions reading and changing the page's tree, and a queue of things
to run (a click's handler, a timer's function), one at a time.

## 0. Where the code is, and a reading order

| Module | What | Section |
|---|---|---|
| `Js_lexer` | the text to tokens | 1 |
| `Js_ast` | the tree: expressions and statements | 2, 3 |
| `Js_parse` | the tokens to a tree: Pratt for expressions, recursive descent for statements | 2, 3 |
| `Js_value` | values, objects, environments | 4, 5 |
| `Js_eval` | the tree walked | 5, 6, 7 |
| `Js_builtins` | `console`, `Math`, strings' and arrays' methods | 8 |
| `Browser_script` | the page seen by a script: the mutable tree, the host objects, the events and timers, the console | 9, 10 |
| `TinyFirefox` | the browser, its console and live tree | 11 |

Read `Formula.mli` (`libs/languages/formula/`) first if you have never
written a parser: a spreadsheet's arithmetic by recursive descent, 250
lines. This tutorial starts where it stops.

## 1. Tokens

The lexer turns characters into **tokens**: a keyword (`let`,
`function`), a name (`n`, `document`), a number (`3`, `0.5`), a string
(`"hi"` or `'hi'`, its escapes decoded), a punctuation or an operator
(`(`, `===`, `=>`). Spaces and comments (`// ...`, `/* ... */`) are
dropped -- except that each token remembers whether a **newline** came
before it, the one fact about spacing the grammar needs (section 3).

The one real decision is **the longest match**: `===` is one token, not
`==` then `=`, and `=>` one, not `=` then `>`. So the lexer tries the
three-character operators first, then the two-, then the one-character
ones.

Worked example (the tests'):

```
let s = "a" + 'b'; // two strings
x=>x===1

Keyword let  Name s  Punct =  String "a"  Punct +  String "b"  Punct ;
Name x (a newline before)  Punct =>  Name x  Punct ===  Number 1
```

## 2. Expressions: Pratt parsing

`1 + 2 * 3` is `1 + (2 * 3)`: multiplication **binds tighter**. A
recursive descent grammar says it with one rule per level (`Formula`:
`expr` made of `term`s made of `factor`s); JavaScript has twenty
levels, and a rule each is a lot of functions saying the same thing.
Vaughan Pratt's way (1973, and Douglas Crockford's JavaScript parser in
JavaScript, 2007) gives each operator a number, its **binding power**,
and one function does it all:

```
parse_expression (min):
  left <- a prefix thing: a number, a name, ( ... ), -x, !x, [ ... ], { ... }, function
  while the next token is an operator binding at least as tight as min:
    take it; right <- parse_expression (its power + 1, or its power if right-associative)
    left <- Binary (op, left, right)
  return left
```

The powers used here, loosest first:

| power | operators | associativity |
|---|---|---|
| 1 | `=` `+=` `-=` `*=` `/=` | right: `a = b = 1` is `a = (b = 1)` |
| 2 | `? :` | right |
| 3 | `\|\|` | left |
| 4 | `&&` | left |
| 5 | `===` `!==` `==` `!=` | left |
| 6 | `<` `>` `<=` `>=` | left |
| 7 | `+` `-` | left: `1 - 2 - 3` is `(1 - 2) - 3` |
| 8 | `*` `/` `%` | left |
| 9 | prefix `!` `-` `typeof` `++` `--` | (prefix) |
| 10 | postfix `.x` `[i]` `(args)` `++` `--` | left: `a.b(c)[d]` is `((a.b)(c))[d]` |

Left-associative: the right side is parsed with power + 1, so an
operator of the same power stops it and becomes the next loop's.
Right-associative: with the same power, so it continues. That one
"+ 1" is the whole difference.

An arrow function `x => x + 1` looks like a name until the `=>`, and
`(a, b) => a + b` like a parenthesized expression. The specification
parses them as expressions and turns them into parameters when the
`=>` comes ("cover grammars"); `Js_parse` looks ahead instead: seeing
a name or a `(`, it finds the matching `)` in the token array and
checks for a `=>` after it -- cheap, since all the tokens are there.

Worked example (the tests'):

```
1 + 2 * 3 - 4          ((1 + (2 * 3)) - 4)
a = b = c || d && e    (a = (b = (c || (d && e))))
-x.y(1)[0]             (-(((x.y)(1))[0]))
f(x => x * 2, 3)       (f((x) => (x * 2), 3))
```

**Why not yacc.** yacc (Stephen Johnson, Bell Labs, 1975) is the
classic way to write a parser, and ocamlyacc comes with OCaml: the
grammar is the program, and the table above is its precedence
declarations. The two say the same thing:

```
yacc                              Pratt (the table above)
%right '=' PLUS_EQ ...            1, right
%left OR                          3
%left AND                         4
%left '+' '-'                     7
%left '*' '/' '%'                 8
%right UMINUS '!' TYPEOF          9 (prefix; "%prec UMINUS" in a rule)
```

-- the later a line, the higher its power. But JavaScript's grammar
fights LALR(1) at every turn a teaching parser cares about: an arrow's
`(a, b` cannot be told from a parenthesized expression with one token
of lookahead; "a newline may end a statement" and "`return` alone on
its line" need the lexer and the parser to talk (yacc can only imitate
them with error productions); `{` is a block at a statement's start and
an object elsewhere; and yacc's mistakes say "syntax error", where a
teaching engine should say "expected ')' on line 3". Every real engine
parses JavaScript by hand (V8, SpiderMonkey, JavaScriptCore, QuickJS,
and Acorn, Esprima, Babel). `Js_parse.mli` has the whole argument; yacc
is the right tool for a language designed for it -- Wirth's Pascal, a C
subset -- and exercise 11 puts the two side by side.

## 3. Statements: recursive descent

Statements have keywords at their start, so the parser knows which one
it reads from its first token: `let`, `const`, `var`, `function`, `if`,
`while`, `for`, `return`, `break`, `continue`, `throw`, `try`, `{`, or
else an expression followed by `;`. One function per statement, as in
`Formula`.

**Semicolons.** JavaScript lets a newline end a statement when the
next line cannot continue it (ECMAScript's "automatic semicolon
insertion"). The simple half kept here: a statement may end at `;`, at
`}`, at the end of the text, or **before a token that follows a
newline**. That is why the lexer remembers newlines. The famous traps
of the full rule (a line starting with `(` or `[` continuing the one
before; `return` alone on its line returning nothing) are listed in the
exercises; the second one is kept, because the rule above gives it.

`for (x of xs)`, `for (let i = 0; i < n; i++)`, and blocks `{ ... }`
each make a new scope (section 5).

Worked example (the tests'):

```
let a = 1
let b = a + 1; if (b > a) { b = 0 } else b = 1
function f() {
  return
  a
}

Let a 1
Let b (a + 1)
If ((b > a), Block [Expr (b = 0)], Expr (b = 1))
Function f [] [Return; Expr a]
                   -- the newline ended the return: f() is undefined
```

## 4. Values

Seven kinds, as JavaScript has them (less `symbol` and `bigint`):

```
undefined   null   boolean   number (a float: 1 is 1.0)   string   object   function
```

An **object** is a table from names to values, mutable, its keys kept
in the order they were added (`JSON.stringify` and `for...in` show
that order). An **array** is an object whose keys are `"0"`, `"1"`, ...
and a `length`; here it is a growable OCaml array, with `length` and
indexing done on it directly, which is what engines do too behind the
same appearance. A **function** is an object too (it can have
properties), holding its parameters, its body, and the **environment**
it was created in -- a closure (section 5). A **host function** is an
OCaml function the engine calls (`console.log`, `document.getElementById`).

Two values are **the same object** only if they are one object: `{} ===
{}` is false. Objects are compared, passed and stored by reference;
numbers, strings and booleans by value.

`typeof` says which kind: `"undefined"`, `"object"` (for `null` too --
a mistake of 1995, kept by every engine since, because pages relied on
it), `"boolean"`, `"number"`, `"string"`, `"function"`.

A string is OCaml's, in UTF-8, and its `length` and indexes count
bytes: `"é".length` is 2 here, where JavaScript, counting UTF-16 units,
says 1. The pages here are ASCII where it matters; counting properly is
an exercise.

## 5. Scopes and closures

An **environment** is a frame (names to values) and a pointer to the
frame around it. `let x` adds `x` to the current frame; reading `x`
looks in the current frame, then the one around it, up to the global
one, else a `ReferenceError`. A block, a function's call and each
iteration of a `for` make a new frame.

A function value keeps the frame it was **created** in, not the one it
is called from (lexical scope); calling it makes a frame for its
parameters **around which is that kept frame**. So a function can use,
and change, variables of a call that has returned: a closure.

Worked example (the tests'):

```
function counter() {
  let n = 0;
  return () => { n = n + 1; return n; };
}
const c = counter();
c(); c();            // 2
const d = counter();
d();                 // 1: another call of counter, another n

   global:  counter, c, d
     ^                 ^
     |                 |
   frame of counter() #1: n = 2  <-- c's closure keeps it alive
   frame of counter() #2: n = 1  <-- d's
```

**`let` in a `for`.** Each iteration gets its own `i`, so functions made
in the loop each see their own (`[0, 1, 2]` below); with the old `var`,
one `i` for the loop, they all saw 3 -- the most asked JavaScript
question of the 2000s. Here `var` is read as `let` (plan), so both give
`[0, 1, 2]`, and the notes say so.

```
const fs = [];
for (let i = 0; i < 3; i++) fs.push(() => i);
fs.map(f => f())     // [0, 1, 2]
```

**`this`.** In `o.f()`, the function runs with `this` bound to `o`; in
a plain call `f()`, to `undefined`. An arrow function has no `this` of
its own: it sees the `this` of where it was written -- why event
handlers are written as arrows.

## 6. Running statements: how `return` gets out

Evaluating an expression gives a value. Running a statement gives an
**outcome**: it finished normally, or it wants to leave -- `return v`,
`break`, `continue`, or an exception thrown. A block runs its
statements until one does not finish normally and passes that outcome
up; a loop catches `break` and `continue`; a function call catches
`return`. `Js_eval` writes it as a variant (`Normal | Return of value |
Break | Continue`), and a thrown JavaScript exception as an OCaml
exception, since it crosses function calls -- including the OCaml ones
of the host (a handler throwing inside `forEach`).

## 7. Coercions: what `+` does

JavaScript converts rather than refuses. `+` adds two numbers, but if
either side is a string (or an object, which becomes one) it
**concatenates** their strings; the other arithmetic operators convert
both sides to numbers. **Truthiness**: `false`, `0`, `NaN`, `""`,
`null` and `undefined` are false in an `if`; everything else, `"0"` and
`[]` included, is true. A number printed is its shortest form that
reads back the same (`7`, not `7.0`; `0.1 + 0.2` is `0.30000000000000004`).

Worked example (the tests', Gary Bernhardt's "Wat", 2012, in part):

```
1 + 2            3
"1" + 2          "12"
1 + "2"          "12"
"3" * "4"        12
true + 1         2
[] + []          ""               an empty array's string is ""
[] + {}          "[object Object]"
[1, 2] + [3]     "1,23"           arrays joined with commas
"b" + "a" + +"a" + "a"   "baNaNa"  +"a" is NaN
0.1 + 0.2        0.30000000000000004
typeof null      "object"
```

`==` is read as `===` here: no conversion before comparing. The real
`==` table (`"1" == 1` true, `null == undefined` true, `[] == ![]`
true) is an exercise, and the reason every style guide says `===`.

## 8. Errors, and the built-ins

Three errors, each with its **line** (the statement's, kept by the
parser), worded as browsers word them: a `SyntaxError` from the parser
(`expected ')', not ';'`), a `ReferenceError` (`x is not defined`), a
`TypeError` (`f is not a function`, `Cannot read properties of
undefined (reading 'y')`). `throw v` throws any value; `try { } catch
(e) { }` catches it. And two a page must never do: recurse without end
(`RangeError: Maximum call stack size exceeded`), or loop without end --
a script runs to its end before the page moves again, so `while (true)
{}` would freeze the browser; after a budget of steps the engine stops
it with an error, as browsers ask "a script on this page is busy: stop
it?".

The built-ins are ordinary host functions in the global frame and on
strings and arrays: `console.log`, `Math`, `String`, `Number`,
`parseInt`, `JSON.stringify`; `"abc".length`, `.toUpperCase()`,
`.slice`, `.indexOf`, `.split`, `.trim`; arrays' `length`, `push`,
`pop`, `join`, `indexOf`, `forEach`, `map`, `filter`. A method call on
a string or an array looks the method up in a table of the kind's host
functions -- where a real engine looks in a **prototype** (the
`String.prototype` object), which is the exercise that brings `new`
and `class` with it.

## 9. The DOM, seen from a script

A script reaches the page through **host objects**: `document`, and an
object per element it asks for. Their properties and methods are OCaml
functions of `Browser_script`, over its copy of the page's tree:

```
document.getElementById("count")          find the element whose id= is "count"
el.textContent = "3"                      replace its children by the text "3"
el.innerHTML = "<b>3</b>"                 replace them by Html_tree's parse of the string
el.style.color = "red"                    set "color: red" in its style= (CSS, notes_browser 10)
el.className = "done"                     its class=: another CSS rule may match now
document.querySelector("ul li.done")      the first element Css.matches
el.appendChild(document.createElement("li"))
```

**The copy.** The browser's tree (`Dom`) is a value, read by everything
from the looks to the hit test. The script works on a **mutable copy**
of it -- nodes with a parent, children, attributes -- and after a
**task** (section 10) that changed it, the copy is **frozen** back into
a `Dom.element` and the page laid out again. The same element keeps
the same host object across tasks (a table from nodes to objects), so
`const el = document.getElementById("x")` stays valid.

An element's properties are not values stored in a table: its
`textContent` *is* its tree's text, computed when read and replacing its
children when written. So `Js_value` has **host objects**, whose reads
and writes call the host's functions -- what the specification calls
getters and setters, here the browser's only.

The page's `<script>`s run in order once the whole page is read (as the
attribute `defer` asks), rather than as the parser meets them: a script
finds every element, whatever its place. An error goes to the console,
with its line, and the next script still runs.

Worked example (the tests', on a fake page):

```
<p id=x>a</p>   +   document.getElementById("x").textContent = "b"
  frozen:  html > body > p id="x" > "b"
```

## 10. Events and the event loop

A browser runs one thing at a time. A **task** is a unit of script:
the page's `<script>`s at load, one event's handlers, one timer's
function. Tasks wait in a **queue**; the browser takes the first, runs
it **to its end** (a script is never interrupted: while it runs, the
page does not move), then, if it changed the tree, lays the page out
again -- once, however many changes it made -- and takes the next.
This is the event loop of `notes_browser.md` section 14, JavaScript's
one thread.

**A click** is found in the layout (`Hit`, as a link is), turned into
the element it fell on, and **bubbles**: the handlers of that element
run, then of its parent, and so on up to `document`, unless one calls
`event.stopPropagation()`. `event.preventDefault()` cancels what the
browser would have done after (follow the link, send the form). That
path is HyperCard's (`appkits/hypertalk`), a quarter century apart:

```
HyperCard (1987)                   the DOM (1998)
button -> card -> background -> stack      element -> parent -> ... -> body -> document
"pass mouseUp" goes on             bubbling goes on unless stopPropagation()
the card's script answers every    a handler on <ul> answers every <li>:
  button that does not                "event delegation"
```

**Timers.** `setTimeout(f, ms)` puts `f` in the queue when the time
comes; `setInterval(f, ms)` every `ms`. The time is the frame clock's,
so a golden frame under `-fixed-time` sees the same ticks each run.

Worked example (the tests'):

```
<ul id=list></ul>
<script>
  const list = document.getElementById("list");
  for (let i = 0; i < 100; i++) {
    const li = document.createElement("li");
    li.textContent = "item " + i;
    list.appendChild(li);
  }
</script>

  one task (the script), 100 changes, 1 reflow
```

## 11. TinyFirefox

The browser adds to TinyNetscape's engine (extensions, tables, CSS1,
all on) the scripts, and a panel after Firebug (2006): the **console**
(what `console.log` printed, each error with its line) and the **live
tree** (the page's tree as the scripts leave it, updated after each
task). Its built-in pages are the classic first programs of the web:
a counter (a click, a number changed), a to-do list (an item added, a
class toggled, CSS crossing it out), a clock (`setInterval`), and
tic-tac-toe in a table (event delegation: one handler on the table).

## 12. What the real engines add

- **Bytecode and a JIT.** A tree walker is slow: each node a function
  call. Engines compile the tree to bytecode (QuickJS stops there),
  then the hot parts to machine code (TraceMonkey in Firefox 3.5, 2009;
  V8 in Chrome, 2008), guessing types from what they have seen.
- **Hidden classes and inline caches**: objects of the same shape share
  a layout, so `o.x` is an offset, not a table lookup (Self, 1989, then
  V8).
- **A garbage collector** of their own: ours is OCaml's.
- **The whole language**: ECMAScript 2024 is about 800 pages against
  the first edition's 100 -- prototypes, classes, promises, generators,
  modules, regular expressions, proxies, and the `==` table.
- **The whole DOM** and its events, and the security around them (the
  same-origin policy, 1995, Netscape 2 again).

## Exercises

1. `==` as JavaScript has it (ECMA-262's abstract equality), and the
   table of its surprises.
2. Prototypes: `new`, `Object.create`, the lookup chain; strings' and
   arrays' methods moved onto `String.prototype` and `Array.prototype`.
   Then `class`, as the sugar it is.
3. `var` as it is: hoisted to the function's top, one per function;
   the loop of section 5 printing `[3, 3, 3]`.
4. Automatic semicolon insertion's other half: a line starting with
   `(` continues the previous one.
5. Template literals `` `n = ${n}` ``, destructuring, spread.
6. A `TinyNode` REPL in `apps/devtools/`: the engine with no page.
7. `getComputedStyle`: the cascade's result, read back by a script.
8. `fetch` over `Cmd.Http_get`, and promises: the microtask queue that
   runs between two tasks.
9. A bytecode compiler for the same tree, and the speed measured
   against the tree walker.
10. `<canvas>` and `requestAnimationFrame`: a game in a page.
11. The expressions of `Js_parse` written again in ocamlyacc (the
    `%left`/`%right` lines of section 2), the two parsers run on the
    same tests: what the grammar file makes clearer, and where its
    conflicts come from when arrows are added.

## Glossary

- **token**: a word of the language, its kind and its text.
- **binding power**: an operator's precedence as a number (Pratt).
- **AST**: abstract syntax tree, the program as a tree.
- **environment**, **frame**: the names in scope, a chain of tables.
- **closure**: a function and the frame it was created in.
- **host object**, **host function**: provided by the program embedding
  the engine (the browser), not by the language.
- **task**: a unit of script run to its end by the event loop.
- **bubbling**: an event going from its target up to the document.
- **reflow**: layout run again after the tree changed.

## References

- Robert Nystrom, *Crafting Interpreters* (2021): jlox, a tree-walking
  interpreter for a JavaScript-like language, chapters 4 to 13; the
  model for sections 1 to 8.
- Vaughan Pratt, "Top Down Operator Precedence" (POPL, 1973); Douglas
  Crockford, "Top Down Operator Precedence" (2007), the same in
  JavaScript.
- ECMA-262, 1st edition (1997), and the living ECMAScript
  specification: the values (section 6), `+` (13.15.3,
  ApplyStringOrNumericBinaryOperator), automatic semicolon insertion
  (12.10).
- W3C, *Document Object Model Level 1* (1998) and *Level 2 Events*
  (2000); WHATWG, *DOM* (events, dispatch) and *HTML*, 8.1.7 (event
  loops).
- Pavel Panchekha and Chris Harrelson, *Web Browser Engineering*,
  chapter 9 "Running Interactive Scripts" (a browser joined to an
  engine, Duktape through DukPy) and chapter 12 (scheduling, tasks).
- Allen Wirfs-Brock and Brendan Eich, "JavaScript: The First 20 Years"
  (HOPL IV, 2020).
