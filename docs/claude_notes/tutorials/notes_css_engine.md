# A CSS engine for real pages: a tutorial

TinyNetscape's CSS (`notes_browser.md` section 10, N5) was the idea:
rules, selectors, specificity, inheritance, over the looks' table. Real
pages ask for the engine: Wikipedia sends 1,391 rules in 229 KB, with
`@media`, custom properties and `calc()`; Google centres its search box
with `inline-block` and auto margins; GitHub lays out rows with flexbox
(`plan_tiny_chrome.md`, its survey). This tutorial follows the engine
TinyChrome is built on, stage by stage, as a browser's is:

```
text of a style sheet
   |  Css_syntax      tokens (Syntax Level 3), then rules and blocks
   v
rules: selectors, declarations, at-rules (@media, @import)
   |  Selectors       each selector parsed; matched right to left
   |  Cascade         for each element and property, the winner:
   |                  origin and importance, specificity, order
   v
declared values: "var(--gap)", "calc(100% - 2em)", "1.2em"
   |  Css_values      var() substituted, lengths and calc() resolved,
   |  Computed        inherited or initial: a record per element
   v
computed styles
   |  Box_layout      the box model: margins, borders, paddings, widths
   |  Flex_layout     rows and columns
   v
boxes, placed
```

Each section ends with the worked example its tests check.

## 1. Tokens (C1)

CSS's syntax is its own, older than its properties: CSS Syntax Level 3
wrote down the tokenizer every browser had converged on, and it is
small -- identifiers, functions (`rgb(`), at-keywords (`@media`),
hashes (`#top`), strings, `url()`s, numbers with their kind (`2`,
`50%`, `1.5em`), delimiters (`>`, `.`, `*`), and the punctuation
(`:` `;` `,` `(` `)` `[` `]` `{` `}`). Comments go, spaces are one
token (a descendant combinator is a space). The one thing a
hand-written split on braces gets wrong is **nesting**: a `{` inside a
string, a `}` inside a `url()`, a block inside `@media`. Tokens first,
then blocks by matching brackets, and nothing is lost.

Worked example (the tests'):

```
a:hover > .x { color: #f00 !important }

Ident a, Colon, Ident hover, Whitespace, Delim >, Whitespace, Delim .,
Ident x, Whitespace, { ... }: Ident color, Colon, Whitespace, Hash f00,
Whitespace, Delim !, Ident important, Whitespace
```

## 2. Rules, blocks, and errors (C1)

A style sheet is a list of **rules**: a *qualified rule* (a prelude, the
selectors, then a `{ }` block of declarations) or an *at-rule* (`@media
screen and (max-width: 800px) { rules }`, `@import url(x.css);`,
`@font-face { }`). A declaration is `name: value`, ended by `;` or the
block's end, `!important` at its end taken off and remembered.

**Errors are skipped, not fatal** -- CSS's great rule, what lets a
browser read a sheet written for a newer one: a declaration that does
not parse is dropped up to its `;`; a rule, up to its block's end; an
unknown at-rule, with its block. So a sheet using what we do not know
still gives us what we do.

Worked example:

```
p { color: red; width: ; margin: 0 }     two declarations: color, margin
@media (max-width: 800px) { p { color: blue } }
                                         a media rule holding one rule
@supports (display: grid) { ... }        unknown: skipped whole
```

## 3. Selectors (C1)

A selector is **compounds joined by combinators**:

```
ul > li.item:not(.done)
   compound "ul";  combinator ">" (a child);
   compound "li.item:not(.done)": the name li, the class item, and
   :not(.done)
```

Combinators: a space (a descendant, at any depth), `>` (a child), `+`
(the sibling just before), `~` (any sibling before). A compound: a name
or `*`, then any of `#id`, `.class`, `[attr]`, `[attr=v]`, `[attr~=v]`
(one of its words), `[attr^=v]` `[attr$=v]` `[attr*=v]` (starts, ends,
contains), `[attr|=v]`, pseudo-classes (`:first-child`, `:last-child`,
`:only-child`, `:nth-child(an+b)`, `:not(...)`, `:link`, `:visited`,
`:hover`, `:root`, `:empty`, `:checked`), and a pseudo-element at the
end (`::before`, `::after`: boxes the element generates).

**Matching goes right to left**: the element must match the last
compound; then, for a descendant combinator, some ancestor the one
before, and so on. Right to left because most elements fail the last
compound at once (it names a class they do not have), and nothing
further is looked at.

**Specificity** is (ids, classes and attributes and pseudo-classes,
names and pseudo-elements); `:not(x)` counts as x; `*` counts nothing:

```
ul > li.item:not(.done)     (0, 2, 2)
#nav a:hover                (1, 1, 1)
*                           (0, 0, 0)
```

## 4. The cascade (C2)

For one element and one property, the declarations that apply are
sorted, the last winning:

1. **origin and importance**: the browser's sheet, then the page's,
   then the page's `!important`, then (not ours) the user's
   `!important`;
2. **specificity**;
3. **order** of appearance -- `style="..."` counting as the most
   specific.

Where no declaration applies, the property is **inherited** (color,
font, text-align, white-space, line-height, visibility, list-style...)
or takes its **initial value** (margins 0, display inline, background
transparent...).

**The index.** Trying every rule on every element is rules x elements:
Wikipedia's 1,391 on an article's ~10,000 elements is fourteen million
matches. Engines index the rules by their last compound's most specific
part -- its id, else its first class, else its name, else "any" -- and
try on an element only the rules under its id, its classes, its name,
and "any": a few dozen.

Worked example (the tests'):

```
p { color: black }  .x { color: green !important }  #a { color: red }
<p id=a class=x>    green: !important beats a higher specificity
```

## 5. Values: var(), calc(), lengths (C2)

**Custom properties** are properties whose name starts with `--`,
inherited, with no meaning of their own; `var(--name, fallback)`
substitutes one's value into another property's before that is read:

```
:root { --accent: #36c; --gap: 8px }
a     { color: var(--accent); margin: 0 var(--gap) }
      color #36c; margin "0 8px"
.dark { --accent: #9cf }        inside .dark, the same rule gives #9cf
```

**Lengths** resolve to pixels: `px`; `em` (the element's font size --
its parent's, for `font-size` itself); `rem` (the root's); `%` (of the
containing block's width, for widths and margins); `pt` (96/72 px).
**`calc()`** combines them, resolved when the percentage's base is
known:

```
width: calc(100% - 2em), the block 600 wide, the font 16:  600 - 32 = 568
```

**Media queries** are evaluated once against the window:
`(max-width: 800px)` is false in TinyChrome's 976 pixels, so Wikipedia's
mobile rules stay asleep; `screen` true, `print` false,
`(prefers-color-scheme: dark)` false.

## 6. Computed style, and the browser's own sheet (C2)

A **computed style** is a record per element with every property's
value, lengths in pixels -- what layout reads, never a declaration's
text. The browser's defaults are a style sheet like the page's, the
**user-agent sheet**, in CSS (`ua.css`): `h1 { display: block;
font-size: 2em; font-weight: bold; margin: 0.67em 0 }`, and so on,
CSS 2.1's appendix D -- the table of `Looks.mli` written in the
language the pages use, which is what it always was.

## 7. The box model (C3)

Every block is a content box inside its padding, border and margin:

```
margin-left | border | padding | content width | padding | border | margin-right
```

Their sum is the width of the containing block: CSS 2.1's section
10.3.3, one equation with the `auto`s as its unknowns. With a width
given and both margins `auto`, the margins share what is left: centring.

```
containing block 976, width 400, padding 10 each, border 1 each,
margins auto:  (976 - 400 - 22) / 2 = 277 each
```

`box-sizing: border-box` makes the width include padding and border
(GitHub's and Google's sheets set it everywhere). Vertical margins of
adjacent blocks **collapse** (the larger of the two), as
`Html_layout` already does. An **inline-block** is laid out as a block
of its own width (its content's, shrink-to-fit, when `auto`) and set in
the line as one unit, as a picture is.

## 8. Flexbox (C5)

A flex container lays its children along a main axis (a row, or a
column), then:

1. each item's **base size** (its `flex-basis`, else its width, else its
   content's);
2. the room left (the container's size less the bases and the gaps) is
   shared by `flex-grow` -- or, if negative, taken by `flex-shrink`
   weighted by the base sizes;
3. `justify-content` places the items along the axis (start, center,
   end, space-between, space-around), `align-items` across it
   (stretch, start, center, end);
4. with `flex-wrap: wrap`, items that do not fit start a new line.

```
row 600 wide, gap 10, three items of base 100, grow 0, 1, 2:
   room 600 - 300 - 20 = 280; shared 0 : 1 : 2 -> 0, 93.3, 186.7
   widths 100, 193.3, 286.7
```

## Exercises

1. CSS grid's common part: `grid-template-columns` with `fr`, and items
   placed in order -- Wikipedia's sidebar beside the article.
2. `:nth-child(an+b)` with its `of S` form; `:is()` and `:where()` (the
   latter of specificity 0).
3. `::before` and `::after` with `counter()` in `content`.
4. Absolute positioning done properly: the containing block found, the
   static position.
5. The index measured: Wikipedia's article styled with and without it.

## References

- W3C, *CSS Syntax Module Level 3* (tokens, rules, error recovery);
  *Selectors Level 3*; *CSS Cascading and Inheritance Level 4*; *CSS
  Custom Properties Level 1*; *CSS Values and Units Level 3*; *Media
  Queries Level 3*; *CSS 2.1* chapters 8 to 10 and appendix D; *CSS
  Flexible Box Layout Level 1*, section 9 (the algorithm).
- Pavel Panchekha and Chris Harrelson, *Web Browser Engineering*,
  chapters 6 ("Applying Author Styles") and 7.
- Matt Brubeck, "Let's build a browser engine!" (robinson), parts 3 to
  6: a CSS parser, selector matching, the style tree, the box model.
- WebKit's and Blink's selector matching ("right to left", rule
  hashing by the rightmost compound): David Hyatt's posts on WebKit's
  style system.
