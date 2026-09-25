# web/: a web browser's engine, from scratch, for teaching

One idea per module, each `.mli` with its diagram, worked example and
references. Pure OCaml, no shapes: bytes in, a tree, then boxes with
positions out; the browser that fetches the bytes and draws the boxes
is TinyMosaic (`apps/internet/`). The plan is
`docs/claude_notes/plans/plan_browser_teaching.md`, the tutorial
`docs/claude_notes/tutorials/notes_browser.md`.

    bytes --Charset--> text --Html_lexer--> tokens --Html_tree--> tree
          --Looks/Css--> looks --Html_layout--> boxes --(the app)--> shapes

| folder (library) | what | modules |
|---|---|---|
| `html/` (`web_html`) | a page read | `Charset` (which encoding, and the page in UTF-8), `Entities` (`&eacute;`, `&#233;`), `Html_lexer` (the text to tokens, the WHATWG's state machine), `Dom` (the tree), `Dtd` (HTML's rules as data, MMM's way), `Html_tree` (tokens to a tree, the stack of open elements), `Line_mode` (the tree as the 1991 Line Mode Browser showed it) |
| `style/` (`web_style`) | what each element looks like | `Looks` (Mosaic's fixed table, CSS 2.1's appendix D numbers); to come: `Css` (CSS1's cascade) |
| `layout/` (`web_layout`) | where everything goes | `Html_layout` (blocks stacked, margins collapsed, inline content on lines sharing a baseline); to come: `Hit` (a point to a link), `Table_layout` |

`web` itself has no module: it is all of them, for what says
`(libraries web)`. The fetching is `networking/`'s (`Url`, `Http`,
`Http_request`), the images `graphics/images/`', the line breaking
`appkits/typeset`'s.
