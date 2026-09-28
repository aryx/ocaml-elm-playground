# Plan: what's left for the teaching browsers (TinyMosaic, TinyNetscape)

The plan is done: see
[`done/plan_browser_teaching.md`](../done/plan_browser_teaching.md) --
the engine in `libs/web/` (`Html_lexer`, `Dom`, `Dtd`, `Html_tree`,
`Line_mode`, `Looks`, `Html_layout`, `Hit`, `Forms`, `Table_layout`,
`Css`), the appkit `appkits/browser` (`Browser_page`, `Browser_draw`,
`Browser_history`, `Browser_forms`, then `Browser_tab`), `tiny_httpd`
and its CGI (`Http_server`), `Worker`'s threads, and two browsers over
them: TinyMosaic (1993: the pipeline, pictures one at a time, forms)
and TinyNetscape (1994-1997: not waiting, Netscape's extensions marked
in the tree, tables, CSS1), with the tutorial
[`notes_browser.md`](../../tutorials/notes_browser.md). Its successors
have their own: TinyFirefox (`plan_tiny_firefox_remaining.md`) and
TinyChrome (`plan_tiny_chrome_remaining.md`), whose engine
(`Box_layout`, `Cascade`, `Computed`) already does some of what is
below for TinyChrome only.

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. The tree: the adoption agency

- `<b><i>x</b>y</i>`: ours gives `b(i("x")), "y"`, every browser
  shows y in italics. The WHATWG's adoption agency algorithm, the
  misnested formatting elements reopened (`Html_tree.mli` has the
  worked example; `notes_browser.md`'s first exercise).
- Then compared again with html5lib on the listed soup cases, by hand,
  as phase 2 did.

## 2. TinyMosaic's exercises

- **XBM**, the first inline format Mosaic read: a C file as a picture,
  a reader of thirty lines in `graphics/images/xbm/`, beside `xpm/`.
- **A `<select>` that pops a menu** of its options, as Motif's did,
  instead of cycling through them on a click.
- **Mosaic's way with pictures exactly**: the page not shown until its
  last picture is in, the view frozen meanwhile (`fetch=mosaic`), felt
  against a slow server.
- **A guest book**: a CGI program in OCaml, over `Urlencoded`, that
  appends what a form sent to a file and answers with the whole book.
- **`TinyLineMode`**: the Line Mode Browser (1991) as a program of its
  own, fifty lines on the engine (`Line_mode` is the `l` view today).

## 3. N3's Netscape HTML not honoured

Marked in `Dtd`'s table, ignored by `Looks` and `Html_layout`:

- `<blink>`, `<basefont>`, `<nobr>` (and `<wbr>` inside it);
- a picture's `border=`, `hspace=`, `vspace=` (TinyChrome's `Cascade`
  maps hspace and vspace as presentational hints; TinyNetscape does
  not);
- the lists' `type=` (disc, circle, square; 1, a, A, i, I);
- `background=`, a picture tiled under the page.

## 4. N4's tables

- `rowspan=`, a cell down several rows (in `Table_layout`'s grid; also
  TinyChrome's, section 3 of its remaining);
- a cell's `width=` and `bgcolor=` (Netscape 3.0);
- the fixed layout (`table-layout: fixed`: the first row decides);
- caching the cells' two measures (each laid out at width 0 and
  without limit, again at each relayout).

## 5. N5's CSS1, in TinyNetscape

`Css` was rewritten over TinyChrome's `Css_syntax` and `Selectors`
(C1), which brought `!important` and `:link`/`:visited` to the
matching. What TinyNetscape still does not draw:

- **`<link rel=stylesheet>`**: TinyChrome's tab fetches them
  (`Browser_page.sheets_wanted`); TinyNetscape's settings answer no
  sheet (`sheet = fun _ -> None`). Netscape 4 did fetch them.
- **padding and borders**, and an inline element's background:
  `Box_layout` has them, `Html_layout` (a box's `indent` and `right`)
  does not.
- **The user agent's sheet written as CSS**: the looks' table is still
  OCaml in TinyNetscape (TinyChrome has `ua.css`); the two compared.
- `a:link` and `a:visited` checked on TinyNetscape's own pages, with a
  golden frame.

## 6. TinyNetscape's exercises

- **Frames** (Netscape 2, 1996): `<frameset>` and `<frame>`, a window
  cut into pages, each its own `Browser_tab`; `Html_tree` leaves
  frameset out today.
- **Cookies** (Lou Montulli, 1994): out of scope here, planned in
  TinyChrome's remaining (section 5: request headers first, then a jar).
- **A page drawn while it arrives**: `Http_request` giving its body in
  pieces, the page parsed and laid out again as they come (N1 waits for
  the whole page).
- **The slow server**: `tiny_httpd delay=` was not done (a sleep in its
  handler stops the whole server); a server that answers slowly without
  blocking, to show `threads=on` against `threads=off`.
- **Domains**: the pictures decoded on `Worker`'s pool, measured (no
  faster under OCaml 4.14's lock), then on OCaml 5's domains.
- **Incremental layout**: the page is laid out whole each time; what
  real engines do instead is TinyChrome's "relayout only where it
  changed" (its section 2).
