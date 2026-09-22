# Plan: loading and saving documents -- Save, Open, Export, Import

## Context

The applications of [`plan_gui_teaching.md`](plan_gui_teaching.md)
(TinyExcel, TinyWord, TinyMacPaint, TinyOpenDoc, TinyPowerPoint) edit
documents that never leave memory. Each can already write itself down
-- as a value (`Sheet.t`, `Rich.t`, `Bitmap.t`, an outline) or as text
(`Compound.save`) -- and none can put it anywhere. The author's
decisions (2026-09-21):

- **Marshal**, not a format of our own: a document is an OCaml value,
  and `Marshal` writes values.
- **Both kinds of saving**: a store of named documents (Save / Open),
  and real files in and out (Export / Import).
- **On the web too**, with a web build of the apps to try it:
  `apps/web/` (not `apps/js/`; the other `js/` directories are to be
  renamed later by the author, for consistency).
- **With capabilities** -- the authority side is
  [`plan_caps.md`](plan_caps.md); this plan only says which capability
  each operation takes.

## Marshal: what it can and cannot write

- **Data, yes**: `Sheet.t`, `Rich.t` (the piece table and its runs --
  sharing is kept, so shared pieces stay shared), `Bitmap.t` (its
  `Bytes`), an outline's text, a master.
- **Closures, no**: TinyOpenDoc's parts and TinyPowerPoint's embedded
  parts are records of functions (`Component.part`). Native `Marshal`
  writes closures with `[Closures]`, but only the very same binary can
  read them back, and js_of_ocaml cannot marshal functions at all. So
  for those, what is marshalled is each part's `kind` and saved text,
  read back through the registry -- the compound-document lesson again:
  a part is code, only its data travels.
- **Unchecked on reading**: `Marshal.from_string` believes the type it
  is told; a file from an older build whose types changed can crash the
  program. So a document starts with a line naming the application and
  a format version (`"TinyExcel 1\n"`), checked first, which turns that
  crash into "not a file I can read". (The honest limit: a version
  number bumped by hand is only as good as the person bumping it.)
- **Between native and web**: js_of_ocaml implements the same format
  for data, with 32-bit ints, so an int beyond 2^31 written natively
  would not load in the browser. Nothing in these documents comes
  close.

One small pure module for this, **`appkits/document/Saved`**:
`to_string : magic:string -> 'a -> string` and
`of_string : magic:string -> string -> 'a option`, its `.mli`
explaining the above, tested (round trips, a wrong magic, a truncated
string).

## Save / Open: a store of named documents

Synchronous, so it fits `update` as it is:

- `Playground_platform.store : < Cap.open_out; .. > -> string -> string -> unit`
  (a document's name, its bytes)
- `Playground_platform.fetch : < Cap.open_in; .. > -> string -> string option`
- perhaps `stored : < Cap.readdir; .. > -> string list`, for an Open
  menu that lists what there is.

Natively: files in a directory -- the current one (simplest, visible)
or a per-app one (`~/.elm-playground/TinyExcel/`), to decide. On the
web: the browser's **`localStorage`** -- strings only, so the bytes are
base64-encoded (`graphics/images/Base64` exists but belongs to the
native package: a copy, or a move after asking). It lives in that
browser, for that site, and survives a reload.

## Export / Import: real files

- **Export**: `Playground_platform.export : < Cap.open_out; .. > -> string -> string -> unit`.
  Natively a file written in the current directory; on the web a
  download (a `Blob` and a click on an `<a download>`).
- **Import**: a file **dropped on the window** -- SDL's `SDL_DROPFILE`
  natively, the HTML5 `drop` event on the web. The platform reads it
  and gives `update` its name and contents in a new transient field of
  the computer, `computer.dropped : (string * string) option`, set for
  one frame like `typed`. No capability on the program's side: the drop
  is the grant (the powerbox, see `plan_caps.md`). A file *picker*
  could come later; on the web it needs a user gesture and answers
  asynchronously, which the drop avoids.

## What each app saves

- **TinyExcel**: the `Sheet.t`. **TinyVisiCalc** could share its file
  (the same engine) -- VisiCalc's `/S` command, if wanted.
- **TinyWord**: the `Rich.t` and the alignment.
- **TinyMacPaint**: the `Bitmap.t`.
- **TinyPowerPoint**: the outline's text, the master, and each part as
  (slide number, kind, saved text).
- **TinyOpenDoc**: the document as a tree of (kind, saved text) --
  `Compound.save`'s content as a value rather than as its text format,
  or simply that text; to decide while doing it.
- Each app: File > Save, Open, Export; Import by dropping. The status
  line says what happened ("saved, 1,204 bytes"; "not a TinyExcel
  document").

## Phases

1. `appkits/document/Saved` and its tests -- **done** (2026-09-21),
   with a check that all of Marshal's data is there as well as the
   line; its first user is TinyOpenDoc's drawing part
   (`apps/Part_drawing`), whose part text is a drawing so saved.
2. `store` / `fetch` / `export` in the native and software backends
   (with `caps`, `plan_caps.md` phase 1); File > Save / Open / Export in
   TinyExcel first, then TinyWord, TinyMacPaint, TinyPowerPoint,
   TinyOpenDoc. **Done** (2026-09-22): `Playground_platform.store`,
   `fetch`, `stored`, `export`, each taking its capability
   (`native_common/Store`: $ELM_PLAYGROUND_STORE, else
   ~/.elm-playground/documents); `apps/File_menu`, the menu and its two
   dialogs (Save As's name field takes the keys at once, Open lists the
   documents of the app's extension), in all the menu-driven apps --
   TinyExcel (.sheet), TinyWord (.doc), TinyMacPaint (.paint),
   TinyMacDraw (.draw), TinyPowerPoint (.slides), TinyOpenDoc
   (.opendoc), TinyFrameMaker (.frame), TinyOffice (.office, one type
   for its five kinds, and Open... on its start screen) -- and in their
   period's own way in TinyHyperCard (no Save: once named, every change
   is written) and TinyVisiCalc (/S S and /S L, the same .sheet file as
   TinyExcel). TinyBravo is left without, its own command letters for
   files not being checked. The documents with parts save each part as
   (kind, saved text) -- TinyOffice by making its records polymorphic
   in the part, so the saved form is the same records.
3. Import by drop: `computer.dropped`, from SDL's drop event.
4. `apps/web/`: the apps built for the browser (like `games/<genre>/web/`:
   `copy_files` of `../Foo.ml`, `(modes js)`, an `.html` each) -- to
   check on the way that `Stroke_text`'s Hershey data (`graphics_font`)
   compiles to JavaScript and that the text area and menus behave with
   DOM key names; then the web `store`/`fetch` (localStorage, base64),
   `export` (download) and the `drop` event. **The store and export
   done** (2026-09-22; built, not yet tried in a browser):
   `playground/web/Web_store`, localStorage under "elm-playground:" and
   the name, the bytes base64-encoded in OCaml (Marshal's bytes are not
   UTF-8, so the browser's btoa cannot take them), `export` an `<a
   download>` clicked; `caps` links under js_of_ocaml with no warning.
   The `drop` event goes with phase 3.
5. A golden scene saving and opening again in one run (the software
   backend's store in a temporary directory); the apps' headers lose
   their "does not do: saving" lines; the status here. **Done**
   (2026-09-22): tests/common/Testutil_golden gives every rendering an
   empty store of its own; scenes TinyExcel_saved,
   TinyVisiCalc_storage, TinyOffice_reopened, and TinyOpenDoc_reopened
   and _drawing (its old Save/Revert in memory replaced by the store).

## Verification

- `Saved`'s tests: a round trip of each document type, a wrong magic
  refused, a truncated string refused.
- Natively: save, quit, start again, open -- by hand once; then the
  golden scene.
- On the web: the same in a browser, a reload in between
  (localStorage survives it), and an exported file dropped back in.
