# Plan: personal information managers (`apps/pim/`)

## Context

CATALOG.md's `## PIM` section is empty ("a calendar, an address book,
a to-do list would go here"), listed among the office kinds of program,
and `apps/system/dune` names the Mac's desk accessories (Alarm Clock,
Note Pad) among what it might hold. The author's decisions
(2026-09-24):

- **A category of its own**, `apps/pim/`, laid out like the others
  (the programs, `software/` for the golden frames, `web/`), its
  section in CATALOG.md leaving the office introduction.
- **Three programs**, in this order: **TinyClock**, **TinyCalendar**,
  **TinyPalmPilot**.
- **The address book and the to-do list, classics, are in**, but as
  the Palm's own applications rather than programs of their own: the
  Palm Pilot *is* Date Book, Address, To Do and Memo Pad behind four
  buttons. Each is a module, `Part_*`-style, the way `Part_sheet` is
  both TinyExcel and a part of TinyOffice; a standalone
  `TinyAddressBook` or `TinyTodo` only if one day it has a lesson the
  Palm's does not (an exercise in the plan, not a phase).
- **No TinyNotes, no TinyReminder** on their own: notes are TinyWord's
  lesson again (the Palm's Memo Pad, plain text, is enough), and a
  reminder cannot remind anyone while the program is closed -- a to-do
  with a due date is what is left, the Palm's To Do.

## Principles (the README's, the ones that matter here)

- **No wall clock inside a library** (principle 5). The date library
  takes seconds since the epoch and an offset as arguments; only the
  apps read `computer.time`, which `-fixed-time` already pins for the
  golden frames.
- **The simple version beside the better one**: the day of the week by
  Zeller's congruence beside the days-from-civil count it checks.
- **Every `.mli` explains its idea**, worked examples tested:
  1970-01-01 is day 0 and a Thursday; 2000-02-29 exists and
  1900-02-29 does not; `cal 9 1752`.
- **Our own formats, read and written**, like MOD and XPM: iCalendar
  (`.ics`) and vCard (`.vcf`), the subsets we use, a file any other
  calendar or address book opens.

## Target layout

```
libs/core/time/           (library `elm_core`, unwrapped; a folder only)
  Civil.ml/.mli           days <-> (year, month, day); leap years; weekday
                          (Hinnant's days_from_civil, 2013; Zeller, 1882)
  Julian.ml/.mli          the calendar before 1752-09-14 (England's switch),
                          for cal's famous month; Julian day numbers
  Clock.ml/.mli           seconds -> (hour, minute, second) with an offset;
                          what a day is once the epoch is split
  Recur.ml/.mli           repeating events: RFC 5545's RRULE, the subset
                          (DAILY, WEEKLY;BYDAY, MONTHLY;BYMONTHDAY,
                          YEARLY, COUNT, UNTIL), occurrences in a window
libs/core/tests/          Unit_civil, Unit_clock, Unit_julian, Unit_recur
                          (beside Unit_base64)
appkits/pim/              (library `appkit_pim`: the PIM apps' files)
  Ics.ml/.mli             iCalendar read and written: VEVENT, VTODO,
                          line folding, escaping, RRULE's text
  Vcard.ml/.mli           vCard 3.0 read and written: N, FN, TEL, EMAIL, ADR
appkits/tests/            Unit_ics, Unit_vcard
apps/pim/
  TinyClock.ml
  TinyCalendar.ml
  Pim_date_book.ml/.mli   the Palm's four applications, each a module
  Pim_address.ml/.mli     with its model, update and view, the device
  Pim_todo.ml/.mli        around them in TinyPalmPilot
  Pim_memo.ml/.mli
  Palm.ml/.mli            the screen (160 x 160 dots, 4 x), the stylus,
                          the shared data (the four databases)
  TinyPalmPilot.ml
  software/  web/
```

The date modules go in `libs/core/` (the author, 2026-09-24: core is no
longer only Elm's stand-ins), in its folder `time/` (core's dune says
`(include_subdirs unqualified)`, as `playground/`'s does) -- and one day
a `space/` beside it, the coordinates and projections of a map program
(a TinyGoogleMaps). The files, iCalendar and vCard, are the apps'
data, so an appkit's (the author, 2026-09-24), `appkit_pim`, where the
Palm's shared models can go too. Both are unwrapped, so each name is
global: none of `Civil`, `Julian`, `Clock`, `Recur`, `Ics`, `Vcard` is
taken today; grep again before adding one.

## The platform: the local time

The native loop's time is `Unix.gettimeofday ()`, seconds since the
epoch, so the wall clock is already there; the **offset from UTC** is
not. One addition, per backend:

- natively, `Unix.localtime` against `Unix.gmtime` of the same instant;
- on the web, `Date.prototype.getTimezoneOffset`;
- pinned to 0 (UTC) under `-fixed-time`, so the golden frames do not
  depend on where they are rendered.

Shape (done, phase 2): `Playground_platform.utc_offset : time -> int`,
minutes east of UTC at that instant, since daylight saving changes it
-- a function the apps call with the time they have, rather than a
field of `computer`, which no backend's loop had to learn. Natively in
`Native_loop_2d` (it knows `-fixed-time`), shared by the Cairo and
software platforms; the 3D platforms have none until a 3D app wants
one. The zone *rules* (the tz database) are out of scope:
the platform knows them, we ask it.

## The programs

### TinyClock -- after the Mac's Alarm Clock (1984) and xclock (1986)

The smallest program, there to force the plumbing: the date library's
first modules (`Civil`, `Clock`) and the offset hook.

- An analog face (xclock's) and the digital one (the Alarm Clock's
  strip), switchable; the date under it.
- The alarm: a time set with the keys, a sound (`Audio`) when it comes,
  while the program runs -- honestly said, the limit of a Playground app.
- A second row of cities, UTC offsets typed in: the world clock, what
  an offset is.

What it brought: the time you read on a wall, not the time an animation
counts; a day as 86400 seconds split by an offset.

### TinyCalendar -- after Unix's `cal` and iCal (Apple, 2002)

- A month as `cal` prints it, a grid of weeks, moved with the arrows;
  `cal 9 1752` shows its eleven missing days (`Julian`), a flag or key
  switching England's switch off.
- A week view with events as boxes in their hours; an event added,
  moved, stretched with the mouse.
- Repeating events (`Recur`): each occurrence computed in the visible
  window, never stored.
- Exported and imported as `.ics` (`Ics`), with
  `Playground_platform.export`; saved in the store (`plan_io.md`),
  through `File_menu`. (Done: Import is a menu of the store's `.ics`
  files, since the platform has no file chooser; their UTC times are
  shown at the local offset of their own instant.)

What it brought: the Gregorian calendar computed, not looked up; a
repetition as a rule, not a list; a file every other calendar reads.

### TinyPalmPilot -- after the Palm Pilot (Jeff Hawkins, Palm, 1996)

A 160x160 screen, drawn in its greys, and the four buttons under it.

- **Date Book** (`Pim_date_book`): the day as a list of hours, over
  TinyCalendar's modules, its events the same `.ics`.
- **Address** (`Pim_address`): records sorted by last name, found as
  you type (the Palm's look-up line), categories; exported as `.vcf`
  (`Vcard`).
- **To Do** (`Pim_todo`): items with a priority 1 to 5, a due date and
  a check box, sorted by priority then date, "show completed" and
  "show only due"; exported as `VTODO`s in the `.ics`.
- **Memo Pad** (`Pim_memo`): plain text memos, the first line their
  title.
- The device: the four buttons as keys (1-4), the menu, categories.
  (Done: the buttons clicked on the case or F1..F4 -- the digits are
  for writing phone numbers -- and the rocker; no menu, no categories,
  both left as exercises in the header. The data stored at every frame
  that changed it (Saved, one file in the store) and read back at the
  start: no Save. The text drawn from Hershey's strokes by Palm itself,
  not the playground's [words], so that a measured line is the drawn
  one on every backend.)
- Later (exercises, or a phase if it goes well): **Graffiti**, the
  single-stroke alphabet recognized from the mouse's path (a small
  recognizer, `ai/`-style, with its worked strokes); **HotSync**, two
  copies edited apart and merged, the conflicts shown -- over
  `networking/`'s `Sim_net`, the record-level merge the lesson.

What it brought: a computer that fits in a shirt pocket because it does
four things; zero waiting (instant on, no Save: every change kept);
syncing with the desktop as the design's other half.

## Phases

1. **`Civil` and `Clock`**, their tests (the worked examples above,
   and a round trip over every day from 1600 to 2400).
2. **The offset hook**, native and web, pinned under `-fixed-time`.
3. **TinyClock**: CATALOG.md's section moved to its own category,
   the row, golden frame, web page (`tests/catalog/` checks all three).
4. **`Julian`, `Recur`, `Ics`**, tested (RFC 5545's own examples for
   `Recur`; a round trip and a file from another calendar for `Ics`).
5. **TinyCalendar**, month and week views, events, export/import, store.
6. **`Vcard`**, tested.
7. **TinyPalmPilot**: the four modules and the device; Date Book over
   phase 4-5's code.
8. (Optional) Graffiti; HotSync.

## Verification

- `make test-lite`, plus the new unit tests and golden frames
  (`tests/2d/golden/TinyClock.png`, `TinyCalendar.png`,
  `TinyPalmPilot.png`, rendered with `-fixed-time` at a chosen instant,
  UTC); a `-script` scene per program showing an event added, a
  contact found, a to-do checked.
- An `.ics` and a `.vcf` we write, opened by another program (GNOME
  Calendar, Thunderbird) by hand, once.
- Every executable on the web too (`apps/pim/web/`).

## Out of scope

- The tz database (the platform's offset is enough).
- Notifications while the program is closed.
- CalDAV/CardDAV, any server (TinyIRC is the network's app).
- Calendars other than the Gregorian and the Julian.
