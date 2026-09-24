# Plan: what's left for the personal information managers

The plan is done: see [`done/plan_pim.md`](done/plan_pim.md) -- core's
`time/` (`Civil`, the Gregorian calendar computed; `Clock`, the wall
clock's time with a UTC offset; `Julian`, the calendar before the
switch; `Recur`, repeating events), `Playground_platform.utc_offset`,
the appkit `appkits/pim` (`Ics`, iCalendar, and `Vcard`, vCard 3.0,
read and written), and `apps/pim/`'s three programs: TinyClock (the
Alarm Clock and xclock), TinyCalendar (cal and iCal: a month, a week,
September 1752) and TinyPalmPilot (Date Book, Address, To Do, Memo Pad
over `Palm`, the device's screen).

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. Graffiti

The Pilot's alphabet, and the half of the Pilot TinyPalmPilot leaves
out (the keyboard stands in for it): each letter one stroke, drawn the
way the letter looks, so that the machine never guesses -- the person
learns what it reads.

- **The recognizer**, pure, in `ai/` or `appkits/pim`: a stroke (the
  mouse's path while the button is down, in the silk-screened area) to
  a letter. Two versions, the simple one kept: the **$1 recognizer**
  (Wobbrock, Wilson and Li, 2007: resample to 64 points, rotate to the
  indicative angle, scale, translate, nearest template by path
  distance) against Graffiti's 26 letter strokes and 10 digits as
  templates; then a small classifier from `ai/learning/` trained on
  strokes drawn by hand, the accuracy of each shown as a number.
- **The areas**: letters on the left, digits on the right (the
  silk-screen already draws them), a tap for a space, the stroke right
  to left for backspace, and the shift stroke.
- **The ink**: the stroke drawn as it is made, fading after, as on the
  device.
- Worked example: an "A" drawn as an upside-down V, a "T" as its two
  strokes in one, recognized, with their distances to the other
  templates.

## 2. HotSync

The cradle's one button: the Palm's databases and the desktop's copied
into each other, the conflicts found. What made a Palm usable is that
it was never the only copy.

- **The merge**, pure (`appkits/pim`): two copies of the four databases
  and the last synchronized one, a record-level three-way merge by uid
  -- added on one side, changed on one side, deleted on one side,
  changed on both (a conflict: the Palm kept both records, the second
  marked) -- with its worked example and a table of the cases tested.
- **Over the wire**: the desktop side as TinyCalendar (its events,
  iCalendar) and an address book, the two ends connected through
  `networking`'s `Sim_net` (a latency, a dropped packet) or the
  `Relay`; a record at a time, so an interrupted sync resumes.
- **The files**: a Palm's databases exported as an `.ics` and a `.vcf`
  (Export on each application, as TinyCalendar has), which is the
  sync's poor man's version and the first step.

## 3. The calendar's missing pieces

- **"This occurrence or all of them?"**: iCalendar's EXDATE (a date
  taken out of a rule) and RECURRENCE-ID (one occurrence changed), in
  `Recur` and `Ics`; dragging an occurrence of a repeating event in
  TinyCalendar then asks, as iCal does. The one thing a real calendar
  does that ours visibly doesn't.
- **Time zones**: an event at 9:00 in New York shown at 15:00 in Paris.
  The platform knows only the local offset; the rules of other zones
  are the tz database (a zone's history of offsets and daylight-saving
  rules), read from `/usr/share/zoneinfo`'s TZif files natively -- a
  small binary format of its own, worth a module and a worked example
  -- and absent on the web (where `Intl.DateTimeFormat` knows them).
  Then `Ics`'s TZID read instead of taken as floating.
- **The rest of RRULE**: BYDAY with an ordinal (the 1st Friday, the
  last Sunday: "1FR", "-1SU"), BYMONTH, BYSETPOS -- refused today by
  `Ics.rule_of_string`; with RFC 5545's examples that use them as the
  tests (US Thanksgiving, Election Day).
- **Views**: overlapping events side by side in the week (the columns
  an interval graph's coloring gives), a year view (twelve `cal`s), and
  the Date Book's week view on the Palm.

## 4. The Palm's missing pieces

- **Categories** (Business, Personal, Unfiled): the Palm's one way of
  sorting anything, a pop-up in each application's title bar.
- **Find**: a word looked for across the four databases, the results
  listed by application.
- **The Details dialogs**: an event's time and repetition, a to-do's
  date and category, set by taps rather than typed.
- **A standalone TinyAddressBook or TinyTodo**, only if one has a
  lesson the Palm's doesn't (the plan's decision, 2026-09-24): e.g. an
  address book over `.vcf` files with vCard 4.0 and its differences, or
  a to-do list after Getting Things Done.

## 5. Checks not yet made

- An `.ics` from TinyCalendar's Export and a `.vcf` from `Vcard`
  opened in another program (GNOME Calendar, Thunderbird, Evolution),
  once, by hand -- the round trips are tested, a foreign reader isn't.
- The three programs run in a browser (`apps/pim/web/`: built, not
  played), and `apps/pim/web` in the Makefile's `js` target.
- `Playground3d_platform.utc_offset`, when a 3D program wants the local
  time (none does).

## 6. Settled, and not to be re-opened

- The date modules are in core (`libs/core/time/`), the files in an
  appkit (`appkits/pim`): core knows time, the appkit knows the apps'
  data.
- No wall clock inside a library: the seconds and the offset are
  arguments, only an app reads `computer.time`.
- The offset is asked of the platform, never computed from rules we'd
  have to carry.
- The Palm's text is drawn by `Palm` from Hershey's strokes, not the
  playground's `words`.
