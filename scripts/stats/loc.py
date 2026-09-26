#!/usr/bin/env python3
# Claude Code
#
# Copyright (C) 2026 Yoann Padioleau
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License
# (LGPL) as published by the Free Software Foundation; either version
# 2 of the License, or (at your option) any later version.
#
# Lines of OCaml across the project (.ml and .mli), grouped by what
# they are: the library (libs/ and playground/), the games (gamekits/
# and games/), the apps (appkits/ and apps/), the examples, and apart
# from all of them the tests (every tests/ directory, and the top
# tests/). Each line is counted once, as code (it has some code, maybe
# a comment too), comment (only a comment, or inside one) or blank.
#
# The files are git's (tracked, and new ones not ignored), so _build/
# and the generated web copies are never counted twice.
#
# Usage: scripts/stats/loc.py [-v]
#   -v: every subdirectory of libs/, playground/, games/, apps/, ...
#       rather than one line per group (libs/'s are always listed)
#
# The lines come first, next to the name they count; files, .ml,
# .mli, code, comment and blank lines after the name.

import re
import subprocess
import sys
from collections import defaultdict

# ---------------------------------------------------------------------
# Counting the lines of a file
# ---------------------------------------------------------------------

CHAR = re.compile(r"'(\\[\\'\"ntbr ]|\\[0-9]{3}|\\x[0-9a-fA-F]{2}|[^\\'\n])'")
QUOTED = re.compile(r"\{([a-z_]*)\|")


def count(text):
    """(code, comment, blank) lines of an OCaml source: a small lexer
    for comments (nested, and with strings inside them), strings,
    quoted strings {id|...|id} and character literals ('"')."""
    code = comment = blank = 0
    has_code = has_comment = False
    depth = 0  # comments nesting
    close = None  # inside a string: what ends it
    i, n = 0, len(text)
    while i <= n:
        if i == n or text[i] == "\n":
            if has_code:
                code += 1
            elif has_comment or depth > 0:
                comment += 1
            elif i < n or (n > 0 and text[-1] != "\n"):
                blank += 1
            has_code = False
            has_comment = depth > 0
            i += 1
            continue
        c = text[i]
        if close is not None:
            if depth > 0:
                has_comment = True
            elif not c.isspace():
                has_code = True
            if close == '"' and c == "\\":
                # an escape, but not over the newline of a "...\
                # continued" string: the line must still be counted
                i += 1 if text.startswith("\\\n", i) else 2
                continue
            if text.startswith(close, i):
                i += len(close)
                close = None
                continue
            i += 1
            continue
        if text.startswith("(*", i):
            depth += 1
            has_comment = True
            i += 2
            continue
        if depth > 0 and text.startswith("*)", i):
            depth -= 1
            i += 2
            continue
        if depth > 0:
            if not c.isspace():
                has_comment = True
            if c == '"':
                close = '"'
            i += 1
            continue
        if not c.isspace():
            has_code = True
        if c == '"':
            close = '"'
            i += 1
            continue
        if c == "{":
            m = QUOTED.match(text, i)
            if m:
                close = "|" + m.group(1) + "}"
                i = m.end()
                continue
        if c == "'":
            m = CHAR.match(text, i)
            if m:
                i = m.end()
                continue
        i += 1
    return code, comment, blank


# ---------------------------------------------------------------------
# Grouping the files
# ---------------------------------------------------------------------

# (group, its top directories), in the order printed; the rest is
# "other" (scripts/, docs/'s toy examples, ...)
GROUPS = [
    ("library", ["playground", "libs"]),
    ("games", ["gamekits", "games"]),
    ("apps", ["appkits", "apps"]),
    ("examples", ["examples"]),
]
# the top directories whose subdirectories are listed even without -v
DETAILED = ["libs"]


def classify(path):
    """(group, subgroup) of a file: tests wherever they are, else by
    its top directory; the subgroup is the directory under it (a
    library, a genre, a category), or the top directory itself for a
    file right under it (playground/Playground.ml)."""
    parts = path.split("/")
    if "tests" in parts[:-1]:
        return "tests", parts[0] + "/"
    for group, tops in GROUPS:
        if parts[0] in tops:
            if len(parts) > 2:
                return group, parts[0] + "/" + parts[1] + "/"
            return group, parts[0] + "/"
    return "other", parts[0] + "/" if len(parts) > 1 else "./"


def files():
    out = subprocess.run(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard",
         "--", "*.ml", "*.mli"],
        check=True, capture_output=True, text=True).stdout
    return [f for f in out.splitlines() if f]


# ---------------------------------------------------------------------
# Printing
# ---------------------------------------------------------------------

FIELDS = ["files", "ml", "mli", "code", "comment", "blank", "lines"]
# the lines first, right beside the name they count, the rest after it
REST = [f for f in FIELDS if f != "lines"]
# 80 columns: the lines (7), 2 spaces, the name, 6 cells of 8
WIDTH = 23  # of the name column: "  playground/platforms/" with -v


def row(name, s, indent=0):
    cells = "".join(f"{s[f]:>8,}" for f in REST)
    print(f"{s['lines']:>7,}  {' ' * indent}{name:<{WIDTH - indent}}{cells}")


def main():
    verbose = "-v" in sys.argv[1:]
    stats = defaultdict(lambda: defaultdict(lambda: defaultdict(int)))
    for path in files():
        try:
            with open(path, encoding="utf-8", errors="replace") as f:
                text = f.read()
        except FileNotFoundError:  # deleted, not yet staged
            continue
        code, comment, blank = count(text)
        group, sub = classify(path)
        s = stats[group][sub]
        s["files"] += 1
        s["ml" if path.endswith(".ml") else "mli"] += 1
        s["code"] += code
        s["comment"] += comment
        s["blank"] += blank
        s["lines"] += code + comment + blank

    def total(subs):
        t = defaultdict(int)
        for s in subs:
            for f in FIELDS:
                t[f] += s[f]
        return t

    print(f"{'lines':>7}  {'':<{WIDTH}}" + "".join(f"{f:>8}" for f in REST))
    order = [g for g, _ in GROUPS] + ["tests", "other"]
    for group in order:
        subs = stats.get(group, {})
        if not subs:
            continue
        if verbose:
            print()
            for sub in sorted(subs):
                row(sub, subs[sub], 2)
        elif group != "tests":
            # the two halves of a group: its kits, its programs
            tops = dict(GROUPS).get(group, [])
            for top in tops:
                mine = {k: s for k, s in subs.items()
                        if k.split("/")[0] == top}
                # libs/'s libraries are independent of each other: how
                # much is ai/, audio/, graphics/, ...
                if top in DETAILED:
                    for sub in sorted(mine):
                        row(sub, mine[sub], 4)
                row(top + "/", total(mine.values()), 2)
        row(group, total(subs.values()))
    print()
    row("total", total(s for g in stats.values() for s in g.values()))
    row("total without tests",
        total(s for g, subs in stats.items() if g != "tests"
              for s in subs.values()))


if __name__ == "__main__":
    main()
