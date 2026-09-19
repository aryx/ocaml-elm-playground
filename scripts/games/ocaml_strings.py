# Claude Code
#
# Copyright (C) 2026 Yoann Padioleau
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License
# (LGPL) as published by the Free Software Foundation; either version
# 2 of the License, or (at your option) any later version.
#
# Reading data out of OCaml sources, for the tools checking a game's
# levels, maze or track: the string literals of a top-level definition.

import re

def block(path, name):
    """The text of the top-level [let <name>] in the file: from that line
    to the next top-level definition or section comment."""
    text = open(path).read()
    m = re.search(r'^let ' + re.escape(name) + r'\b', text, re.M)
    if not m:
        raise SystemExit(f"no 'let {name}' in {path}")
    rest = text[m.start():]
    end = re.search(r'\n(let |type |\(\*\*\*)', rest[4:])
    return rest[: end.start() + 4] if end else rest

def strings(text):
    """The OCaml string literals of [text], in order (no escapes needed
    for the games' levels)"""
    return re.findall(r'"([^"\\]*)"', text)

def lists_of_strings(text):
    """The innermost [ ... ] lists of string literals of [text]"""
    return [strings(inner) for inner in re.findall(r'\[([^\[\]]*)\]', text) if '"' in inner]
