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
# Drive a running playground window from the command line -- move the
# mouse, click, press keys -- without xdotool (not installed in this
# sandbox) or python-xlib (not installed either): Python's standard
# ctypes calls libX11 and libXtst (the XTEST extension, whose whole
# purpose is to inject fake input events) directly. See
# docs/claude_notes/notes_debugging_techniques.md section 8 for a
# tutorial on how and why this works.
#
# Usage (x, y are window-relative pixels, origin at the top-left, like
# SDL's -- not Elm's centered coordinates):
#   scripts/xdrive.py find  <exe-basename>        # prints the window id, e.g. Mouse.exe
#   scripts/xdrive.py move  <wid> <x> <y>
#   scripts/xdrive.py click <wid> <x> <y> [button]  # button 1 = left (default)
#   scripts/xdrive.py down  <wid> [button]        # press and hold, e.g. to
#   scripts/xdrive.py up    <wid> [button]        #  screenshot while held
#   scripts/xdrive.py key   <wid> <keysym> [hold-seconds]  # e.g. Left, space, a
#   scripts/xdrive.py query <wid>                 # where is the pointer?
#
# Example (see also scripts/screenshot_playground3d.sh):
#   _build/default/examples/Mouse.exe &
#   sleep 2
#   WID=$(scripts/xdrive.py find Mouse.exe)
#   scripts/xdrive.py move $WID 700 200
#   import -window $WID /tmp/mouse.png

import ctypes
import subprocess
import sys
import time

x11 = ctypes.CDLL("libX11.so.6")
xtst = ctypes.CDLL("libXtst.so.6")

# ctypes assumes every C function returns an int; X handles (Display*,
# Window, KeySym) are pointer-sized, so declare them or they get
# truncated to 32 bits on a 64-bit machine.
x11.XOpenDisplay.restype = ctypes.c_void_p
x11.XOpenDisplay.argtypes = [ctypes.c_char_p]
x11.XStringToKeysym.restype = ctypes.c_ulong
x11.XStringToKeysym.argtypes = [ctypes.c_char_p]
x11.XKeysymToKeycode.restype = ctypes.c_ubyte
x11.XKeysymToKeycode.argtypes = [ctypes.c_void_p, ctypes.c_ulong]
x11.XWarpPointer.argtypes = [ctypes.c_void_p, ctypes.c_ulong, ctypes.c_ulong,
                             ctypes.c_int, ctypes.c_int, ctypes.c_uint,
                             ctypes.c_uint, ctypes.c_int, ctypes.c_int]
x11.XSetInputFocus.argtypes = [ctypes.c_void_p, ctypes.c_ulong, ctypes.c_int,
                               ctypes.c_ulong]
x11.XRaiseWindow.argtypes = [ctypes.c_void_p, ctypes.c_ulong]
x11.XSync.argtypes = [ctypes.c_void_p, ctypes.c_int]
x11.XQueryPointer.argtypes = [ctypes.c_void_p, ctypes.c_ulong] + \
    [ctypes.c_void_p] * 7
xtst.XTestFakeButtonEvent.argtypes = [ctypes.c_void_p, ctypes.c_uint,
                                      ctypes.c_int, ctypes.c_ulong]
xtst.XTestFakeKeyEvent.argtypes = [ctypes.c_void_p, ctypes.c_uint,
                                   ctypes.c_int, ctypes.c_ulong]

REVERT_TO_PARENT = 2
CURRENT_TIME = 0


def open_display():
    d = x11.XOpenDisplay(None)
    if not d:
        sys.exit("xdrive: cannot open X display (is DISPLAY set?)")
    return d


def find(exe, timeout=10.):
    # Same trick as scripts/screenshot_playground3d.sh: only the app's
    # inner content window has the executable's basename as WM_CLASS;
    # its title is shared with the window manager's outer frame.
    # Retries, since right after launching the app the window may not
    # exist yet (how long that takes varies from run to run), and skips
    # windows that aren't visible: at startup SDL can briefly have a
    # window with the right WM_CLASS that is destroyed right after
    # (seen as "BadWindow" errors on the next command).
    deadline = time.time() + timeout
    while True:
        tree = subprocess.run(["xwininfo", "-root", "-tree"], capture_output=True,
                              text=True, check=True).stdout
        for line in tree.splitlines():
            if '"%s"' % exe in line:
                wid = line.split()[0]
                info = subprocess.run(["xwininfo", "-id", wid], capture_output=True,
                                      text=True).stdout
                if "IsViewable" in info:
                    return wid
        if time.time() > deadline:
            sys.exit("xdrive: no window for %s" % exe)
        time.sleep(0.2)


def focus(d, wid):
    # Key events go to the focused window, and SDL also ignores some
    # mouse events for an unfocused one.
    x11.XRaiseWindow(d, wid)
    x11.XSetInputFocus(d, wid, REVERT_TO_PARENT, CURRENT_TIME)
    x11.XSync(d, 0)


def move(d, wid, x, y):
    focus(d, wid)
    # Warp 1 pixel off first: warping onto the pointer's current
    # position generates no MotionNotify at all, and we observed the
    # first warp into a freshly mapped window sometimes being dropped.
    x11.XWarpPointer(d, 0, wid, 0, 0, 0, 0, x + 1, y)
    x11.XSync(d, 0)
    time.sleep(0.05)
    x11.XWarpPointer(d, 0, wid, 0, 0, 0, 0, x, y)
    x11.XSync(d, 0)


def query(d, wid):
    root, child = ctypes.c_ulong(), ctypes.c_ulong()
    rx, ry, wx, wy = (ctypes.c_int() for _ in range(4))
    mask = ctypes.c_uint()
    x11.XQueryPointer(d, wid, ctypes.byref(root), ctypes.byref(child),
                      ctypes.byref(rx), ctypes.byref(ry),
                      ctypes.byref(wx), ctypes.byref(wy), ctypes.byref(mask))
    return (wx.value, wy.value)


def main(argv):
    if len(argv) < 3:
        sys.exit("usage: xdrive.py find <exe> | move <wid> <x> <y> | "
                 "click <wid> <x> <y> [button] | down|up <wid> [button] | "
                 "key <wid> <keysym> [hold] | "
                 "query <wid>")
    cmd = argv[1]
    if cmd == "find":
        print(find(argv[2]))
        return
    d = open_display()
    wid = int(argv[2], 0)
    if cmd == "move":
        move(d, wid, int(argv[3]), int(argv[4]))
    elif cmd == "click":
        button = int(argv[5]) if len(argv) > 5 else 1
        move(d, wid, int(argv[3]), int(argv[4]))
        xtst.XTestFakeButtonEvent(d, button, 1, CURRENT_TIME)
        x11.XSync(d, 0)
        time.sleep(0.1)
        xtst.XTestFakeButtonEvent(d, button, 0, CURRENT_TIME)
        x11.XSync(d, 0)
    elif cmd in ("down", "up"):
        button = int(argv[3]) if len(argv) > 3 else 1
        focus(d, wid)
        xtst.XTestFakeButtonEvent(d, button, 1 if cmd == "down" else 0, CURRENT_TIME)
        x11.XSync(d, 0)
    elif cmd == "key":
        hold = float(argv[4]) if len(argv) > 4 else 0.1
        keysym = x11.XStringToKeysym(argv[3].encode())
        if keysym == 0:
            sys.exit("xdrive: unknown keysym %s (see /usr/include/X11/keysymdef.h, "
                     "without the XK_ prefix)" % argv[3])
        keycode = x11.XKeysymToKeycode(d, keysym)
        focus(d, wid)
        xtst.XTestFakeKeyEvent(d, keycode, 1, CURRENT_TIME)
        x11.XSync(d, 0)
        time.sleep(hold)
        xtst.XTestFakeKeyEvent(d, keycode, 0, CURRENT_TIME)
        x11.XSync(d, 0)
    elif cmd == "query":
        print("%d %d" % query(d, wid))
    else:
        sys.exit("xdrive: unknown command %s" % cmd)


if __name__ == "__main__":
    main(sys.argv)
