(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tinybox_menu.mli.
 *
 * A Playground game like the others, its world the catalogue
 * (Tinybox_data, made at build time from CATALOG.md and the golden
 * frames). The screen, 16:9, 1778 by 1000 (run_app's ~screen, scaled
 * to the window):
 *
 *   TINYBOX  GAMES  APPS  / search
 *   < Platform >              3 / 15  +-------------+  TinyMario  2D
 *   Run and jump from platform to...  |             |  After Super Mario
 *   +----+ +----+ +----+ +----+ +----+|  the chosen |  Bros. (...)
 *   |    | |    | |    | |    | |    ||  one, live  |  Run, jump, stomp...
 *   +----+ +----+ +----+ +----+ +----+|             |  The side-scroller:
 *   TinyMario TinySonic ...           +-------------+  ...
 *   +----+ +----+ +----+ +----+ +----++--------------------------------+
 *   ...                               |  its code (Codemap.preview),   |
 *                                     |  a click (or s) opens it all   |
 *                                     +--------------------------------+
 *   arrows move  tab section  / search  s read its code  enter play it
 *
 * Keys: the arrows, Tab and Shift-Tab (the next section, across both
 * shelves), g and a (the games' and the apps' first section), Enter
 * (play), / (search every program by name, original and line; Escape
 * leaves it). The mouse: a click chooses, a double click plays (so does
 * a click on the preview), the wheel scrolls, the tabs and arrows at the
 * top are buttons.
 *
 * Groups and filters, after Batocera's and RetroBox's (the catalogue's
 * Year, Platform and Players columns): b groups the programs by genre
 * (the catalogue's sections, the default), by era (a decade a
 * section, oldest first), by platform or by players; p, e, m and l
 * filter by players (1, 2, over the network), era, machine and look
 * (2D, 2.5D, 3D, app), each key going through the values and back to
 * any; c clears them; r jumps to a random program of the grid. The
 * filter bar under the section's title says what is chosen, and a click
 * on one of its words does what its key does.
 *
 * The look: a dark cabinet's colours, the chosen thumbnail's frame
 * pulsing, scanlines over everything (thin translucent rectangles, a
 * CRT's gaps between its lines).
 *
 * The thumbnails (250 by 250) are the host's: natively PNGs in the
 * binary, on the web URLs (Tinybox_menu.mli).
 *
 * The code: s opens the chosen program's code map (codemap/, after
 * codemap), its files and what it uses as a treemap to zoom into, a file
 * read by clicking on it; Escape comes back.
 *
 * The previews: a second (60 frames) on a program, and its picture comes
 * alive -- the program itself, playing its golden scene's script in the
 * detail panel, run by the menu (natively: Tinybox_native's "Previews").
 *)

open Playground
open Menu_model
open Menu_layout

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run ?(network : < Cap.network ; .. > option) (host : host) : unit =
  let network = (network :> < Cap.network > option) in
  let flags = Playground_platform.flags () in
  Playground_platform.run_app ~window:{ Playground.default_window with screen_size = Some (screen_w, screen_h) } ~flags ?network (game (Menu_view.view host) (Menu_update.update host) (Menu_groups.initial flags))
