(* tinybox's menu: every program of the catalogue, on two shelves
 * (games, apps), a section at a time, each with its screenshot, the
 * original it is after, what it is and what the original brought.
 * Enter starts the one chosen, in a process of its own (tinybox <Name>):
 * its own window, its own Cap.main; the menu stays, waits for it, and
 * says if it failed.
 *
 * [run caps names]: the menu, over the programs [names] can start (the
 * others are shown greyed); [caps]: the authority to start one. *)
val run : < Cap.fork ; Cap.exec ; Cap.wait ; .. > -> string list -> unit
