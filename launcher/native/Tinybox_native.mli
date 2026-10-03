(* tinybox's menu, natively: its host (Menu_model.host). Every program
 * is linked in (Tinybox_collect): the one chosen is started in a process
 * of its own, this same binary under its name (tinybox <Name>), the menu
 * waiting for it and saying if it failed; the thumbnails and the
 * repository's sources are in the binary (made at build time,
 * Tinybox_thumbs and Tinybox_sources); and the chosen one is previewed
 * live in the panel, the program itself run by the menu.
 *
 * [host caps names]: over the programs [names] can start; [caps]: the
 * authority to start one and wait for it. *)
val host : < Cap.fork ; Cap.exec ; Cap.wait ; .. > -> string list -> Menu_model.host

(* claude: a directory's sources, read from the disk, for its code map
 * (tinybox codemap <dir>): the OCaml and C files under it, their paths
 * relative to it; not what is under a name starting with . or _ (.git,
 * _build), nor under a symbolic link to a directory (xix's principia/,
 * principia again), nor what its .codemapignore leaves out -- with its
 * .codemapignore read (Code_config), its projects' tops (a .git or a
 * dune-project in them, Code_names.find), and claude: every directory's
 * .codemapconfig, evaluated as jsonnet (Code_guide), and their mistakes *)
type directory = { roots : string list; sources : (string * string) list; guide : Code_guide.t; mistakes : string list }

val directory_sources : < Cap.readdir ; Cap.open_in ; .. > -> string -> directory
