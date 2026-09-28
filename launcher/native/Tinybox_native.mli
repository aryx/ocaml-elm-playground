(* tinybox's menu, natively: its host (Tinybox_menu.host). Every program
 * is linked in (Tinybox_collect): the one chosen is started in a process
 * of its own, this same binary under its name (tinybox <Name>), the menu
 * waiting for it and saying if it failed; the thumbnails and the
 * repository's sources are in the binary (made at build time,
 * Tinybox_thumbs and Tinybox_sources); and the chosen one is previewed
 * live in the panel, the program itself run by the menu.
 *
 * [host caps names]: over the programs [names] can start; [caps]: the
 * authority to start one and wait for it. *)
val host : < Cap.fork ; Cap.exec ; Cap.wait ; .. > -> string list -> Tinybox_menu.host

(* claude: a directory's sources, read from the disk, for its code map
 * (tinybox codemap <dir>): the OCaml and C files under it, their paths
 * relative to it; not what is under a name starting with . or _ (.git,
 * _build), nor under a symbolic link to a directory (xix's principia/,
 * principia again), nor what its .codemapignore leaves out -- with its
 * .codemapignore and .codemapconfig read (Code_config) and its projects'
 * tops (a .git or a dune-project in them, Code_names.find), or the
 * config's mistake *)
val directory_sources :
  < Cap.readdir ; Cap.open_in ; .. > -> string -> (Code_config.t * string list * (string * string) list, string) result
