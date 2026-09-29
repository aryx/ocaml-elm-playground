(* Code_walk: a directory's code read from the disk, what its code map
   shows (tinybox codemap <dir>) or bundles for a web page
   (make_codemap_data, Code_bundle): its source files, its configs and
   what they import, its projects' tops.

   Walked in name order, leaving out:
   - hidden entries (.git) and directories starting with _ (_build,
     _opam; a file may: Linux 0.01's lib/_exit.c);
   - what its .codemapignore says (Code_config);
   - symbolic links (not followed: a link back up would loop).

   A project's top is a directory holding a .git or a dune-project (not
   an mkfile: Plan 9 has one per program). *)

(* the files the code map colours: OCaml's, C's, assembly's *)
val source_extensions : string list

type t = {
  roots : string list; (* the projects' tops, relative ("" the directory) *)
  sources : (string * string) list; (* path relative to the directory, text *)
  configs : string list; (* the .codemapconfig files' paths, relative *)
  jsonnet : (string * string) list; (* what configs may import: .libsonnet and .jsonnet files, and the configs *)
}

val walk : string -> t

(* a file's text, None if it cannot be read *)
val read : string -> string option
