(* Code_bundle: a directory's code as one file, for its code map on a
   web page (launcher/codemap/web, Codemap_web): written by
   make_codemap_data from the disk (Code_walk), fetched and read by the
   page.

   The format is tinybox_sources.txt's (make_tinybox_data sources-file,
   Tinybox_web): each file its path, a newline, its length in bytes, a
   newline, its text. Three entries are not files: "#name", the map's
   name, "#roots", the projects' tops, a line each, and "#rank", what
   lexing every file tells (Code_rank's, too slow to count in a
   browser). A config
   (.codemapconfig) and what configs import (.libsonnet, .jsonnet) are
   files among the rest, told apart by their names.

   Worked example (the tests'): a bundle of name "ix", roots [""], the
   source a.ml "let x = 1" and the config .codemapconfig "{}" is read
   back the same. *)

type t = {
  name : string;
  roots : string list;
  sources : (string * string) list; (* path, text *)
  configs : string list; (* the .codemapconfig files' paths *)
  jsonnet : (string * string) list; (* the configs and what they import, path, text *)
  rank : string option; (* claude: every definition's uses and the files' links, counted when made (Code_rank.to_string) *)
}

val to_string : t -> string

(* Failure on a malformed bundle *)
val of_string : string -> t

(* each entry, a path and a text (the format alone: tinybox_sources.txt) *)
val entries : string -> (string * string) list
