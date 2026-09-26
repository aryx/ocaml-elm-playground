(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* tinybox, every game and app of the repository in one binary, after
 * BusyBox (one multi-call binary instead of hundreds) and the retro
 * boxes (a menu of games). See plan_launcher.md.
 *
 *   tinybox                        the programs, by name
 *   tinybox list                   the same
 *   tinybox TinyMario [args]       run one, with its own command line
 *   tinybox mario [args]           a name found case-insensitively,
 *                                  "Tiny" optional, or any unique part
 *   tinybox TinyTurboPascal -tty   an editor in this terminal, not a window
 *   tinymario [args]               BusyBox's way: started under a
 *                                  program's name (a link to tinybox)
 *
 * By the time this module runs, every program has been linked and
 * recorded (Tinybox_collect, Program.collected); running one is
 * Program.run, which gives it its own argv, as if started alone:
 * tinybox's own words never reach it.
 *
 * Capabilities: a process of tinybox is either tinybox listing (its
 * own Cap.main, for stdout and stderr) or one program (whose main calls
 * Cap.main itself, if it needs any), never both -- Cap.main can be
 * called once.
 *
 * What it uses: Program (elm_core), Tty_unix and appkits/editor for
 * -tty. To come (plan_launcher.md): the menu, a Playground app with
 * the catalogue's screenshots, run when no name is given.
 *)

(*****************************************************************************)
(* The programs *)
(*****************************************************************************)

let programs () : string list = List.map fst (Program.collected ())

(* The editors that also run in a terminal (apps/devtools/tty/, whose
 * modules can't be linked here: they have the GUI versions' names). *)
let tty_programs : (string * (< Cap.stdin ; Cap.stdout ; .. > -> unit)) list =
  [
    ("TinyVi", fun caps -> Tty_unix.run caps Tui_vi.program);
    ("TinyEmacs", fun caps -> Tty_unix.run caps Tui_emacs.program);
    ("TinyTurboPascal", fun caps -> Tty_unix.run caps Tui_turbo.program);
  ]

(* The program a name means: the exact name, ignoring case; the same
 * with "Tiny" in front ("mario"); else the only one containing it *)
let resolve (query : string) : (string, string) result =
  let names = programs () in
  let low = String.lowercase_ascii in
  let q = low query in
  let contains (s : string) =
    let n = String.length q in
    let rec at i = i + n <= String.length s && (String.sub s i n = q || at (i + 1)) in
    n > 0 && at 0
  in
  match List.find_opt (fun p -> low p = q || low p = "tiny" ^ q) names with
  | Some p -> Ok p
  | None -> (
      match List.filter (fun p -> contains (low p)) names with
      | [ p ] -> Ok p
      | [] -> Error (Printf.sprintf "no program named %s (tinybox list: the names)" query)
      | several -> Error (Printf.sprintf "%s: which one? %s" query (String.concat " " several)))

(*****************************************************************************)
(* Output *)
(*****************************************************************************)

(* the names in columns, as ls *)
let columns (names : string list) : string =
  let width = 2 + List.fold_left (fun w s -> max w (String.length s)) 0 names in
  let per_line = max 1 (80 / width) in
  let b = Buffer.create 4096 in
  List.iteri
    (fun i s ->
      Buffer.add_string b s;
      if (i + 1) mod per_line = 0 then Buffer.add_char b '\n'
      else Buffer.add_string b (String.make (width - String.length s) ' '))
    names;
  if List.length names mod per_line <> 0 then Buffer.add_char b '\n';
  Buffer.contents b

let usage =
  "usage: tinybox [list | <program> [args] | <program> -tty]\n\
  \  <program>: its name, case-insensitive, \"Tiny\" optional, or a unique part of it\n\
  \  args: the program's own, e.g. -debug-keys, artwork=shapes\n"

(* claude: tinybox's own output, when it runs no program: the
 * capability taken as proof, the Stdlib doing the printing, as
 * Tty_unix does *)
let print (_caps : < Cap.stdout ; .. >) (s : string) : unit = print_string s
let eprint (_caps : < Cap.stderr ; .. >) (s : string) : unit = prerr_string s

let list_programs () : unit =
  Cap.main (fun caps ->
      print caps (columns (List.sort compare (programs ())));
      print caps (Printf.sprintf "%d programs; -tty also for %s\n" (List.length (programs ()))
                    (String.concat ", " (List.map fst tty_programs))))

let fail (msg : string) : 'a =
  Cap.main (fun caps -> eprint caps (msg ^ "\n"));
  exit 2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* [start name args]: the program, or its terminal version *)
let start (query : string) (args : string list) : unit =
  match resolve query with
  | Error msg -> fail msg
  | Ok name when List.mem "-tty" args -> (
      match List.assoc_opt name tty_programs with
      | Some run -> Cap.main (fun caps -> run caps)
      | None -> fail (Printf.sprintf "%s has no terminal version (-tty: %s)" name (String.concat ", " (List.map fst tty_programs))))
  | Ok name -> Program.run name ~argv:(Array.of_list (name :: args))

let () =
  let invoked = Filename.remove_extension (Filename.basename Sys.argv.(0)) in
  match Array.to_list Sys.argv with
  (* BusyBox's way: tinybox under another name *)
  | _ :: args when String.lowercase_ascii invoked <> "tinybox" -> start invoked args
  | [ _ ] | [ _; "list" ] -> list_programs ()
  | [ _; ("-h" | "-help" | "--help") ] -> Cap.main (fun caps -> print caps usage)
  | _ :: query :: args -> start query args
  | [] -> fail usage
