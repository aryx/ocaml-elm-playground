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
 *   tinybox [menu flags]           the menu (Tinybox_menu): the catalogue,
 *                                  its screenshots, Enter to play
 *   tinybox list                   the programs, by name
 *   tinybox TinyMario [args]       run one, with its own command line
 *   tinybox mario [args]           a name found case-insensitively,
 *                                  "Tiny" optional, or any unique part
 *   tinybox TinyTurboPascal -tty   an editor in this terminal, not a window
 *   tinybox codemap ~/principia    the code map of a directory's OCaml
 *                                  and C files, read from the disk
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
 * The menu is a program too, registered as "tinybox" (it goes through
 * Program.run like the others, so that the platform reads its command
 * line, Program.argv), and left out of the lists.
 *
 * What it uses: Program (elm_core), Tinybox_menu, Tty_unix and
 * appkits/editor for -tty.
 *)

(*****************************************************************************)
(* The programs *)
(*****************************************************************************)

let menu = "tinybox"

(* claude: a directory's code map, one more program too, the directory
 * given before it runs *)
let codemap = "codemap"
let codemap_dir = ref "."

let programs () : string list = List.filter (fun p -> p <> menu && p <> codemap) (List.map fst (Program.collected ()))

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
  "usage: tinybox [-platform flags | chosen=<program> | code=<program> | list | codemap [-check] <dir> | <program> [args] | <program> -tty]\n\
  \  chosen=, code=: the menu on a program, or in its code map\n\
  \  codemap <dir>: the code map of a directory's OCaml and C files\n\
  \  codemap -check <dir>: what its .codemapconfig files say that does not hold\n\
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

(* the menu, one more program: its entry, Cap.main and all *)
let () = Program.main menu (fun () -> Cap.main (fun caps -> Tinybox_menu.run (Tinybox_native.host caps (programs ()))))

let () =
  Program.main codemap (fun () ->
      Cap.main (fun caps ->
          let dir = !codemap_dir in
          let stop msg = eprint caps (Printf.sprintf "tinybox codemap: %s\n" msg); exit 2 in
          match Tinybox_native.directory_sources caps dir with
          | { sources = []; _ } -> stop (Printf.sprintf "no OCaml nor C file under %s" dir)
          | { roots; sources; guide; mistakes } ->
              (* claude: a config's mistake said, the map drawn without it *)
              List.iter (fun m -> eprint caps (Printf.sprintf "tinybox codemap: %s\n" m)) mistakes;
              (* its name: the directory's own, not "." *)
              let name = Filename.basename (if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir) in
              Codemap.run_directory ~guide ~colours:(Code_guide.colours guide) ~roots ~name ~sources ()))

(* claude: tinybox codemap -check <dir>: what its .codemapconfig files
 * say that does not hold (Code_guide.check), the warnings after; exits 1
 * on a mistake *)
let check_directory (caps : < Cap.readdir ; Cap.open_in ; Cap.stdout ; Cap.stderr ; .. >) (dir : string) : unit =
  let d = Tinybox_native.directory_sources caps dir in
  let results =
    List.map (fun m -> Error m) d.mistakes
    @ Code_guide.check d.guide
        ~file:(fun p -> Option.map (fun text -> (Code_file.make p text, text)) (List.assoc_opt p d.sources))
        ~exists:(fun p -> Sys.file_exists (Filename.concat dir p))
  in
  let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) results in
  let warnings = List.filter_map (function Ok w -> Some w | Error _ -> None) results in
  List.iter (fun e -> eprint caps ("error: " ^ e ^ "\n")) errors;
  List.iter (fun w -> eprint caps ("warning: " ^ w ^ "\n")) warnings;
  print caps
    (Printf.sprintf "%d config%s, %d mistake%s, %d warning%s\n" (List.length (Code_guide.dirs d.guide))
       (if List.length (Code_guide.dirs d.guide) = 1 then "" else "s")
       (List.length errors) (if List.length errors = 1 then "" else "s")
       (List.length warnings) (if List.length warnings = 1 then "" else "s"));
  if errors <> [] then exit 1

let () =
  let invoked = Filename.remove_extension (Filename.basename Sys.argv.(0)) in
  match Array.to_list Sys.argv with
  (* BusyBox's way: tinybox under another name *)
  | _ :: args when String.lowercase_ascii invoked <> "tinybox" -> start invoked args
  | [ _; "list" ] -> list_programs ()
  (* claude: a directory's code map, with the platform's flags if any *)
  | [ _; "codemap"; "-check"; dir ] -> Cap.main (fun caps -> check_directory caps dir)
  | exe :: "codemap" :: dir :: flags ->
      codemap_dir := dir;
      Program.run codemap ~argv:(Array.of_list (exe :: flags))
  | [ _; ("-h" | "-help" | "--help") ] -> Cap.main (fun caps -> print caps usage)
  (* the menu, with the platform's flags if any (-fixed-time, -dump-frame),
   * claude: and its own, name=value (code=TinyVi: Tinybox_menu.run), no
   * program's name having an = *)
  | [ exe ] -> Program.run menu ~argv:[| exe |]
  | exe :: (flag :: _ as args) when String.length flag > 1 && (flag.[0] = '-' || String.contains flag '=') ->
      Program.run menu ~argv:(Array.of_list (exe :: args))
  | _ :: query :: args -> start query args
  | [] -> fail usage
