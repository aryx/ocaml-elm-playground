(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Talk

(* See Basic_session.mli *)

let session ~(dialect : Basic_run.dialect) ~(program : Basic_run.program) (banner : string) : unit talk =
  let rec prompt (disk : Basic_disk.file list) (dialect : Basic_run.dialect) (program : Basic_run.program) : unit talk =
    (* the Apple II's prompts: > for Integer BASIC, ] for Applesoft *)
    let* line = ask (match dialect with Integer -> ">" | Applesoft -> "]") in
    (* a file from the disk, as its BASIC and its program; DOS said
       FILE NOT FOUND, not in either BASIC's words *)
    let load name (k : Basic_run.dialect -> Basic_run.program -> unit talk) : unit talk =
      match List.find_opt (fun (f : Basic_disk.file) -> f.name = name) disk with
      | None ->
          let* () = print "FILE NOT FOUND\n" in
          prompt disk dialect program
      | Some f -> (
          match Basic_run.of_lines f.lines with
          | Ok p -> k f.dialect p
          | Error msg ->
              let* () = print ("BAD FILE: " ^ msg ^ "\n") in
              prompt disk dialect program)
    in
    let run dialect program child =
      let* status = spawn child in
      let* () = if status = Interrupted then print "*** BREAK\n" else return () in
      prompt disk dialect program
    in
    if String.trim line = "" then prompt disk dialect program
    else
      match Basic_parse.parse_line line with
      | Error msg ->
          let* () = print (match dialect with Integer -> "*** SYNTAX ERR: " ^ msg ^ "\n" | Applesoft -> "?SYNTAX ERROR: " ^ msg ^ "\n") in
          prompt disk dialect program
      | Ok (Numbered (n, None)) -> prompt disk dialect (Basic_run.remove program n)
      | Ok (Numbered (n, Some stmts)) -> prompt disk dialect (Basic_run.add program n (Basic_run.text_of line) stmts)
      | Ok (Direct [ List ]) ->
          let* () = print (Basic_run.listing program) in
          prompt disk dialect program
      | Ok (Direct [ New ]) -> prompt disk dialect Basic_run.empty
      | Ok (Direct [ Bye ]) -> return ()
      | Ok (Direct [ Fp ]) -> prompt disk Applesoft program
      | Ok (Direct [ Int ]) -> prompt disk Integer program
      | Ok (Direct [ Catalog ]) ->
          let* () = print (Basic_disk.catalog disk) in
          prompt disk dialect program
      | Ok (Direct [ Load name ]) -> load name (fun d p -> prompt disk d p)
      | Ok (Direct [ Run_file name ]) -> load name (fun d p -> run d p (Basic_run.run d p))
      | Ok (Direct [ Save name ]) ->
          let lines = String.split_on_char '\n' (Basic_run.listing program) |> List.filter (( <> ) "") in
          let file = { Basic_disk.name; dialect; lines } in
          prompt (List.filter (fun (f : Basic_disk.file) -> f.name <> name) disk @ [ file ]) dialect program
      | Ok (Direct [ Run ]) -> run dialect program (Basic_run.run dialect program)
      | Ok (Direct stmts) -> run dialect program (Basic_run.direct dialect program stmts)
  in
  let* () = print banner in
  prompt Basic_disk.files dialect program
