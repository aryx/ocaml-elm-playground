(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* our media (Our_media.playlist) written as files in the directory
 * given, one per item under its name: toy files of every format the
 * repository reads, to give TinyMediaPlayer (or any other reader) on
 * the command line; make test-data *)
let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  Our_media.playlist
  |> List.iter (fun (name, bytes) ->
         let file = Filename.concat dir name in
         Out_channel.with_open_bin file (fun oc -> output_string oc (Lazy.force bytes));
         print_endline file)
