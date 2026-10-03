(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Native_cursor.mli *)

type t = [ `Arrow | `Hand | `Text | `Crosshair | `Hidden ]

(* the system's cursors, each made once *)
let made : (t, Tsdl.Sdl.cursor) Hashtbl.t = Hashtbl.create 4
let current : t ref = ref `Arrow

let set (c : t) : unit =
  let open Tsdl in
  if c <> !current then (
    current := c;
    match c with
    | `Hidden -> ignore (Sdl.show_cursor false)
    | (`Arrow | `Hand | `Text | `Crosshair) as shape -> (
        ignore (Sdl.show_cursor true);
        let system = match shape with `Arrow -> Sdl.System_cursor.arrow | `Hand -> Sdl.System_cursor.hand | `Text -> Sdl.System_cursor.ibeam | `Crosshair -> Sdl.System_cursor.crosshair in
        match Hashtbl.find_opt made c with
        | Some cursor -> Sdl.set_cursor (Some cursor)
        | None -> (
            (* a driver with no cursors (SDL's dummy one, for a frame dumped): left as it is *)
            match Sdl.create_system_cursor system with
            | Ok cursor ->
                Hashtbl.replace made c cursor;
                Sdl.set_cursor (Some cursor)
            | Error _ -> ())))
