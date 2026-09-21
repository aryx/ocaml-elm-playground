(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

type part = {
  kind : string;
  height : float -> float;
  draw : Widget.box -> active:bool -> shape list;
  input : computer -> Widget.box -> part;
  menu : string list;
  command : string -> part;
  save : unit -> string;
}

type registry = (string * (string -> part)) list

let rec placeholder ~kind text =
  {
    kind;
    height = (fun _ -> 50.);
    draw =
      (fun (b : Widget.box) ~active:_ ->
        [
          rectangle (rgb 225 225 225) b.w b.h |> move b.x b.y;
          words (rgb 90 90 90) (Printf.sprintf "a part of kind %S, which nothing here can show (%d bytes, kept)" kind (String.length text))
          |> move b.x b.y;
        ]);
    input = (fun _ _ -> placeholder ~kind text);
    menu = [];
    command = (fun _ -> placeholder ~kind text);
    (* exactly what it was given: not understood, not lost *)
    save = (fun () -> text);
  }

let load registry ~kind text =
  match List.assoc_opt kind registry with Some load -> load text | None -> placeholder ~kind text
