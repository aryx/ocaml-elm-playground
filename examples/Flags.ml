(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A program's parameters, its flags (see Playground.flags): listed on
 * the screen, and two of them used, a square's color and how fast the
 * arrow keys move it:
 *
 *   dune exec examples/Flags.exe -- color=red speed=3 whatever
 *   http://localhost:8001/examples/js/Flags.html?color=red&speed=3&whatever
 *
 * (the web page with 'make serve-build'). Without flags: a purple
 * square, at speed 1. *)
open Playground

let color_of_name (name : string) : color =
  match name with
  | "red" -> red
  | "green" -> green
  | "blue" -> blue
  | _ -> purple

let view (computer : computer) (x, y) =
  let color = color_of_name (Option.value (List.assoc_opt "color" computer.flags) ~default:"") in
  let lines =
    "computer.flags:"
    :: List.map (fun (name, value) -> Printf.sprintf "%s = \"%s\"" name value) computer.flags
  in
  (square color 40. |> move x y)
  :: List.mapi (fun i line -> words black line |> move 0. (300. -. (40. *. float_of_int i))) lines

let update (computer : computer) (x, y) =
  let speed =
    Option.value (Option.bind (List.assoc_opt "speed" computer.flags) float_of_string_opt) ~default:1.
  in
  (x +. (speed *. to_x computer.keyboard), y +. (speed *. to_y computer.keyboard))

let app = game view update (0., 0.)

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
