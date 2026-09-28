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

type t = { name : string; color : color; front : Component.part; device : Rack_device.t }
type catalogue = (string * (unit -> t)) list

let of_effect ~kind ~name ~(color : color) (fx : Effect.t) : t =
  let bypass = ref false in
  { name; color; front = Part_effect.make ~kind ~name ~color fx bypass; device = Rack_device.of_effect ~kind ~bypass:(fun () -> !bypass) fx }

let matrix () : t =
  let d = Rack_matrix.create () in
  { name = "Matrix"; color = rgb 250 200 60; front = Part_matrix.make d; device = d }

let mixer ~(peak : Rack_device.t -> int -> float) : t =
  let d = Rack_mixer.create () in
  { name = "Mixer 14:2"; color = rgb 200 200 210; front = Part_mixer.make d ~peak:(peak d); device = d }
