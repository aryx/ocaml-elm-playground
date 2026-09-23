(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Modulation.mli *)

type kind = Chorus | Flanger | Phaser

let kinds = [ Chorus; Flanger; Phaser ]
let name = function Chorus -> "chorus" | Flanger -> "flanger" | Phaser -> "phaser"

let knobs : Effect.knob list =
  [
    { name = "kind"; control = Selector (List.map name kinds); initial = 0. };
    { name = "rate"; control = Knob (0.05, 5.); initial = 0.5 };
    { name = "depth"; control = Knob (0., 1.); initial = 0.5 };
    { name = "feedback"; control = Knob (0., 0.9); initial = 0. };
    { name = "mix"; control = Knob (0., 1.); initial = 0.5 };
  ]

let effect () : Effect.t =
  (* each kind keeps its own state: switching back finds it as it was *)
  let delay = Modulated_delay.create () and phaser = Phaser.create () in
  let kind = ref Chorus and rate = ref 0. and depth = ref 0. and feedback = ref 0. and mix = ref 0. in
  let set (knob : string) (x : float) =
    match knob with
    | "kind" -> kind := List.nth kinds (max 0 (min (List.length kinds - 1) (Control.index x)))
    | "rate" -> rate := x
    | "depth" -> depth := x
    | "feedback" -> feedback := x
    | "mix" -> mix := x
    | _ -> ()
  in
  List.iter (fun (k : Effect.knob) -> set k.name k.initial) knobs;
  let process (out : Signal.stereo) =
    match !kind with
    | Chorus ->
        Modulated_delay.process delay
          { center = 0.015; depth = 0.006 *. !depth; rate = !rate; feedback = !feedback; mix = !mix }
          out
    | Flanger ->
        Modulated_delay.process delay
          { center = 0.0025; depth = 0.004 *. !depth; rate = !rate; feedback = !feedback; mix = !mix }
          out
    | Phaser ->
        Phaser.process phaser { low = 200.; high = 200. *. Float.pow 2. (8. *. !depth); rate = !rate; feedback = !feedback; mix = !mix } out
  in
  { name = "modulation"; knobs; set; process; meters = (fun () -> []) }
