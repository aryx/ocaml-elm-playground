(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Eq.mli *)

(* three bands a side *)
type t = { left : Filter.memory array; right : Filter.memory array }

let create () : t =
  let bands () = Array.init 3 (fun _ -> Filter.silence ()) in
  { left = bands (); right = bands () }

let process (t : t) ~(bass : float) ~(middle : float) ~(treble : float) (s : Signal.stereo) : unit =
  let bands =
    [|
      Filter.low_shelf ~frequency:200. ~gain:bass;
      Filter.peaking ~frequency:1000. ~q:0.7 ~gain:middle;
      Filter.high_shelf ~frequency:4000. ~gain:treble;
    |]
  in
  Array.iteri
    (fun k f ->
      Filter.process f t.left.(k) s.left;
      Filter.process f t.right.(k) s.right)
    bands

let knobs : Effect.knob list =
  List.map (fun name : Effect.knob -> { name; control = Knob (-12., 12.); initial = 0. }) [ "bass"; "middle"; "treble" ]

let effect () : Effect.t =
  let t = create () and gains = [| 0.; 0.; 0. |] in
  let set (knob : string) (x : float) =
    match knob with "bass" -> gains.(0) <- x | "middle" -> gains.(1) <- x | "treble" -> gains.(2) <- x | _ -> ()
  in
  { name = "eq"; knobs; set; process = (fun s -> process t ~bass:gains.(0) ~middle:gains.(1) ~treble:gains.(2) s) }
