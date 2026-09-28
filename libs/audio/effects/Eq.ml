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
type t = {
  left : Filter.memory array;
  right : Filter.memory array;
  mutable last : float array; (* the last block's gains, [||]: none yet *)
}

let create () : t =
  let bands () = Array.init 3 (fun _ -> Filter.silence ()) in
  { left = bands (); right = bands (); last = [||] }

let bands (g : float array) : Filter.biquad array =
  [|
    Filter.low_shelf ~frequency:200. ~gain:g.(0);
    Filter.peaking ~frequency:1000. ~q:0.7 ~gain:g.(1);
    Filter.high_shelf ~frequency:4000. ~gain:g.(2);
  |]

(* a gain turned is ramped too, but its coefficients can't be
 * multiplied in: they are recomputed every [step] samples, the gains
 * part of the way there each time (the common compromise: 32 samples,
 * 0.7 ms, too short a step to hear) *)
let step = 32

let process (t : t) ~(bass : float) ~(middle : float) ~(treble : float) (s : Signal.stereo) : unit =
  let now = [| bass; middle; treble |] and n = Array.length s.left in
  let last = if t.last = [||] then now else t.last in
  let i = ref 0 in
  while !i < n do
    let len = min step (n - !i) in
    let f = bands (Array.mapi (fun k g -> Effect.ramp last.(k) g (!i + len - 1) n) now) in
    for j = !i to !i + len - 1 do
      for k = 0 to 2 do
        s.left.(j) <- Filter.step f.(k) t.left.(k) s.left.(j);
        s.right.(j) <- Filter.step f.(k) t.right.(k) s.right.(j)
      done
    done;
    i := !i + len
  done;
  t.last <- now

let knobs : Effect.knob list =
  List.map (fun name : Effect.knob -> { name; control = Knob (-12., 12.); initial = 0. }) [ "bass"; "middle"; "treble" ]

let effect () : Effect.t =
  let t = create () and gains = [| 0.; 0.; 0. |] in
  let set (knob : string) (x : float) =
    match knob with "bass" -> gains.(0) <- x | "middle" -> gains.(1) <- x | "treble" -> gains.(2) <- x | _ -> ()
  in
  {
    name = "eq";
    knobs;
    set;
    process = (fun s -> process t ~bass:gains.(0) ~middle:gains.(1) ~treble:gains.(2) s);
    meters = (fun () -> []);
  }
