(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Graphic_eq.mli *)

let frequencies = [| 60.; 170.; 310.; 600.; 1000.; 3000.; 6000.; 12000.; 14000.; 16000. |]
let range = 12.
let q = 1.4

(* a bell a band, a memory a band a side; the bells made again only
 * when a gain changed *)
type t = {
  left : Filter.memory array;
  right : Filter.memory array;
  mutable gains : float array;
  mutable bells : Filter.biquad array;
}

let bells (gains : float array) : Filter.biquad array =
  Array.mapi (fun k frequency -> Filter.peaking ~frequency ~q ~gain:gains.(k)) frequencies

let create () : t =
  let memories () = Array.map (fun _ -> Filter.silence ()) frequencies in
  let gains = Array.make 10 0. in
  { left = memories (); right = memories (); gains; bells = bells gains }

let db_to_gain (db : float) : float = 10. ** (db /. 20.)

let process (t : t) ~(preamp : float) ~(gains : float array) (s : Signal.stereo) : unit =
  if gains <> t.gains then (
    t.gains <- Array.copy gains;
    t.bells <- bells gains);
  let pre = db_to_gain preamp in
  let one (x : Signal.t) (memories : Filter.memory array) =
    for i = 0 to Array.length x - 1 do
      let v = ref (x.(i) *. pre) in
      for k = 0 to 9 do
        v := Filter.step t.bells.(k) memories.(k) !v
      done;
      x.(i) <- !v
    done
  in
  one s.left t.left;
  one s.right t.right

let response ~(preamp : float) ~(gains : float array) (f : float) : float =
  Array.fold_left (fun db b -> db +. (20. *. log10 (Filter.response b f))) preamp (bells gains)

let presets : (string * float array) list =
  [
    ("Flat", [| 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0. |]);
    ("Rock", [| 5.; 3.; -3.; -5.; -2.; 2.; 5.; 7.; 7.; 7. |]);
    ("Pop", [| -1.; 3.; 5.; 5.; 3.; -1.; -2.; -2.; -1.; -1. |]);
    ("Classical", [| 0.; 0.; 0.; 0.; 0.; 0.; -4.; -4.; -4.; -6. |]);
    ("Full Bass", [| 7.; 7.; 7.; 4.; 1.; -3.; -6.; -8.; -8.; -8. |]);
    ("Full Treble", [| -8.; -8.; -8.; -3.; 2.; 7.; 10.; 10.; 10.; 10. |]);
    ("Techno", [| 5.; 4.; 0.; -4.; -3.; 0.; 5.; 6.; 6.; 5. |]);
  ]
