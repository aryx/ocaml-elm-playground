(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_reason.mli *)

let t = Testo.create

(* an instrument playing a constant, and one recording its notes *)
let constant (v : float) : Instrument.t =
  { note_on = (fun _ _ -> ()); note_off = (fun _ -> ()); set = (fun _ _ -> ()); fill = (fun b -> Array.fill b.left 0 (Array.length b.left) v; Array.fill b.right 0 (Array.length b.right) v) }

let recorder (notes : (int * int) list ref) (chunks : int ref) : Instrument.t =
  {
    note_on = (fun n _ -> notes := (n, !chunks) :: !notes);
    note_off = (fun _ -> ());
    set = (fun _ _ -> ());
    fill = (fun _ -> incr chunks);
  }

(* a rack from its devices, in the rack's order under the hardware *)
let rack (devices : (string * Rack_device.t) list) : Studio_reason.t * Studio_reason.id list =
  let p, ids =
    List.fold_left
      (fun (p, ids) (kind, _) ->
        let p, id = Studio_reason.add p ~kind ~below:None in
        (p, ids @ [ id ]))
      (Studio_reason.empty, []) devices
  in
  let r = Studio_reason.create p in
  List.iter2 (fun id (_, d) -> Studio_reason.attach r id d) ids devices;
  Studio_reason.set_patch r p;
  (r, ids)

let cable (a, j) (b, k) : Studio_reason.cable = { out = { device = a; jack = j }; into = { device = b; jack = k } }

let plug (r : Studio_reason.t) (c : Studio_reason.cable) : unit =
  match Studio_reason.connect (Studio_reason.lookup r) (Studio_reason.patch r) c with
  | Ok p -> Studio_reason.set_patch r p
  | Error e -> Alcotest.failf "refused: %s" e

let pull ?(block = 735) (r : Studio_reason.t) (n : int) : Signal.t =
  let i = Studio_reason.instrument r in
  let out = Array.make n 0. in
  let k = ref 0 in
  while !k < n do
    let m = min block (n - !k) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 out !k m;
    k := !k + m
  done;
  out

(*****************************************************************************)
(* The graph *)
(*****************************************************************************)

(* source -> delay -> mixer -> hardware: the delay after the source,
 * whichever cable went in first *)
let test_order () =
  let make () = rack [ ("mixer", Rack_mixer.create ()); ("delay", Rack_device.of_effect ~kind:"delay" ~bypass:(fun () -> true) (Delay.fx ())); ("src", Rack_device.of_instrument ~kind:"src" (constant 1.)) ] in
  let cables m d s = [ cable (s, 0) (d, 0); cable (d, 1) (m, 0); cable (m, Rack_mixer.master_out) (Studio_reason.hardware, 0) ] in
  let orders =
    List.map
      (fun rev ->
        let r, ids = make () in
        let m, d, s = match ids with [ m; d; s ] -> (m, d, s) | _ -> assert false in
        let cs = cables m d s in
        List.iter (plug r) (if rev then List.rev cs else cs);
        (Studio_reason.order (Studio_reason.lookup r) (Studio_reason.patch r), s, d, m))
      [ false; true ]
  in
  match orders with
  | [ (Some a, s, d, m); (Some b, _, _, _) ] ->
      Alcotest.(check (list (pair int int))) "the same order, whatever the cables' order" a b;
      let index n = let rec go i = function [] -> -1 | x :: r -> if x = n then i else go (i + 1) r in go 0 a in
      Alcotest.(check bool) "the source before the delay" true (index (s, 0) < index (d, 0));
      Alcotest.(check bool) "the delay before the mixer" true (index (d, 0) < index (m, 0))
  | _ -> Alcotest.fail "no order"

(* what a rack refuses, and the send and return it takes *)
let test_refused () =
  let r, ids = rack [ ("mixer", Rack_mixer.create ()); ("delay", Rack_device.of_effect ~kind:"delay" ~bypass:(fun () -> false) (Delay.fx ())); ("matrix", Rack_matrix.create ()) ] in
  let m, d, x = match ids with [ m; d; x ] -> (m, d, x) | _ -> assert false in
  let try_ c = match Studio_reason.connect (Studio_reason.lookup r) (Studio_reason.patch r) c with Ok _ -> "ok" | Error e -> e in
  Alcotest.(check string) "Out to Out" "Out to Out" (try_ (cable (d, 1) (m, Rack_mixer.aux_send)));
  Alcotest.(check string) "In to In" "In to In" (try_ (cable (d, 0) (m, 0)));
  Alcotest.(check string) "CV into audio" "CV into audio" (try_ (cable (x, Rack_matrix.gate_out) (m, 0)));
  plug r (cable (m, Rack_mixer.aux_send) (d, 0));
  Alcotest.(check string) "the send's return" "ok" (try_ (cable (d, 1) (m, Rack_mixer.aux_return)));
  Alcotest.(check string) "the send back into a channel: a loop" "a loop through the mixer" (try_ (cable (d, 1) (m, 3)))

(* Reason's routing by itself: instruments to channels 1 and 2; a delay
 * with the mixer selected its send, with an instrument selected its
 * insert; the insert taken out, the instrument joined again *)
let test_route () =
  let devices =
    [
      ("mixer", Rack_mixer.create ()); ("a", Rack_device.of_instrument ~kind:"a" (constant 0.));
      ("b", Rack_device.of_instrument ~kind:"b" (constant 0.)); ("send", Rack_device.of_effect ~kind:"send" ~bypass:(fun () -> false) (Delay.fx ()));
      ("insert", Rack_device.of_effect ~kind:"insert" ~bypass:(fun () -> false) (Delay.fx ())); ("matrix", Rack_matrix.create ());
    ]
  in
  let r, ids = rack devices in
  let m, a, b, send, ins, x = match ids with [ m; a; b; s; i; x ] -> (m, a, b, s, i, x) | _ -> assert false in
  let lookup = Studio_reason.lookup r in
  let p = Studio_reason.patch r in
  let p = Studio_reason.route lookup p m ~selected:None in
  let p = Studio_reason.route lookup p a ~selected:None in
  let p = Studio_reason.route lookup p b ~selected:None in
  let p = Studio_reason.route lookup p send ~selected:(Some m) in
  let p = Studio_reason.route lookup p ins ~selected:(Some a) in
  let p = Studio_reason.route lookup p x ~selected:(Some b) in
  let has c = List.mem c p.cables in
  Alcotest.(check bool) "the mixer to the hardware" true (has (cable (m, Rack_mixer.master_out) (Studio_reason.hardware, 0)));
  Alcotest.(check bool) "b to channel 2" true (has (cable (b, 0) (m, 1)));
  Alcotest.(check bool) "the send" true (has (cable (m, Rack_mixer.aux_send) (send, 0)) && has (cable (send, 1) (m, Rack_mixer.aux_return)));
  Alcotest.(check bool) "a through its insert to channel 1" true (has (cable (a, 0) (ins, 0)) && has (cable (ins, 1) (m, 0)));
  Alcotest.(check bool) "the matrix into b" true (has (cable (x, Rack_matrix.note_out) (b, 1)) && has (cable (x, Rack_matrix.gate_out) (b, 2)));
  let p = Studio_reason.remove lookup p ins in
  Alcotest.(check bool) "the insert out: a to channel 1 again" true (List.mem (cable (a, 0) (m, 0)) p.cables)

(*****************************************************************************)
(* The sound *)
(*****************************************************************************)

(* a constant 1 into channel 1: out, its level times the master, soft
 * clipped; muted or unplugged, silence *)
let test_levels () =
  let mixer = Rack_mixer.create () in
  let r, ids = rack [ ("mixer", mixer); ("src", Rack_device.of_instrument ~kind:"src" (constant 1.)) ] in
  let m, s = match ids with [ m; s ] -> (m, s) | _ -> assert false in
  plug r (cable (s, 0) (m, 0));
  plug r (cable (m, Rack_mixer.master_out) (Studio_reason.hardware, 0));
  Studio_reason.set_patch r { (Studio_reason.patch r) with volume = 1. };
  mixer.set "ch1.level" 0.5;
  mixer.set "master" 1.;
  let out = pull r 1000 in
  Alcotest.(check (float 1e-12)) "level 0.5" (Mix.soft_clip 0.5) out.(999);
  mixer.set "ch1.mute" 1.;
  Alcotest.(check (float 0.)) "muted" 0. (pull r 1000).(999);
  mixer.set "ch1.mute" 0.;
  Studio_reason.set_patch r (Studio_reason.disconnect (Studio_reason.patch r) { device = m; jack = 0 });
  Alcotest.(check (float 0.)) "unplugged" 0. (pull r 1000).(999)

(* the chunks are the rack's, not the mixer's pulls: the same samples in
 * blocks of 735, 100 or 1 *)
let test_blocks () =
  let play block =
    let d = Rack_device.of_instrument ~kind:"sine" (Instrument.sine ()) in
    let r, ids = rack [ ("sine", d) ] in
    plug r (cable (List.hd ids, 0) (Studio_reason.hardware, 0));
    d.note_on 69 1.;
    pull ~block r 5000
  in
  let a = play 735 in
  Alcotest.(check bool) "735 as 100" true (a = play 100);
  Alcotest.(check bool) "735 as 1" true (a = play 1);
  Alcotest.(check bool) "not silence" true (Array.exists (fun x -> x <> 0.) a)

(* the Matrix at 120 BPM: a sixteenth is 5512.5 samples; its second
 * note heard at the first chunk past it, chunk 87 (sample 5568) *)
let test_matrix () =
  Alcotest.(check (pair int (float 1e-9))) "step 1 at 5512.5" (1, 0.) (Rack_matrix.position ~tempo:120. 5512.5);
  let notes = ref [] and chunks = ref 0 in
  let r, ids = rack [ ("matrix", Rack_matrix.create ()); ("rec", Rack_device.of_instrument ~kind:"rec" (recorder notes chunks)) ] in
  let x, s = match ids with [ x; s ] -> (x, s) | _ -> assert false in
  plug r (cable (x, Rack_matrix.note_out) (s, 1));
  plug r (cable (x, Rack_matrix.gate_out) (s, 2));
  Studio_reason.run r true;
  ignore (pull r (Rack_device.chunk * 100));
  match List.rev !notes with
  | (n0, c0) :: (n1, c1) :: _ ->
      Alcotest.(check (pair int int)) "the first note, C2, at once" (36, 0) (n0, c0);
      Alcotest.(check (pair int int)) "the second, C3, at chunk 87" (48, 87) (n1, c1)
  | _ -> Alcotest.fail "fewer than two notes"

let tests =
  Testo.categorize "Studio_reason"
    [
      t "the order, whatever the cables' order" test_order;
      t "the cables refused, a send and return taken" test_refused;
      t "the automatic routing" test_route;
      t "levels, mutes, unplugged" test_levels;
      t "the same samples whatever the blocks" test_blocks;
      t "the Matrix's timing" test_matrix;
    ]
