(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let channels = 14
let aux_send = 14
let aux_return = 15
let master_out = 16

let create () : Rack_device.t =
  let level = Array.make channels 0.7 and pan = Array.make channels 0. and aux = Array.make channels 0. in
  let mute = Array.make channels false and master = ref 0.8 in
  (* the channels' sum, kept from the first stage for the second *)
  let sum = ref { Signal.left = [||]; right = [||] } in
  let jacks =
    Array.append
      (Array.init channels (fun k -> { Rack_device.label = Printf.sprintf "Channel %d" (k + 1); dir = In; signal = Audio }))
      [|
        { label = "Aux Send"; dir = Out; signal = Audio };
        { label = "Aux Return"; dir = In; signal = Audio };
        { label = "Master Out"; dir = Out; signal = Audio };
      |]
  in
  let sends (io : Rack_device.io) =
    let send = io.audio_out aux_send in
    let n = Array.length send.left in
    if Array.length !sum.left <> n then sum := { left = Array.make n 0.; right = Array.make n 0. };
    let s = !sum in
    Array.fill s.left 0 n 0.;
    Array.fill s.right 0 n 0.;
    Array.fill send.left 0 n 0.;
    Array.fill send.right 0 n 0.;
    for k = 0 to channels - 1 do
      if not mute.(k) then begin
        let x = io.audio_in k in
        (* a balance: the side turned away from goes down *)
        let gl = level.(k) *. Float.min 1. (1. -. pan.(k)) and gr = level.(k) *. Float.min 1. (1. +. pan.(k)) in
        for i = 0 to n - 1 do
          let l = gl *. x.left.(i) and r = gr *. x.right.(i) in
          s.left.(i) <- s.left.(i) +. l;
          s.right.(i) <- s.right.(i) +. r;
          send.left.(i) <- send.left.(i) +. (aux.(k) *. l);
          send.right.(i) <- send.right.(i) +. (aux.(k) *. r)
        done
      end
    done
  in
  let master_stage (io : Rack_device.io) =
    let ret = io.audio_in aux_return and out = io.audio_out master_out in
    let s = !sum in
    for i = 0 to Array.length out.left - 1 do
      out.left.(i) <- !master *. (s.left.(i) +. ret.left.(i));
      out.right.(i) <- !master *. (s.right.(i) +. ret.right.(i))
    done
  in
  (* "ch3.level": the array and the channel *)
  let find name =
    match String.index_opt name '.' with
    | Some dot when String.length name > 2 && String.sub name 0 2 = "ch" -> (
        match int_of_string_opt (String.sub name 2 (dot - 2)) with
        | Some c when c >= 1 && c <= channels -> Some (String.sub name (dot + 1) (String.length name - dot - 1), c - 1)
        | _ -> None)
    | _ -> None
  in
  let set name v =
    if name = "master" then master := v
    else
      match find name with
      | Some ("level", k) -> level.(k) <- v
      | Some ("pan", k) -> pan.(k) <- v
      | Some ("aux", k) -> aux.(k) <- v
      | Some ("mute", k) -> mute.(k) <- v >= 0.5
      | _ -> ()
  in
  let get name =
    if name = "master" then !master
    else
      match find name with
      | Some ("level", k) -> level.(k)
      | Some ("pan", k) -> pan.(k)
      | Some ("aux", k) -> aux.(k)
      | Some ("mute", k) -> if mute.(k) then 1. else 0.
      | _ -> 0.
  in
  {
    kind = "mixer";
    role = Mixer;
    jacks;
    stages =
      [
        { reads = List.init channels (fun k -> k); writes = [ aux_send ]; run = sends };
        { reads = [ aux_return ]; writes = [ master_out ]; run = master_stage };
      ];
    set;
    get;
    note_on = (fun _ _ -> ());
    note_off = (fun _ -> ());
    run = (fun _ -> ());
    tempo = (fun _ -> ());
    step = (fun () -> None);
  }
