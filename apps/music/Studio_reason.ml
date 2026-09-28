(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type id = int
type port = { device : id; jack : int }
type cable = { out : port; into : port }
type patch = { devices : (id * string) list; cables : cable list; tempo : float; volume : float; next : id }

let hardware = 0

let hardware_device : Rack_device.t =
  {
    kind = "hardware";
    role = Hardware;
    jacks = [| { label = "Audio In"; dir = In; signal = Audio } |];
    (* nothing to do: the studio reads its input itself *)
    stages = [ { reads = [ 0 ]; writes = []; run = (fun _ -> ()) } ];
    set = (fun _ _ -> ());
    get = (fun _ -> 0.);
    note_on = (fun _ _ -> ());
    note_off = (fun _ -> ());
    run = (fun _ -> ());
    tempo = (fun _ -> ());
    step = (fun () -> None);
  }

let empty : patch = { devices = [ (hardware, "hardware") ]; cables = []; tempo = 120.; volume = 0.8; next = 1 }

(*****************************************************************************)
(* The graph *)
(*****************************************************************************)

type lookup = id -> Rack_device.t

(* the stage of [d] that writes, or reads, a jack *)
let stage_of (d : Rack_device.t) (jack : int) ~(writes : bool) : int option =
  let rec go k = function
    | [] -> None
    | (s : Rack_device.stage) :: rest -> if List.mem jack (if writes then s.writes else s.reads) then Some k else go (k + 1) rest
  in
  go 0 d.stages

(* Kahn's algorithm: the nodes, (device, stage), in the rack's order;
 * an edge from a stage to the next of its device, and along each cable
 * from the stage writing its output to the stage reading its input.
 * Each round takes the first node, in the rack's order, that nothing
 * left feeds. *)
let order (lookup : lookup) (p : patch) : (id * int) list option =
  let nodes = List.concat_map (fun (id, _) -> List.init (List.length (lookup id).stages) (fun k -> (id, k))) p.devices in
  let edges =
    List.concat_map (fun (id, k) -> if k > 0 then [ ((id, k - 1), (id, k)) ] else []) nodes
    @ List.filter_map
        (fun c ->
          match (stage_of (lookup c.out.device) c.out.jack ~writes:true, stage_of (lookup c.into.device) c.into.jack ~writes:false) with
          | Some a, Some b -> Some ((c.out.device, a), (c.into.device, b))
          | _ -> None)
        p.cables
  in
  let rec go left edges acc =
    if left = [] then Some (List.rev acc)
    else
      match List.find_opt (fun n -> not (List.exists (fun (_, b) -> b = n) edges)) left with
      | None -> None
      | Some n -> go (List.filter (fun m -> m <> n) left) (List.filter (fun (a, _) -> a <> n) edges) (n :: acc)
  in
  go nodes edges []

let disconnect (p : patch) (port : port) : patch = { p with cables = List.filter (fun c -> c.out <> port && c.into <> port) p.cables }
let cable_at (p : patch) (port : port) : cable option = List.find_opt (fun c -> c.out = port || c.into = port) p.cables

let connect (lookup : lookup) (p : patch) (c : cable) : (patch, string) result =
  let o = (lookup c.out.device).jacks.(c.out.jack) and i = (lookup c.into.device).jacks.(c.into.jack) in
  match (o.dir, i.dir, o.signal, i.signal) with
  | In, In, _, _ -> Error "In to In"
  | Out, Out, _, _ -> Error "Out to Out"
  | In, Out, _, _ -> Error "In to Out"
  | _, _, Audio, Cv -> Error "audio into CV"
  | _, _, Cv, Audio -> Error "CV into audio"
  | _ ->
      let p' = { p with cables = c :: (disconnect (disconnect p c.out) c.into).cables } in
      if order lookup p' = None then Error ("a loop through the " ^ (lookup c.into.device).kind) else Ok p'

let add (p : patch) ~kind ~(below : id option) : patch * id =
  let id = p.next in
  let rec insert = function
    | [] -> [ (id, kind) ]
    | (d, k) :: rest -> if Some d = below then (d, k) :: (id, kind) :: rest else (d, k) :: insert rest
  in
  ({ p with devices = insert p.devices; next = id + 1 }, id)

let free (p : patch) (port : port) : bool = cable_at p port = None

(* a connection tried, the patch as it was if refused *)
let try_connect lookup p c = match connect lookup p c with Ok p -> p | Error _ -> p

(* an audio output to the first free mixer channel, else the hardware *)
let to_mixer (lookup : lookup) (p : patch) (out : port) : patch =
  let channel =
    List.find_map
      (fun (id, _) ->
        if (lookup id).role = Mixer then
          List.find_map (fun k -> if free p { device = id; jack = k } then Some { device = id; jack = k } else None) (List.init Rack_mixer.channels (fun k -> k))
        else None)
      p.devices
  in
  match channel with
  | Some into -> try_connect lookup p { out; into }
  | None ->
      let hw = { device = hardware; jack = 0 } in
      if free p hw then try_connect lookup p { out; into = hw } else p

let route (lookup : lookup) (p : patch) (id : id) ~(selected : id option) : patch =
  let d = lookup id in
  let sel = Option.map (fun s -> (s, lookup s)) selected in
  let port device jack = { device; jack } in
  match d.role with
  | Hardware -> p
  | Mixer ->
      let hw = port hardware 0 in
      if free p hw then try_connect lookup p { out = port id Rack_mixer.master_out; into = hw } else p
  | Instrument -> to_mixer lookup p (port id 0)
  | Sequencer -> (
      let target =
        match sel with
        | Some (s, sd) when sd.role = Instrument -> Some s
        | _ ->
            List.find_map
              (fun (i, _) -> let di = lookup i in if di.role = Instrument && Option.fold ~none:false ~some:(fun g -> free p (port i g)) (Rack_device.jack di "Seq Gate") then Some i else None)
              p.devices
      in
      match target with
      | None -> p
      | Some s -> (
          let ds = lookup s in
          match (Rack_device.jack ds "Seq Note", Rack_device.jack ds "Seq Gate") with
          | Some n, Some g ->
              let p = try_connect lookup p { out = port id Rack_matrix.note_out; into = port s n } in
              try_connect lookup p { out = port id Rack_matrix.gate_out; into = port s g }
          | _ -> p))
  | Effect -> (
      let fx_in = port id 0 and fx_out = port id 1 in
      match sel with
      | Some (m, md) when md.role = Mixer && free p (port m Rack_mixer.aux_send) && free p (port m Rack_mixer.aux_return) ->
          let p = try_connect lookup p { out = port m Rack_mixer.aux_send; into = fx_in } in
          try_connect lookup p { out = fx_out; into = port m Rack_mixer.aux_return }
      | Some (i, _) -> (
          (* inserted after the selected device's audio output *)
          let out = port i 0 in
          match cable_at p out with
          | Some c when c.out = out ->
              let p = try_connect lookup p { out; into = fx_in } in
              try_connect lookup p { out = fx_out; into = c.into }
          | _ -> to_mixer lookup (try_connect lookup p { out; into = fx_in }) fx_out)
      | None -> to_mixer lookup p fx_out)

let remove (lookup : lookup) (p : patch) (id : id) : patch =
  let d = lookup id in
  let into = List.find_opt (fun c -> c.into.device = id && d.jacks.(c.into.jack).signal = Audio) p.cables in
  let from = List.find_opt (fun c -> c.out.device = id && d.jacks.(c.out.jack).signal = Audio) p.cables in
  let p = { p with devices = List.filter (fun (i, _) -> i <> id) p.devices; cables = List.filter (fun c -> c.out.device <> id && c.into.device <> id) p.cables } in
  match (d.role, into, from) with
  | Effect, Some a, Some b -> { p with cables = { out = a.out; into = b.into } :: p.cables }
  | _ -> p

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t = {
  mutable patch : patch;
  devices : (id, Rack_device.t) Hashtbl.t;
  mutable order : (id * int) list;
  incoming : (id * int, port) Hashtbl.t; (* an input's cable's output *)
  audio : (id * int, Signal.stereo) Hashtbl.t; (* the outputs' buffers *)
  cv : (id * int, float) Hashtbl.t;
  silence : Signal.stereo;
  out : Signal.stereo; (* the chunk being played *)
  mutable played : int; (* how much of it *)
  mutable running : bool;
  ring : Signal.t;
  mutable at : int;
}

let chunk = Rack_device.chunk
let stereo () : Signal.stereo = { left = Array.make chunk 0.; right = Array.make chunk 0. }
let lookup (t : t) : lookup = fun id -> match Hashtbl.find_opt t.devices id with Some d -> d | None -> hardware_device
let patch (t : t) : patch = t.patch

let set_patch (t : t) (p : patch) : unit =
  if p.tempo <> t.patch.tempo then Hashtbl.iter (fun _ (d : Rack_device.t) -> d.tempo p.tempo) t.devices;
  let ids = List.map fst p.devices in
  Hashtbl.filter_map_inplace (fun id d -> if List.mem id ids then Some d else None) t.devices;
  t.patch <- p;
  (match order (lookup t) p with Some o -> t.order <- o | None -> ());
  Hashtbl.reset t.incoming;
  List.iter (fun c -> Hashtbl.replace t.incoming (c.into.device, c.into.jack) c.out) p.cables

let create (p : patch) : t =
  let t =
    {
      patch = p;
      devices = Hashtbl.create 16;
      order = [];
      incoming = Hashtbl.create 16;
      audio = Hashtbl.create 32;
      cv = Hashtbl.create 16;
      silence = stereo ();
      out = stereo ();
      played = chunk;
      running = false;
      ring = Array.make 2048 0.;
      at = 0;
    }
  in
  Hashtbl.replace t.devices hardware hardware_device;
  set_patch t p;
  t

let attach (t : t) (id : id) (d : Rack_device.t) : unit =
  Hashtbl.replace t.devices id d;
  d.tempo t.patch.tempo;
  (match order (lookup t) t.patch with Some o -> t.order <- o | None -> ())

let run (t : t) (on : bool) : unit =
  t.running <- on;
  Hashtbl.iter (fun _ (d : Rack_device.t) -> d.run on) t.devices

let running (t : t) : bool = t.running

let buffer (t : t) (key : id * int) : Signal.stereo =
  match Hashtbl.find_opt t.audio key with
  | Some b -> b
  | None ->
      let b = stereo () in
      Hashtbl.replace t.audio key b;
      b

let peak (t : t) (p : port) : float =
  match Hashtbl.find_opt t.audio (p.device, p.jack) with
  | Some b -> Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. b.left
  | None -> 0.

let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))

(* one chunk: every stage in order, then what reaches the hardware *)
let compute (t : t) : unit =
  List.iter
    (fun (id, k) ->
      match Hashtbl.find_opt t.devices id with
      | None -> ()
      | Some (d : Rack_device.t) ->
          let io : Rack_device.io =
            {
              audio_in = (fun j -> match Hashtbl.find_opt t.incoming (id, j) with Some p -> buffer t (p.device, p.jack) | None -> t.silence);
              cv_in = (fun j -> match Hashtbl.find_opt t.incoming (id, j) with Some p -> Hashtbl.find_opt t.cv (p.device, p.jack) | None -> None);
              audio_out = (fun j -> buffer t (id, j));
              cv_out = (fun j v -> Hashtbl.replace t.cv (id, j) v);
            }
          in
          (List.nth d.stages k).run io)
    t.order;
  let src = match Hashtbl.find_opt t.incoming (hardware, 0) with Some p -> buffer t (p.device, p.jack) | None -> t.silence in
  for i = 0 to chunk - 1 do
    t.out.left.(i) <- Mix.soft_clip (t.patch.volume *. src.left.(i));
    t.out.right.(i) <- Mix.soft_clip (t.patch.volume *. src.right.(i))
  done;
  t.played <- 0

let fill (t : t) (out : Signal.stereo) : unit =
  for i = 0 to Array.length out.left - 1 do
    if t.played >= chunk then compute t;
    out.left.(i) <- t.out.left.(t.played);
    out.right.(i) <- t.out.right.(t.played);
    t.played <- t.played + 1;
    t.ring.(t.at) <- out.left.(i);
    t.at <- (t.at + 1) mod 2048
  done

let instrument (t : t) : Instrument.t = { note_on = (fun _ _ -> ()); note_off = (fun _ -> ()); set = (fun _ _ -> ()); fill = fill t }
