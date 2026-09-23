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

(* See Multiplayer.mli *)

type player = { id : int; keyboard : keyboard; pressed : keyboard }

(*****************************************************************************)
(* A player's input, a byte *)
(*****************************************************************************)

let empty = initial_computer.keyboard

(* the keys that travel, a bit each *)
let bits (k : keyboard) : (bool * int) list =
  [ (k.kup, 1); (k.kdown, 2); (k.kleft, 4); (k.kright, 8); (k.kspace, 16); (k.kenter, 32); (k.kshift, 64) ]

let encode (k : keyboard) : string =
  String.make 1 (Char.chr (List.fold_left (fun acc (held, bit) -> if held then acc lor bit else acc) 0 (bits k)))

let decode (s : string) : keyboard =
  let b = if s = "" then 0 else Char.code s.[0] in
  let on bit = b land bit <> 0 in
  { empty with kup = on 1; kdown = on 2; kleft = on 4; kright = on 8; kspace = on 16; kenter = on 32; kshift = on 64 }

(* held now and not before *)
let rising (now : keyboard) (before : keyboard) : keyboard =
  decode (encode { empty with
    kup = now.kup && not before.kup; kdown = now.kdown && not before.kdown;
    kleft = now.kleft && not before.kleft; kright = now.kright && not before.kright;
    kspace = now.kspace && not before.kspace; kenter = now.kenter && not before.kenter;
    kshift = now.kshift && not before.kshift })

(* one physical keyboard, shared: player 0 the arrows (and space, enter,
 * shift), player 1 w a s d, the others nothing *)
let local_keyboard (physical : keyboard) (n : int) : keyboard =
  match n with
  | 0 -> decode (encode physical)
  | 1 -> { empty with kup = physical.kw; kdown = physical.ks; kleft = physical.ka; kright = physical.kd }
  | _ -> empty

(* what update may see of the computer: what every peer agrees on *)
let cleaned (flags : flags) (tick : int) : computer =
  { initial_computer with flags; time = Time (Time.millis_to_posix ((tick * 1000 / 60) + 1)) }

(*****************************************************************************)
(* The state *)
(*****************************************************************************)

(* one peer's game: its model, and the players' keyboards of the tick
 * before (for [pressed]) *)
type 'model side = { model : 'model; last : keyboard array; tick : int }

type knobs = { latency : int (* ms *); jitter : int; loss : int (* % *); delay : int }

type 'model state =
  | Starting of 'model
  | Local of 'model side
  | Simulate of {
      net : Sim_net.t;
      peers : (Lockstep.t * 'model side) array;
      frame : int;
      knobs : knobs;
      held : string list; (* the knob keys held, for their rising edge *)
    }

let one_tick update flags (s : 'model side) (keyboards : keyboard array) : 'model side =
  let players = Array.to_list (Array.mapi (fun id k -> { id; keyboard = k; pressed = rising k s.last.(id) }) keyboards) in
  { model = update (cleaned flags s.tick) players s.model; last = keyboards; tick = s.tick + 1 }

let config_of (k : knobs) : Sim_net.config =
  { latency = float_of_int k.latency /. 1000.; jitter = float_of_int k.jitter /. 1000.; loss = float_of_int k.loss /. 100.; duplication = 0. }

let int_flag (flags : flags) (name : string) (default : int) : int =
  Option.value (Option.bind (List.assoc_opt name flags) int_of_string_opt) ~default

let start ~players (flags : flags) (model : 'model) : 'model state =
  let side = { model; last = Array.make players empty; tick = 0 } in
  match List.assoc_opt "net" flags with
  | Some "simulate" ->
      let knobs =
        { latency = int_flag flags "latency" 50; jitter = int_flag flags "jitter" 10; loss = int_flag flags "loss" 5;
          delay = int_flag flags "delay" 3 }
      in
      Simulate
        {
          net = Sim_net.create ~seed:(int_flag flags "seed" 1) (config_of knobs);
          peers = Array.init players (fun me -> (Lockstep.create ~me ~players ~delay:knobs.delay, side));
          frame = 0;
          knobs;
          held = [];
        }
  | _ -> Local side

(*****************************************************************************)
(* Simulate: the peers, the fake network between them *)
(*****************************************************************************)

(* [ and ] the latency by 20 ms, - and = the loss by 5% *)
let turn_knobs (physical : keyboard) (held : string list) (k : knobs) : knobs * string list =
  let keys = [ "["; "]"; "-"; "=" ] in
  let now = List.filter (fun key -> Set_.mem key physical.keys) keys in
  let pressed key = List.mem key now && not (List.mem key held) in
  let clamp lo hi x = max lo (min hi x) in
  let k = if pressed "[" then { k with latency = clamp 0 1000 (k.latency - 20) } else k in
  let k = if pressed "]" then { k with latency = clamp 0 1000 (k.latency + 20) } else k in
  let k = if pressed "-" then { k with loss = clamp 0 100 (k.loss - 5) } else k in
  let k = if pressed "=" then { k with loss = clamp 0 100 (k.loss + 5) } else k in
  (k, now)

(* one frame of every peer: its packets read, its tick simulated if it
 * has every input (else it stalls, its model frozen), its packet sent *)
let simulate_frame update (computer : computer) net peers frame players =
  let now = float_of_int frame /. 60. in
  Array.mapi
    (fun me (lockstep, (side : 'model side)) ->
      List.iter (fun (_, bytes) -> Lockstep.receive lockstep bytes) (Sim_net.receive net ~now me);
      let side =
        match Lockstep.step lockstep (encode (local_keyboard computer.keyboard me)) with
        | None -> side
        | Some inputs ->
            let tick = side.tick in
            let side = one_tick update computer.flags side (Array.map decode inputs) in
            if tick mod 60 = 0 then Lockstep.checksum lockstep ~tick (Checksum.of_model side.model);
            side
      in
      let packet = Lockstep.packet lockstep in
      for other = 0 to players - 1 do
        if other <> me then Sim_net.send net ~now ~src:me ~dst:other packet
      done;
      (lockstep, side))
    peers

(*****************************************************************************)
(* The views *)
(*****************************************************************************)

let text (color : color) (s : string) : shape = words color s |> scale 1.6

let hud (knobs : knobs) peers : shape list =
  let desync =
    Array.to_list peers |> List.find_map (fun (l, _) -> Lockstep.desync l)
  in
  [ text white
      (Printf.sprintf "latency %d ms ([ ])   jitter %d ms   loss %d%% (- =)   input delay %d ticks" knobs.latency
         knobs.jitter knobs.loss knobs.delay)
    |> move_y (-440.);
    (match desync with
    | None -> text green "the two games agree (checksums every second)"
    | Some (tick, peer) -> text red (Printf.sprintf "DESYNC at tick %d, with peer %d" tick peer))
    |> move_y (-470.) ]

let side_by_side view (computer : computer) (knobs : knobs) peers : shape list =
  let n = Array.length peers in
  let width = 1000. /. float_of_int n in
  let halves =
    Array.to_list
      (Array.mapi
         (fun me (lockstep, (side : 'model side)) ->
           let x = (-500.) +. (width *. (float_of_int me +. 0.5)) in
           let stats = Lockstep.stats lockstep in
           [ group (view computer me side.model) |> scale (1. /. float_of_int n) |> move_x x;
             text white (Printf.sprintf "computer %d: tick %d, %d stalls" me side.tick stats.stalls) |> move x 300. ])
         peers)
  in
  (rectangle (rgb 40 40 40) 1000. 1000. :: List.concat halves) @ hud knobs peers

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let game ~(players : int) view update (model : 'model) =
  let rec update_state (computer : computer) (state : 'model state) : 'model state =
    match state with
    (* the flags known at last: the mode chosen, and this frame's tick
     * played in it, as a one-player game would *)
    | Starting model -> update_state computer (start ~players computer.flags model)
    | Local side -> Local (one_tick update computer.flags side (Array.init players (local_keyboard computer.keyboard)))
    | Simulate s ->
        let knobs, held = turn_knobs computer.keyboard s.held s.knobs in
        if knobs <> s.knobs then Sim_net.set_config s.net (config_of knobs);
        Simulate { s with peers = simulate_frame update computer s.net s.peers s.frame players; frame = s.frame + 1; knobs; held }
  in
  let view_state (computer : computer) (state : 'model state) : shape list =
    match state with
    | Starting model -> view computer 0 model
    | Local side -> view computer 0 side.model
    | Simulate s -> side_by_side view computer s.knobs s.peers
  in
  Playground.game view_state update_state (Starting model)
