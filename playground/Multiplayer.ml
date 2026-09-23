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
 * shift), player 1 w a s d (and q for its space), the others nothing *)
let local_keyboard (physical : keyboard) (n : int) : keyboard =
  match n with
  | 0 -> decode (encode physical)
  | 1 ->
      { empty with kup = physical.kw; kdown = physical.ks; kleft = physical.ka; kright = physical.kd;
        kspace = Set_.mem "q" physical.keys }
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

let one_tick update flags (s : 'model side) (keyboards : keyboard array) : 'model side =
  let players = Array.to_list (Array.mapi (fun id k -> { id; keyboard = k; pressed = rising k s.last.(id) }) keyboards) in
  { model = update (cleaned flags s.tick) players s.model; last = keyboards; tick = s.tick + 1 }

(*****************************************************************************)
(* A peer: its netcode, and its game *)
(*****************************************************************************)

(* lockstep waits for every input (Lockstep.mli); rollback guesses the
 * missing ones and fixes its game when they arrive (Rollback.mli) *)
type 'model peer = Waiting of Lockstep.t * 'model side ref | Guessing of 'model side Rollback.t

let new_peer ~(netcode : string) ~(me : int) ~(players : int) ~(delay : int) update flags (side : 'model side) : 'model peer =
  match netcode with
  | "rollback" ->
      let rec peer =
        lazy
          (Rollback.create ~me ~players ~delay
             ~update:(fun inputs s -> one_tick update flags s (Array.map decode inputs))
             (* the checksums of confirmed models only: a guessed one
              * differs between peers, and means nothing *)
             ~on_confirm:(fun tick (s : 'model side) ->
               if tick mod 60 = 0 then Rollback.checksum (Lazy.force peer) ~tick (Checksum.of_model s.model))
             side)
      in
      Guessing (Lazy.force peer)
  | _ -> Waiting (Lockstep.create ~me ~players ~delay, ref side)

let receive (peer : 'model peer) (bytes : string) : unit =
  match peer with Waiting (l, _) -> Lockstep.receive l bytes | Guessing r -> Rollback.receive r bytes

let packet (peer : 'model peer) : string = match peer with Waiting (l, _) -> Lockstep.packet l | Guessing r -> Rollback.packet r

(* one frame of a peer, my keys read now *)
let play update flags (peer : 'model peer) (keys : keyboard) : unit =
  match peer with
  | Waiting (l, side) -> (
      match Lockstep.step l (encode keys) with
      | None -> ()
      | Some inputs ->
          let tick = !side.tick in
          side := one_tick update flags !side (Array.map decode inputs);
          if tick mod 60 = 0 then Lockstep.checksum l ~tick (Checksum.of_model !side.model))
  | Guessing r -> Rollback.step r (encode keys)

let side_of (peer : 'model peer) : 'model side = match peer with Waiting (_, s) -> !s | Guessing r -> Rollback.model r
let desync (peer : 'model peer) = match peer with Waiting (l, _) -> Lockstep.desync l | Guessing r -> Rollback.desync r

let describe (peer : 'model peer) : string =
  match peer with
  | Waiting (l, s) -> Printf.sprintf "tick %d, %d stalls" !s.tick (Lockstep.stats l).stalls
  | Guessing r ->
      let st = Rollback.stats r in
      Printf.sprintf "tick %d, %d rollbacks, %d ticks replayed, %d stalls" (Rollback.tick r) st.rollbacks st.replayed st.stalls

(*****************************************************************************)
(* The modes *)
(*****************************************************************************)

type knobs = { latency : int (* ms *); jitter : int; loss : int (* % *); delay : int; netcode : string }

(* the transport of net=host and net=join, installed by a platform
 * that has sockets (Udp.connect, natively) *)
let connect : (Cap.network -> Transport.role -> (Transport.t, string) result) ref =
  ref (fun _ _ -> Error "no sockets here (in a browser: WebSockets, plan_networking_teaching.md phase 5)")

let set_connect f = connect := f

type 'model state =
  | Starting of 'model
  | Local of 'model side
  (* one peer, me, and the other one over a real network *)
  | Remote of { transport : Transport.t; peer : 'model peer; me : int; netcode : string }
  (* the network couldn't be opened: why, shown on the screen *)
  | Failed of string * 'model
  | Simulate of {
      net : Sim_net.t;
      peers : 'model peer array;
      frame : int;
      knobs : knobs;
      held : string list; (* the knob keys held, for their rising edge *)
      initial : 'model; (* to start again with the other netcode *)
    }

let config_of (k : knobs) : Sim_net.config =
  { latency = float_of_int k.latency /. 1000.; jitter = float_of_int k.jitter /. 1000.; loss = float_of_int k.loss /. 100.; duplication = 0. }

let int_flag (flags : flags) (name : string) (default : int) : int =
  Option.value (Option.bind (List.assoc_opt name flags) int_of_string_opt) ~default

(* netcode=lockstep (the default), rollback, or 1997; the input delay
 * 3 ticks for lockstep, none for the others, unless delay= *)
let netcode_of (flags : flags) : string * int =
  let netcode = match List.assoc_opt "netcode" flags with Some (("rollback" | "1997") as n) -> n | _ -> "lockstep" in
  (netcode, if netcode = "1997" then 0 else int_flag flags "delay" (if netcode = "rollback" then 0 else 3))

let simulate ~players update (flags : flags) (knobs : knobs) (model : 'model) : 'model state =
  let side = { model; last = Array.make players empty; tick = 0 } in
  Simulate
    {
      net = Sim_net.create ~seed:(int_flag flags "seed" 1) (config_of knobs);
      peers = Array.init players (fun me -> new_peer ~netcode:knobs.netcode ~me ~players ~delay:knobs.delay update flags side);
      frame = 0;
      knobs;
      held = [];
      initial = model;
    }

let start ~players ?(network : Cap.network option) update (flags : flags) (model : 'model) : 'model state =
  let side = { model; last = Array.make players empty; tick = 0 } in
  let netcode, delay = netcode_of flags in
  match List.assoc_opt "net" flags with
  | Some "simulate" ->
      let knobs = { latency = int_flag flags "latency" 50; jitter = int_flag flags "jitter" 10; loss = int_flag flags "loss" 5; delay; netcode } in
      simulate ~players update flags knobs model
  | Some (("host" | "join") as net) -> (
      let port = int_flag flags "port" 7777 in
      let role, me =
        if net = "host" then
          (Transport.Host { bind = Option.value (List.assoc_opt "bind" flags) ~default:"127.0.0.1"; port }, 0)
        else (Transport.Join { host = Option.value (List.assoc_opt "host" flags) ~default:"127.0.0.1"; port }, 1)
      in
      match network with
      | None -> Failed (Printf.sprintf "net=%s: this program wasn't granted the network (Cap.network)" net, model)
      | Some caps -> (
          match !connect caps role with
          | Ok transport -> Remote { transport; peer = new_peer ~netcode ~me ~players ~delay update flags side; me; netcode }
          | Error why -> Failed (Printf.sprintf "net=%s: %s" net why, model)))
  | _ -> Local side

(*****************************************************************************)
(* Simulate: the peers, the fake network between them *)
(*****************************************************************************)

(* [ and ] the latency by 20 ms, - and = the loss by 5%; n the next
 * netcode, lockstep, rollback, 1997 (the game starts again) *)
let turn_knobs (physical : keyboard) (held : string list) (k : knobs) : knobs * string list =
  let keys = [ "["; "]"; "-"; "="; "n" ] in
  let now = List.filter (fun key -> Set_.mem key physical.keys) keys in
  let pressed key = List.mem key now && not (List.mem key held) in
  let clamp lo hi x = max lo (min hi x) in
  let k = if pressed "[" then { k with latency = clamp 0 1000 (k.latency - 20) } else k in
  let k = if pressed "]" then { k with latency = clamp 0 1000 (k.latency + 20) } else k in
  let k = if pressed "-" then { k with loss = clamp 0 100 (k.loss - 5) } else k in
  let k = if pressed "=" then { k with loss = clamp 0 100 (k.loss + 5) } else k in
  let k =
    if pressed "n" then
      match k.netcode with
      | "lockstep" -> { k with netcode = "rollback"; delay = 0 }
      | "rollback" -> { k with netcode = "1997"; delay = 0 }
      | _ -> { k with netcode = "lockstep"; delay = 3 }
    else k
  in
  (k, now)

(* one frame of every peer: its packets read, its tick played (or not:
 * a stall), its packet sent *)
let simulate_frame update (computer : computer) net (peers : 'model peer array) frame =
  let now = float_of_int frame /. 60. in
  let players = Array.length peers in
  Array.iteri
    (fun me peer ->
      List.iter (fun (_, bytes) -> receive peer bytes) (Sim_net.receive net ~now me);
      play update computer.flags peer (local_keyboard computer.keyboard me);
      let bytes = packet peer in
      for other = 0 to players - 1 do
        if other <> me then Sim_net.send net ~now ~src:me ~dst:other bytes
      done)
    peers

(*****************************************************************************)
(* Remote: me here, the other peer over a real network *)
(*****************************************************************************)

(* the same as a simulated peer's frame, the transport instead of
 * Sim_net; my keys are the arrows, whichever player I am *)
let remote_frame update (computer : computer) (transport : Transport.t) (peer : 'model peer) : unit =
  List.iter (receive peer) (transport.receive ());
  play update computer.flags peer (local_keyboard computer.keyboard 0);
  transport.send (packet peer)

(*****************************************************************************)
(* The views *)
(*****************************************************************************)

let text (color : color) (s : string) : shape = words color s |> scale 1.6

let agree (d : (int * int) option) : shape =
  match d with
  | None -> text green "the two games agree (checksums every second)"
  | Some (tick, peer) -> text red (Printf.sprintf "DESYNC at tick %d, with player %d" tick peer)

let netcode_line (netcode : string) (delay : int) : string =
  match netcode with
  | "rollback" -> Printf.sprintf "rollback, input delay %d" delay
  | "1997" -> "1997: send, then wait for the answer, every tick"
  | _ -> Printf.sprintf "lockstep, input delay %d ticks" delay

let hud (knobs : knobs) (peers : 'model peer array) : shape list =
  [ text white
      (Printf.sprintf "latency %d ms ([ ])   jitter %d ms   loss %d%% (- =)   %s (n)" knobs.latency knobs.jitter
         knobs.loss (netcode_line knobs.netcode knobs.delay))
    |> move_y (-440.);
    agree (Array.to_list peers |> List.find_map desync) |> move_y (-470.) ]

let remote_hud (transport : Transport.t) (peer : 'model peer) (me : int) (netcode : string) : shape list =
  [ text white (Printf.sprintf "%s -- you are player %d, %s, %s" (transport.status ()) me netcode (describe peer))
    |> scale 0.7 |> move_y (-440.);
    agree (desync peer) |> move_y (-470.) ]

let side_by_side view (computer : computer) (knobs : knobs) (peers : 'model peer array) : shape list =
  let n = Array.length peers in
  let width = 1000. /. float_of_int n in
  let halves =
    Array.to_list
      (Array.mapi
         (fun me peer ->
           let x = (-500.) +. (width *. (float_of_int me +. 0.5)) in
           [ group (view computer me (side_of peer).model) |> scale (1. /. float_of_int n) |> move_x x;
             text white (Printf.sprintf "computer %d: %s" me (describe peer)) |> scale 0.8 |> move x 300. ])
         peers)
  in
  (rectangle (rgb 40 40 40) 1000. 1000. :: List.concat halves) @ hud knobs peers

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* several views side by side, [scale]d to share the screen *)
let panels (views : shape list list) : shape list =
  let n = List.length views in
  let width = 1000. /. float_of_int n in
  List.concat
    (List.mapi
       (fun i shapes -> [ group shapes |> scale (1. /. float_of_int n) |> move_x ((-500.) +. (width *. (float_of_int i +. 0.5))) ])
       views)

let game ?(network : < Cap.network ; .. > option) ?(split = false) ~(players : int) view update (model : 'model) =
  let network = Option.map (fun caps -> (caps :> Cap.network)) network in
  let rec update_state (computer : computer) (state : 'model state) : 'model state =
    match state with
    (* the flags known at last: the mode chosen, and this frame's tick
     * played in it, as a one-player game would *)
    | Starting model -> update_state computer (start ~players ?network update computer.flags model)
    | Local side -> Local (one_tick update computer.flags side (Array.init players (local_keyboard computer.keyboard)))
    | Simulate s ->
        let knobs, held = turn_knobs computer.keyboard s.held s.knobs in
        if knobs.netcode <> s.knobs.netcode then
          (* the other netcode: the game again, from its start *)
          match simulate ~players update computer.flags knobs s.initial with
          | Simulate s' -> Simulate { s' with held }
          | other -> other
        else begin
          if knobs <> s.knobs then Sim_net.set_config s.net (config_of knobs);
          simulate_frame update computer s.net s.peers s.frame;
          Simulate { s with frame = s.frame + 1; knobs; held }
        end
    | Remote r ->
        remote_frame update computer r.transport r.peer;
        state
    | Failed _ -> state
  in
  let view_state (computer : computer) (state : 'model state) : shape list =
    match state with
    | Starting model -> view computer 0 model
    | Local side when split -> rectangle black 1000. 1000. :: panels (List.init players (fun n -> view computer n side.model))
    | Local side -> view computer 0 side.model
    | Simulate s -> side_by_side view computer s.knobs s.peers
    | Remote r -> view computer r.me (side_of r.peer).model @ remote_hud r.transport r.peer r.me r.netcode
    | Failed (why, model) -> view computer 0 model @ [ text red why |> move_y (-450.) ]
  in
  Playground.game view_state update_state (Starting model)
