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

(* the keys that travel, a bit each: the first byte, and w a s d in a
 * second one, sent only when one of them is held -- so a game played
 * with the arrows sends the one byte it always did *)
let bits (k : keyboard) : (bool * int) list =
  [ (k.kup, 1); (k.kdown, 2); (k.kleft, 4); (k.kright, 8); (k.kspace, 16); (k.kenter, 32); (k.kshift, 64) ]

let bits2 (k : keyboard) : (bool * int) list = [ (k.kw, 1); (k.ks, 2); (k.ka, 4); (k.kd, 8) ]

let byte (bits : (bool * int) list) : int = List.fold_left (fun acc (held, bit) -> if held then acc lor bit else acc) 0 bits

let encode (k : keyboard) : string =
  let b2 = byte (bits2 k) in
  String.make 1 (Char.chr (byte (bits k))) ^ if b2 = 0 then "" else String.make 1 (Char.chr b2)

(* the keys by name too ([keyboard.keys]), for a game that asks
 * Set_.mem "w" *)
let decode (s : string) : keyboard =
  let b = if s = "" then 0 else Char.code s.[0] and b2 = if String.length s < 2 then 0 else Char.code s.[1] in
  let on bit = b land bit <> 0 and on2 bit = b2 land bit <> 0 in
  let k =
    { empty with kup = on 1; kdown = on 2; kleft = on 4; kright = on 8; kspace = on 16; kenter = on 32; kshift = on 64;
      kw = on2 1; ks = on2 2; ka = on2 4; kd = on2 8 }
  in
  let names =
    [ (k.kup, "ArrowUp"); (k.kdown, "ArrowDown"); (k.kleft, "ArrowLeft"); (k.kright, "ArrowRight"); (k.kspace, "space");
      (k.kenter, "Enter"); (k.kshift, "Shift"); (k.kw, "w"); (k.ks, "s"); (k.ka, "a"); (k.kd, "d") ]
  in
  { k with keys = List.fold_left (fun keys (held, name) -> if held then Set_.add name keys else keys) k.keys names }

(* held now and not before *)
let rising (now : keyboard) (before : keyboard) : keyboard =
  decode (encode { empty with
    kup = now.kup && not before.kup; kdown = now.kdown && not before.kdown;
    kleft = now.kleft && not before.kleft; kright = now.kright && not before.kright;
    kspace = now.kspace && not before.kspace; kenter = now.kenter && not before.kenter;
    kshift = now.kshift && not before.kshift;
    kw = now.kw && not before.kw; ks = now.ks && not before.ks; ka = now.ka && not before.ka; kd = now.kd && not before.kd })

(* one physical keyboard, shared: player 0 the arrows (and space, enter,
 * shift), player 1 w a s d (and q for its space), the others nothing *)
let local_keyboard (physical : keyboard) (n : int) : keyboard =
  match n with
  | 0 -> decode (encode { physical with kw = false; ks = false; ka = false; kd = false })
  | 1 ->
      decode
        (encode
           { empty with kup = physical.kw; kdown = physical.ks; kleft = physical.ka; kright = physical.kd;
             kspace = Set_.mem "q" physical.keys })
  | _ -> empty

(* the whole keyboard, mine: over a real network, I am alone at it *)
let own_keyboard (physical : keyboard) : keyboard = decode (encode physical)

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
 * missing ones and fixes its game when they arrive (Rollback.mli); a
 * client of a server predicts its own game, the server's word
 * correcting it (Snapshot.mli, Prediction.mli) *)
type 'model peer =
  | Waiting of Lockstep.t * 'model side ref
  | Guessing of 'model side Rollback.t
  | Predicting of Snapshot.Client.t * 'model side Prediction.t

(* the server of netcode=server: the game itself, everyone's inputs in *)
type 'model host = { inputs : Snapshot.Server.t; mutable world : 'model side }

(* the world a snapshot carries: Marshal's bytes, between copies of the
 * same program only (Snapshot.mli) -- fine through Sim_net *)
let world_bytes (s : 'model side) : string = Marshal.to_string s []
let world_of (bytes : string) : 'model side = Marshal.from_string bytes 0

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
  | "server" ->
      Predicting
        ( Snapshot.Client.create ~me ~players,
          Prediction.create ~me ~players ~update:(fun inputs s -> one_tick update flags s (Array.map decode inputs)) side )
  | _ -> Waiting (Lockstep.create ~me ~players ~delay, ref side)

let receive (peer : 'model peer) (bytes : string) : unit =
  match peer with
  | Waiting (l, _) -> Lockstep.receive l bytes
  | Guessing r -> Rollback.receive r bytes
  | Predicting (c, p) -> (
      match Snapshot.Client.receive c bytes with
      | Some s -> Prediction.correct p ~world:(world_of s.world) ~acked:s.acked ~latest:s.latest
      | None -> ())

let packet (peer : 'model peer) : string =
  match peer with Waiting (l, _) -> Lockstep.packet l | Guessing r -> Rollback.packet r | Predicting (c, _) -> Snapshot.Client.packet c

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
  | Predicting (c, p) ->
      let seq = Snapshot.Client.record c (encode keys) in
      Prediction.step p ~seq (encode keys)

let side_of (peer : 'model peer) : 'model side =
  match peer with Waiting (_, s) -> !s | Guessing r -> Rollback.model r | Predicting (_, p) -> Prediction.model p

(* no desync with a server: there is one game, its *)
let desync (peer : 'model peer) =
  match peer with Waiting (l, _) -> Lockstep.desync l | Guessing r -> Rollback.desync r | Predicting _ -> None

let describe (peer : 'model peer) : string =
  match peer with
  | Waiting (l, s) -> Printf.sprintf "tick %d, %d stalls" !s.tick (Lockstep.stats l).stalls
  | Guessing r ->
      let st = Rollback.stats r in
      Printf.sprintf "tick %d, %d rollbacks, %d ticks replayed, %d stalls" (Rollback.tick r) st.rollbacks st.replayed st.stalls
  | Predicting (_, p) -> Printf.sprintf "tick %d, %d mispredictions" (Prediction.model p).tick (Prediction.mispredictions p)

(*****************************************************************************)
(* The modes *)
(*****************************************************************************)

type knobs = { latency : int (* ms *); jitter : int; loss : int (* % *); delay : int; netcode : string }

type 'model state =
  | Starting of 'model
  | Local of 'model side
  (* connected, waiting to know which player I am (a relay says so);
   * the packets arrived meanwhile, kept *)
  | Connecting of { transport : Transport.t; model : 'model; netcode : string; delay : int; early : string list }
  (* one peer, me, and the other ones over a real network *)
  | Remote of { transport : Transport.t; peer : 'model peer; me : int; netcode : string }
  (* the network couldn't be opened: why, shown on the screen *)
  | Failed of string * 'model
  | Simulate of {
      net : Sim_net.t;
      peers : 'model peer array;
      frame : int;
      knobs : knobs;
      held : string list; (* the knob keys held, for their rising edge *)
      server : 'model host option; (* netcode=server: the server, Sim_net's peer [players] *)
      initial : 'model; (* to start again with the other netcode *)
    }

let config_of (k : knobs) : Sim_net.config =
  { latency = float_of_int k.latency /. 1000.; jitter = float_of_int k.jitter /. 1000.; loss = float_of_int k.loss /. 100.; duplication = 0. }

let int_flag (flags : flags) (name : string) (default : int) : int =
  Option.value (Option.bind (List.assoc_opt name flags) int_of_string_opt) ~default

(* netcode=lockstep (the default), rollback, or 1997; the input delay
 * 3 ticks for lockstep, none for the others, unless delay= *)
let netcode_of (flags : flags) : string * int =
  let netcode = match List.assoc_opt "netcode" flags with Some (("rollback" | "1997" | "server") as n) -> n | _ -> "lockstep" in
  (netcode, if netcode = "1997" || netcode = "server" then 0 else int_flag flags "delay" (if netcode = "rollback" then 0 else 3))

let simulate ~players update (flags : flags) (knobs : knobs) (model : 'model) : 'model state =
  let side = { model; last = Array.make players empty; tick = 0 } in
  Simulate
    {
      net = Sim_net.create ~seed:(int_flag flags "seed" 1) (config_of knobs);
      peers = Array.init players (fun me -> new_peer ~netcode:knobs.netcode ~me ~players ~delay:knobs.delay update flags side);
      frame = 0;
      knobs;
      held = [];
      server = (if knobs.netcode = "server" then Some { inputs = Snapshot.Server.create ~players; world = side } else None);
      initial = model;
    }

let start ~players ?(network : Cap.network option) update (flags : flags) (model : 'model) : 'model state =
  let side = { model; last = Array.make players empty; tick = 0 } in
  let netcode, delay = netcode_of flags in
  match List.assoc_opt "net" flags with
  | Some "simulate" ->
      let knobs = { latency = int_flag flags "latency" 50; jitter = int_flag flags "jitter" 10; loss = int_flag flags "loss" 5; delay; netcode } in
      simulate ~players update flags knobs model
  | Some (("host" | "join" | "relay") as net) -> (
      let flag name default = Option.value (List.assoc_opt name flags) ~default in
      let role : Transport.role =
        match net with
        | "host" -> Host { bind = flag "bind" "127.0.0.1"; port = int_flag flags "port" 7777 }
        | "join" -> Join { host = flag "host" "127.0.0.1"; port = int_flag flags "port" 7777 }
        | _ -> Relay { host = flag "host" "127.0.0.1"; port = int_flag flags "port" 8765 }
      in
      ignore side;
      match network with
      | _ when netcode = "server" -> Failed ("netcode=server: in net=simulate only, so far", model)
      | None -> Failed (Printf.sprintf "net=%s: this program wasn't granted the network (Cap.network)" net, model)
      | Some caps -> (
          match Transport.connect caps role with
          | Ok transport -> Connecting { transport; model; netcode; delay; early = [] }
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
      | "1997" -> { k with netcode = "server"; delay = 0 }
      | _ -> { k with netcode = "lockstep"; delay = 3 }
    else k
  in
  (k, now)

(* one frame of every peer: its packets read, its tick played (or not:
 * a stall), its packet sent *)
let simulate_frame update (computer : computer) net (peers : 'model peer array) (server : 'model host option) frame =
  let now = float_of_int frame /. 60. in
  let players = Array.length peers in
  (* the server, if any: everyone's inputs in, a tick of the game, the
   * world to each client every 3 ticks (20 a second) *)
  Option.iter
    (fun h ->
      List.iter (fun (_, bytes) -> Snapshot.Server.receive h.inputs bytes) (Sim_net.receive net ~now players);
      h.world <- one_tick update computer.flags h.world (Array.map decode (Snapshot.Server.inputs h.inputs));
      if frame mod 3 = 0 then
        for p = 0 to players - 1 do
          Sim_net.send net ~now ~src:players ~dst:p (Snapshot.Server.packet h.inputs ~tick:frame ~world:(world_bytes h.world) p)
        done)
    server;
  Array.iteri
    (fun me peer ->
      List.iter (fun (_, bytes) -> receive peer bytes) (Sim_net.receive net ~now me);
      play update computer.flags peer (local_keyboard computer.keyboard me);
      let bytes = packet peer in
      (* to the server, or to every other peer *)
      if server <> None then Sim_net.send net ~now ~src:me ~dst:players bytes
      else
        for other = 0 to players - 1 do
          if other <> me then Sim_net.send net ~now ~src:me ~dst:other bytes
        done)
    peers

(*****************************************************************************)
(* Remote: me here, the other peer over a real network *)
(*****************************************************************************)

(* the same as a simulated peer's frame, the transport instead of
 * Sim_net; my keys are the arrows (and w a s d), whichever player I am *)
let remote_frame update (computer : computer) (transport : Transport.t) (peer : 'model peer) : unit =
  List.iter (receive peer) (transport.receive ());
  play update computer.flags peer (own_keyboard computer.keyboard);
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
  | "server" -> "a server owns the game, the clients predict"
  | _ -> Printf.sprintf "lockstep, input delay %d ticks" delay

let hud (knobs : knobs) (peers : 'model peer array) : shape list =
  [ text white
      (Printf.sprintf "latency %d ms ([ ])   jitter %d ms   loss %d%% (- =)   %s (n)" knobs.latency knobs.jitter
         knobs.loss (netcode_line knobs.netcode knobs.delay))
    |> move_y (-440.);
    (if knobs.netcode = "server" then text green "one game, the server's: nothing to disagree about"
     else agree (Array.to_list peers |> List.find_map desync))
    |> move_y (-470.) ]

let remote_hud (transport : Transport.t) (peer : 'model peer) (me : int) (netcode : string) : shape list =
  [ text white (Printf.sprintf "%s -- you are player %d, %s, %s" (transport.status ()) me netcode (describe peer))
    |> scale 0.7 |> move_y (-440.);
    agree (desync peer) |> move_y (-470.) ]

(* What a mode shows, whatever draws it (here in 2D, Multiplayer3d in
 * 3D): whose game, and the lines of the network's state *)
type 'model screen = { player : int; model : 'model; label : string option }

type 'model layout = {
  screens : 'model screen list;
  background : color option;
  columns : bool;
  status : shape list;
}

let one (player : int) (model : 'model) (status : shape list) : 'model layout =
  { screens = [ { player; model; label = None } ]; background = None; columns = false; status }

let layout ~(split : bool) ~(players : int) (state : 'model state) : 'model layout =
  match state with
  | Starting model -> one 0 model []
  | Local side when split ->
      { screens = List.init players (fun player -> { player; model = side.model; label = None }); background = Some black;
        columns = false; status = [] }
  | Local side -> one 0 side.model []
  | Simulate s ->
      (* each computer's screen; the server's, the truth, in the middle *)
      let clients =
        Array.to_list
          (Array.mapi
             (fun me peer -> { player = me; model = (side_of peer).model; label = Some (Printf.sprintf "computer %d: %s" me (describe peer)) })
             s.peers)
      in
      let screens =
        match s.server with
        | None -> clients
        | Some h ->
            let half = (List.length clients + 1) / 2 in
            List.filteri (fun i _ -> i < half) clients
            @ [ { player = 0; model = h.world.model; label = Some (Printf.sprintf "the server: tick %d, the game itself" h.world.tick) } ]
            @ List.filteri (fun i _ -> i >= half) clients
      in
      { screens; background = Some (rgb 40 40 40); columns = true; status = hud s.knobs s.peers }
  | Connecting c -> one 0 c.model [ text white (c.transport.status ()) |> scale 0.7 |> move_y (-440.) ]
  | Remote r -> one r.me (side_of r.peer).model (remote_hud r.transport r.peer r.me r.netcode)
  | Failed (why, model) -> one 0 model [ text red why |> move_y (-450.) ]

(* the layout in 2D: one screen as it is, several side by side, each
 * [scale]d to share the screen *)
let draw view (computer : computer) (l : 'model layout) : shape list =
  match (l.screens, l.background) with
  | [ { player; model; label = None } ], None -> view computer player model @ l.status
  | screens, background ->
      let n = List.length screens in
      let width = 1000. /. float_of_int n in
      let shown =
        List.mapi
          (fun i (sc : 'model screen) ->
            let x = (-500.) +. (width *. (float_of_int i +. 0.5)) in
            (group (view computer sc.player sc.model) |> scale (1. /. float_of_int n) |> move_x x)
            :: (match sc.label with Some label -> [ text white label |> scale (1.6 /. float_of_int n) |> move x 300. ] | None -> []))
          screens
      in
      (match background with Some c -> [ rectangle c 1000. 1000. ] | None -> []) @ List.concat shown @ l.status

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let initial (model : 'model) : 'model state = Starting model

let update_state ?(network : Cap.network option) ~(players : int) update : computer -> 'model state -> 'model state =
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
          simulate_frame update computer s.net s.peers s.server s.frame;
          Simulate { s with frame = s.frame + 1; knobs; held }
        end
    | Connecting c -> (
        let early = c.early @ c.transport.receive () in
        match c.transport.player () with
        | None -> Connecting { c with early }
        | Some me ->
            let side = { model = c.model; last = Array.make players empty; tick = 0 } in
            let peer = new_peer ~netcode:c.netcode ~me ~players ~delay:c.delay update computer.flags side in
            List.iter (receive peer) early;
            Remote { transport = c.transport; peer; me; netcode = c.netcode })
    | Remote r ->
        remote_frame update computer r.transport r.peer;
        state
    | Failed _ -> state
  in
  update_state

let game ?(network : < Cap.network ; .. > option) ?(split = false) ~(players : int) view update (model : 'model) =
  let network = Option.map (fun caps -> (caps :> Cap.network)) network in
  Playground.game
    (fun computer state -> draw view computer (layout ~split ~players state))
    (update_state ?network ~players update)
    (initial model)
