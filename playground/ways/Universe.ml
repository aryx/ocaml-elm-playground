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

(* See Universe.mli *)

type 'w package = 'w * string list

type connection = Unopened | Open of Transport.t | Refused of string

type 'w world = {
  w : 'w;
  connection : connection;
  outbox : string list; (* sent before the universe answered *)
  keys_before : keyboard;
  mouse_before : mouse;
  frames : int;
  stopped : bool;
}

let big_bang (init : 'w) ~(to_draw : 'w -> Bigbang.image) ?(on_tick : ('w -> 'w package) option) ?(tick_rate = 1. /. 60.)
    ?(on_key : ('w -> string -> 'w package) option) ?(on_mouse : ('w -> number -> number -> string -> 'w package) option)
    ?(on_receive : ('w -> string -> 'w package) option) ?(stop_when : ('w -> bool) option) ?(register = "localhost")
    ?(port = 4567) ~(network : < Cap.network ; .. >) () : ('w world game, msg) app =
  let network = (network :> Cap.network) in
  (* a handler's package: its world, its messages added to the outbox *)
  let apply (world : 'w world) ((w, out) : 'w package) : 'w world = { world with w; outbox = world.outbox @ out } in
  let view (_ : computer) (world : 'w world) : shape list =
    let status =
      match world.connection with
      | Open t when t.player () <> None -> []
      | Open t -> [ words gray (t.status ()) |> move_y (-480.) ]
      | Refused why -> [ words red why |> move_y (-480.) ]
      | Unopened -> []
    in
    Bigbang.to_shape (to_draw world.w) :: status
  in
  let update (computer : computer) (world : 'w world) : 'w world =
    (* the universe, reached at the first frame (the flags known) *)
    let world =
      match world.connection with
      | Unopened ->
          let host = Option.value (List.assoc_opt "host" computer.flags) ~default:register in
          let port = Option.value (Option.bind (List.assoc_opt "port" computer.flags) int_of_string_opt) ~default:port in
          let connection = match Transport.connect network (Relay { host; port }) with Ok t -> Open t | Error why -> Refused why in
          { world with connection }
      | _ -> world
    in
    if world.stopped then world
    else
      (* the mail first, then the events since the last frame, then a
       * tick if one is due -- as Bigbang's *)
      let received = match world.connection with Open t -> t.receive () | _ -> [] in
      let world = match on_receive with Some f -> List.fold_left (fun world m -> apply world (f world.w m)) world received | None -> world in
      let pressed, _ = Bigbang.key_events world.keys_before computer.keyboard in
      let world = match on_key with Some f -> List.fold_left (fun world k -> apply world (f world.w k)) world pressed | None -> world in
      let world =
        match (on_mouse, Bigbang.mouse_event world.mouse_before computer.mouse) with
        | Some f, Some e ->
            let scene = to_draw world.w in
            apply world
              (f world.w (computer.mouse.mx +. (Bigbang.width scene /. 2.)) ((Bigbang.height scene /. 2.) -. computer.mouse.my) e)
        | _ -> world
      in
      let every = max 1 (int_of_float (Float.round (tick_rate *. 60.))) in
      let world = match on_tick with Some f when (world.frames + 1) mod every = 0 -> apply world (f world.w) | _ -> world in
      (* the outbox, once the universe has answered *)
      let outbox =
        match world.connection with
        | Open t when t.player () <> None ->
            List.iter t.send world.outbox;
            []
        | _ -> world.outbox
      in
      let stopped = match stop_when with Some f -> f world.w | None -> false in
      { world with outbox; keys_before = computer.keyboard; mouse_before = computer.mouse; frames = world.frames + 1; stopped }
  in
  game view update
    { w = init; connection = Unopened; outbox = []; keys_before = initial_computer.keyboard; mouse_before = initial_computer.mouse;
      frames = 0; stopped = false }
