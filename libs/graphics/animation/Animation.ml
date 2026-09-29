(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Animation.mli *)

type 'a t = { from : 'a; to_ : 'a; start : float; duration : float; timing : Timing.t }

let make ?(timing = Timing.default) ~(duration : float) ~(now : float) (from : 'a) (to_ : 'a) : 'a t = { from; to_; start = now; duration; timing }
let still (v : 'a) : 'a t = { from = v; to_ = v; start = 0.; duration = 0.; timing = Timing.linear }

let progress (a : 'a t) ~(now : float) : float =
  if a.duration <= 0. then 1. else Timing.at a.timing ((now -. a.start) /. a.duration)

let finished (a : 'a t) ~(now : float) : bool = a.duration <= 0. || now >= a.start +. a.duration

let value ~(lerp : 'a -> 'a -> float -> 'a) (a : 'a t) ~(now : float) : 'a =
  if finished a ~now then a.to_ else lerp a.from a.to_ (progress a ~now)

let lerp_float (a : float) (b : float) (p : float) : float = a +. ((b -. a) *. p)

type ('k, 'a) keyed = { timing : Timing.t; duration : float; lerp : 'a -> 'a -> float -> 'a; anims : ('k * 'a t) list }

let keyed ?(timing = Timing.default) ~(duration : float) ~(lerp : 'a -> 'a -> float -> 'a) () : ('k, 'a) keyed = { timing; duration; lerp; anims = [] }

let set (k : ('k, 'a) keyed) ~(now : float) (key : 'k) (target : 'a) : ('k, 'a) keyed =
  match List.assoc_opt key k.anims with
  | None -> { k with anims = (key, still target) :: k.anims }
  | Some a when a.to_ = target -> k
  | Some a ->
      (* from where it is now: an interruption without a jump *)
      let here = value ~lerp:k.lerp a ~now in
      { k with anims = (key, make ~timing:k.timing ~duration:k.duration ~now here target) :: List.remove_assoc key k.anims }

let value_of (k : ('k, 'a) keyed) ~(now : float) (key : 'k) : 'a option =
  Option.map (fun a -> value ~lerp:k.lerp a ~now) (List.assoc_opt key k.anims)

let moving (k : ('k, 'a) keyed) ~(now : float) : bool = List.exists (fun (_, a) -> not (finished a ~now)) k.anims
