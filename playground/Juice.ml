(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Juice.mli *)

open Playground

(* the flag juice=off: every effect does nothing *)
let off (computer : computer) : bool = List.assoc_opt "juice" computer.flags = Some "off"

(*****************************************************************************)
(* Effects as functions of time *)
(*****************************************************************************)

type ease = Ease.t

let linear = Ease.linear

let in_quad = Ease.quad
let out_quad = Ease.out Ease.quad
let in_out_quad = Ease.in_out Ease.quad

let in_cubic = Ease.cubic
let out_cubic = Ease.out Ease.cubic
let in_out_cubic = Ease.in_out Ease.cubic

let in_sine = Ease.sine
let out_sine = Ease.out Ease.sine
let in_out_sine = Ease.in_out Ease.sine

let in_back = Ease.back
let out_back = Ease.out Ease.back
let in_out_back = Ease.in_out Ease.back

let in_elastic = Ease.elastic
let out_elastic = Ease.out Ease.elastic
let in_out_elastic = Ease.in_out Ease.elastic

let in_bounce = Ease.bounce
let out_bounce = Ease.out Ease.bounce
let in_out_bounce = Ease.in_out Ease.bounce

let curve (ease : ease) (t : number) : number = ease t

let tween (ease : ease) (from : number) (to_ : number) (seconds : number) (Time started : time) (computer : computer) :
    number =
  if off computer then to_
  else
    let (Time now) = computer.time in
    Tween.value ease from to_ ~start:started ~duration:seconds now
