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
open Playground3d

(* See Multiplayer3d.mli *)

(* the strip of the network's lines, along the bottom: 8% of the window *)
let strip = 0.08

(* Multiplayer's lines are laid out for the bottom of a 1000 x 1000
 * screen, at y -440 and -470: moved up by 455, they are the strip's,
 * around its middle *)
let status_view (screen : screen) (lines : shape list) : view =
  let area = { x = 0.; y = 0.; w = 1.; h = strip } in
  (* a view's HUD is not clipped to its area: the background as big as
   * the strip, not bigger *)
  let own = area_screen screen area in
  { camera = camera ~eye:(0., 0., 1.) ~target:(0., 0., 0.) ();
    area;
    shapes = List.map hud (rectangle (rgb 40 40 40) own.width own.height :: List.map (move_y 455.) lines) }

(* the areas of [n] screens, above the strip if any: columns side by
 * side (net=simulate), or Playground3d.split's *)
let areas ~(columns : bool) ~(bottom : number) (n : int) : area list =
  let above (a : area) = { a with y = bottom +. (a.y *. (1. -. bottom)); h = a.h *. (1. -. bottom) } in
  if columns then List.init n (fun i -> above { x = float_of_int i /. float_of_int n; y = 0.; w = 1. /. float_of_int n; h = 1. })
  else List.map above (split n)

let views view (computer : computer) (l : 'model Multiplayer.layout) : view list =
  let bottom = if l.status = [] then 0. else strip in
  let screens = List.combine l.screens (areas ~columns:l.columns ~bottom (List.length l.screens)) in
  List.map
    (fun ((sc : 'model Multiplayer.screen), area) ->
      let screen = area_screen computer.screen area in
      let camera, shapes = view { computer with screen } sc.player sc.model in
      let label =
        match sc.label with
        (* at the bottom: a game's HUD is at its top, mostly *)
        | Some label -> [ hud (words white label |> scale 1.5 |> move_y (screen.bottom +. 15.)) ]
        | None -> []
      in
      { camera; area; shapes = shapes @ label })
    screens
  @ if l.status = [] then [] else [ status_view computer.screen l.status ]

let game3d ?(network : < Cap.network ; .. > option) ?(split = false) ~(players : int) view update (model : 'model) =
  let network = Option.map (fun caps -> (caps :> Cap.network)) network in
  split3d
    (fun computer state -> views view computer (Multiplayer.layout ~split ~players state))
    (Multiplayer.update_state ?network ~players update)
    (Multiplayer.initial model)
