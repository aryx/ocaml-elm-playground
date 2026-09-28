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

let input (computer : computer) ~dx ~dy : Widget.input =
  let i = Gui.input computer in
  { i with mx = i.mx -. dx; my = i.my -. dy }

let neutral : Widget.input = { Widget.no_input with mx = 1e9; my = 1e9 }

type selector_look = Rotary | Stepped_knob

let box (x, y) (w, h) : Widget.box = { Widget.x; y; w; h }

let control (ui : Immediate.t) ~selector ~at (c : Control.t) (v : float) : Immediate.t * float =
  let th = Immediate.theme ui in
  match c with
  | Knob (from, to_) -> Immediate.knob ui (box at (Immediate.knob_size th)) ~from ~to_ v
  | Switch ->
      let ui, on = Immediate.rocker ui (box at (Immediate.rocker_size th)) (v >= 0.5) in
      (ui, if on then 1. else 0.)
  | Selector labels -> (
      match selector with
      | Rotary ->
          let ui, i = Immediate.selector ui (box at (Immediate.selector_size th labels)) labels (int_of_float v) in
          (ui, float_of_int i)
      | Stepped_knob ->
          let ui, x = Immediate.knob ui (box at (Immediate.knob_size th)) ~from:0. ~to_:(float_of_int (List.length labels - 1)) v in
          (ui, Float.round x))

let shapes (ui : Immediate.t) ~dx ~dy : shape list = List.map (move dx dy) (Gui.shapes (Immediate.paint ui))
