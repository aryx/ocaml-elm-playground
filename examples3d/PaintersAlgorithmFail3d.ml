(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Two long boxes, one horizontal, one tilted so it crosses through the
 * first one -- designed specifically to make painter's algorithm's
 * depth sort visibly fail (toggle with "z" while running; the default
 * is the z-buffer, which handles this scene correctly). See
 * docs/claude_notes/notes_3d.md sections 6 and 11.
 *
 * Why this scene (unlike Cubes3d.ml's grid of separate, same-size,
 * non-overlapping cubes) actually breaks painter's algorithm: the two
 * boxes genuinely intersect where they cross. Right at that crossing,
 * *part* of the horizontal box is in front of the tilted one, and
 * *another part of that same face* is behind it (because the tilted
 * box dips below on one side of the crossing and rises above it on the
 * other). render_shape3d's painter's-algorithm mode sorts and draws
 * one whole face at a time -- it has to pick a single "this face is
 * entirely in front, or entirely behind" answer for each face, which
 * cannot be correct across the whole crossing region at once. The
 * z-buffer needs no such single answer, since it resolves visibility
 * independently at every pixel.
 *
 * This scene is also a good place to see "b" (backface culling) do
 * something, unlike e.g. Cubes3d.ml: on its own, "b" never changes
 * what a *filled* render looks like (the z-buffer already guarantees a
 * solid's front face wins over its own back face, culled or not -- see
 * notes_3d.md section 11), so toggling it alone here won't look any
 * different either. But press "f" first (wireframe) and *then* toggle
 * "b": wireframe has no depth test of any kind, so with culling off
 * you'll see extra lines from each box's hidden/inside faces (visible
 * right through the crossing, and around the far side of each box)
 * that culling normally removes before they're ever drawn. *)
open Playground
open Playground3d

let view (computer : Playground.computer) () =
  let angle = spin 20. computer.time in
  let box_a = box red 6. 0.4 1.2 in
  let box_b = box blue 1.2 0.4 6. |> rotate3d 25. 0. 0. in
  let scene = group3d [ box_a; box_b ] |> rotate3d 0. angle 0. in
  let cam = camera ~eye:(0., 5., 9.) ~target:(0., 0., 0.) () in
  (cam, [ scene ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
