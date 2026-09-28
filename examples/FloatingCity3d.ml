(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A tapering stack of decreasing-size boxes with a small cluster of
 * "buildings" and one tall banded accent tower on top -- an *original*
 * scene inspired by the general composition of lucamug's "Floating
 * City" image for elm-playground-3d (the hero image at the top of
 * https://github.com/lucamug/elm-playground-3d's README, referenced
 * from https://lucamug.medium.com/basic-3d-rendering-in-svg-elm-playground-3d-d1e8846cd06e).
 * No source code for that scene exists in the public repo (checked
 * examples/Examples.elm, which only has simple demo shapes); the
 * actual image/GIF were reviewed as visual reference (Medium blocked a
 * direct fetch and the image's original netlify hosting is gone, so
 * these were provided directly). This is a fresh interpretation, not a
 * port: fresh box counts/sizes/colors/layout throughout, and
 * deliberately skips the original's "Elm" text/logo detail on its
 * crane (this library can't render text in 3D yet anyway, and that
 * detail is specifically their own branding, not just a generic
 * compositional idea worth reusing). *)
open Playground
open Playground3d

(* claude: (width/depth, height, color) for each tier, bottom to top,
 * each one WIDER than the last -- an inverted taper (narrow point at
 * the bottom, widening up to the platform that holds the city), not a
 * standard pyramid, which is what actually gives this its "floating/
 * flying city" look (confirmed against the reference image: the
 * platform directly under the buildings is the widest slab, and each
 * step going down is narrower, ending in the thinnest sliver at the
 * very bottom -- I had this backwards at first). A single warm, pale
 * tone (our palette has no true cream/beige) -- letting the
 * directional-light flat shading (see notes_3d.md section 8) provide
 * the light-top/darker-side variation naturally, rather than
 * hand-alternating two colors that would fight with it. *)
let base_tiers = [ (2.2, 1.2, white); (3.2, 1.2, white); (4.5, 1.2, white); (6., 1.2, white); (8., 1.2, white) ]

let top_tier_width = 8.
let top_of_base = 1.2 *. float_of_int (List.length base_tiers)

(* a stack of (width/depth, height, color) tiers, bottom starting at
 * [y0], centered at [x, _, z] *)
let stack_tiers tiers x z y0 =
  let rec go tiers_left y_bottom =
    match tiers_left with
    | [] -> []
    | (width, height, color) :: rest ->
        let tier = box color width height width |> move3d x (y_bottom +. (height /. 2.)) z in
        tier :: go rest (y_bottom +. height)
  in
  group3d (go tiers y0)

let base = stack_tiers base_tiers 0. 0. 0.

(* claude: a fixed seed, not Random.self_init, so this example demo
 * looks the same every run (unlike e.g. StarCollector3d.ml,
 * where varying each run is the point) *)
let () = Random.init 42

let building_colors = [| orange; purple; lightPurple; green; yellow; blue; gray; lightYellow |]

let city =
  group3d
    (List.init 22 (fun i ->
         let half = top_tier_width /. 2. *. 0.8 in
         let bx = Random.float (2. *. half) -. half in
         let bz = Random.float (2. *. half) -. half in
         let height = 0.3 +. Random.float 2.2 in
         let width = 0.2 +. Random.float 0.3 in
         let color = building_colors.(i mod Array.length building_colors) in
         box color width height width |> move3d bx (top_of_base +. (height /. 2.)) bz))

(* claude: one taller, distinctively-colored tower (red with a white
 * band) as a focal point, off-center like the original's spire --
 * a generic tapered-tower silhouette, not a recreation of any specific
 * real landmark's actual structure (no lattice/truss detail, just
 * plain tapering boxes, same primitive as everything else here). *)
let accent_tower =
  stack_tiers [ (0.9, 1.2, red); (0.6, 1., red); (0.45, 0.25, white); (0.3, 1., red) ] 0.6 (-0.5) top_of_base

let scene = group3d [ base; city; accent_tower ]

let view (computer : Playground.computer) () =
  let angle = spin 30. computer.time in
  let radians = angle *. Float.pi /. 180. in
  let radius = 15. in
  (* claude: eye above top_of_base (the widest platform, at the top of
   * the inverted taper) so the camera looks slightly *down* at the
   * whole structure, showing the tapering steps from the side like the
   * reference image, rather than from below looking up at their
   * undersides *)
  let eye = (radius *. cos radians, top_of_base +. 3., radius *. sin radians) in
  let cam = camera ~eye ~target:(0., top_of_base /. 2., 0.) () in
  (cam, [ scene ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
