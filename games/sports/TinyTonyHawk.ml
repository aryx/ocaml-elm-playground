(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Tony Hawk's Pro Skater (Neversoft, Activision,
 * 1999, on the PlayStation): skateboarding on one half-pipe, seen
 * from the side, for two minutes.
 *
 *   down          pump (in a curve, going down)
 *   left right    push (on the flat); spin (in the air); balance
 *   x             flip trick (with up, down: another one)
 *   z             grab trick, held (with up, down: another one)
 *   c             held at the lip: a lip trick
 *   v             just after landing: revert, the combo goes on
 *   up            on the flat: manual, the combo goes on
 *   space         a new run, when the time is up
 *
 * The game came out the year its skater landed the first 900 (two
 * and a half turns) at the X Games, and made skateboarding a genre.
 * What it brought is the combo: tricks chained without touching the
 * ground between them count together, the chain's points multiplied
 * by its length, so that a long chain is worth far more than its
 * tricks one by one. The sequels made the chains longer: the manual
 * (THPS2, 2000), riding on two wheels, linked the tricks landed on
 * the flat; the revert (THPS3, 2001), a quick turn as the skater
 * lands on a ramp, linked the airs; and Neversoft kept adding links
 * (the spine transfer, the wall plant) for ten years. (Names and
 * dates from memory, to check.)
 *
 * The rules of the chain ([bank], [trick_value]):
 *
 *   combo score = (sum of its tricks' values) x (number of tricks)
 *
 * where a trick already in the chain is worth half as much each time
 * it comes back, so that the chain pays for variety as much as for
 * length; a bail (a bad landing, a lost balance) loses the chain.
 * The chain ends, and is banked, when the skater lands without a
 * revert or rolls on the flat without a manual.
 *
 * The half-pipe is a curve, and the skater a point on it
 * ([pos], [slope]): its position is the distance [s] along the ramp,
 * its speed [v] the speed along it, and gravity pulls along the slope
 * (dv = -g sin(slope)), which is all a skateboard's physics is, the
 * wheels keeping it on the ramp:
 *
 *         lip |<-- vert                 vert -->| lip
 *             |                                  |
 *              \  curve (radius r)     curve    /
 *                `-._                     _.-'
 *                     `-----  flat -----'
 *            s: -s_lip        0           s_lip
 *
 * At a lip the ramp is vertical, so the skater leaving it goes
 * straight up and comes back down onto the same lip: in the air only
 * the height and the board's angle are left. The landing is judged by
 * that angle: the board must be along the wall again (either way up,
 * fakie counts), within [land_tolerance]. Energy is kept by gravity,
 * lost by friction, and gained by pumping: pressing down in a curve,
 * going down (the skater, crouching and standing up at the right
 * moment, raises their centre of mass where the ramp turns; here
 * just a push along the ramp).
 *
 * A simplification of the side view: THPS's spins turn around the
 * vertical axis, which a side view does not show, so ours turn in the
 * screen's plane (they are flips of the whole body, counted in half
 * turns, "180", "360", ...); and a grind along the coping runs across
 * the screen, so ours is the lip trick, a stall on the coping.
 *
 * The balance of a manual or a lip trick ([balance]): a needle that
 * falls away from the middle faster the further it is from it, and
 * the longer the trick lasts, and the player pushes it back. No dice:
 * the first push is left or right with the chain's length.
 *
 * What it uses: Scene2d (keys pressed). Not Physics: a point on a
 * curve, parameterized by its length, is a few lines (a train on its
 * track), where a body on a polygon would bounce off its corners.
 *
 * Left undone, exercises: a second ramp or a rail, for a real grind
 * and a transfer between them; the special meter (THPS's, filled by
 * tricks, giving access to the special tricks); goals for the run
 * (a high score, letters S K A T E to collect); the 900, a two and
 * a half turns spin that lands only in fakie... or not, since ours
 * counts both ways.
 *)
open Playground

(*****************************************************************************)
(* The half-pipe *)
(*****************************************************************************)

let flat = 150. (* half the flat bottom *)
let r = 150. (* the curves' radius *)
let vert = 40. (* the vertical part above the curves *)
let s_curve = flat +. (Float.pi *. r /. 2.) (* where a curve ends *)
let s_lip = s_curve +. vert
let lip_height = r +. vert
let ground = -250. (* the flat's y on the screen *)

let gravity = 0.3
let friction = 0.9997
let pump = 0.05
let push = 0.1
let max_speed = 15.
let spin_speed = 6. (* degrees per frame *)
let land_tolerance = 25. (* degrees *)

let sign (x : float) : float = if x < 0. then -1. else 1.

(* The slope at [s], in radians: 0 on the flat, pi/2 on the verts,
 * unsigned (the same on both sides). *)
let slope (s : float) : float =
  let a = Float.abs s in
  if a <= flat then 0. else if a <= s_curve then (a -. flat) /. r else Float.pi /. 2.

(* the point of the ramp at [s], (0, 0) the middle of the flat *)
let pos (s : float) : float * float =
  let a = Float.abs s and sg = sign s in
  if a <= flat then (s, 0.)
  else if a <= s_curve then
    let t = slope s in
    (sg *. (flat +. (r *. Float.sin t)), r *. (1. -. Float.cos t))
  else (sg *. (flat +. r), r +. (a -. s_curve))

(* the direction of the ramp at [s], going towards s increasing, in
 * degrees: the board's angle when riding *)
let tangent (s : float) : float = sign s *. slope s *. 180. /. Float.pi

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type trick = { name : string; points : int }

(* a manual's or a lip trick's balance *)
type balance = { needle : float; (* -1 to 1: fallen *) speed : float; frames : int }

type air = {
  side : float; (* the lip it left, -1. or 1. *)
  height : float; (* above the lip *)
  vy : float;
  angle : float; (* the board's, in degrees *)
  spun : float; (* degrees turned, signed *)
  flip : (string * int) option; (* the flip under way, and its frames left *)
  grab : (string * int) option; (* the grab held, and its frames *)
}

type skater =
  | Rolling
  | Manual of balance
  | Lip of balance
  | Air of air
  | Bailed of int (* frames lying on the ramp *)

type run = {
  s : float;
  v : float;
  skater : skater;
  combo : trick list; (* the chain under way, newest first *)
  landed : int; (* frames left to revert after a landing, 0 when none *)
  flat_frames : int; (* on the flat, with a chain and no manual *)
  score : int;
  best : int; (* the best chain *)
  time : int; (* frames left in the run *)
  message : string * int; (* and its frames left *)
}

let run_frames = 120 * 60

let start : run =
  { s = -.(s_lip -. 1.); v = 0.; skater = Rolling; combo = []; landed = 0; flat_frames = 0; score = 0; best = 0;
    time = run_frames; message = ("drop in!", 90) }

(*****************************************************************************)
(* The chain *)
(*****************************************************************************)

(* a trick's value in the chain: halved for each time it is there already *)
let trick_value (combo : trick list) (t : trick) : int =
  let seen = List.length (List.filter (fun (t' : trick) -> t'.name = t.name) combo) in
  t.points / (1 lsl seen)

(* the chain's sum, each trick valued against the ones before it *)
let combo_sum (combo : trick list) : int =
  let rec go = function [] -> 0 | t :: older -> trick_value older t + go older in
  go combo

let combo_score (combo : trick list) : int = combo_sum combo * List.length combo

let add (t : trick) (g : run) : run = { g with combo = t :: g.combo }

(* the chain ends well *)
let bank (g : run) : run =
  if g.combo = [] then g
  else
    let n = combo_score g.combo in
    { g with score = g.score + n; best = max g.best n; combo = []; landed = 0; flat_frames = 0;
             message = (Printf.sprintf "+%d" n, 60) }

let bail (g : run) : run =
  { g with skater = Bailed 60; combo = []; landed = 0; flat_frames = 0; v = 0.; message = ("bail!", 60) }

(*****************************************************************************)
(* The balance *)
(*****************************************************************************)

let new_balance (g : run) : balance =
  { needle = (if List.length g.combo mod 2 = 0 then 0.05 else -0.05); speed = 0.; frames = 0 }

(* one frame: the needle falls away from the middle, faster the longer
 * the trick; [dir] (-1, 0, 1) pushes it back. None when it fell. *)
let balance (dir : float) (b : balance) : balance option =
  let pull = 0.002 +. (float_of_int b.frames *. 0.00002) in
  let speed = (b.speed +. (b.needle *. pull *. 4.) +. (sign b.needle *. pull *. 0.3) +. (dir *. 0.004)) *. 0.98 in
  let needle = b.needle +. speed in
  if Float.abs needle >= 1. then None else Some { needle; speed; frames = b.frames + 1 }

(* what a balanced trick is worth after [frames] *)
let balanced (name : string) (base : int) (frames : int) : trick = { name; points = base + (frames * 2) }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* what the player does this frame *)
type input = {
  dir : float; (* left -1, right 1 *)
  down : bool;
  up : bool; (* pressed: a manual *)
  up_held : bool;
  flip : bool; (* pressed *)
  grab : bool; (* held *)
  lip : bool; (* held *)
  revert : bool; (* pressed *)
}

let nothing = { dir = 0.; down = false; up = false; up_held = false; flip = false; grab = false; lip = false; revert = false }

(* the flip or grab chosen by up and down *)
let variant (i : input) (neutral : string) (up : string) (down : string) : string =
  if i.up_held then up else if i.down then down else neutral

(* A frame in the air. The height is a ball's thrown up; the angle
 * turns with left and right; a flip takes 18 frames and a grab counts
 * once held 8. Back at the lip, the landing. *)
let fly (i : input) (a : air) (g : run) : run =
  let a = { a with vy = a.vy -. gravity; height = a.height +. a.vy -. gravity } in
  let a =
    match a.flip, a.grab with
    | Some (n, k), _ -> { a with flip = (if k > 1 then Some (n, k - 1) else None) }
    | None, None when i.flip -> { a with flip = Some (variant i "kickflip" "impossible" "heelflip", 18) }
    | None, Some (n, k) when i.grab -> { a with grab = Some (n, k + 1) }
    | None, None when i.grab -> { a with grab = Some (variant i "indy" "melon" "stalefish", 1) }
    | _ -> a
  in
  (* a flip ended, or a grab released: into the chain *)
  let g, a =
    match a.flip, a.grab with
    | Some (n, 1), _ -> (add { name = n; points = (if n = "impossible" then 150 else 100) } g, a)
    | None, Some (n, k) when not i.grab -> ((if k >= 8 then add { name = n; points = 100 + (k * 3) } g else g), { a with grab = None })
    | _ -> (g, a)
  in
  let turn = i.dir *. spin_speed in
  let a = { a with angle = a.angle +. turn; spun = a.spun +. turn } in
  if a.height > 0. then { g with skater = Air a }
  else
    (* the landing: the board along the wall, either way, and no trick under way *)
    let off = Float.abs (Float.rem (Float.abs (a.angle -. tangent (a.side *. s_lip))) 180.) in
    let off = Float.min off (180. -. off) in
    if off > land_tolerance || a.flip <> None || a.grab <> None then bail { g with s = a.side *. (s_lip -. 1.) }
    else
      let halves = int_of_float ((Float.abs a.spun +. 45.) /. 180.) in
      let g = if halves > 0 then add { name = string_of_int (halves * 180); points = halves * 100 } g else g in
      { g with skater = Rolling; s = a.side *. (s_lip -. 1.); v = a.side *. a.vy; landed = (if g.combo = [] then 0 else 12) }

(* A frame on the ramp, rolling or balancing: gravity along the slope,
 * the pump and the push, and the lip, to fly or to stall. *)
let roll (i : input) (g : run) : run =
  let s = g.s and sg = sign g.s in
  let a = Float.abs s in
  let v = (g.v -. (gravity *. sg *. Float.sin (slope s))) *. friction in
  let v = if i.down && a > flat && a < s_curve && s *. v < 0. then v -. (sg *. pump) else v in
  let v = if g.skater = Rolling && a <= flat && i.dir <> 0. && i.dir *. v < 7. then v +. (i.dir *. push) else v in
  let v = Float.max (-.max_speed) (Float.min max_speed v) in
  let s' = s +. v in
  if Float.abs s' < s_lip then { g with s = s'; v }
  else if i.lip then
    { g with s = sg *. s_lip; v = 0.; skater = Lip (new_balance g) }
  else
    { g with s = sg *. s_lip; v = 0.;
             skater = Air { side = sg; height = 0.; vy = Float.abs v; angle = tangent s; spun = 0.; flip = None; grab = None } }

(* a manual ended by the player on the flat ends the chain; one ended
 * by the curve leaves it going *)
let bank_if_flat (g : run) : run = if Float.abs g.s <= flat then bank g else g

let step (i : input) (g : run) : run =
  let g = { g with message = (fst g.message, max 0 (snd g.message - 1)); time = max 0 (g.time - 1) } in
  (* the revert, or the chain banked when its time is over *)
  let g =
    if g.landed > 0 && i.revert then { (add { name = "revert"; points = 100 } g) with landed = 0 }
    else if g.landed = 1 then bank g
    else { g with landed = max 0 (g.landed - 1) }
  in
  let on_flat = Float.abs g.s <= flat in
  match g.skater with
  | Bailed 0 -> { g with skater = Rolling }
  | Bailed n -> { g with skater = Bailed (n - 1) }
  | Air a -> fly i a g
  | Lip b -> (
      match balance i.dir b with
      | None -> bail { g with s = sign g.s *. (s_lip -. 5.) }
      | Some b when i.lip -> { g with skater = Lip b }
      | Some b -> roll nothing { (add (balanced "rock to fakie" 150 b.frames) g) with skater = Rolling; s = sign g.s *. (s_lip -. 1.); v = -.sign g.s }
      )
  | Manual b -> (
      match balance i.dir b with
      | None -> bail g
      | Some b when on_flat && not i.up -> roll i { g with skater = Manual b }
      | Some b -> roll i (bank_if_flat (add (balanced "manual" 50 b.frames) { g with skater = Rolling }))
      )
  | Rolling ->
      if on_flat && i.up then roll i { g with skater = Manual (new_balance g); flat_frames = 0 }
      else
        (* on the flat with a chain and no manual: 6 frames of grace *)
        let g = if on_flat && g.combo <> [] && g.landed = 0 then { g with flat_frames = g.flat_frames + 1 } else { g with flat_frames = 0 } in
        roll i (if g.flat_frames > 6 then bank g else g)

(*****************************************************************************)
(* The run *)
(*****************************************************************************)

type model = run Scene2d.t

let initial_model : model = Scene2d.start start

let over (g : run) : bool = g.time = 0 && g.combo = [] && (match g.skater with Air _ | Lip _ | Manual _ -> false | _ -> true)

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  let key k = Set_.mem k computer.keyboard.keys in
  let k = computer.keyboard in
  let g = scenes.scene in
  if over g then if pressed (fun k -> k.kspace) then { scenes with scene = { start with best = g.best } } else scenes
  else
    let i =
      { dir = (if k.kleft then -1. else if k.kright then 1. else 0.); down = k.kdown; up = pressed (fun k -> k.kup);
        up_held = k.kup; flip = pressed (fun k -> Set_.mem "x" k.keys); grab = key "z"; lip = key "c";
        revert = pressed (fun k -> Set_.mem "v" k.keys) }
    in
    { scenes with scene = step i g }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let concrete = rgb 190 190 200
let wood = rgb 200 150 90
let coping = rgb 120 120 130
let skin = rgb 240 200 160
let shirt = rgb 220 60 50
let pants = rgb 40 60 120

(* a limb: a thin rectangle from (x1, y1) to (x2, y2) *)
let limb (c : color) (w : float) (x1 : float) (y1 : float) (x2 : float) (y2 : float) : shape =
  let len = Float.hypot (x2 -. x1) (y2 -. y1) in
  rectangle c len w |> rotate (Float.atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* The skater, the board along x and standing up along y, crouching
 * [crouch] (0 to 1); [flip] the board's own turn (0 to 1), seen as its
 * length shrinking and its colour changing sides. *)
let skater_shape ~(crouch : float) ~(flip : float) ~(grab : bool) : shape =
  let board_len = 56. *. Float.abs (Float.cos (flip *. Float.pi)) +. 6. in
  let board = if Float.cos (flip *. Float.pi) >= 0. then wood else rgb 40 40 40 in
  let knee = 22. -. (crouch *. 8.) and hip = 42. -. (crouch *. 16.) in
  let shoulder = hip +. 28. in
  let hand_y = if grab then 10. else shoulder -. 16. in
  group
    [ circle (rgb 60 60 60) 4. |> move (-18.) (-2.); circle (rgb 60 60 60) 4. |> move 18. (-2.);
      rectangle board board_len 5. |> move 0. 4.;
      limb pants 7. (-10.) 6. (-6.) knee; limb pants 7. (-6.) knee 0. hip;
      limb pants 7. 10. 6. 8. knee; limb pants 7. 8. knee 0. hip;
      limb shirt 10. 0. hip 0. shoulder;
      limb skin 5. 0. shoulder (-14.) hand_y; limb skin 5. 0. shoulder (if grab then 6. else 14.) hand_y;
      circle skin 8. |> move 0. (shoulder +. 10.) ]

let ramp_shapes : shape list =
  let n = 60 in
  let top = List.init (n + 1) (fun k -> pos (-.s_lip +. (2. *. s_lip *. float_of_int k /. float_of_int n))) in
  let x_lip = flat +. r in
  let body = polygon concrete ((-.x_lip, -60.) :: (top @ [ (x_lip, -60.) ])) in
  let deck sg = rectangle wood 200. 20. |> move (sg *. (x_lip +. 100.)) (lip_height -. 10.) in
  let wall sg = rectangle (rgb 150 110 70) 200. (lip_height +. 40.) |> move (sg *. (x_lip +. 100.)) ((lip_height /. 2.) -. 40.) in
  [ wall (-1.); wall 1.; body; rectangle (rgb 90 90 100) (2. *. x_lip) 30. |> move 0. (-45.); deck (-1.); deck 1.;
    circle coping 6. |> move (-.x_lip) lip_height; circle coping 6. |> move x_lip lip_height ]

let meter (b : balance) : shape =
  group [ rectangle (rgb 40 40 40) 204. 14.; rectangle (rgb 90 200 90) 60. 10.; rectangle (rgb 250 250 250) 4. 22. |> move (b.needle *. 100.) 0. ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let g = model.scene in
  let x, y = pos g.s in
  let skater, balancing =
    match g.skater with
    | Rolling -> (skater_shape ~crouch:0.2 ~flip:0. ~grab:false |> rotate (tangent g.s) |> move x y, None)
    | Manual b -> (skater_shape ~crouch:0. ~flip:0. ~grab:false |> rotate (tangent g.s +. (sign g.v *. 12.)) |> move x y, Some b)
    | Lip b -> (skater_shape ~crouch:0.4 ~flip:0. ~grab:false |> rotate (tangent g.s -. (sign g.s *. 40.)) |> move x y, Some b)
    | Bailed _ -> (skater_shape ~crouch:0. ~flip:0. ~grab:false |> rotate (tangent g.s +. 90.) |> move x y, None)
    | Air a ->
        let flip = match a.flip with Some (_, k) -> float_of_int (18 - k) /. 18. | None -> 0. in
        (skater_shape ~crouch:(if a.grab <> None then 1. else 0.5) ~flip ~grab:(a.grab <> None) |> rotate a.angle
         |> move (a.side *. (flat +. r -. 12.)) (lip_height +. a.height), None)
  in
  let combo =
    if g.combo = [] then []
    else
      [ words (rgb 255 255 255) (String.concat " + " (List.rev_map (fun (t : trick) -> t.name) g.combo)) |> scale 1.4 |> move 0. 250.;
        words (rgb 255 220 80) (Printf.sprintf "%d x %d" (combo_sum g.combo) (List.length g.combo)) |> scale 2. |> move 0. 215. ]
  in
  let secs = (g.time + 59) / 60 in
  [ rectangle (rgb 70 110 160) screen.width screen.height;
    rectangle (rgb 60 60 70) screen.width 200. |> move 0. (ground -. 100.);
    group (ramp_shapes @ [ skater ]) |> move 0. ground;
    words (rgb 255 255 255) (Printf.sprintf "score %d" g.score) |> scale 1.8 |> move (-.300.) 300.;
    words (rgb 255 255 255) (Printf.sprintf "best combo %d" g.best) |> scale 1.4 |> move (-.300.) 270.;
    words (rgb 255 255 255) (Printf.sprintf "%d:%02d" (secs / 60) (secs mod 60)) |> scale 1.8 |> move 300. 300. ]
  @ combo
  @ (match balancing with Some b -> [ meter b |> move 0. 170. ] | None -> [])
  @ (if snd g.message > 0 then [ words (rgb 255 220 80) (fst g.message) |> scale 2.5 |> move 0. 120. ] else [])
  @ (if over g then [ words (rgb 255 255 255) "time! space for a new run" |> scale 2. |> move 0. 120. ] else [])
  @ [ words (rgb 230 230 230) "down pump, x flip, z grab, c lip, v revert, up manual, left right spin" |> scale 1.1 |> move 0. (-.330.) ]

let help = {|TinyTonyHawk
  down pumps (in a curve, going down), left right push on the flat
  in the air: left right spin, x a flip, z held a grab (with up or down: other ones)
  c held at the lip: a lip trick; v just after landing: a revert; up on the flat: a manual
  a chain's score is the sum of its tricks times their number; a bail loses it
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
