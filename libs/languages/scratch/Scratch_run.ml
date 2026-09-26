(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scratch_run.mli *)

open Scratch_blocks

type value = Num of float | Str of string | Bool of bool
type rotation = All_around | Left_right | Dont_rotate

type sprite = {
  name : string;
  x : float;
  y : float;
  direction : float;
  size : float;
  visible : bool;
  costume : int;
  costumes : int;
  rotation : rotation;
  radius : float;
  pen : bool;
  pen_hue : float;
  pen_size : float;
  bubble : string option;
  scripts : script list;
}

type ink = Line of (float * float) * (float * float) * float * float | Stamp of sprite

(* what a thread does next: the rest of a stack; a loop, which yields
   before each turn but the first; a wait *)
type frame =
  | Run of block list
  | Loop of loop * block list * bool (* yield first *)
  | Wait of float (* until *)
  | Wait_until of arg
  | Glide of (float * float) * (float * float) * float * float (* from, to, start, seconds *)

and loop = Times of int | Forever | Until of arg

type thread = { sprite : string; script : int; frames : frame list }

type t = {
  sprites : sprite list;
  vars : (string * value) list;
  ink : ink list;
  threads : thread list;
  now : float;
  timer_start : float;
  seed : int;
  broadcasts : string list;
  halt : bool;
}

type input = { mouse_x : float; mouse_y : float; mouse_down : bool; keys : string list; time : float }

let sprite ~name ~costumes ~radius scripts =
  {
    name;
    x = 0.;
    y = 0.;
    direction = 90.;
    size = 100.;
    visible = true;
    costume = 0;
    costumes;
    rotation = All_around;
    radius;
    pen = false;
    pen_hue = 133.;
    pen_size = 1.;
    bubble = None;
    scripts;
  }

let stage sprites = { sprites; vars = []; ink = []; threads = []; now = 0.; timer_start = 0.; seed = 1; broadcasts = []; halt = false }

(*****************************************************************************)
(* Values *)
(*****************************************************************************)

let number = function
  | Num f -> f
  | Bool b -> if b then 1. else 0.
  | Str s -> ( match float_of_string_opt (String.trim s) with Some f when Float.is_finite f -> f | _ -> 0.)

let text = function
  | Num f -> if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f
  | Str s -> s
  | Bool b -> string_of_bool b

let truth = function Bool b -> b | Num f -> f <> 0. | Str s -> not (List.mem (String.lowercase_ascii s) [ ""; "0"; "false" ])

(* as numbers if both are, else as texts ignoring case *)
let compare_values a b =
  let n v = match v with Num f -> Some f | Bool _ -> None | Str s -> float_of_string_opt (String.trim s) in
  match (n a, n b) with
  | Some x, Some y -> compare x y
  | _ -> compare (String.lowercase_ascii (text a)) (String.lowercase_ascii (text b))

(*****************************************************************************)
(* Sprites *)
(*****************************************************************************)

let find t name = List.find (fun s -> s.name = name) t.sprites
let update t s = { t with sprites = List.map (fun s' -> if s'.name = s.name then s else s') t.sprites }
let variable t name = match List.assoc_opt name t.vars with Some v -> v | None -> Num 0.
let radians d = d *. Float.pi /. 180.
let degrees r = r *. 180. /. Float.pi

(* in (-180, 180] *)
let wrap d =
  let d = Float.rem d 360. in
  if d > 180. then d -. 360. else if d <= -180. then d +. 360. else d

let reach s = s.radius *. s.size /. 100.

(* a sprite to a place, its pen drawing on the way *)
let move_to t s (x, y) =
  let t = if s.pen then { t with ink = Line ((s.x, s.y), (x, y), s.pen_hue, s.pen_size) :: t.ink } else t in
  update t { s with x; y }

let touching t s = function
  | "edge" -> Float.abs s.x +. reach s > 240. || Float.abs s.y +. reach s > 180.
  | "mouse-pointer" -> false (* answered in [eval], which has the mouse *)
  | other -> (
      match List.find_opt (fun o -> o.name = other) t.sprites with
      | Some o -> o.visible && s.visible && Float.hypot (o.x -. s.x) (o.y -. s.y) < reach o +. reach s
      | None -> false)

(*****************************************************************************)
(* Evaluating *)
(*****************************************************************************)

(* the frame's input, and its random numbers (a Lehmer generator, its
   seed kept in the stage at the end of the frame) *)
type ctx = { input : input; rng : int ref }

let random ctx =
  ctx.rng := !(ctx.rng) * 48271 mod 2147483647;
  float_of_int !(ctx.rng) /. 2147483647.

let rec eval ctx t s (a : arg) : value =
  match a with
  | Lit l -> Str l
  | Block b -> (
      let arg i = eval ctx t s (List.nth b.args i) in
      let num i = number (arg i) in
      let lit i = match List.nth b.args i with Lit l -> l | Block _ -> text (arg i) in
      match b.op with
      | "motion_xposition" -> Num s.x
      | "motion_yposition" -> Num s.y
      | "motion_direction" -> Num s.direction
      | "looks_size" -> Num s.size
      | "sensing_touchingobject" ->
          let what = lit 0 in
          Bool (if what = "mouse-pointer" then Float.hypot (ctx.input.mouse_x -. s.x) (ctx.input.mouse_y -. s.y) < reach s else touching t s what)
      | "sensing_keypressed" -> let k = lit 0 in Bool (if k = "any" then ctx.input.keys <> [] else List.mem k ctx.input.keys)
      | "sensing_mousedown" -> Bool ctx.input.mouse_down
      | "sensing_mousex" -> Num ctx.input.mouse_x
      | "sensing_mousey" -> Num ctx.input.mouse_y
      | "sensing_timer" -> Num (t.now -. t.timer_start)
      | "operator_add" -> Num (num 0 +. num 1)
      | "operator_subtract" -> Num (num 0 -. num 1)
      | "operator_multiply" -> Num (num 0 *. num 1)
      | "operator_divide" -> Num (num 0 /. num 1)
      | "operator_mod" ->
          let a = num 0 and b = num 1 in
          (* Scratch's mod takes the divisor's sign: -1 mod 10 is 9 *)
          Num (a -. (b *. Float.of_int (int_of_float (Float.floor (a /. b)))))
      | "operator_round" -> Num (Float.round (num 0))
      | "operator_random" ->
          let lo = num 0 and hi = num 1 in
          let lo, hi = (Float.min lo hi, Float.max lo hi) in
          let r = random ctx in
          if Float.is_integer lo && Float.is_integer hi then Num (lo +. Float.floor (r *. (hi -. lo +. 1.))) else Num (lo +. (r *. (hi -. lo)))
      | "operator_lt" -> Bool (compare_values (arg 0) (arg 1) < 0)
      | "operator_gt" -> Bool (compare_values (arg 0) (arg 1) > 0)
      | "operator_equals" -> Bool (compare_values (arg 0) (arg 1) = 0)
      | "operator_and" -> Bool (truth (arg 0) && truth (arg 1))
      | "operator_or" -> Bool (truth (arg 0) || truth (arg 1))
      | "operator_not" -> Bool (not (truth (arg 0)))
      | "operator_join" -> Str (text (arg 0) ^ text (arg 1))
      | "data_variable" -> variable t (lit 0)
      | _ -> Str "")

(*****************************************************************************)
(* Running *)
(*****************************************************************************)

(* the scripts whose hat answers, started, or started again *)
let start answers t =
  List.fold_left
    (fun t s ->
      List.fold_left
        (fun (t, i) sc ->
          match sc.blocks with
          | hat :: body when answers s hat ->
              let others = List.filter (fun th -> not (th.sprite = s.name && th.script = i)) t.threads in
              ({ t with threads = others @ [ { sprite = s.name; script = i; frames = [ Run body ] } ] }, i + 1)
          | _ -> (t, i + 1))
        (t, 0) s.scripts
      |> fst)
    t t.sprites

let lit_of (b : block) i = match List.nth_opt b.args i with Some (Lit l) -> l | _ -> ""
let stop t = { t with threads = []; broadcasts = [] }
let green_flag t = start (fun _ hat -> hat.op = "event_whenflagclicked") { (stop t) with timer_start = t.now }
let key k t = start (fun _ hat -> hat.op = "event_whenkeypressed" && (lit_of hat 0 = k || lit_of hat 0 = "any")) t
let click name t = start (fun s hat -> s.name = name && hat.op = "event_whenthisspriteclicked") t

let run_script name i t =
  let s = find t name in
  match List.nth_opt s.scripts i with
  | Some sc ->
      let body = match sc.blocks with hat :: rest when (spec hat.op).shape = Hat -> rest | blocks -> blocks in
      let others = List.filter (fun th -> not (th.sprite = name && th.script = i)) t.threads in
      { t with threads = others @ [ { sprite = name; script = i; frames = [ Run body ] } ] }
  | None -> t

(* a stack block done: the stage changed, and what the thread does
   next pushed on its frames *)
let exec ctx t name (b : block) rest =
  let s = find t name in
  let arg i = eval ctx t s (List.nth b.args i) in
  let num i = number (arg i) in
  let str i = text (arg i) in
  let mouth i = List.nth b.mouths i in
  let set s = update t s in
  let same = (t, rest) in
  match b.op with
  | "motion_movesteps" ->
      let d = radians s.direction in
      (move_to t s (s.x +. (num 0 *. sin d), s.y +. (num 0 *. cos d)), rest)
  | "motion_turnright" -> (set { s with direction = wrap (s.direction +. num 0) }, rest)
  | "motion_turnleft" -> (set { s with direction = wrap (s.direction -. num 0) }, rest)
  | "motion_pointindirection" -> (set { s with direction = wrap (num 0) }, rest)
  | "motion_pointtowards" ->
      let tx, ty =
        match str 0 with
        | "mouse-pointer" -> (ctx.input.mouse_x, ctx.input.mouse_y)
        | other -> ( match List.find_opt (fun o -> o.name = other) t.sprites with Some o -> (o.x, o.y) | None -> (s.x, s.y))
      in
      if tx = s.x && ty = s.y then same else (set { s with direction = wrap (degrees (Float.atan2 (tx -. s.x) (ty -. s.y))) }, rest)
  | "motion_gotoxy" -> (move_to t s (num 0, num 1), rest)
  | "motion_changexby" -> (move_to t s (s.x +. num 0, s.y), rest)
  | "motion_setx" -> (move_to t s (num 0, s.y), rest)
  | "motion_changeyby" -> (move_to t s (s.x, s.y +. num 0), rest)
  | "motion_sety" -> (move_to t s (s.x, num 0), rest)
  | "motion_glidesecstoxy" -> (t, Glide ((s.x, s.y), (num 1, num 2), t.now, num 0) :: rest)
  | "motion_ifonedgebounce" ->
      let r = reach s in
      let d = radians s.direction in
      let dx = sin d and dy = cos d in
      let dx, x = if s.x +. r > 240. then (-.Float.abs dx, 240. -. r) else if s.x -. r < -240. then (Float.abs dx, -240. +. r) else (dx, s.x) in
      let dy, y = if s.y +. r > 180. then (-.Float.abs dy, 180. -. r) else if s.y -. r < -180. then (Float.abs dy, -180. +. r) else (dy, s.y) in
      (set { s with x; y; direction = wrap (degrees (Float.atan2 dx dy)) }, rest)
  | "motion_setrotationstyle" ->
      let rotation = match str 0 with "left-right" -> Left_right | "don't rotate" -> Dont_rotate | _ -> All_around in
      (set { s with rotation }, rest)
  | "looks_sayforsecs" ->
      (* the bubble, the wait, and a say of nothing to take it away *)
      let silence = { (make "looks_say") with args = [ Lit "" ] } in
      (set { s with bubble = Some (str 0) }, Wait (t.now +. num 1) :: Run [ silence ] :: rest)
  | "looks_say" -> (set { s with bubble = (if str 0 = "" then None else Some (str 0)) }, rest)
  | "looks_switchcostumeto" ->
      let n = int_of_float (num 0) - 1 in
      (set { s with costume = ((n mod s.costumes) + s.costumes) mod s.costumes }, rest)
  | "looks_nextcostume" -> (set { s with costume = (s.costume + 1) mod s.costumes }, rest)
  | "looks_changesizeby" -> (set { s with size = Float.max 5. (s.size +. num 0) }, rest)
  | "looks_setsizeto" -> (set { s with size = Float.max 5. (num 0) }, rest)
  | "looks_show" -> (set { s with visible = true }, rest)
  | "looks_hide" -> (set { s with visible = false }, rest)
  | "event_broadcast" -> ({ t with broadcasts = t.broadcasts @ [ str 0 ] }, rest)
  | "control_wait" -> (t, Wait (t.now +. num 0) :: rest)
  | "control_repeat" -> (t, Loop (Times (int_of_float (Float.round (num 0))), mouth 0, false) :: rest)
  | "control_forever" -> (t, Loop (Forever, mouth 0, false) :: rest)
  | "control_repeat_until" -> (t, Loop (Until (List.nth b.args 0), mouth 0, false) :: rest)
  | "control_if" -> if truth (arg 0) then (t, Run (mouth 0) :: rest) else same
  | "control_if_else" -> (t, Run (mouth (if truth (arg 0) then 0 else 1)) :: rest)
  | "control_wait_until" -> (t, Wait_until (List.nth b.args 0) :: rest)
  | "control_stop" -> ( match str 0 with "all" -> ({ t with halt = true }, []) | _ -> (t, []))
  | "sensing_resettimer" -> ({ t with timer_start = t.now }, rest)
  | "data_setvariableto" -> ({ t with vars = (lit_of b 0, arg 1) :: List.remove_assoc (lit_of b 0) t.vars }, rest)
  | "data_changevariableby" ->
      let v = Num (number (variable t (lit_of b 0)) +. num 1) in
      ({ t with vars = (lit_of b 0, v) :: List.remove_assoc (lit_of b 0) t.vars }, rest)
  | "pen_clear" -> ({ t with ink = [] }, rest)
  | "pen_stamp" -> ({ t with ink = Stamp s :: t.ink }, rest)
  | "pen_pendown" ->
      (* a dot where the pen touches down *)
      ({ (set { s with pen = true }) with ink = Line ((s.x, s.y), (s.x, s.y), s.pen_hue, s.pen_size) :: t.ink }, rest)
  | "pen_penup" -> (set { s with pen = false }, rest)
  | "pen_setpencolorto" -> (set { s with pen_hue = Float.rem (Float.rem (num 0) 200. +. 200.) 200. }, rest)
  | "pen_changepencolorby" -> (set { s with pen_hue = Float.rem (Float.rem (s.pen_hue +. num 0) 200. +. 200.) 200. }, rest)
  | "pen_setpensizeto" -> (set { s with pen_size = Float.max 1. (num 0) }, rest)
  | "pen_changepensizeby" -> (set { s with pen_size = Float.max 1. (s.pen_size +. num 0) }, rest)
  | _ -> same

(* a thread run to its next yield: Some with what it still has to do,
   None when it is done; [fuel] bounds a frame's turn *)
let rec run ctx t th fuel =
  let continue frames = run ctx t { th with frames } (fuel - 1) in
  if fuel = 0 || t.halt then (t, Some th)
  else
    match th.frames with
    | [] -> (t, None)
    | Run [] :: rest -> continue rest
    | Run (b :: bs) :: rest ->
        let t, frames = exec ctx t th.sprite b (Run bs :: rest) in
        run ctx t { th with frames } (fuel - 1)
    | Loop (kind, body, true) :: rest -> (t, Some { th with frames = Loop (kind, body, false) :: rest })
    | Loop (kind, body, false) :: rest -> (
        let again kind = Run body :: Loop (kind, body, true) :: rest in
        match kind with
        | Times n when n <= 0 -> continue rest
        | Times n -> continue (again (Times (n - 1)))
        | Forever -> continue (again Forever)
        | Until c -> if truth (eval ctx t (find t th.sprite) c) then continue rest else continue (again kind))
    | Wait until :: rest -> if t.now >= until then continue rest else (t, Some th)
    | Wait_until c :: rest -> if truth (eval ctx t (find t th.sprite) c) then continue rest else (t, Some th)
    | Glide ((x0, y0), (x1, y1), start, secs) :: rest ->
        let p = if secs <= 0. then 1. else Float.min 1. ((t.now -. start) /. secs) in
        let t = move_to t (find t th.sprite) (x0 +. (p *. (x1 -. x0)), y0 +. (p *. (y1 -. y0))) in
        if p >= 1. then run ctx t { th with frames = rest } (fuel - 1) else (t, Some th)

let step input t =
  let ctx = { input; rng = ref t.seed } in
  (* the messages of the last frame heard now *)
  let t = List.fold_left (fun t m -> start (fun _ hat -> hat.op = "event_whenbroadcastreceived" && String.lowercase_ascii (lit_of hat 0) = String.lowercase_ascii m) t) { t with broadcasts = []; now = input.time } t.broadcasts in
  let t, alive =
    List.fold_left
      (fun (t, alive) th ->
        let t, th = run ctx t th 10000 in
        (t, match th with Some th -> th :: alive | None -> alive))
      (t, []) t.threads
  in
  let t = { t with seed = !(ctx.rng) } in
  if t.halt then { (stop t) with halt = false } else { t with threads = List.rev alive }
