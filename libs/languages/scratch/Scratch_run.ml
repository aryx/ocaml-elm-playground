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

type value = Num of float | Str of string | Bool of bool | List of int | Ring of ring
and ring = { command : bool; expr : arg; script : block list; env : env }
and env = (string * int) list

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
  bubble : value option;
  scripts : script list;
}

type ink = Line of (float * float) * (float * float) * float * float | Stamp of sprite

(* what a thread does next, each with the environment it runs in: the
   rest of a stack; a loop, which yields before each turn but the
   first; a wait; the end of a custom block's run, where a report comes
   back to (true: a reporter's, whose value is wanted) *)
type frame =
  | Run of block list * env
  | Loop of loop * block list * bool (* yield first *) * env
  | Wait of float (* until *)
  | Wait_until of arg * env
  | Glide of (float * float) * (float * float) * float * float (* from, to, start, seconds *)
  | Return of bool

and loop = Times of int | Forever | Until of arg

type thread = { sprite : string; script : int; frames : frame list; result : value option }

type t = {
  sprites : sprite list;
  vars : (string * value) list;
  cells : (int * value) list;
  lists : (int * value list) list;
  next_id : int;
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

let stage sprites =
  { sprites; vars = []; cells = []; lists = []; next_id = 1; ink = []; threads = []; now = 0.; timer_start = 0.; seed = 1; broadcasts = []; halt = false }

(*****************************************************************************)
(* The heap: lists and variables' cells *)
(*****************************************************************************)

let new_cell t v = ({ t with cells = (t.next_id, v) :: t.cells; next_id = t.next_id + 1 }, t.next_id)
let new_list t items = (List t.next_id, { t with lists = (t.next_id, items) :: t.lists; next_id = t.next_id + 1 })
let items t id = match List.assoc_opt id t.lists with Some l -> l | None -> []
let set_items t id l = { t with lists = (id, l) :: List.remove_assoc id t.lists }

(*****************************************************************************)
(* Values *)
(*****************************************************************************)

let number = function
  | Num f -> f
  | Bool b -> if b then 1. else 0.
  | Str s -> ( match float_of_string_opt (String.trim s) with Some f when Float.is_finite f -> f | _ -> 0.)
  | List _ | Ring _ -> 0.

let text_of_number f = if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f

let text = function
  | Num f -> text_of_number f
  | Str s -> s
  | Bool b -> string_of_bool b
  | List _ -> "a list"
  | Ring _ -> "a ring"

(* a list's items, as texts, with the heap at hand *)
let rec show t = function List id -> "(" ^ String.concat " " (List.map (show t) (items t id)) ^ ")" | v -> text v

let truth = function Bool b -> b | Num f -> f <> 0. | Str s -> not (List.mem (String.lowercase_ascii s) [ ""; "0"; "false" ]) | List _ | Ring _ -> true

(* as numbers if both are, else as texts ignoring case *)
let compare_values t a b =
  let n v = match v with Num f -> Some f | Str s -> float_of_string_opt (String.trim s) | _ -> None in
  match (n a, n b) with
  | Some x, Some y -> compare x y
  | _ -> compare (String.lowercase_ascii (show t a)) (String.lowercase_ascii (show t b))

(*****************************************************************************)
(* Sprites and variables *)
(*****************************************************************************)

let find t name = List.find (fun s -> s.name = name) t.sprites
let update t s = { t with sprites = List.map (fun s' -> if s'.name = s.name then s else s') t.sprites }

(* a name: a parameter or a script variable if the environment has it,
   else a global *)
let lookup t env name =
  match List.assoc_opt name env with
  | Some cell -> ( match List.assoc_opt cell t.cells with Some v -> v | None -> Num 0.)
  | None -> ( match List.assoc_opt name t.vars with Some v -> v | None -> Num 0.)

let assign t env name v =
  match List.assoc_opt name env with
  | Some cell -> { t with cells = (cell, v) :: List.remove_assoc cell t.cells }
  | None -> { t with vars = (name, v) :: List.remove_assoc name t.vars }

let variable t name = lookup t [] name
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
  | other -> (
      match List.find_opt (fun o -> o.name = other) t.sprites with
      | Some o -> o.visible && s.visible && Float.hypot (o.x -. s.x) (o.y -. s.y) < reach o +. reach s
      | None -> false)

(*****************************************************************************)
(* Custom blocks and rings *)
(*****************************************************************************)

(* the parameters and the body of a custom block, from the "define"
   script, of any sprite, whose template makes its op *)
let definition t op =
  List.find_map
    (fun s ->
      List.find_map
        (fun (sc : script) ->
          match sc.blocks with
          | { op = "procedures_definition"; args = [ Lit kind; Lit template ]; _ } :: body when custom_op kind template = op -> Some (params template, body)
          | _ -> None)
        s.scripts)
    t.sprites

let is_ring_op op = op = "snap_reifyreporter" || op = "snap_reifypredicate" || op = "snap_reifyscript"

(* A ring's implicit parameters: its empty slots, filled with the
   inputs it is called with -- all of them with the one input if there
   is one, else from left to right (Snap!'s rule). Each becomes the
   variable #1, #2..., bound in the call's environment; the slots of the
   rings inside are theirs, not ours. *)
let fill n (a : arg) (script : block list) =
  let k = ref 0 in
  let hole () = incr k; Block (Scratch_blocks.variable ("#" ^ string_of_int (if n = 1 then 1 else !k))) in
  let rec arg = function Lit "" -> hole () | Lit _ as l -> l | Block b -> Block (block b)
  and block (b : block) = if is_ring_op b.op then b else { b with args = List.map arg b.args; mouths = List.map (List.map block) b.mouths } in
  if n = 0 then (a, script) else (arg a, List.map block script)

(*****************************************************************************)
(* Evaluating *)
(*****************************************************************************)

(* the frame's input, its random numbers (a Lehmer generator, its seed
   kept in the stage at the end of the frame), and whether we run a
   reporter's body, to its value, without yielding *)
type ctx = { input : input; rng : int ref; sync : bool }

let random ctx =
  ctx.rng := !(ctx.rng) * 48271 mod 2147483647;
  float_of_int !(ctx.rng) /. 2147483647.

let lit_of (b : block) i = match List.nth_opt b.args i with Some (Lit l) -> l | _ -> ""

(* a reporter's value; reporters have effects too (a list added to, a
   custom reporter's body), hence the stage given back *)
let rec eval ctx t env s (a : arg) : value * t =
  match a with
  | Lit l -> (Str l, t)
  | Block b -> (
      match b.op with
      | "data_variable" -> (lookup t env (lit_of b 0), t)
      | "snap_reifyreporter" | "snap_reifypredicate" -> (Ring { command = false; expr = List.hd b.args; script = []; env }, t)
      | "snap_reifyscript" -> (Ring { command = true; expr = Lit ""; script = List.concat b.mouths; env }, t)
      | op ->
          let vals, t = eval_all ctx t env s b.args in
          reporter ctx t s b op vals)

and eval_all ctx t env s args =
  let vals, t = List.fold_left (fun (acc, t) a -> let v, t = eval ctx t env s a in (v :: acc, t)) ([], t) args in
  (List.rev vals, t)

and reporter ctx t s (b : block) op vals =
  let v i = match List.nth_opt vals i with Some v -> v | None -> Str "" in
  let num i = number (v i) in
  let items_of i = match v i with List id -> items t id | _ -> [] in
  let pure x = (x, t) in
  match op with
  | "motion_xposition" -> pure (Num s.x)
  | "motion_yposition" -> pure (Num s.y)
  | "motion_direction" -> pure (Num s.direction)
  | "looks_size" -> pure (Num s.size)
  | "sensing_touchingobject" ->
      let what = text (v 0) in
      pure (Bool (if what = "mouse-pointer" then Float.hypot (ctx.input.mouse_x -. s.x) (ctx.input.mouse_y -. s.y) < reach s else touching t s what))
  | "sensing_keypressed" -> let k = text (v 0) in pure (Bool (if k = "any" then ctx.input.keys <> [] else List.mem k ctx.input.keys))
  | "sensing_mousedown" -> pure (Bool ctx.input.mouse_down)
  | "sensing_mousex" -> pure (Num ctx.input.mouse_x)
  | "sensing_mousey" -> pure (Num ctx.input.mouse_y)
  | "sensing_timer" -> pure (Num (t.now -. t.timer_start))
  | "operator_add" -> pure (Num (num 0 +. num 1))
  | "operator_subtract" -> pure (Num (num 0 -. num 1))
  | "operator_multiply" -> pure (Num (num 0 *. num 1))
  | "operator_divide" -> pure (Num (num 0 /. num 1))
  | "operator_mod" ->
      let a = num 0 and b = num 1 in
      (* Scratch's mod takes the divisor's sign: -1 mod 10 is 9 *)
      pure (Num (a -. (b *. Float.floor (a /. b))))
  | "operator_round" -> pure (Num (Float.round (num 0)))
  | "operator_random" ->
      let lo = Float.min (num 0) (num 1) and hi = Float.max (num 0) (num 1) in
      let r = random ctx in
      pure (if Float.is_integer lo && Float.is_integer hi then Num (lo +. Float.floor (r *. (hi -. lo +. 1.))) else Num (lo +. (r *. (hi -. lo))))
  | "operator_lt" -> pure (Bool (compare_values t (v 0) (v 1) < 0))
  | "operator_gt" -> pure (Bool (compare_values t (v 0) (v 1) > 0))
  | "operator_equals" -> pure (Bool (compare_values t (v 0) (v 1) = 0))
  | "operator_and" -> pure (Bool (truth (v 0) && truth (v 1)))
  | "operator_or" -> pure (Bool (truth (v 0) || truth (v 1)))
  | "operator_not" -> pure (Bool (not (truth (v 0))))
  | "operator_join" -> pure (Str (text (v 0) ^ text (v 1)))
  (* Snap!'s *)
  | "snap_call" -> call ctx t s (v 0) []
  | "snap_callwith" -> call ctx t s (v 0) [ v 1 ]
  | "snap_list" ->
      (* the empty slots at the end are no items *)
      let rec drop = function (Lit "", _) :: rest -> drop rest | l -> l in
      new_list t (List.rev_map snd (drop (List.rev (List.combine b.args vals))))
  | "snap_numbers" ->
      let a = int_of_float (num 0) and z = int_of_float (num 1) in
      let step = if z >= a then 1 else -1 in
      new_list t (List.init (abs (z - a) + 1) (fun i -> Num (float_of_int (a + (i * step)))))
  | "snap_item" -> pure (match List.nth_opt (items_of 1) (int_of_float (num 0) - 1) with Some x -> x | None -> Str "" | exception Invalid_argument _ -> Str "")
  | "snap_length" -> pure (Num (float_of_int (List.length (items_of 0))))
  | "snap_cons" -> new_list t (v 0 :: items_of 1)
  | "snap_cdr" -> new_list t (match items_of 0 with _ :: rest -> rest | [] -> [])
  | "snap_isempty" -> pure (Bool (items_of 0 = []))
  | "snap_contains" -> pure (Bool (List.exists (fun x -> compare_values t x (v 1) = 0) (items_of 0)))
  | "snap_map" ->
      let results, t = List.fold_left (fun (acc, t) x -> let r, t = call ctx t s (v 0) [ x ] in (r :: acc, t)) ([], t) (items_of 1) in
      new_list t (List.rev results)
  | "snap_keep" ->
      let kept, t = List.fold_left (fun (acc, t) x -> let r, t = call ctx t s (v 0) [ x ] in ((if truth r then x :: acc else acc), t)) ([], t) (items_of 1) in
      new_list t (List.rev kept)
  | "snap_combine" -> (
      match items_of 0 with
      | [] -> pure (Str "")
      | first :: rest -> List.fold_left (fun (acc, t) x -> call ctx t s (v 1) [ acc; x ]) (first, t) rest)
  | op when String.length op > 7 && String.sub op 0 7 = "custom:" -> (
      match definition t op with
      | Some (names, body) ->
          let t, env = bind t names vals in
          run_to_value ctx t s env body
      | None -> pure (Str ""))
  | _ -> pure (Str "")

(* the names bound to new cells holding the values *)
and bind t names vals =
  List.fold_left
    (fun (t, env) (name, v) -> let t, cell = new_cell t v in (t, (name, cell) :: env))
    (t, []) (List.combine names (List.filteri (fun i _ -> i < List.length names) (vals @ List.init (max 0 (List.length names - List.length vals)) (fun _ -> Str ""))))

(* a ring called: its empty slots filled with the inputs, in its own
   environment -- the one it was made in, which a closure keeps *)
and call ctx t s f inputs =
  match f with
  | Ring r ->
      let expr, script = fill (List.length inputs) r.expr r.script in
      let t, bound = bind t (List.mapi (fun i _ -> "#" ^ string_of_int (i + 1)) inputs) inputs in
      let env = bound @ r.env in
      if r.command then run_to_value ctx t s env script
      else (match (expr, inputs) with Lit "", [ x ] -> (x, t) | _ -> eval ctx t env s expr)
  | v -> (v, t)

(* a body run to its report, now, without yielding (a reporter's value
   is wanted at once): its loops turn without the stage being drawn *)
and run_to_value ctx t s env body =
  let th = { sprite = s.name; script = -1; frames = [ Run (body, env); Return true ]; result = None } in
  let t, th = run { ctx with sync = true } t th 100_000 in
  ((match th with Some { result = Some v; _ } -> v | _ -> Str ""), t)

(*****************************************************************************)
(* Running *)
(*****************************************************************************)

(* the frames under the custom block's run a report ends, and whether
   its value is wanted *)
and pop_to_return = function [] -> ([], false) | Return wanted :: rest -> (rest, wanted) | _ :: rest -> pop_to_return rest

(* a stack block done: the stage changed, and what the thread does
   next pushed on its frames. Its arguments are evaluated first, but
   the conditions a loop or a wait tests again, which [run] evaluates
   each time *)
and exec ctx t name env (b : block) rest =
  let s = find t name in
  let vals, t = if b.op = "control_repeat_until" || b.op = "control_wait_until" then ([], t) else eval_all ctx t env s b.args in
  let s = find t name in
  let v i = match List.nth_opt vals i with Some v -> v | None -> Str "" in
  let num i = number (v i) in
  let str i = text (v i) in
  let mouth i = List.nth b.mouths i in
  let set s = update t s in
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
      if tx = s.x && ty = s.y then (t, rest) else (set { s with direction = wrap (degrees (Float.atan2 (tx -. s.x) (ty -. s.y))) }, rest)
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
      (set { s with bubble = Some (v 0) }, Wait (t.now +. num 1) :: Run ([ silence ], env) :: rest)
  | "looks_say" -> (set { s with bubble = (if v 0 = Str "" then None else Some (v 0)) }, rest)
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
  | "control_repeat" -> (t, Loop (Times (int_of_float (Float.round (num 0))), mouth 0, false, env) :: rest)
  | "control_forever" -> (t, Loop (Forever, mouth 0, false, env) :: rest)
  | "control_repeat_until" -> (t, Loop (Until (List.nth b.args 0), mouth 0, false, env) :: rest)
  | "control_if" -> if truth (v 0) then (t, Run (mouth 0, env) :: rest) else (t, rest)
  | "control_if_else" -> (t, Run (mouth (if truth (v 0) then 0 else 1), env) :: rest)
  | "control_wait_until" -> (t, Wait_until (List.nth b.args 0, env) :: rest)
  | "control_stop" -> ( match str 0 with "all" -> ({ t with halt = true }, []) | _ -> (t, []))
  | "sensing_resettimer" -> ({ t with timer_start = t.now }, rest)
  | "data_setvariableto" -> (assign t env (lit_of b 0) (v 1), rest)
  | "data_changevariableby" -> (assign t env (lit_of b 0) (Num (number (lookup t env (lit_of b 0)) +. num 1)), rest)
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
  (* Snap!'s *)
  | "snap_scriptvariables" -> (
      (* a new cell, seen by the rest of this stack *)
      let t, cell = new_cell t (Num 0.) in
      match rest with Run (bs, e) :: others -> (t, Run (bs, (lit_of b 0, cell) :: e) :: others) | _ -> (t, rest))
  | "snap_run" | "snap_runwith" -> (
      match v 0 with
      | Ring ({ command = true; _ } as r) ->
          let inputs = if b.op = "snap_runwith" then [ v 1 ] else [] in
          let _, script = fill (List.length inputs) r.expr r.script in
          let t, bound = bind t (List.mapi (fun i _ -> "#" ^ string_of_int (i + 1)) inputs) inputs in
          (t, Run (script, bound @ r.env) :: Return false :: rest)
      | f -> (snd (call ctx t s f (if b.op = "snap_runwith" then [ v 1 ] else [])), rest))
  | "snap_add" -> ((match v 1 with List id -> set_items t id (items t id @ [ v 0 ]) | _ -> t), rest)
  | "snap_delete" -> ((match v 1 with List id -> set_items t id (List.filteri (fun i _ -> i <> int_of_float (num 0) - 1) (items t id)) | _ -> t), rest)
  | "snap_replace" -> ((match v 1 with List id -> set_items t id (List.mapi (fun i x -> if i = int_of_float (num 0) - 1 then v 2 else x) (items t id)) | _ -> t), rest)
  | op when String.length op > 7 && String.sub op 0 7 = "custom:" -> (
      match definition t op with
      | Some (names, body) ->
          let t, bound = bind t names vals in
          (t, Run (body, bound) :: Return false :: rest)
      | None -> (t, rest))
  | "procedures_report" -> (t, rest) (* handled by [run], which has the value to give back *)
  | _ -> (t, rest)

(* a thread run to its next yield: Some with what it still has to do,
   None when it is done; [fuel] bounds a frame's turn *)
and run ctx t th fuel =
  let continue frames = run ctx t { th with frames } (fuel - 1) in
  (* a condition tested, its effects kept *)
  let test c env k =
    let v, t = eval ctx t env (find t th.sprite) c in
    k t (truth v)
  in
  if fuel = 0 || t.halt then (t, Some th)
  else
    match th.frames with
    | [] -> (t, None)
    | Return _ :: rest -> continue rest
    | Run ([], _) :: rest -> continue rest
    | Run ({ op = "procedures_report"; args; _ } :: _, env) :: rest ->
        let v, t = eval ctx t env (find t th.sprite) (List.hd args) in
        let frames, wanted = pop_to_return rest in
        (* a reporter's value: its run is over *)
        if wanted then (t, Some { th with frames; result = Some v }) else run ctx t { th with frames } (fuel - 1)
    | Run (b :: bs, env) :: rest ->
        let t, frames = exec ctx t th.sprite env b (Run (bs, env) :: rest) in
        run ctx t { th with frames } (fuel - 1)
    | Loop (kind, body, true, env) :: rest ->
        if ctx.sync then continue (Loop (kind, body, false, env) :: rest) else (t, Some { th with frames = Loop (kind, body, false, env) :: rest })
    | Loop (kind, body, false, env) :: rest -> (
        let again kind = Run (body, env) :: Loop (kind, body, true, env) :: rest in
        match kind with
        | Times n when n <= 0 -> continue rest
        | Times n -> continue (again (Times (n - 1)))
        | Forever -> continue (again Forever)
        | Until c -> test c env (fun t yes -> run ctx t { th with frames = (if yes then rest else again kind) } (fuel - 1)))
    | Wait until :: rest -> if ctx.sync || t.now >= until then continue rest else (t, Some th)
    | Wait_until (c, env) :: rest -> test c env (fun t yes -> if ctx.sync || yes then run ctx t { th with frames = rest } (fuel - 1) else (t, Some th))
    | Glide ((x0, y0), (x1, y1), start, secs) :: rest ->
        let p = if ctx.sync || secs <= 0. then 1. else Float.min 1. ((t.now -. start) /. secs) in
        let t = move_to t (find t th.sprite) (x0 +. (p *. (x1 -. x0)), y0 +. (p *. (y1 -. y0))) in
        if p >= 1. then run ctx t { th with frames = rest } (fuel - 1) else (t, Some th)

(*****************************************************************************)
(* Starting *)
(*****************************************************************************)

let thread sprite script body = { sprite; script; frames = [ Run (body, []) ]; result = None }

(* the scripts whose hat answers, started, or started again *)
let start answers t =
  List.fold_left
    (fun t s ->
      List.fold_left
        (fun (t, i) sc ->
          match sc.blocks with
          | hat :: body when answers s hat ->
              let others = List.filter (fun th -> not (th.sprite = s.name && th.script = i)) t.threads in
              ({ t with threads = others @ [ thread s.name i body ] }, i + 1)
          | _ -> (t, i + 1))
        (t, 0) s.scripts
      |> fst)
    t t.sprites

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
      { t with threads = others @ [ thread name i body ] }
  | None -> t

let step input t =
  let ctx = { input; rng = ref t.seed; sync = false } in
  (* the messages of the last frame heard now *)
  let t =
    List.fold_left
      (fun t m -> start (fun _ hat -> hat.op = "event_whenbroadcastreceived" && String.lowercase_ascii (lit_of hat 0) = String.lowercase_ascii m) t)
      { t with broadcasts = []; now = input.time }
      t.broadcasts
  in
  let t, alive =
    List.fold_left
      (fun (t, alive) th ->
        let t, th = run ctx t th 10000 in
        (t, match th with Some th -> th :: alive | None -> alive))
      (t, []) t.threads
  in
  let t = { t with seed = !(ctx.rng) } in
  if t.halt then { (stop t) with halt = false } else { t with threads = List.rev alive }

(* a reporter's value, evaluated at once in a sprite: a reporter clicked
   in the editor, Snap!'s speech balloon with its value *)
let report input t name (b : block) =
  let ctx = { input; rng = ref t.seed; sync = true } in
  let v, t = eval ctx t [] (find t name) (Block b) in
  (v, { t with seed = !(ctx.rng) })
