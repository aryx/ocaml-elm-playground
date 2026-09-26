(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Block_layout.mli *)

open Scratch_blocks

type step = At of int | Mouth of int | Arg of int
type path = { script : int; steps : step list }

type piece =
  | Body of { path : path; spec : spec; x : float; y : float; w : float; h : float; mouths : (float * float) list }
  | Label of { x : float; y : float; text : string }
  | Slot of { path : path; part : part; x : float; y : float; w : float; h : float; text : string }

type target = Below of path | Above of int | In_mouth of path * int

(* the C block's arm, left of its mouths, and under the last *)
let arm = 14.
let snap_distance = 20.
let gap = 4.
let side = 8.
let slot_h = 18.
let stack_h = 28.
let reporter_h = 22.
let hat_top = 14.

(*****************************************************************************)
(* Sizes *)
(*****************************************************************************)

let word_w ~measure w = measure w

(* a line's items: a word, or a slot with what is in it *)
type item = W of string | S of part * arg

let items (parts : part list) args =
  let args = ref args in
  let next () = match !args with a :: rest -> args := rest; a | [] -> Lit "" in
  List.map (function Word w -> W w | p -> S (p, next ())) parts

(* the lines of a block, the arguments given out in turn *)
let lines_of (b : block) =
  let s = spec b.op in
  let args = ref b.args in
  List.map
    (fun parts ->
      let n = List.length (List.filter (function Word _ -> false | _ -> true) parts) in
      let mine = List.filteri (fun i _ -> i < n) !args in
      args := List.filteri (fun i _ -> i >= n) !args;
      items parts mine)
    s.lines

let is_variable (b : block) = b.op = "data_variable"
let variable_name (b : block) = match b.args with [ Lit n ] -> n | _ -> ""

let rec item_size ~measure = function
  | W w -> (word_w ~measure w, 0.)
  | S (_, Block b) -> size ~measure b
  | S (Bool, Lit _) -> (30., slot_h -. 2.)
  | S (_, Lit s) -> (Float.max 24. (word_w ~measure s +. 10.), slot_h)

and line_size ~measure items =
  let sizes = List.map (item_size ~measure) items in
  (List.fold_left (fun acc (w, _) -> acc +. w) 0. sizes +. (gap *. float_of_int (max 0 (List.length sizes - 1))), List.fold_left (fun acc (_, h) -> Float.max acc h) 0. sizes)

and size ~measure (b : block) =
  let s = spec b.op in
  if is_variable b then (word_w ~measure (variable_name b) +. 20., reporter_h)
  else
    let lines = List.map (line_size ~measure) (lines_of b) in
    let w = List.fold_left (fun acc (w, _) -> Float.max acc w) 0. lines in
    match s.shape with
    | Reporter | Predicate -> (w +. (2. *. side) +. 4., Float.max reporter_h (snd (List.hd lines) +. 6.))
    | Hat -> (Float.max 100. (w +. (2. *. side)), hat_top +. Float.max stack_h (snd (List.hd lines) +. 8.))
    | Stack | Cap -> (Float.max 40. (w +. (2. *. side)), Float.max stack_h (snd (List.hd lines) +. 8.))
    | C_block | C_cap ->
        let line_hs = List.map (fun (_, h) -> Float.max stack_h (h +. 8.)) lines in
        let mouth_hs = List.map (fun m -> Float.max arm (height ~measure m)) b.mouths in
        (Float.max 80. (w +. (2. *. side)), List.fold_left ( +. ) 0. line_hs +. List.fold_left ( +. ) 0. mouth_hs +. arm)

and height ~measure blocks = List.fold_left (fun acc b -> acc +. snd (size ~measure b)) 0. blocks

(*****************************************************************************)
(* Placing *)
(*****************************************************************************)

(* a line's items from x, their middles at cy; its slots are the
   block's arguments from [first] on *)
let rec place_line ~measure ?(first = 0) path items x cy =
  let _, pieces, _ =
    List.fold_left
      (fun (x, acc, i) item ->
        let w, h = item_size ~measure item in
        match item with
        | W w' -> (x +. w +. gap, Label { x; y = cy; text = w' } :: acc, i)
        | S (part, Lit s) ->
            let slot = Slot { path = { path with steps = path.steps @ [ Arg i ] }; part; x; y = cy +. (h /. 2.); w; h; text = s } in
            (x +. w +. gap, slot :: acc, i + 1)
        | S (_, Block b) -> (x +. w +. gap, List.rev (place_block ~measure { path with steps = path.steps @ [ Arg i ] } b x (cy +. (h /. 2.))) @ acc, i + 1))
      (x, [], first) items
  in
  List.rev pieces

(* a block from its top-left: its body, then what is in it; the C
   blocks' mouths' stacks are placed by [place_stack] *)
and place_block ~measure path (b : block) x y =
  let s = spec b.op in
  let w, h = size ~measure b in
  if is_variable b then [ Body { path; spec = s; x; y; w; h; mouths = [] }; Label { x = x +. 10.; y = y -. (h /. 2.); text = variable_name b } ]
  else
    let lines = lines_of b in
    match s.shape with
    | C_block | C_cap ->
        (* the lines and the mouths in turn; the slots numbered on
           across the lines, as the arguments are *)
        let _, _, mouths, pieces, _ =
          List.fold_left
            (fun (top, k, mouths, acc, first_arg) line ->
              let lh = Float.max stack_h (snd (line_size ~measure line) +. 8.) in
              let line_pieces = place_line ~measure ~first:first_arg path line (x +. side) (top -. (lh /. 2.)) in
              let n_args = List.length (List.filter (function S _ -> true | W _ -> false) line) in
              let mh = Float.max arm (height ~measure (List.nth b.mouths k)) in
              (top -. lh -. mh, k + 1, (top -. lh, mh) :: mouths, acc @ line_pieces, first_arg + n_args))
            (y, 0, [], [], 0) lines
        in
        Body { path; spec = s; x; y; w; h; mouths = List.rev mouths } :: pieces
    | _ ->
        let top = if s.shape = Hat then y -. hat_top else y in
        let lh = h -. (y -. top) in
        Body { path; spec = s; x; y; w; h; mouths = [] } :: place_line ~measure path (List.hd lines) (x +. side) (top -. (lh /. 2.))

(* a stack from its top-left, and the places a stack can go in it *)
let rec place_stack ~measure script prefix blocks x y =
  let _, pieces, targets, _ =
    List.fold_left
      (fun (y, pieces, targets, i) (b : block) ->
        let path = { script; steps = prefix @ [ At i ] } in
        let _, h = size ~measure b in
        let s = spec b.op in
        let body = place_block ~measure path b x y in
        let inner, inner_targets =
          match body with
          | Body { mouths; _ } :: _ when b.mouths <> [] ->
              List.split
                (List.mapi
                   (fun k (top, _) ->
                     let p, t = place_stack ~measure script (prefix @ [ At i; Mouth k ]) (List.nth b.mouths k) (x +. arm) top in
                     (p, ((In_mouth (path, k), (x +. arm, top)) :: t)))
                   mouths)
          | _ -> ([], [])
        in
        let below = if s.shape = Cap || s.shape = C_cap then [] else [ (Below path, (x, y -. h)) ] in
        (y -. h, pieces @ body @ List.concat inner, targets @ below @ List.concat inner_targets, i + 1))
      (y, [], [], 0) blocks
  in
  (pieces, targets)

let layout ~measure scripts =
  let all = List.mapi (fun i (sc : script) -> let p, t = place_stack ~measure i [] sc.blocks sc.x sc.y in (p, (Above i, (sc.x, sc.y)) :: t)) scripts in
  (List.concat_map fst all, List.concat_map snd all)

(*****************************************************************************)
(* Snapping and hitting *)
(*****************************************************************************)

let is_cap (b : block) = let s = spec b.op in s.shape = Cap || s.shape = C_cap
let is_hat (b : block) = (spec b.op).shape = Hat

(* the stack a path's steps end in *)
let rec stack_at blocks = function
  | [] -> Some blocks
  | At i :: Mouth m :: rest -> ( match List.nth_opt blocks i with Some b -> Option.bind (List.nth_opt b.mouths m) (fun ms -> stack_at ms rest) | None -> None)
  | _ -> None

let snap ~measure scripts dragged ~at:(ax, ay) =
  match dragged with
  | [] -> None
  | first :: _ ->
      let ends_capped = is_cap (List.nth dragged (List.length dragged - 1)) in
      let dragged_h = height ~measure dragged in
      let _, targets = layout ~measure scripts in
      let valid = function
        | Above i -> (
            (not ends_capped) && match (List.nth scripts i).blocks with b :: _ -> not (is_hat b) | [] -> false)
        | Below { script; steps } -> (
            (not (is_hat first))
            &&
            let prefix = List.filteri (fun k _ -> k < List.length steps - 1) steps in
            match (List.rev steps, stack_at (List.nth scripts script).blocks prefix) with
            | At i :: _, Some stack -> (not ends_capped) || i = List.length stack - 1
            | _ -> false)
        | In_mouth ({ script; steps }, m) -> (
            (not (is_hat first))
            && match stack_at (List.nth scripts script).blocks (steps @ [ Mouth m ]) with Some stack -> (not ends_capped) || stack = [] | None -> false)
      in
      let distance (target, (px, py)) =
        match target with Above _ -> Float.hypot (ax -. px) (ay -. dragged_h -. py) | _ -> Float.hypot (ax -. px) (ay -. py)
      in
      List.fold_left
        (fun best ((target, _) as t) ->
          let d = distance t in
          if d > snap_distance || not (valid target) then best else match best with Some (_, bd) when bd <= d -> best | _ -> Some (target, d))
        None targets
      |> Option.map fst

let inside x y w h (px, py) = px >= x && px <= x +. w && py <= y && py >= y -. h

let block_at pieces p =
  List.fold_left
    (fun found piece ->
      match piece with
      | Body { path; x; y; w; h; mouths; _ } ->
          (* a C block is its arms, not its mouths *)
          let in_mouth = List.exists (fun (top, mh) -> fst p > x +. arm && snd p <= top && snd p > top -. mh) mouths in
          if inside x y w h p && not in_mouth then Some path else found
      | _ -> found)
    None pieces

let slot_at pieces p =
  List.fold_left (fun found piece -> match piece with Slot { path; x; y; w; h; _ } when inside x y w h p -> Some path | _ -> found) None pieces
