(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Block_edit.mli *)

open Scratch_blocks
open Block_layout

type dragged = Stack of block list | Reporter of block

(* a path's steps: those to a block (to its last At: a block in a
   ring's mouth is down the stacks, into an argument, into its mouth),
   then those into its slots *)
let split steps =
  let last_at = List.fold_left (fun (i, last) s -> (i + 1, match s with At _ -> i | _ -> last)) (0, -1) steps |> snd in
  (List.filteri (fun i _ -> i <= last_at) steps, List.filteri (fun i _ -> i > last_at) steps)

(* the stack the steps end in, changed by f: down the stacks by At,
   into a block by its mouths and its arguments *)
let rec map_stack steps f blocks =
  match steps with
  | [] -> f blocks
  | At i :: rest -> List.mapi (fun k b -> if k = i then map_in rest f b else b) blocks
  | _ -> blocks

and map_in steps f (b : block) =
  match steps with
  | Mouth m :: rest -> { b with mouths = List.mapi (fun j ms -> if j = m then map_stack rest f ms else ms) b.mouths }
  | Arg a :: rest -> { b with args = List.mapi (fun k x -> match x with Block inner when k = a -> Block (map_in rest f inner) | _ -> x) b.args }
  | _ -> b

(* the block the steps end at, changed by g *)
let map_block steps g blocks =
  match List.rev steps with At i :: prefix -> map_stack (List.rev prefix) (List.mapi (fun k b -> if k = i then g b else b)) blocks | _ -> blocks

(* the argument the Arg steps lead to in a block, changed by h *)
let rec map_arg args h (b : block) =
  match args with
  | [ Arg a ] -> { b with args = List.mapi (fun k x -> if k = a then h x else x) b.args }
  | Arg a :: rest -> { b with args = List.mapi (fun k x -> match x with Block inner when k = a -> Block (map_arg rest h inner) | _ -> x) b.args }
  | _ -> b

let rec get_stack steps blocks =
  match steps with
  | [] -> Some blocks
  | At i :: rest -> Option.bind (List.nth_opt blocks i) (get_in rest)
  | _ -> None

and get_in steps (b : block) =
  match steps with
  | Mouth m :: rest -> Option.bind (List.nth_opt b.mouths m) (get_stack rest)
  | Arg a :: rest -> ( match List.nth_opt b.args a with Some (Block inner) -> get_in rest inner | _ -> None)
  | _ -> None

let get_block steps blocks =
  match List.rev steps with At i :: prefix -> Option.bind (get_stack (List.rev prefix) blocks) (fun s -> List.nth_opt s i) | _ -> None

let rec get_arg args (b : block) =
  match args with
  | [ Arg a ] -> List.nth_opt b.args a
  | Arg a :: rest -> ( match List.nth_opt b.args a with Some (Block inner) -> get_arg rest inner | _ -> None)
  | _ -> None

let map_script scripts i f = List.mapi (fun k (sc : script) -> if k = i then { sc with blocks = f sc.blocks } else sc) scripts

let take scripts (p : path) =
  match List.nth_opt scripts p.script with
  | None -> None
  | Some sc -> (
      match split p.steps with
      | steps, [] -> (
          match List.rev steps with
          | At i :: prefix -> (
              let prefix = List.rev prefix in
              match get_stack prefix sc.blocks with
              | Some stack when i < List.length stack ->
                  let taken = List.filteri (fun k _ -> k >= i) stack in
                  let scripts = map_script scripts p.script (map_stack prefix (List.filteri (fun k _ -> k < i))) in
                  let scripts = List.filter (fun (s : script) -> s.blocks <> []) scripts in
                  Some (Stack taken, scripts)
              | _ -> None)
          | _ -> None)
      | steps, args -> (
          match Option.bind (get_block steps sc.blocks) (get_arg args) with
          | Some (Block r) -> Some (Reporter r, map_script scripts p.script (map_block steps (map_arg args (fun _ -> Lit ""))))
          | _ -> None))

let drop ~measure scripts dragged target =
  match target with
  | Above i ->
      (* the script grows upwards, so that what was there stays put *)
      List.mapi (fun k (sc : script) -> if k = i then { sc with blocks = dragged @ sc.blocks; y = sc.y +. Block_layout.height ~measure dragged } else sc) scripts
  | Below p -> (
      match List.rev p.steps with
      | At i :: prefix ->
          map_script scripts p.script (map_stack (List.rev prefix) (fun stack -> List.filteri (fun k _ -> k <= i) stack @ dragged @ List.filteri (fun k _ -> k > i) stack))
      | _ -> scripts)
  | In_mouth (p, m) -> map_script scripts p.script (map_stack (p.steps @ [ Mouth m ]) (fun stack -> dragged @ stack))

let drop_in_slot scripts (p : path) r =
  let steps, args = split p.steps in
  map_script scripts p.script (map_block steps (map_arg args (fun _ -> Block r)))

let alone scripts (x, y) blocks = scripts @ [ { x; y; blocks } ]

let text scripts (p : path) =
  let steps, args = split p.steps in
  match Option.bind (List.nth_opt scripts p.script) (fun sc -> Option.bind (get_block steps sc.blocks) (get_arg args)) with
  | Some (Lit s) -> Some s
  | _ -> None

let set_text scripts (p : path) s =
  let steps, args = split p.steps in
  map_script scripts p.script (map_block steps (map_arg args (function Lit _ -> Lit s | a -> a)))
