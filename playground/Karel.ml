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

type direction = North | East | South | West

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

type world = {
  avenues : int;
  streets : int;
  walls : (int * int * direction) list; (* a wall on that side of that corner, kept on both sides *)
  corners : ((int * int) * int) list; (* the corners with beepers, and how many *)
  avenue : int;
  street : int;
  facing : direction;
  in_bag : int;
}

let beepers (w : world) (a : int) (s : int) : int = Option.value (List.assoc_opt (a, s) w.corners) ~default:0
let beepers_left (w : world) : int = List.fold_left (fun n (_, k) -> n + k) 0 w.corners
let karel (w : world) = (w.avenue, w.street, w.facing)
let bag (w : world) = w.in_bag

let set_beepers (w : world) (a : int) (s : int) (n : int) : world =
  let others = List.remove_assoc (a, s) w.corners in
  { w with corners = (if n = 0 then others else ((a, s), n) :: others) }

let opposite = function North -> South | South -> North | East -> West | West -> East

let world ?(bag = 0) (rows : string list) : world =
  let rows = Array.of_list rows in
  let streets = (Array.length rows + 1) / 2 in
  let avenues = (Array.fold_left (fun m r -> max m (String.length r)) 0 rows + 1) / 2 in
  let at r c = if c < String.length rows.(r) then rows.(r).[c] else ' ' in
  let w = ref { avenues; streets; walls = []; corners = []; avenue = 1; street = 1; facing = East; in_bag = bag } in
  let wall a s d =
    let a', s' = match d with East -> (a + 1, s) | West -> (a - 1, s) | North -> (a, s + 1) | South -> (a, s - 1) in
    w := { !w with walls = (a, s, d) :: (a', s', opposite d) :: !w.walls }
  in
  for r = 0 to Array.length rows - 1 do
    for c = 0 to (2 * avenues) - 2 do
      let a = (c / 2) + 1 and s = streets - (r / 2) in
      match (r mod 2, c mod 2, at r c) with
      | 0, 0, ('1' .. '9' as n) -> w := set_beepers !w a s (Char.code n - Char.code '0')
      | 0, 0, ('>' | '<' | '^' | 'v' as k) ->
          let facing = match k with '>' -> East | '<' -> West | '^' -> North | _ -> South in
          w := { !w with avenue = a; street = s; facing }
      | 0, 1, '|' -> wall a s East
      | 1, 0, '-' -> wall a s South
      | _ -> ()
    done
  done;
  !w

let to_strings (w : world) : string list =
  List.init ((2 * w.streets) - 1) (fun r ->
      String.init ((2 * w.avenues) - 1) (fun c ->
          let a = (c / 2) + 1 and s = w.streets - (r / 2) in
          match (r mod 2, c mod 2) with
          | 0, 0 ->
              if (a, s) = (w.avenue, w.street) then
                match w.facing with East -> '>' | West -> '<' | North -> '^' | South -> 'v'
              else
                let n = beepers w a s in
                if n = 0 then '.' else Char.chr (Char.code '0' + min n 9)
          | 0, _ -> if List.mem (a, s, East) w.walls then '|' else ' '
          | _, 0 -> if List.mem (a, s, South) w.walls then '-' else ' '
          | _ -> ' '))

(*****************************************************************************)
(* The program *)
(*****************************************************************************)

type condition =
  | Front_clear
  | Left_clear
  | Right_clear
  | Beeper_here
  | Bag_not_empty
  | Facing of direction
  | Not of condition

let front_is_clear = Front_clear
let left_is_clear = Left_clear
let right_is_clear = Right_clear
let next_to_a_beeper = Beeper_here
let any_beepers_in_beeper_bag = Bag_not_empty
let facing d = Facing d
let not_ c = Not c

type command =
  | Move
  | Turn_left
  | Pick_beeper
  | Put_beeper
  | Turn_off
  | Iterate of int * command list
  | If of condition * command list * command list
  | While of condition * command list
  | Call of string

let move = Move
let turn_left = Turn_left
let pick_beeper = Pick_beeper
let put_beeper = Put_beeper
let turn_off = Turn_off
let iterate n body = Iterate (n, body)
let if_ ?(else_ = []) c body = If (c, body, else_)
let while_ c body = While (c, body)
let block body = Iterate (1, body)
let call name = Call name

let left_of = function North -> West | West -> South | South -> East | East -> North
let right_of d = opposite (left_of d)

(* can Karel go one corner [d] from where it stands? *)
let clear (w : world) (d : direction) : bool =
  let a, s = (w.avenue, w.street) in
  let inside = match d with East -> a < w.avenues | West -> a > 1 | North -> s < w.streets | South -> s > 1 in
  inside && not (List.mem (a, s, d) w.walls)

let rec holds (w : world) (c : condition) : bool =
  match c with
  | Front_clear -> clear w w.facing
  | Left_clear -> clear w (left_of w.facing)
  | Right_clear -> clear w (right_of w.facing)
  | Beeper_here -> beepers w w.avenue w.street > 0
  | Bag_not_empty -> w.in_bag > 0
  | Facing d -> w.facing = d
  | Not c -> not (holds w c)

(*****************************************************************************)
(* Running *)
(*****************************************************************************)

type status = Running | Finished | Error of string

(* what is left to do, as a stack of commands; [work] counts the loops
 * unfolded too, so that WHILE front-is-clear DO BEGIN END stops *)
type run = {
  world : world;
  todo : command list;
  definitions : (string * command list) list;
  status : status;
  steps : int;
  work : int;
}

let status r = r.status
let current r = r.world
let steps r = r.steps
let max_work = 1_000_000

let start ?(definitions = []) (w : world) (program : command list) : run =
  { world = w; todo = program; definitions; status = Running; steps = 0; work = 0 }

let rec step (r : run) : run =
  if r.status <> Running then r
  else if r.work > max_work then { r with status = Error "too much work: a loop that never ends?" }
  else
    let w = r.world in
    let r = { r with work = r.work + 1 } in
    match r.todo with
    | [] -> { r with status = Finished }
    | c :: rest -> (
        let did (w : world) = { r with world = w; todo = rest; steps = r.steps + 1 } in
        let error msg = { r with status = Error msg; steps = r.steps + 1 } in
        match c with
        | Move ->
            if not (clear w w.facing) then error "Karel moved into a wall"
            else
              let a, s = match w.facing with East -> (1, 0) | West -> (-1, 0) | North -> (0, 1) | South -> (0, -1) in
              did { w with avenue = w.avenue + a; street = w.street + s }
        | Turn_left -> did { w with facing = left_of w.facing }
        | Pick_beeper ->
            let n = beepers w w.avenue w.street in
            if n = 0 then error "no beeper to pick up here"
            else did { (set_beepers w w.avenue w.street (n - 1)) with in_bag = w.in_bag + 1 }
        | Put_beeper ->
            if w.in_bag = 0 then error "no beeper in the bag to put down"
            else did { (set_beepers w w.avenue w.street (beepers w w.avenue w.street + 1)) with in_bag = w.in_bag - 1 }
        | Turn_off -> { (did w) with status = Finished; todo = [] }
        | Iterate (n, body) ->
            step { r with todo = (if n <= 0 then rest else body @ (Iterate (n - 1, body) :: rest)) }
        | If (cond, yes, no) -> step { r with todo = (if holds w cond then yes else no) @ rest }
        | While (cond, body) -> step { r with todo = (if holds w cond then body @ (c :: rest) else rest) }
        | Call name -> (
            match List.assoc_opt name r.definitions with
            | Some body -> step { r with todo = body @ rest }
            | None -> error ("no instruction " ^ name)))

let execute ?definitions ?(max_steps = 10000) (w : world) (program : command list) : run =
  let rec go r =
    if r.status <> Running then r
    else if r.steps >= max_steps then { r with status = Error "too many steps: a loop that never ends?" }
    else go (step r)
  in
  go (start ?definitions w program)

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let street_color = rgb 200 190 170
let wall_color = rgb 120 40 30

let draw ?(size = 50.) (w : world) : shape =
  let x a = (float_of_int a -. (float_of_int (w.avenues + 1) /. 2.)) *. size in
  let y s = (float_of_int s -. (float_of_int (w.streets + 1) /. 2.)) *. size in
  let width = float_of_int w.avenues *. size and height = float_of_int w.streets *. size in
  let ground = rectangle (rgb 250 245 230) width height in
  let lines =
    List.init w.avenues (fun a -> rectangle street_color 1. (height -. size) |> move_x (x (a + 1)))
    @ List.init w.streets (fun s -> rectangle street_color (width -. size) 1. |> move_y (y (s + 1)))
  in
  let dots =
    List.concat
      (List.init w.avenues (fun a -> List.init w.streets (fun s -> circle street_color 2.5 |> Playground.move (x (a + 1)) (y (s + 1)))))
  in
  (* each wall once, halfway between its two corners *)
  let walls =
    List.filter_map
      (fun (a, s, d) ->
        match d with
        | East -> Some (rectangle wall_color 5. (size +. 5.) |> Playground.move (x a +. (size /. 2.)) (y s))
        | South -> Some (rectangle wall_color (size +. 5.) 5. |> Playground.move (x a) (y s -. (size /. 2.)))
        | _ -> None)
      w.walls
  in
  let border = [ rectangle wall_color (width +. 10.) 5. |> move_y ((height /. 2.) +. 2.5);
                 rectangle wall_color (width +. 10.) 5. |> move_y (-.(height /. 2.) -. 2.5);
                 rectangle wall_color 5. (height +. 10.) |> move_x ((width /. 2.) +. 2.5);
                 rectangle wall_color 5. (height +. 10.) |> move_x (-.(width /. 2.) -. 2.5) ] in
  let beepers =
    List.map
      (fun ((a, s), n) ->
        group
          ([ square (rgb 90 90 100) (size *. 0.36) |> rotate 45.; square (rgb 170 170 180) (size *. 0.26) |> rotate 45. ]
          @ if n > 1 then [ words black (string_of_int n) |> scale (size /. 60.) ] else [])
        |> Playground.move (x a) (y s))
      w.corners
  in
  (* Karel: a box, its screen a face, the triangle the way it faces *)
  let angle = match w.facing with East -> 0. | North -> 90. | West -> 180. | South -> 270. in
  let robot =
    group
      [ square (rgb 60 60 70) (size *. 0.62);
        square (rgb 120 200 230) (size *. 0.4);
        triangle (rgb 250 200 40) (size *. 0.16) |> rotate (-90.) |> move_x (size *. 0.3) ]
    |> rotate angle
    |> Playground.move (x w.avenue) (y w.street)
  in
  group ((ground :: lines) @ dots @ beepers @ walls @ border @ [ robot ])

(*****************************************************************************)
(* Applications *)
(*****************************************************************************)

let picture (w : world) (program : command list) = Playground.picture [ draw (current (execute w program)) ]

let animation ?(speed = 4.) (w : world) (program : command list) =
  let every = max 1 (int_of_float (60. /. speed)) in
  let view (_computer : computer) (s : run Scene2d.t) =
    let r = s.scene in
    let status =
      match r.status with
      | Running -> Printf.sprintf "%d steps" r.steps
      | Finished -> Printf.sprintf "done, %d steps" r.steps
      | Error msg -> "error: " ^ msg
    in
    [ draw r.world; words black status |> scale 2. |> move_y (-.(float_of_int r.world.streets *. 25.) -. 50.) ]
  in
  let update (computer : computer) (s : run Scene2d.t) =
    let s = Scene2d.update computer s in
    if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (start w program) s
    else if s.frames mod every = 0 then { s with scene = step s.scene }
    else s
  in
  game view update (Scene2d.start (start w program))
