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

(* See Sokoban.mli *)

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

type board = { map : Tilemap.t; col : int; row : int; moves : int; pushes : int }

let start (size : number) (rows : string list) : board =
  let map = Tilemap.of_strings size rows in
  match Tilemap.find map '@' @ Tilemap.find map '+' with
  | (col, row) :: _ ->
      (* the player is not part of the map: what's under them stays *)
      let under = if Tilemap.get map col row = Some '+' then '.' else ' ' in
      { map = Tilemap.set map col row under; col; row; moves = 0; pushes = 0 }
  | [] -> { map; col = 0; row = 0; moves = 0; pushes = 0 }

let is_box (c : char option) = c = Some '$' || c = Some '*'
let is_free (c : char option) = c = Some ' ' || c = Some '.'

(* a box leaving a cell, arriving in one: the goals stay *)
let without_box (c : char option) = if c = Some '*' then '.' else ' '
let with_box (c : char option) = if c = Some '.' then '*' else '$'

(* One step in direction (dc, dr): into a free cell, or pushing a box
 * into the free cell behind it; otherwise (a wall, two boxes in a
 * row), nothing.
 *
 *     @$ .   ->    @$.   ->    @*      a push, then another: on the goal
 *)
let step (b : board) ((dc, dr) : int * int) : board option =
  let get (c, r) = Tilemap.get b.map c r in
  match Push.chain ~blocked:(fun p -> not (is_free (get p))) ~pushable:(fun p -> is_box (get p)) ~limit:1 (b.col, b.row) (dc, dr) with
  | None -> None
  | Some [] -> Some { b with col = b.col + dc; row = b.row + dr; moves = b.moves + 1 }
  | Some chain ->
      (* the box from the chain's one cell to the next *)
      let c1, r1 = List.hd chain in
      let c2 = c1 + dc and r2 = r1 + dr in
      let map = Tilemap.set b.map c1 r1 (without_box (get (c1, r1))) in
      let map = Tilemap.set map c2 r2 (with_box (get (c2, r2))) in
      Some { map; col = c1; row = r1; moves = b.moves + 1; pushes = b.pushes + 1 }

let solved (b : board) : bool = Tilemap.find b.map '$' = []

(*****************************************************************************)
(* The look *)
(*****************************************************************************)

let player =
  Sprite.pixels 6. [ ('#', rgb 250 250 250) ]
    [ "..###.."; "..###.."; "...#..."; ".#####."; "#.###.#"; "..#.#.."; ".##.##." ]

let box (color : color) = group [ square (rgb 110 70 30) 52.; square color 40. ]

let tile (c : char) : shape =
  match c with
  | '#' -> group [ square (rgb 90 90 110) 60.; square (rgb 120 120 140) 50. ]
  | '.' -> circle (rgb 240 200 40) 10.
  | '$' -> box (rgb 170 110 50)
  | '*' -> box (rgb 240 200 40)
  | _ -> group []

(*****************************************************************************)
(* The file format *)
(*****************************************************************************)

(* a line of a map: only the format's characters, and not only spaces
 * (a blank line separates two levels) *)
let is_map_row (line : string) : bool =
  String.exists (fun c -> c <> ' ') line && String.for_all (fun c -> String.contains "#@$.*+ -_" c) line

let of_xsb (text : string) : string list list =
  let floor c = if c = '-' || c = '_' then ' ' else c in
  let levels, last =
    String.split_on_char '\n' text
    (* a file saved on Windows ends its lines with "\r\n" *)
    |> List.map (fun l -> if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1) else l)
    |> List.fold_left
         (fun (levels, rows) line ->
           if is_map_row line then (levels, String.map floor line :: rows)
           else if rows = [] then (levels, [])
           else (List.rev rows :: levels, []))
         ([], [])
  in
  List.rev (if last = [] then levels else List.rev last :: levels)

let to_xsb (levels : string list list) : string =
  levels |> List.mapi (fun i rows -> Printf.sprintf "; %d\n%s\n\n" (i + 1) (String.concat "\n" rows)) |> String.concat ""

(* the row without its spaces at the end *)
let rstrip (s : string) : string =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do decr n done;
  String.sub s 0 !n

let trim (rows : string list) : string list =
  let rec drop_blank rows = match rows with "" :: rest -> drop_blank rest | _ -> rows in
  let rows = List.map rstrip rows |> drop_blank |> List.rev |> drop_blank |> List.rev in
  let indent (s : string) = String.length s - String.length (String.trim s) in
  let left = List.fold_left (fun acc r -> if r = "" then acc else min acc (indent r)) max_int rows in
  List.map (fun r -> if r = "" then r else String.sub r left (String.length r - left)) rows

(*****************************************************************************)
(* Checking a level *)
(*****************************************************************************)

let count (chars : string) (rows : string list) : int =
  List.fold_left (fun n r -> String.fold_left (fun n c -> if String.contains chars c then n + 1 else n) n r) 0 rows

let problems (rows : string list) : string list =
  let players = count "@+" rows and boxes = count "$*" rows and goals = count ".+*" rows in
  let plural n word ending = Printf.sprintf "%d %s%s" n word (if n = 1 then "" else ending) in
  (if players = 0 then [ "no player" ] else if players > 1 then [ plural players "player" "s" ] else [])
  @ (if boxes = 0 then [ "no box" ]
     else if boxes <> goals then [ plural boxes "box" "es" ^ ", " ^ plural goals "goal" "s" ]
     else if count "$" rows = 0 then [ "every box already on a goal" ]
     else [])

type solution = Moves of string | Unsolvable | Gave_up of int

(* a position: the map's boxes, and where the player is (not how many
 * moves it took to get there) *)
let key (b : board) : string = Printf.sprintf "%s/%d,%d" (String.concat "" (Tilemap.to_strings b.map)) b.col b.row

(* Breadth first: all the positions one move away, then two, ... each
 * seen once. With every move costing one, the first solved position
 * reached is one of the nearest, and the path that reached it is a
 * shortest solution.
 *
 *     start --u--> A --l--> C ...     the queue: the positions still
 *       |                             to look at, nearest first, each
 *       +----r---> B --r--> ...       with the moves that reached it
 *)
let solve ?(max_positions = 100_000) (rows : string list) : solution =
  let start = start 1. rows in
  let seen = Hashtbl.create 4096 in
  Hashtbl.replace seen (key start) ();
  let queue = Queue.create () in
  Queue.add (start, "") queue;
  let dirs = [ ('u', (0, -1)); ('d', (0, 1)); ('l', (-1, 0)); ('r', (1, 0)) ] in
  let result = ref (if solved start then Some (Moves "") else None) in
  while !result = None do
    if Queue.is_empty queue then result := Some Unsolvable
    else if Hashtbl.length seen > max_positions then result := Some (Gave_up (Hashtbl.length seen))
    else
      let b, path = Queue.pop queue in
      dirs
      |> List.iter (fun (letter, dir) ->
             match step b dir with
             | Some next when !result = None && not (Hashtbl.mem seen (key next)) ->
                 let letter = if next.pushes > b.pushes then Char.uppercase_ascii letter else letter in
                 let path = path ^ String.make 1 letter in
                 if solved next then result := Some (Moves path)
                 else begin
                   Hashtbl.replace seen (key next) ();
                   Queue.add (next, path) queue
                 end
             | _ -> ())
  done;
  Option.get !result
