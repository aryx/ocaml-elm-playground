(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ortho_route.mli *)

type point = float * float
type dir = Left | Right | Up | Down
type box = float * float * float * float

let vector = function Left -> (-1., 0.) | Right -> (1., 0.) | Up -> (0., 1.) | Down -> (0., -1.)
let index = function Left -> 0 | Right -> 1 | Up -> 2 | Down -> 3
let opposite = function Left -> Right | Right -> Left | Up -> Down | Down -> Up

let inside (x, y) (l, b, r, t) = x > l +. 1e-9 && x < r -. 1e-9 && y > b +. 1e-9 && y < t -. 1e-9
let close (x1, y1) (x2, y2) = Float.abs (x1 -. x2) < 1e-9 && Float.abs (y1 -. y2) < 1e-9

(* the corners only: no point twice, none in the middle of a straight
   piece *)
let simplify (points : point list) : point list =
  let rec dedup = function a :: (b :: _ as rest) when close a b -> dedup rest | a :: rest -> a :: dedup rest | [] -> [] in
  let straight (x1, y1) (x2, y2) (x3, y3) = (Float.abs (x1 -. x2) < 1e-9 && Float.abs (x2 -. x3) < 1e-9) || (Float.abs (y1 -. y2) < 1e-9 && Float.abs (y2 -. y3) < 1e-9) in
  let rec go = function a :: b :: (c :: _ as rest) when straight a b c -> go (a :: rest) | a :: rest -> a :: go rest | [] -> [] in
  go (dedup points)

let bends points = max 0 (List.length (simplify points) - 2)

(* a binary heap of (cost, state), the cheapest on top: Dijkstra's
   queue *)
type heap = { mutable items : (float * int) array; mutable size : int }

let heap_push h (c, s) =
  if h.size = Array.length h.items then h.items <- Array.append h.items (Array.make (max 16 h.size) (0., 0));
  let i = ref h.size in
  h.items.(!i) <- (c, s);
  h.size <- h.size + 1;
  while !i > 0 && fst h.items.((!i - 1) / 2) > fst h.items.(!i) do
    let p = (!i - 1) / 2 in
    let tmp = h.items.(p) in
    h.items.(p) <- h.items.(!i);
    h.items.(!i) <- tmp;
    i := p
  done

let heap_pop h =
  let top = h.items.(0) in
  h.size <- h.size - 1;
  h.items.(0) <- h.items.(h.size);
  let i = ref 0 and moving = ref true in
  while !moving do
    let l = (2 * !i) + 1 and r = (2 * !i) + 2 in
    let smallest = ref !i in
    if l < h.size && fst h.items.(l) < fst h.items.(!smallest) then smallest := l;
    if r < h.size && fst h.items.(r) < fst h.items.(!smallest) then smallest := r;
    if !smallest = !i then moving := false
    else begin
      let tmp = h.items.(!i) in
      h.items.(!i) <- h.items.(!smallest);
      h.items.(!smallest) <- tmp;
      i := !smallest
    end
  done;
  top

let route ~(boxes : box list) ~(margin : float) ((start, sdir) : point * dir option) ((goal, gdir) : point * dir option) : point list =
  (* the stubs: straight out of a glued end's side, clear of its box *)
  let stub p = function None -> p | Some d -> let dx, dy = vector d in (fst p +. (dx *. margin *. 2.), snd p +. (dy *. margin *. 2.)) in
  let s = stub start sdir and g = stub goal gdir in
  let obstacles =
    List.map (fun (l, b, r, t) -> (l -. margin, b -. margin, r +. margin, t +. margin)) boxes
    |> List.filter (fun o -> not (inside s o || inside g o))
  in
  let blocked p = List.exists (inside p) obstacles in
  let fallback () =
    let mx = (fst s +. fst g) /. 2. in
    simplify [ start; s; (mx, snd s); (mx, snd g); g; goal ]
  in
  (* the lines a route can run along: the boxes' edges and the stubs *)
  let coords f = List.sort_uniq compare (f s :: f g :: List.concat_map (fun (l, b, r, t) -> f (l, b) :: [ f (r, t) ]) obstacles) |> Array.of_list in
  let xs = coords fst and ys = coords snd in
  let nx = Array.length xs and ny = Array.length ys in
  let node i j = (j * nx) + i in
  let at n = (xs.(n mod nx), ys.(n / nx)) in
  let find a v = let r = ref 0 in Array.iteri (fun i x -> if x = v then r := i) a; !r in
  let from = node (find xs (fst s)) (find ys (snd s)) and target = node (find xs (fst g)) (find ys (snd g)) in
  (* a step from a node in a direction: the neighbour, if the piece
     between them is clear *)
  let neighbour n d =
    let i = n mod nx and j = n / nx in
    let i', j' = match d with Left -> (i - 1, j) | Right -> (i + 1, j) | Down -> (i, j - 1) | Up -> (i, j + 1) in
    if i' < 0 || i' >= nx || j' < 0 || j' >= ny then None
    else
      let m = node i' j' in
      let (x1, y1), (x2, y2) = (at n, at m) in
      if blocked (x2, y2) || blocked ((x1 +. x2) /. 2., (y1 +. y2) /. 2.) then None else Some m
  in
  (* a state is a node and the direction it was reached in; 4 is "not
     yet moved", for an end with no side *)
  let bend = margin *. 5. in
  let states = nx * ny * 5 in
  let cost = Array.make states infinity and previous = Array.make states (-1) in
  let first = (from * 5) + match sdir with Some d -> index d | None -> 4 in
  cost.(first) <- 0.;
  let h = { items = Array.make 64 (0., 0); size = 0 } in
  heap_push h (0., first);
  while h.size > 0 do
    let c, st = heap_pop h in
    if c <= cost.(st) then
      List.iter
        (fun d ->
          let arrived = st mod 5 in
          (* no turning back on oneself *)
          if arrived = 4 || index (opposite d) <> arrived then
            match neighbour (st / 5) d with
            | None -> ()
            | Some m ->
                let (x1, y1), (x2, y2) = (at (st / 5), at m) in
                let turn = if arrived = 4 || arrived = index d then 0. else bend in
                let c' = c +. Float.abs (x2 -. x1) +. Float.abs (y2 -. y1) +. turn in
                let st' = (m * 5) + index d in
                if c' < cost.(st') then begin
                  cost.(st') <- c';
                  previous.(st') <- st;
                  heap_push h (c', st')
                end)
        [ Left; Right; Up; Down ]
  done;
  (* the best arrival: into the goal's side straight, or it costs a
     turn *)
  let into = match gdir with Some d -> Some (index (opposite d)) | None -> None in
  let best = ref (-1) and best_cost = ref infinity in
  for d = 0 to 4 do
    let st = (target * 5) + d in
    let c = cost.(st) +. match into with Some k when k <> d && d <> 4 -> bend | _ -> 0. in
    if c < !best_cost then begin best := st; best_cost := c end
  done;
  if !best < 0 || !best_cost = infinity then fallback ()
  else
    let rec back st acc = if st < 0 then acc else back previous.(st) (at (st / 5) :: acc) in
    simplify ((start :: back !best []) @ [ goal ])
