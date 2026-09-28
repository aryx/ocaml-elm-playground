(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type id = int

(* back to front *)
type t = { figures : (id * Figure.t) list; next : id }

let empty = { figures = []; next = 1 }
let add f t = ({ figures = t.figures @ [ (t.next, f) ]; next = t.next + 1 }, t.next)
let figures t = t.figures
let get t id = List.assoc_opt id t.figures

(* in front first: the list the other way round *)
let at ~tolerance t p = Option.map fst (List.find_opt (fun (_, f) -> Figure.hit ~tolerance f p) (List.rev t.figures))

let within t (b : Figure.box) =
  List.filter_map
    (fun (id, f) ->
      let (fb : Figure.box) = Figure.bounds f in
      if fb.x0 >= b.x0 && fb.x1 <= b.x1 && fb.y0 >= b.y0 && fb.y1 <= b.y1 then Some id else None)
    t.figures

let update id g t = { t with figures = List.map (fun (i, f) -> if i = id then (i, g f) else (i, f)) t.figures }
let move ids dx dy t = { t with figures = List.map (fun (i, f) -> if List.mem i ids then (i, Figure.translate dx dy f) else (i, f)) t.figures }
let delete ids t = { t with figures = List.filter (fun (i, _) -> not (List.mem i ids)) t.figures }

let to_front ids t =
  let chosen, rest = List.partition (fun (i, _) -> List.mem i ids) t.figures in
  { t with figures = rest @ chosen }

let to_back ids t =
  let chosen, rest = List.partition (fun (i, _) -> List.mem i ids) t.figures in
  { t with figures = chosen @ rest }

let group ids t =
  let chosen = List.filter (fun (i, _) -> List.mem i ids) t.figures in
  if List.length chosen < 2 then (t, None)
  else
    let id = t.next in
    let g = Figure.Group (List.map snd chosen) in
    (* the group takes the place of the one in front *)
    let front = fst (List.nth chosen (List.length chosen - 1)) in
    let figures = List.filter_map (fun (i, f) -> if i = front then Some (id, g) else if List.mem i ids then None else Some (i, f)) t.figures in
    ({ figures; next = id + 1 }, Some id)

let ungroup id t =
  match get t id with
  | Some (Figure.Group fs) ->
      let ids = List.mapi (fun k _ -> t.next + k) fs in
      let figures = List.concat_map (fun (i, f) -> if i = id then List.combine ids fs else [ (i, f) ]) t.figures in
      ({ figures; next = t.next + List.length fs }, ids)
  | _ -> (t, [])

let duplicate ids t =
  List.fold_left
    (fun (t, made) (i, f) ->
      if List.mem i ids then
        let t, id = add (Figure.translate 16. (-16.) f) t in
        (t, made @ [ id ])
      else (t, made))
    (t, []) t.figures

let bounds t ids =
  match List.filter_map (fun (i, f) -> if List.mem i ids then Some (Figure.bounds f) else None) t.figures with
  | [] -> None
  | b :: rest -> Some (List.fold_left Figure.union b rest)

type side = Lefts | Rights | Tops | Bottoms | Centers

let align side ids t =
  match bounds t ids with
  | None -> t
  | Some all ->
      let shift (b : Figure.box) =
        match side with
        | Lefts -> (all.x0 -. b.x0, 0.)
        | Rights -> (all.x1 -. b.x1, 0.)
        | Tops -> (0., all.y1 -. b.y1)
        | Bottoms -> (0., all.y0 -. b.y0)
        | Centers -> (((all.x0 +. all.x1) /. 2.) -. ((b.x0 +. b.x1) /. 2.), 0.)
      in
      {
        t with
        figures =
          List.map
            (fun (i, f) ->
              if List.mem i ids then
                let dx, dy = shift (Figure.bounds f) in
                (i, Figure.translate dx dy f)
              else (i, f))
            t.figures;
      }
