(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_layers.mli *)

(*****************************************************************************)
(* The links between siblings *)
(*****************************************************************************)

let within (p : string) (file : string) : bool =
  file = p || (String.length file > String.length p && String.sub file 0 (String.length p) = p && file.[String.length p] = '/')

(* each link counted once, in the directory where its two files part:
 * (the directory, the user's child, the used's child) *)
let sibling_links (links : (string * string * int) list) (tree : 'a Treemap.tree) : (string * string * string, int) Hashtbl.t =
  let h = Hashtbl.create 256 in
  let root = Treemap.child_path "" tree in
  List.iter
    (fun (a, b, n) ->
      let rec down path node =
        match node with
        | Treemap.File _ -> ()
        | Dir (_, kids) -> (
            let holding f = List.find_opt (fun k -> within (Treemap.child_path path k) f) kids in
            match (holding a, holding b) with
            | Some ka, Some kb when ka == kb -> down (Treemap.child_path path ka) ka
            | Some ka, Some kb ->
                let k = (path, Treemap.child_path path ka, Treemap.child_path path kb) in
                Hashtbl.replace h k (n + Option.value (Hashtbl.find_opt h k) ~default:0)
            | _ -> ())
      in
      down root tree)
    links;
  h

(*****************************************************************************)
(* The layers *)
(*****************************************************************************)

let max_bands = 4

let compute (links : (string * string * int) list) (tree : 'a Treemap.tree) : string -> int =
  let h = sibling_links links tree in
  (* by directory: its children's links, the heavier way only when two
   * children use each other (a cycle is layered by its main direction) *)
  let by_dir = Hashtbl.create 64 in
  Hashtbl.iter
    (fun (d, a, b) n ->
      let back = Option.value (Hashtbl.find_opt h (d, b, a)) ~default:0 in
      if n > back then Hashtbl.replace by_dir d ((a, b) :: Option.value (Hashtbl.find_opt by_dir d) ~default:[]))
    h;
  let band = Hashtbl.create 256 in
  Hashtbl.iter
    (fun _ edges ->
      (* the longest path from the children nobody uses: a child one
       * layer below the lowest of its users; capped, cycles left *)
      let nodes = List.sort_uniq compare (List.concat_map (fun (a, b) -> [ a; b ]) edges) in
      let layer = Hashtbl.create 16 in
      List.iter (fun v -> Hashtbl.replace layer v 0) nodes;
      let cap = List.length nodes in
      for _ = 1 to cap do
        List.iter (fun (a, b) -> let la = Hashtbl.find layer a in if la + 1 > Hashtbl.find layer b && la < cap then Hashtbl.replace layer b (la + 1)) edges
      done;
      (* the layers numbered 0 to m - 1, then squeezed into at most
       * max_bands bands *)
      let ls = List.sort_uniq compare (Hashtbl.fold (fun _ l acc -> l :: acc) layer []) in
      let m = List.length ls in
      let rank l = let rec go i = function x :: r -> if x = l then i else go (i + 1) r | [] -> 0 in go 0 ls in
      Hashtbl.iter (fun v l -> Hashtbl.replace band v (rank l * min m max_bands / max 1 m)) layer)
    by_dir;
  fun p -> Option.value (Hashtbl.find_opt band p) ~default:0
