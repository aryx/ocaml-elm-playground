(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Mesh_cache.mli. *)

(* [used]: whether the mesh was used since the last sweep *)
type 'mesh entry = { mesh : 'mesh; mutable used : bool }

type stats = { live : int; built : int; freed : int }

type 'mesh t = {
  meshes : (int, 'mesh entry) Hashtbl.t;
  (* counted during the current frame *)
  mutable built : int;
  (* of the last frame, i.e. at the last sweep *)
  mutable last : stats;
}

let create () : 'mesh t = { meshes = Hashtbl.create 64; built = 0; last = { live = 0; built = 0; freed = 0 } }

let find_or_build (cache : 'mesh t) (id : int) (build : unit -> 'mesh) : 'mesh =
  match Hashtbl.find_opt cache.meshes id with
  | Some e ->
      e.used <- true;
      e.mesh
  | None ->
      let mesh = build () in
      Hashtbl.replace cache.meshes id { mesh; used = true };
      cache.built <- cache.built + 1;
      mesh

let sweep (cache : 'mesh t) ~(free : 'mesh -> unit) : unit =
  (* not removing from the table while iterating over it: undefined *)
  let unused = Hashtbl.fold (fun id e ids -> if e.used then ids else id :: ids) cache.meshes [] in
  List.iter
    (fun id ->
      free (Hashtbl.find cache.meshes id).mesh;
      Hashtbl.remove cache.meshes id)
    unused;
  Hashtbl.iter (fun _ e -> e.used <- false) cache.meshes;
  cache.last <- { live = Hashtbl.length cache.meshes; built = cache.built; freed = List.length unused };
  cache.built <- 0

let stats (cache : 'mesh t) : stats = cache.last
