(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/gpu/Mesh_cache *)

let t = Testo.create

(* Mesh_cache.mli's diagram: frames drawing [A; B], [A; B], [A; C].
 * A mesh is its name, with the number of times it was built, so that
 * a rebuild would show. *)
let test_three_frames () =
  let cache = Mesh_cache.create () in
  let builds = ref [] and freed = ref [] in
  let frame ids =
    let meshes =
      ids
      |> List.map (fun (id, name) ->
             Mesh_cache.find_or_build cache id (fun () ->
                 builds := name :: !builds;
                 name))
    in
    Mesh_cache.sweep cache ~free:(fun mesh -> freed := mesh :: !freed);
    meshes
  in
  let a = (1, "A") and b = (2, "B") and c = (3, "C") in
  let stats = Alcotest.(check (list int)) in
  let of_stats (s : Mesh_cache.stats) = [ s.live; s.built; s.freed ] in

  Alcotest.(check (list string)) "frame 1 draws" [ "A"; "B" ] (frame [ a; b ]);
  Alcotest.(check (list string)) "frame 1 builds A and B" [ "B"; "A" ] !builds;
  stats "frame 1: 2 live, 2 built, 0 freed" [ 2; 2; 0 ] (of_stats (Mesh_cache.stats cache));

  Alcotest.(check (list string)) "frame 2 draws" [ "A"; "B" ] (frame [ a; b ]);
  Alcotest.(check (list string)) "frame 2 builds nothing" [ "B"; "A" ] !builds;
  stats "frame 2: 2 live, 0 built, 0 freed" [ 2; 0; 0 ] (of_stats (Mesh_cache.stats cache));

  Alcotest.(check (list string)) "frame 3 draws" [ "A"; "C" ] (frame [ a; c ]);
  Alcotest.(check (list string)) "frame 3 builds C only" [ "C"; "B"; "A" ] !builds;
  Alcotest.(check (list string)) "frame 3 frees B" [ "B" ] !freed;
  stats "frame 3: 2 live, 1 built, 1 freed" [ 2; 1; 1 ] (of_stats (Mesh_cache.stats cache))

let tests = Testo.categorize "Mesh_cache" [ t "the .mli's three frames" test_three_frames ]
