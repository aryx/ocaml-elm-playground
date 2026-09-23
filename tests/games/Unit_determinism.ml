(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_determinism.mli *)

(* the keys of a game: a move every 20 frames, round a small cycle *)
let key_at (frame : int) : Tetris.msg option =
  if frame mod 20 <> 0 then None
  else Some (List.nth Tetris.[ MoveLeft; Rotate; FullDrop; MoveRight; Rotate; FullDrop ] (frame / 20 mod 6))

(* [frames] frames of Tetris from [seed], each Tick carrying [clock
 * frame] as its time; the checksum of the model every second *)
let play ~(seed : int) ~(clock : int -> float) (frames : int) : int32 list * Tetris.model =
  let (app : (Tetris.model, Tetris.msg) Playground.app) = Tetris.app in
  let model, _ = app.init [ ("seed", string_of_int seed); ("music", "off") ] in
  let rec go frame model sums =
    if frame > frames then (List.rev sums, model)
    else
      let model = match key_at frame with Some k -> fst (app.update k model) | None -> model in
      let model = fst (app.update (Tetris.Tick (clock frame)) model) in
      let sums = if frame mod 60 = 0 then Checksum.of_model model :: sums else sums in
      go (frame + 1) model sums
  in
  go 1 model []

let steady frame = float_of_int frame /. 60.

(* another machine: another epoch, and frames that come late *)
let jittery frame = 1.8e9 +. (float_of_int frame *. 0.017) +. (if frame mod 7 = 0 then 0.05 else 0.)

let tetris_same_game () =
  let a, (model : Tetris.model) = play ~seed:7 ~clock:steady 1200 in
  let b, _ = play ~seed:7 ~clock:jittery 1200 in
  Alcotest.(check bool) "pieces landed" true (model.score > 0);
  Alcotest.(check (list string)) "every second, the same checksum" (List.map Checksum.to_hex a) (List.map Checksum.to_hex b)

let tetris_other_seed () =
  let a, _ = play ~seed:7 ~clock:steady 600 in
  let b, _ = play ~seed:8 ~clock:steady 600 in
  Alcotest.(check bool) "another game" true (List.rev a |> List.hd <> (List.rev b |> List.hd))

let tests =
  Testo.categorize "determinism"
    [
      Testo.create "Tetris, one seed, the same keys, two clocks: one game" tetris_same_game;
      Testo.create "Tetris, another seed: another game" tetris_other_seed;
    ]
