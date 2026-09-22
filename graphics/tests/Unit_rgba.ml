(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/images/Rgba *)

let t = Testo.create

(* a 64x64 RGB PNG (see graphics/tests/dune's deps) *)
let rgb_png = "../../examples/checker.png"

let load ?channels file =
  match Stb_image.load ?channels file with Ok img -> img | Error (`Msg m) -> failwith m

(* What Rgba.mli says about the pinned binding: asked for 4 channels, it
 * reports the file's 3 and allocates for 3. If this test starts failing
 * after an stb_image upgrade, the binding was fixed, and Rgba's
 * workaround could go. *)
let test_binding_bug () =
  let img = load ~channels:4 rgb_png in
  Alcotest.(check int) "channels reported" 3 img.channels;
  Alcotest.(check int) "bytes: 64 * 64 * 3, not * 4" (64 * 64 * 3) (Bigarray.Array1.dim img.data)

let test_rgb_to_rgba () =
  let rgb = load rgb_png in
  let rgba = Rgba.of_stb_image rgb in
  Alcotest.(check int) "channels" 4 rgba.channels;
  Alcotest.(check int) "stride" (64 * 4) rgba.stride;
  Alcotest.(check int) "bytes" (64 * 64 * 4) (Bigarray.Array1.dim rgba.data);
  (* every pixel: the same r, g, b, and an opaque alpha *)
  for i = 0 to (64 * 64) - 1 do
    for k = 0 to 2 do
      if rgb.data.{(i * 3) + k} <> rgba.data.{(i * 4) + k} then Alcotest.failf "pixel %d, channel %d" i k
    done;
    if rgba.data.{(i * 4) + 3} <> 255 then Alcotest.failf "pixel %d: alpha" i
  done

let test_gray_alpha () =
  let data = Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout [| 100; 50 |] in
  match Stb_image.image ~width:1 ~height:1 ~channels:2 data with
  | Error (`Msg m) -> Alcotest.fail m
  | Ok gray ->
      let rgba = Rgba.of_stb_image gray in
      Alcotest.(check (list int)) "gray 100, alpha 50" [ 100; 100; 100; 50 ]
        (List.init 4 (fun k -> rgba.data.{k}))

let tests =
  Testo.categorize "Rgba"
    [
      t "the pinned binding's ~channels:4 bug" test_binding_bug;
      t "RGB to RGBA" test_rgb_to_rgba;
      t "gray + alpha to RGBA" test_gray_alpha;
    ]
