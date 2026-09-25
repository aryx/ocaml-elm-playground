(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_ilbm.mli *)

let bytes (b : Bytes.t) : int list = List.init (Bytes.length b) (fun i -> Char.code (Bytes.get b i))

let picture (w : int) (h : int) (f : int -> int -> int) : Ilbm.t =
  { width = w; height = h; planes = 5; pixels = Bytes.init (w * h) (fun i -> Char.chr (f (i mod w) (i / w)));
    palette = Array.init 32 (fun i -> (i * 8, 255 - (i * 8), i * 3));
    ranges = [ { low = 16; high = 23; rate = 16384; active = true; reverse = false }; { low = 24; high = 31; rate = 4096; active = false; reverse = true } ] }

let tests =
  Testo.categorize "ILBM"
    [
      Testo.create "Ilbm.mli's colour 5, over the planes" (fun () ->
          let t = picture 16 1 (fun x _ -> if x = 1 then 5 else 0) in
          Alcotest.(check (list (list int))) "plane-rows" [ [ 0x40; 0 ]; [ 0; 0 ]; [ 0x40; 0 ]; [ 0; 0 ]; [ 0; 0 ] ] (List.map bytes (Ilbm.plane_rows t 0)));
      Testo.create "written and read back: pixels, palette, ranges" (fun () ->
          (* 37 wide: a row's planes padded to 48 bits *)
          let t = picture 37 5 (fun x y -> ((x * 7) + (y * 3)) mod 32) in
          let back = Ilbm.decode (Ilbm.encode t) in
          Alcotest.(check (pair int int)) "size" (37, 5) (back.width, back.height);
          Alcotest.(check bool) "pixels" true (Bytes.equal t.pixels back.pixels);
          Alcotest.(check bool) "palette" true (t.palette = back.palette);
          Alcotest.(check bool) "ranges" true (t.ranges = back.ranges);
          Alcotest.(check (float 1e-9)) "16384: 60 steps a second" 60. (Ilbm.steps_per_second (List.hd t.ranges)));
      Testo.create "the chunks, in their order" (fun () ->
          let file = Ilbm.encode (picture 4 4 (fun _ _ -> 1)) in
          let at s = let rec go i = if i + 4 > String.length file then -1 else if String.sub file i 4 = s then i else go (i + 1) in go 0 in
          Alcotest.(check (list int)) "FORM ILBM" [ 0; 8 ] [ at "FORM"; at "ILBM" ];
          Alcotest.(check bool) "BMHD, CMAP, CRNG, BODY" true (at "BMHD" < at "CMAP" && at "CMAP" < at "CRNG" && at "CRNG" < at "BODY");
          Alcotest.(check int) "FORM's length: the rest of the file" (String.length file - 8)
            ((Char.code file.[4] lsl 24) lor (Char.code file.[5] lsl 16) lor (Char.code file.[6] lsl 8) lor Char.code file.[7]));
    ]
