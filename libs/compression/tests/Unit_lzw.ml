(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Lzw.mli's worked example *)

let t = Testo.create

let test_lzw () =
  (* clear(4) 1 6 7 end(5): the KwKwK case twice, the width growing to
   * 4 bits before the end code *)
  Alcotest.(check string) "six pixels of color 1" "\001\001\001\001\001\001"
    (Bytes.to_string (Lzw.decode ~min_code_size:2 "\x8C\x5F" ~npixels:6));
  (* the decoder's table, line by line: clear, 1, 6 (adds 6), 7 (adds 7);
   * the six pixels are then written, and the end code isn't read *)
  let steps, _ = Lzw.steps ~min_code_size:2 "\x8C\x5F" ~npixels:6 in
  Alcotest.(check (list (pair (pair int int) (pair int (option int)))))
    "the steps"
    [ ((4, 3), (0, None)); ((1, 3), (1, None)); ((6, 3), (2, Some 6)); ((7, 3), (3, Some 7)) ]
    (List.map (fun (s : Lzw.step) -> ((s.code, s.width), (s.length, s.added))) steps);
  (* clear(4) then 7: right after a clear, only a color can come *)
  match Lzw.decode ~min_code_size:2 "\x3C" ~npixels:6 with
  | _ -> Alcotest.fail "code 7 right after a clear"
  | exception Failure _ -> ()

let tests = Testo.categorize "Lzw" [ t "the worked example" test_lzw ]
