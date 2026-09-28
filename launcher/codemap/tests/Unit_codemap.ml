(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_codemap.mli *)

let show (r : Treemap.rect) : string = Printf.sprintf "%.2f,%.2f %.2fx%.2f" r.x r.y r.w r.h

let tests =
  Testo.categorize "Codemap"
    [
      Testo.create "squarified: the paper's example" (fun () ->
          Alcotest.(check (list string))
            "6 6 4 3 2 2 1 in 6 by 4"
            [ "0.00,0.00 3.00x2.00"; "0.00,2.00 3.00x2.00"; "3.00,0.00 1.71x2.33"; "4.71,0.00 1.29x2.33"; "3.00,2.33 1.20x1.67";
              "4.20,2.33 1.20x1.67"; "5.40,2.33 0.60x1.67" ]
            (List.map show (Treemap.squarify [ 6.; 6.; 4.; 3.; 2.; 2.; 1. ] { x = 0.; y = 0.; w = 6.; h = 4. })));
      Testo.create "slice and dice" (fun () ->
          Alcotest.(check (list string))
            "1 3 across" [ "0.00,0.00 1.00x2.00"; "1.00,0.00 3.00x2.00" ]
            (List.map show (Treemap.slice ~horizontal:true [ 1.; 3. ] { x = 0.; y = 0.; w = 4.; h = 2. })));
      Testo.create "trees from paths" (fun () ->
          let t = Treemap.fold_singletons (Treemap.of_paths [ ("a/b/x.ml", 1., ()); ("a/b/y.ml", 2., ()) ]) in
          (match t with
          | Dir ("a/b", [ File ("x.ml", _, _); File ("y.ml", _, _) ]) -> ()
          | _ -> Alcotest.fail "a/b holding x.ml and y.ml");
          let placed = Treemap.layout Squarified { x = 0.; y = 0.; w = 10.; h = 10. } t in
          Alcotest.(check (list string)) "paths, the directory first" [ "a/b"; "a/b/x.ml"; "a/b/y.ml" ]
            (List.map (fun (p : unit Treemap.placed) -> p.path) placed));
      Testo.create "a file's grid and definitions" (fun () ->
          let f = Code_file.make "x.ml" "(*****)\n(* Model *)\n(*****)\nlet move p = p\ntype t = int\n" in
          Alcotest.(check (list (pair int string))) "the section, the function, the type"
            [ (1, "Model"); (3, "move"); (4, "t") ]
            (List.map (fun (l, n, _) -> (l, n)) f.defs);
          Alcotest.(check (option string)) "line 4, column 0: let" (Some "Keyword")
            (Option.map Highlight_code.show (Code_file.at f 3 0));
          Alcotest.(check (option string)) "a space" None (Option.map Highlight_code.show (Code_file.at f 3 3)));
      Testo.create "modules used" (fun () ->
          Alcotest.(check (list string)) "M.x, open N" [ "List"; "N"; "Playground" ]
            (Code_file.modules_used "open N\nlet x = List.map f (Playground.foo) (* C.x *)"));
      (* claude: Code_config.mli's worked example *)
      Testo.create "a directory's .codemapignore and .codemapconfig" (fun () ->
          match
            Code_config.make ~ignore:(Some "# hi\n/gitlog.txt\ntest/\n*_tests.c\n")
              ~config:(Some {|{ "colors": { "kernel": "#e08030", "MISC/BIG": "#606060" } }|})
          with
          | Error e -> Alcotest.fail e
          | Ok c ->
              Alcotest.(check (list bool)) "gitlog.txt, kernel/test/ out; test.c, kernel/gitlog.txt in; lib/io_tests.c out"
                [ true; true; false; false; true ]
                [
                  Code_config.ignored c "gitlog.txt" ~dir:false;
                  Code_config.ignored c "kernel/test" ~dir:true;
                  Code_config.ignored c "test.c" ~dir:false;
                  Code_config.ignored c "kernel/gitlog.txt" ~dir:false;
                  Code_config.ignored c "lib/io_tests.c" ~dir:false;
                ];
              Alcotest.(check (list (pair string (list int)))) "the colours"
                [ ("kernel", [ 224; 128; 48 ]); ("MISC/BIG", [ 96; 96; 96 ]) ]
                (List.map (fun (p, (r, g, b)) -> (p, [ r; g; b ])) (Code_config.colours c)));
      Testo.create "a config's mistake" (fun () ->
          Alcotest.(check (result unit string)) "not a colour" (Error {|kernel: "orange" is no #rrggbb|})
            (Result.map ignore (Code_config.make ~ignore:None ~config:(Some {|{ "colors": { "kernel": "orange" } }|}))));
    ]
