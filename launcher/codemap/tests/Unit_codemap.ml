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
      (* claude: the ordered layout on the paper's 20 sizes (Shneiderman and
       * Wattenberg 2001, codemap's children_ex_ordered_2001) *)
      Testo.create "ordered: each its share, inside, in order" (fun () ->
          let sizes = List.map float_of_int [ 1; 5; 3; 4; 5; 1; 10; 1; 1; 2; 7; 3; 5; 2; 10; 1; 2; 1; 1; 2 ] in
          let r : Treemap.rect = { x = 0.; y = 0.; w = 6.; h = 4. } in
          let rects = Treemap.ordered_layout sizes r in
          let total = List.fold_left ( +. ) 0. sizes in
          List.iter2
            (fun s (q : Treemap.rect) ->
              Alcotest.(check (float 1e-6)) "its share of the area" (s /. total *. 24.) (q.w *. q.h);
              Alcotest.(check bool) "inside" true (q.x >= -1e-9 && q.y >= -1e-9 && q.x +. q.w <= 6. +. 1e-9 && q.y +. q.h <= 4. +. 1e-9))
            sizes rects;
          let a, b, c = match Treemap.ordered_layout [ 1.; 1.; 1. ] { x = 0.; y = 0.; w = 3.; h = 1. } with [ a; b; c ] -> (a, b, c) | _ -> assert false in
          Alcotest.(check bool) "three in a row, left to right" true (a.x < b.x && b.x < c.x));
      (* claude: Code_names.mli's worked example, and OCaml's rules *)
      Testo.create "names in other files" (fun () ->
          let files srcs = List.map (fun (p, src) -> (p, lazy (Code_file.make p src))) srcs in
          (* where [name], used in [from], goes; "!" if sure *)
          let where ?roots fs from name =
            let (f : Code_file.t) = Lazy.force (List.assoc from fs) in
            let r =
              List.find (fun (r : Highlight_code.reference) -> r.rname = name) (List.concat (Array.to_list f.refs))
            in
            let cs, sure = Code_names.find ?roots fs ~from f r in
            String.concat " "
              (List.map (fun (c : Code_names.candidate) -> Printf.sprintf "%s:%d%s" c.path (c.line + 1) (if c.other_project then "(other)" else "")) cs)
            ^ if sure then " !" else ""
          in
          let c =
            files
              [
                ("rc/exec.c", "#include \"fns.h\"\nvoid f(void) { error(\"x\"); print(\"y\"); }\n");
                ("rc/fns.h", "void error(char*);\n");
                ("rc/subr.c", "void\nerror(char *s)\n{\n}\n");
                ("sam/error.c", "void error(char *s) { }\n");
                ("lib/error.c", "void error(char *s) { }\n");
                ("lib/fmt.c", "int print(char *f) { return 0; }\n");
              ]
          in
          Alcotest.(check string) "error: its own program's definition" "rc/subr.c:2 lib/error.c:1 sam/error.c:1 !" (where c "rc/exec.c" "error");
          Alcotest.(check string) "print: the library's" "lib/fmt.c:1 !" (where c "rc/exec.c" "print");
          let ml =
            files
              [
                ("games/Main.ml", "open Road\nlet a = Road.curve 1\nlet b = straight 2\nlet c = Parser.parse 3\n");
                ("games/Road.ml", "let curve x = x\nlet straight x = x\n");
                ("games/Road.mli", "val curve : int -> int\nval straight : int -> int\n");
                ("games/Parser.ml", "let parse x = x\n");
                ("tools/Parser.ml", "let parse x = x\n");
              ]
          in
          Alcotest.(check string) "M.x: the .ml, then the .mli" "games/Road.ml:1 games/Road.mli:1 !" (where ml "games/Main.ml" "curve");
          Alcotest.(check string) "a bare name, from an open" "games/Road.ml:2 games/Road.mli:2 !" (where ml "games/Main.ml" "straight");
          Alcotest.(check string) "two Parser.ml: the nearest" "games/Parser.ml:1 tools/Parser.ml:1 !" (where ml "games/Main.ml" "parse");
          let ml2 =
            files
              [
                ("games/Other.ml", "let d = let open Road in straight 1\nlet e = Road.(curve 2)\nlet f = Outer.Inner.g 3\nlet h = Lib.M.h 4\n");
                ("games/Road.ml", "let curve x = x\nlet straight x = x\n");
                ("games/Outer.ml", "let g x = x\nmodule Inner = struct\n  let g x = x + 1\nend\n");
                ("games/M.ml", "let h x = x\n");
              ]
          in
          Alcotest.(check string) "let open M in" "games/Road.ml:2 !" (where ml2 "games/Other.ml" "straight");
          Alcotest.(check string) "M.(e)" "games/Road.ml:1 !" (where ml2 "games/Other.ml" "curve");
          Alcotest.(check string) "M.N.x: N's x in M's file, not M's own x" "games/Outer.ml:3 !" (where ml2 "games/Other.ml" "g");
          Alcotest.(check string) "Lib.M.x, Lib not here: M's own file" "games/M.ml:1 !" (where ml2 "games/Other.ml" "h");
          (* claude: a nested project (a submodule) nearer by its path *)
          let p =
            files [ ("src/main.c", "int f(void) { return helper(1); }\n"); ("src/vendor/h.c", "int helper(int x) { return x; }\n"); ("tools/h.c", "int helper(int x) { return x; }\n") ]
          in
          Alcotest.(check string) "without roots: the nearest path" "src/vendor/h.c:1 tools/h.c:1 !" (where p "src/main.c" "helper");
          Alcotest.(check string) "with roots: its own project first" "tools/h.c:1 src/vendor/h.c:1(other) !"
            (where ~roots:[ ""; "src/vendor" ] p "src/main.c" "helper"));
      (* claude: Code_rank.mli's worked example *)
      Testo.create "a definition's population" (fun () ->
          let files =
            List.map
              (fun (p, src) -> (p, lazy (Code_file.make p src)))
              [
                ("games/Road.ml", "let straight x = x\nlet curve x = straight x\n");
                ("games/Main.ml", "open Road\nlet a = Road.curve 1\nlet b = Road.curve 2\nlet c = straight 3\n");
                ("games/Other.ml", "let d = Road.curve 4\n");
              ]
          in
          let r = Code_rank.compute files in
          let show (u : Code_rank.use) = Printf.sprintf "others %d in %d files, own %d" u.others u.files u.own in
          Alcotest.(check string) "curve" "others 3 in 2 files, own 0" (show (Code_rank.uses r "games/Road.ml" 1 "curve"));
          Alcotest.(check string) "straight" "others 1 in 1 files, own 1" (show (Code_rank.uses r "games/Road.ml" 0 "straight"));
          Alcotest.(check bool) "curve scores above straight" true
            (Code_rank.score r "games/Road.ml" 1 "curve" Def_function > Code_rank.score r "games/Road.ml" 0 "straight" Def_function));
      (* claude: Code_labels.mli's worked example *)
      Testo.create "labels placed once, zoom by zoom" (fun () ->
          let mk text x rank = Code_labels.label Def text ~x ~y:10. ~px:12. ~rank ~from_level:0. ~to_level:9. ~fw:1000. ~fh:1000. (255, 255, 255) in
          let a = mk "important" 10. 10. and b = mk "shadowed" 10. 5. and c = mk "far" 900. 1. in
          Code_labels.place ~level:(fun _ -> 2.) ~dir_level:(fun _ -> 2.) ~zmin:0.5 ~zmax:400. [| a; b; c |];
          Alcotest.(check (float 1e-9)) "the more important from the start" 0.5 a.minz;
          Alcotest.(check bool) "the other at the same point never" true (b.minz = Float.infinity);
          Alcotest.(check (float 1e-9)) "one far away from the start too" 0.5 c.minz;
          Alcotest.(check (float 1e-6)) "placed, it stays placed to the closest zoom" 400. a.maxz;
          Alcotest.(check (float 1e-9)) "shown inside its zooms" 1. (Code_labels.alpha a 10.);
          Alcotest.(check (float 1e-9)) "and not the one never placed" 0. (Code_labels.alpha b 10.));
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
            (Code_file.modules_used "open N\nlet x = List.map f (Playground.foo) (* C.x *)");
          (* claude: TinyScratch's aliases, its modules then named B.x *)
          Alcotest.(check (list string)) "module B = M" [ "B"; "R"; "Scratch_blocks"; "Scratch_run" ]
            (Code_file.modules_used "module B = Scratch_blocks\nmodule R = Scratch_run\nlet x = B.f R.g"));
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
      (* claude: a road's spline starts and ends at its parts, and passes
       * near, not through, the directories between (Holten's bundles) *)
      Testo.create "a road's B-spline" (fun () ->
          let pts = Map_atlas.bspline ~per:4 [| (0., 0.); (10., 10.); (20., 0.) |] in
          let first = List.hd pts and last = List.hd (List.rev pts) in
          Alcotest.(check (list (float 1e-9))) "its ends" [ 0.; 0.; 20.; 0. ] [ fst first; snd first; fst last; snd last ];
          let top = List.fold_left (fun m (_, y) -> Float.max m y) 0. pts in
          Alcotest.(check bool) "the middle pulled towards (10, 10), short of it" true (top > 4. && top < 10.);
          Alcotest.(check (list int)) "the zooms' depths" [ 1; 2; max_int ] [ Map_atlas.depth_at 1.; Map_atlas.depth_at 4.; Map_atlas.depth_at 20. ]);
    ]
