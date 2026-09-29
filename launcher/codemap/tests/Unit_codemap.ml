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
      Testo.create "a directory's .codemapignore" (fun () ->
          let c = Code_config.make ~ignore:(Some "# hi\n/gitlog.txt\ntest/\n*_tests.c\n") in
          Alcotest.(check (list bool)) "gitlog.txt, kernel/test/ out; test.c, kernel/gitlog.txt in; lib/io_tests.c out"
            [ true; true; false; false; true ]
            [
              Code_config.ignored c "gitlog.txt" ~dir:false;
              Code_config.ignored c "kernel/test" ~dir:true;
              Code_config.ignored c "test.c" ~dir:false;
              Code_config.ignored c "kernel/gitlog.txt" ~dir:false;
              Code_config.ignored c "lib/io_tests.c" ~dir:false;
            ]);
      (* claude: Code_guide: configs in jsonnet, one per directory, what
       * they say, their anchors found, and the checker's findings *)
      Testo.create "the configs, read and checked" (fun () ->
          let src = "(* A game. *)\n\n(****************************************************************************)\n(* Model *)\n(****************************************************************************)\n\ntype model = int\n\n(* one alien per frame *)\nlet march x = x\nlet view m = march m\n" in
          let files =
            [
              (".codemapconfig", "local c = import 'colors.libsonnet'; { title: 'A test', colors: c, dirs: { games: { summary: 'The games.' } } }");
              ("colors.libsonnet", "{ games: '#e08030' }");
              ( "games/.codemapconfig",
                "{ files: { 'G.ml': { summary: 'The game.', digest: '" ^ Code_guide.digest src ^ "',\n"
                ^ "  capitals: [{ at: 'def:view', say: 'drawn' }], important: [{ at: 'type:model', weight: 3 }, { at: 'comment:\"one alien per frame\"' }, { at: 'section:Model' }] } },\n"
                ^ "  tours: [{ name: 'a walk', stops: [{ at: 'G.ml:def:march' }] }] }" );
              ("games/G.ml", src);
            ]
          in
          let guide, mistakes = Code_guide.load ~read:(fun p -> List.assoc_opt p files) [ ".codemapconfig"; "games/.codemapconfig" ] in
          Alcotest.(check (list string)) "no mistake" [] mistakes;
          Alcotest.(check (option string)) "the title" (Some "A test") (Code_guide.title guide);
          Alcotest.(check (option string)) "games/, said by its parent" (Some "The games.") (Code_guide.dir_summary guide "games");
          Alcotest.(check (option string)) "G.ml" (Some "The game.") (Option.bind (Code_guide.file_note guide "games/G.ml") (fun n -> n.summary));
          Alcotest.(check (list (pair string (list int)))) "the colours, imported" [ ("games", [ 224; 128; 48 ]) ]
            (List.map (fun (p, (r, g, b)) -> (p, [ r; g; b ])) (Code_guide.colours guide));
          let f = Code_file.make "games/G.ml" src in
          Alcotest.(check (list (result int string))) "anchors: def, type, comment, section, line, a miss"
            [ Ok 10; Ok 6; Ok 8; Ok 3; Ok 1; Error "games/G.ml: no def update" ]
            (List.map (Code_guide.find f) [ "def:view"; "type:model"; {|comment:"one alien per frame"|}; "section:Model"; "line:2"; "def:update" ]);
          (* a file, or a directory: a file under it *)
          let there fs p = List.exists (fun (q, _) -> q = p || (String.length q > String.length p && String.sub q 0 (String.length p + 1) = p ^ "/")) fs in
          let check guide = Code_guide.check guide ~file:(fun p -> Option.map (fun s -> (Code_file.make p s, s)) (List.assoc_opt p files)) ~exists:(there files) in
          Alcotest.(check (list (result string string))) "all holds" [] (check guide);
          (* the code edited since: view renamed, a line added *)
          let src' = "(* A game. *)\n\n(****************************************************************************)\n(* Model *)\n(****************************************************************************)\n\ntype model = int\n\n(* one alien per frame *)\nlet march x = x\nlet draw m = march m\n" in
          let files' = List.map (fun (p, s) -> if p = "games/G.ml" then (p, src') else (p, s)) files in
          let found =
            Code_guide.check guide ~file:(fun p -> Option.map (fun s -> (Code_file.make p s, s)) (List.assoc_opt p files')) ~exists:(there files')
          in
          Alcotest.(check (list (result string string))) "a stale digest, an anchor lost"
            [
              Ok ("games/.codemapconfig: G.ml: changed since it was described (digest now " ^ Code_guide.digest src' ^ ")");
              Error "games/.codemapconfig: G.ml: games/G.ml: no def view";
            ]
            found);
      (* claude: Code_ground.mli's worked example, and the weights *)
      Testo.create "the ground: lines as high as they matter" (fun () ->
          let g = Code_ground.layout (Array.append [| 3. |] (Array.make 30 1.)) ~pw:800 ~ph:600 in
          Alcotest.(check int) "one column" 1 g.cols;
          Alcotest.(check (float 0.01)) "the unit, 3% kept" (600. /. (33. *. 1.03)) g.unit;
          Alcotest.(check (float 0.01)) "the header three units" (3. *. g.unit) g.places.(0).h;
          let f = Code_file.make "a.ml" "(* A comment. *)\n\nlet f x =\n  x + 1\n" in
          Alcotest.(check (list (float 0.001))) "a comment, a blank, a header (important: weight 2), a statement, the last newline" [ 0.8; 0.35; 4.5; 1.; 0.35 ]
            (Array.to_list (Code_ground.weights f ~important:[ (2, 2) ]));
          let big = Code_ground.layout (Array.make 1000 1.) ~pw:1600 ~ph:800 in
          Alcotest.(check bool) "a long file in columns, each 80 characters wide at least" true
            (big.cols > 1 && big.colw >= 40. *. big.unit));
      (* claude: the street level: a file's uses of another's names, and
       * the panel of the one used *)
      Testo.create "the street: what a file uses, in its panel" (fun () ->
          let srcs = [ ("game/G.ml", "let go x = Kit.shoot x + Kit.aim x\nlet y = x +. 1.\n"); ("kit/Kit.ml", "let aim x = x\n\nlet shoot x = x\nlet other = 3\n") ] in
          let files = List.map (fun (p, s) -> (p, lazy (Code_file.make p s))) srcs in
          let f = Lazy.force (List.assoc "game/G.ml" files) in
          let edges = Code_street.uses ~index:(Code_names.index files) ~roots:[] ~path:"game/G.ml" f in
          Alcotest.(check (list string)) "shoot and aim, in Kit.ml; no operator"
            [ "aim kit/Kit.ml:0"; "shoot kit/Kit.ml:2" ]
            (List.sort compare (List.map (fun (e : Code_street.edge) -> Printf.sprintf "%s %s:%d" e.name e.target e.target_line) edges));
          let s =
            Code_street.layout ~focus:(Code_ground.weights f ~important:[]) ~file:(fun p -> Option.map Lazy.force (List.assoc_opt p files)) edges ~pw:1000 ~ph:600
          in
          Alcotest.(check (list string)) "one panel, Kit.ml's" [ "kit/Kit.ml" ] (List.map (fun (p : Code_street.panel) -> p.path) s.panels);
          let g = (List.hd s.panels).ground in
          Alcotest.(check bool) "its used definitions tall, the rest thin" true (g.places.(2).h > 3. *. g.places.(3).h && g.places.(0).h > 3. *. g.places.(3).h);
          Alcotest.(check (option (pair string int))) "a pixel on shoot's line" (Some ("kit/Kit.ml", 2))
            (let x, y, _, h = Code_ground.box g 2 in Code_street.line_at s ~focus_path:"game/G.ml" (x +. 5.) (y +. (h /. 2.))));
      (* claude: the repository's own configs: no mistake (tinybox
       * codemap -check . says the same); a file changed since it was
       * described is the checker's warning, not a failure: editing a
       * game must not break the tests *)
      Testo.create "the repository's configs hold" (fun () ->
          let root = "../../.." in
          let sources = Code_deps.repository_sources ~root and configs = Code_deps.repository_configs ~root in
          let paths = List.filter_map (fun (p, _) -> if Filename.basename p = ".codemapconfig" then Some p else None) configs in
          let guide, mistakes = Code_guide.load ~read:(fun p -> List.assoc_opt p configs) paths in
          let found =
            Code_guide.check guide
              ~file:(fun p -> Option.map (fun s -> (Code_file.make p s, s)) (List.assoc_opt p sources))
              ~exists:(fun p -> Sys.file_exists (Filename.concat root p))
          in
          let problems = mistakes @ List.filter_map (function Ok _ -> None | Error e -> Some e) found in
          if problems <> [] then Alcotest.failf "the .codemapconfig files (tinybox codemap -check .):\n  %s" (String.concat "\n  " problems));
      Testo.create "a config's mistakes" (fun () ->
          let load text = snd (Code_guide.load ~read:(fun p -> if p = "d/.codemapconfig" then Some text else None) [ "d/.codemapconfig" ]) in
          Alcotest.(check (list string)) "not a colour" [ {|d/.codemapconfig.colors.kernel: "orange" is no #rrggbb|} ] (load "{ colors: { kernel: 'orange' } }");
          Alcotest.(check (list string)) "a misspelt field"
            [ "d/.codemapconfig: an unknown field summery (known: title, summary, generated, colors, dirs, files, tours, views, layers)" ]
            (load "{ summery: 'x' }");
          Alcotest.(check (list string)) "jsonnet's own" [ "d/.codemapconfig:1: expected ,, not b" ] (load "{ a: 1 b: 2 }"));
      (* claude: Code_layers' worked example *)
      Testo.create "layers: the users above the used" (fun () ->
          let tree = Treemap.of_paths [ ("games/A.ml", 10., ()); ("playground/P.ml", 10., ()); ("libs/L.ml", 10., ()) ] in
          let band =
            Code_layers.compute [ ("games/A.ml", "playground/P.ml", 3); ("games/A.ml", "libs/L.ml", 1); ("playground/P.ml", "libs/L.ml", 2) ] tree
          in
          Alcotest.(check (list int)) "games/, playground/, libs/" [ 0; 1; 2 ] (List.map band [ "games"; "playground"; "libs" ]));
      (* claude: a road's spline starts and ends at its parts, and passes
       * near, not through, the directories between (Holten's bundles) *)
      Testo.create "a road's B-spline" (fun () ->
          let pts = Map_atlas.bspline ~per:4 [| (0., 0.); (10., 10.); (20., 0.) |] in
          let first = List.hd pts and last = List.hd (List.rev pts) in
          Alcotest.(check (list (float 1e-9))) "its ends" [ 0.; 0.; 20.; 0. ] [ fst first; snd first; fst last; snd last ];
          let top = List.fold_left (fun m (_, y) -> Float.max m y) 0. pts in
          Alcotest.(check bool) "the middle pulled towards (10, 10), short of it" true (top > 4. && top < 10.);
          Alcotest.(check (list int)) "the zooms' depths" [ 1; 2; max_int ] [ Map_atlas.depth_at 1.; Map_atlas.depth_at 4.; Map_atlas.depth_at 20. ]);
      (* claude: Code_units' worked example *)
      Testo.create "units: in, out, beside" (fun () ->
          let placed =
            Array.of_list
              (Treemap.layout Ordered { x = 0.; y = 0.; w = 100.; h = 50. }
                 (Treemap.of_paths [ ("kernel/a.ml", 30., ()); ("kernel/b.ml", 30., ()); ("lib/c.ml", 40., ()) ]))
          in
          let at path = let r = ref (-1) in Array.iteri (fun i (p : unit Treemap.placed) -> if p.path = path then r := i) placed; !r in
          let kernel = at "kernel" and lib = at "lib" and a = at "kernel/a.ml" in
          let ar = placed.(a).rect in
          let u, v = (ar.x +. (ar.w /. 2.), ar.y +. (ar.h /. 2.)) in
          Alcotest.(check (option int)) "kernel's parent, the root" (Some 0) (Code_units.parent placed kernel);
          Alcotest.(check (option int)) "from the root toward a.ml: kernel" (Some kernel) (Code_units.toward placed 0 u v);
          Alcotest.(check (option int)) "then a.ml" (Some a) (Code_units.toward placed kernel u v);
          Alcotest.(check bool) "kernel beside lib" true
            (Code_units.sibling placed kernel Right = Some lib || Code_units.sibling placed kernel Down = Some lib);
          Alcotest.(check (list int)) "a.ml's ancestors" [ 0; kernel; a ] (Code_units.ancestors placed a));
      (* claude: Map_v2's names are clickable: a region's, at its centre,
       * is the region, not a file under it *)
      Testo.create "v2: a directory's name clicked" (fun () ->
          let entry path n = { Code_map_base.path; nlines = n; file = lazy (Code_file.make path (String.concat "\n" (List.init n (fun _ -> "let x = 1")))) } in
          let t =
            Code_map_base.make ~style:Map_v2.style ~area:(0., 0., 800, 600) ~title:"t" ~marked:[]
              [ entry "kernel/a.ml" 300; entry "kernel/b.ml" 300; entry "lib/c.ml" 100 ]
          in
          let at = ref (-1) in
          Array.iteri (fun i (p : Code_map_base.entry Treemap.placed) -> if p.path = "kernel" then at := i) t.placed;
          let r = t.placed.(!at).rect in
          let cx = Code_map_base.to_px t.cam (r.x +. (r.w /. 2.)) and cy = Code_map_base.to_py t.cam (r.y +. (r.h /. 2.)) in
          Alcotest.(check (option int)) "kernel" (Some !at) (Map_v2.unit_at t t.cam 1. cx cy);
          Alcotest.(check (option int)) "nothing at a corner" None (Map_v2.unit_at t t.cam 1. 1. 1.));
    ]
