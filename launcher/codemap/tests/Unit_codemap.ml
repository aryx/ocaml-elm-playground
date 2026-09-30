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
      (* claude: a literate program's markers (principia's C): the names
       * shown without them, an anchor on one landing on its code, a peek
       * without the /*s: ... */ above nor the /*e: ... */ below *)
      Testo.create "syncweb markers: names, anchors, peeks" (fun () ->
          let src =
            String.concat "\n"
              [
                "/*s: struct [[Mouseinfo]] */"; "struct Mouseinfo"; "{"; "    int x;"; "};"; "/*e: struct [[Mouseinfo]] */"; "/*s: struct [[Window]] */"; "struct Window"; "{";
                "    int id;"; "};"; "/*e: struct [[Window]] */"; ""; "/*s: function [[wmk]] */"; "// makes a window"; "Window*"; "wmk(int i)"; "{"; "    /*s: [[wmk()]] locals */";
                "    Window *w;"; "    /*e: [[wmk()]] locals */"; "    return w;"; "}"; "/*e: function [[wmk]] */"; "";
              ]
          in
          let f = Code_file.make "rio/dat.c" src in
          Alcotest.(check (list string)) "names" [ "Window"; "_start"; "view"; "advance"; "one alien" ]
            (List.map Code_guide.anchor_name [ {|dat.h:comment:"struct [[Window]]"|}; {|comment:"function [[_start]](arm)"|}; "def:view"; "Shots.ml:def:advance"; {|comment:"one alien"|} ]);
          Alcotest.(check (list bool)) "markers" [ true; true; true; false; false ] (List.map (Code_file.syncweb_marker f) [ 6; 18; 23; 14; 7 ]);
          Alcotest.(check (result int string)) "an anchor on a marker: the code under it" (Ok 7) (Code_guide.find f {|comment:"struct [[Window]]"|});
          (* the struct without the markers around it; the function from
           * its comment and return type, under its marker, to its brace;
           * a line inside it, the same *)
          Alcotest.(check (list (pair int int))) "peeks" [ (7, 10); (14, 22); (14, 22) ] (List.map (Code_map.peek_extent f "rio/dat.c") [ 7; 16; 19 ]));
      (* claude: the calls' reach and the call stack (Code_rank.places):
       * a main calling B.f calling C.g; a let () = body its own, not the
       * definition above it's; kept through the bundle's text *)
      Testo.create "reach and the call stack" (fun () ->
          let files =
            List.map
              (fun (p, src) -> (p, lazy (Code_file.make p src)))
              [ ("a/A.ml", "let helper () = 0\nlet main () = B.f ()\nlet () = ignore (C.g ())\n"); ("b/B.ml", "let f () = C.g ()\n"); ("c/C.ml", "let g () = 1\n") ]
          in
          let r = Code_rank.compute files in
          Alcotest.(check (list int)) "reach: main 2 files, f 1, g 0, helper 0" [ 2; 1; 0; 0 ]
            [ Code_rank.reach r "a/A.ml" 1; Code_rank.reach r "b/B.ml" 0; Code_rank.reach r "c/C.ml" 0; Code_rank.reach r "a/A.ml" 0 ];
          let place p l = match List.find_opt (fun (p', l', _) -> p' = p && l' = l) (Code_rank.places r) with Some (_, _, (x : Code_rank.place)) -> (x.depth, x.height) | None -> (-1, -1) in
          Alcotest.(check (list (pair int int))) "depth and height: main at the top, g at the bottom" [ (0, 2); (1, 1); (2, 0) ]
            [ place "a/A.ml" 1; place "b/B.ml" 0; place "c/C.ml" 0 ];
          let r' = Code_rank.of_string (Code_rank.to_string r) in
          Alcotest.(check (list int)) "through the text" [ 2; 1 ] [ Code_rank.reach r' "a/A.ml" 1; Code_rank.reach r' "b/B.ml" 0 ]);
      (* claude: the roles layer's worked examples (Code_roles.mli) *)
      Testo.create "roles: words, names and structure" (fun () ->
          Alcotest.(check (list string)) "words" [ "games"; "tinyvisicalc"; "tiny"; "visi"; "calc"; "ml" ] (Code_roles.words "games/TinyVisiCalc.ml");
          let paths = [ "kernel/pc/l.s"; "lib/Parser.ml"; "lib/Parser.mly"; "tests/Unit_rank.ml"; "libs/networking/Http.ml"; "games/TinyMario.ml"; "playground/Playground.ml"; "sparse/Matrix.ml"; "lib/Parser.mli" ] in
          let links = [ ("games/TinyMario.ml", "playground/Playground.ml", 3); ("playground/Playground.ml", "sparse/Matrix.ml", 1) ] in
          let tbl = Code_roles.categories ~links paths in
          Alcotest.(check (list string)) "roles"
            [ "per CPU"; "generated"; "parsing"; "tests"; "network"; "entry points"; "the rest"; "the rest"; "generated" ]
            (List.map (fun p -> Code_roles.name (Hashtbl.find tbl p)) paths));
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
          Alcotest.(check string) "error: its own program's definition (sam's, another program's, not linkable)" "rc/subr.c:2 lib/error.c:1 !" (where c "rc/exec.c" "error");
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
          Alcotest.(check string) "two Parser.ml: the one beside it, alone" "games/Parser.ml:1 !" (where ml "games/Main.ml" "parse");
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
          Alcotest.(check string) "without roots: the nearest path, tools/ not linkable" "src/vendor/h.c:1 !" (where p "src/main.c" "helper");
          Alcotest.(check string) "with roots: another project last (tools/ not linkable)" "src/vendor/h.c:1(other) !"
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
      (* claude: Code_bundle.mli's worked example *)
      Testo.create "a directory's code as one file, and back" (fun () ->
          let b : Code_bundle.t =
            { name = "ix"; roots = [ "" ]; sources = [ ("a.ml", "let x = 1") ]; configs = [ ".codemapconfig" ]; jsonnet = [ (".codemapconfig", "{}") ]; rank = Some "L\ta.ml\tb.ml\t2\n" }
          in
          Alcotest.(check bool) "the same" true (Code_bundle.of_string (Code_bundle.to_string b) = b));
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
            Code_street.layout ~mode:Uses ~focus_path:"game/G.ml" ~focus:(Code_ground.weights f ~important:[]) ~file:(fun p -> Option.map Lazy.force (List.assoc_opt p files))
              ~uses:edges ~users:[] ~pw:1000 ~ph:600 ()
          in
          Alcotest.(check (list string)) "one panel on the left, Kit.ml's" [ "kit/Kit.ml" ] (List.map (fun (p : Code_street.panel) -> p.path) s.left);
          Alcotest.(check bool) "the focus right of it" true (s.focus.ox > (List.hd s.left).ground.ox);
          let g = (List.hd s.left).ground in
          Alcotest.(check bool) "its used definitions tall, the rest thin" true (g.places.(2).h > 3. *. g.places.(3).h && g.places.(0).h > 3. *. g.places.(3).h);
          Alcotest.(check (option (pair string int))) "a pixel on shoot's line" (Some ("kit/Kit.ml", 2))
            (let x, y, _, h = Code_ground.box g 2 in Code_street.line_at s (x +. 5.) (y +. (h /. 2.)));
          (* and the other way: G.ml's users of Kit.ml, on Kit.ml's right *)
          let k = Lazy.force (List.assoc "kit/Kit.ml" files) in
          let r =
            Code_street.layout ~mode:Users ~focus_path:"kit/Kit.ml" ~focus:(Code_ground.weights k ~important:[]) ~file:(fun p -> Option.map Lazy.force (List.assoc_opt p files))
              ~uses:[] ~users:edges ~pw:1000 ~ph:600 ()
          in
          Alcotest.(check (list string)) "G.ml, its user, on the right" [ "game/G.ml" ] (List.map (fun (p : Code_street.panel) -> p.path) r.right));
      (* claude: the repository's own configs: no mistake (tinybox
       * codemap -check . says the same); a file changed since it was
       * described is the checker's warning, not a failure: editing a
       * game must not break the tests *)
      Testo.create "the repository's configs hold" (fun () ->
          let root = "../../.." in
          let sources = Code_deps.repository_sources ~root and configs = Code_deps.repository_configs ~root in
          let paths = List.filter_map (fun (p, _) -> if Filename.basename p = ".codemapconfig" then Some p else None) configs in
          let guide, mistakes = Code_guide.load ~read:(fun p -> List.assoc_opt p configs) paths in
          (* claude: a generated file (Photos.mli) is no source of tinybox's
           * but is one of tinybox codemap's, read from the disk; a path not
           * of a source (a related note, a directory of pages) is not
           * among the test's deps in _build: -check looks for those *)
          let read p = In_channel.with_open_bin (Filename.concat root p) In_channel.input_all in
          let source p = Filename.check_suffix p ".ml" || Filename.check_suffix p ".mli" in
          let found =
            Code_guide.check guide
              ~file:(fun p ->
                match List.assoc_opt p sources with
                | Some s -> Some (Code_file.make p s, s)
                | None -> if source p && Sys.file_exists (Filename.concat root p) then (let s = read p in Some (Code_file.make p s, s)) else None)
              ~exists:(fun p -> (not (source p)) || Sys.file_exists (Filename.concat root p))
          in
          let problems = mistakes @ List.filter_map (function Ok _ -> None | Error e -> Some e) found in
          if problems <> [] then Alcotest.failf "the .codemapconfig files (tinybox codemap -check .):\n  %s" (String.concat "\n  " problems));
      (* claude: a skeleton from a template, extended, across files *)
      Testo.create "a skeleton, from a template, across files" (fun () ->
          let files =
            [
              ("skeletons.libsonnet", "{ loop(f):: { name: 'Loop', bones: [{ at: f + ':def:step', role: 'a step' }, { at: f + ':type:state', role: 'the state' }], joints: [{ from: f + ':def:step', to: f + ':type:state' }] } }");
              ( "game/.codemapconfig",
                "local s = import '../skeletons.libsonnet'; { skeletons: [s.loop('G.ml') + { bones+: [{ at: '../kit/K.ml:def:move', role: 'a move' }], joints+: [{ from: 'G.ml:def:step', to: '../kit/K.ml:def:move' }] }] }" );
            ]
          in
          let guide, mistakes = Code_guide.load ~read:(fun p -> List.assoc_opt p files) [ "game/.codemapconfig" ] in
          Alcotest.(check (list string)) "no mistake" [] mistakes;
          match Code_guide.skeletons_of guide "kit/K.ml" with
          | [ sk ] ->
              Alcotest.(check (list string)) "its bones, from the root" [ "game/G.ml def:step"; "game/G.ml type:state"; "kit/K.ml def:move" ]
                (List.map (fun (b : Code_guide.bone) -> b.bpath ^ " " ^ b.banchor) sk.bones);
              Alcotest.(check int) "two joints" 2 (List.length sk.joints)
          | _ -> Alcotest.fail "one skeleton reaching kit/K.ml");
      (* claude: Code_anatomy.mli's worked example, and the skin *)
      Testo.create "the anatomy: nerves, lungs, muscles, skin" (fun () ->
          let src =
            "let update computer m = if computer.keyboard.space then fire m else m\nlet save () = Out_channel.with_open_text \"f\" (fun oc -> ())\n(* the keyboard, in a comment *)\nlet sum l = List.fold_left ( + ) 0 l\n"
          in
          let f = Code_file.make "g.ml" src in
          let fs = Code_anatomy.facts f ~public:(Some [ "update"; "sum" ]) in
          Alcotest.(check (list int)) "the nerves: line 0, not the comment's" [ 0 ] fs.nerves;
          Alcotest.(check (list int)) "the lungs: line 1" [ 1 ] fs.lungs;
          Alcotest.(check (list int)) "the skin: what the .mli shows" [ 0; 3 ] fs.skin;
          let strength l = List.fold_left (fun acc (a, _, st) -> if a = l then st else acc) 0. fs.muscles in
          Alcotest.(check bool) "the muscles: sum's loop works more than update's none" true (strength 3 > strength 0));
      (* claude: codellm's evidence, a directory's brief *)
      Testo.create "the facts: a directory's brief" (fun () ->
          let sources =
            [ ("g/A.ml", "(* Claude Code\n * Copyright (C) 2026\n *)\n(* A game: the heart is march. *)\nlet march x = Kit.step x\nlet main = march 1\n"); ("k/Kit.ml", "let step x = x + 1\n") ]
          in
          let b = Code_facts.brief ~guide:Code_guide.empty ~sources ~dir:"g" in
          let has s = Alcotest.(check bool) s true (let n = String.length s and m = String.length b in let rec go i = i + n <= m && (String.sub b i n = s || go (i + 1)) in go 0) in
          has "# g";
          has "A game: the heart is march.";
          has "def:march, line 5";
          has "Uses: k/Kit.ml (1)";
          has "A program: its entry (a main, Cap.main).");
      Testo.create "a config's mistakes" (fun () ->
          let load text = snd (Code_guide.load ~read:(fun p -> if p = "d/.codemapconfig" then Some text else None) [ "d/.codemapconfig" ]) in
          Alcotest.(check (list string)) "not a colour" [ {|d/.codemapconfig.colors.kernel: "orange" is no #rrggbb|} ] (load "{ colors: { kernel: 'orange' } }");
          Alcotest.(check (list string)) "a misspelt field"
            [ "d/.codemapconfig: an unknown field summery (known: title, summary, generated, colors, dirs, files, tours, skeletons, views, marks, anatomy)" ]
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
      (* claude: the search (/): names, paths, directories, name//, Tab *)
      Testo.create "search: what a query finds" (fun () ->
          let all =
            Code_search.candidates ~dirs:[ "games"; "games/arm"; "libs"; "libs/arm"; "libs/armour" ]
              ~files:[ "games/arm/Step.ml"; "libs/arm/Tiny_invaders.ml" ]
              ~defs:[ ("games/arm/Step.ml", 3, "step"); ("games/arm/Step.ml", 9, "step_ball"); ("libs/arm/Tiny_invaders.ml", 1, "make_step") ]
              ()
          in
          let found q = List.map (fun (h : Code_search.hit) -> h.path ^ (if h.kind = Def then ":" ^ h.name else "")) (Code_search.matches all q) in
          Alcotest.(check (list string)) "step: the file named so, then its definitions, a word's start last"
            [ "games/arm/Step.ml"; "games/arm/Step.ml:step"; "games/arm/Step.ml:step_ball"; "libs/arm/Tiny_invaders.ml:make_step" ] (found "step");
          Alcotest.(check (list string)) "invad: inside a word" [ "libs/arm/Tiny_invaders.ml" ] (found "invad");
          Alcotest.(check (list string)) "libs/step: under libs" [ "libs/arm/Tiny_invaders.ml:make_step" ] (found "libs/step");
          Alcotest.(check (list string)) "arm/: the directories" [ "games/arm"; "libs/arm"; "libs/armour" ] (found "arm/");
          Alcotest.(check (list string)) "arm//: those named so, together" [ "games/arm"; "libs/arm" ] (Code_search.all_named all "arm//");
          Alcotest.(check string) "Tab: as far as the hits agree" "step" (Code_search.complete (Code_search.matches all "ste") "ste");
          Alcotest.(check string) "Tab: a directory's slash" "games/" (Code_search.complete (Code_search.matches all "gam") "gam"));
      (* claude: a config's mark, its colours named by jsonnet locals; the
       * old name, layers:, still read *)
      Testo.create "a config's mark" (fun () ->
          let config = "local fork_color = '#e05050';\n{ layers: [{ name: 'Capabilities', rules: [{ text: 'Cap.fork', color: fork_color, say: 'forks' }] }] }" in
          let g, errs = Code_guide.load ~read:(fun p -> if p = ".codemapconfig" then Some config else None) [ ".codemapconfig" ] in
          Alcotest.(check (list string)) "no mistake" [] errs;
          match Code_guide.marks g with
          | [ { mname = "Capabilities"; rules = [ { text = "Cap.fork"; is_ref = false; colour; rsay = Some "forks" } ]; _ } ] ->
              Alcotest.(check (triple int int int)) "its colour, named" (0xe0, 0x50, 0x50) colour
          | _ -> Alcotest.fail "the mark");
      (* claude: a config's views and tours, their paths from the root *)
      Testo.create "views and tours, from the root" (fun () ->
          let config =
            "{ views: [{ name: 'kit', files: ['A.ml', '../../kits/k/'] }], tours: [{ name: 't', stops: [{ at: 'A.ml:def:f', say: 'f' }, { at: '../../kits/k/K.ml:def:g' }] }] }"
          in
          let g, errs = Code_guide.load ~read:(fun p -> if p = "games/g/.codemapconfig" then Some config else None) [ "games/g/.codemapconfig" ] in
          Alcotest.(check (list string)) "no mistake" [] errs;
          Alcotest.(check (list (list string))) "the view's files" [ [ "games/g/A.ml"; "kits/k" ] ] (List.map (fun (v : Code_guide.view) -> v.files) (Code_guide.views g));
          Alcotest.(check (list (list string))) "the tour's stops" [ [ "games/g/A.ml:def:f"; "kits/k/K.ml:def:g" ] ]
            (List.map (fun (tr : Code_guide.tour) -> List.map (fun (i : Code_guide.item) -> i.at) tr.stops) (Code_guide.tours g)));
      (* claude: @name, the code's references, not the comments' words *)
      Testo.create "search: references" (fun () ->
          let files = [ ("a.ml", [ (0, "Cap.fork"); (2, "CapUnix.fork"); (2, "Unix.fork") ], [| "f Cap.fork"; "(* Cap.fork *)"; "g Unix.fork" |]) ] in
          let lines q = List.map (fun (h : Code_search.hit) -> h.line) (Code_search.ref_matches files q) in
          Alcotest.(check (list int)) "Cap.fork: the code's, once a line" [ 0 ] (lines "Cap.fork");
          Alcotest.(check (list int)) "fork: any path ending so" [ 0; 2 ] (lines "fork");
          Alcotest.(check (option string)) "the query" (Some "Cap.fork") (Code_search.ref_query "@Cap.fork"));
      (* claude: one hit for a definition and its .mli's; the near first *)
      Testo.create "search: an .mli's twin, the near first" (fun () ->
          let all = Code_search.candidates ~dirs:[] ~files:[] ~defs:[ ("b/B.ml", 1, "step"); ("b/B.mli", 1, "step"); ("a/A.ml", 1, "step"); ("c/C.mli", 1, "step") ] () in
          let paths ?near q = List.map (fun (h : Code_search.hit) -> h.path) (Code_search.matches ?near all q) in
          Alcotest.(check (list string)) "B.mli left out, C.mli kept" [ "a/A.ml"; "b/B.ml"; "c/C.mli" ] (paths "step");
          Alcotest.(check (list string)) "b near" [ "b/B.ml"; "a/A.ml"; "c/C.mli" ] (paths ~near:(fun p -> Code_search.starts p "b/") "step"));
    ]
