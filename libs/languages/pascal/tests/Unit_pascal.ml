(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_pascal.mli *)

let check = Alcotest.(check string)
let disk name = List.assoc name Pascal_disk.files
let run ?(answers = []) (source : string) : string = Pmachine.execute source answers

(* a program of a few statements, around its declarations *)
let program ?(decls = "") (body : string) : string = Printf.sprintf "program T;\n%s\nbegin\n%s\nend.\n" decls body

let code (source : string) : string list =
  match Pascal_compile.compile source with
  | Ok p -> Array.to_list (Array.map Pcode.show p.code)
  | Error e -> Alcotest.fail e.message

let contains (s : string) (sub : string) : bool =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

let last_line (s : string) : string =
  match List.rev (List.filter (( <> ) "") (String.split_on_char '\n' s)) with l :: _ -> l | [] -> ""

(* the debugger's helpers: a program started, a step taken to its
   pause, the line of a text in the source *)
let debug (source : string) : Pcode.program * Pmachine.machine =
  match Pascal_compile.compile source with Ok p -> (p, Pmachine.start p) | Error e -> Alcotest.fail e.message

let step (p : Pcode.program) (m : Pmachine.machine) (s : Pdebug.step) : unit =
  match Pmachine.resume ~pause:(Pdebug.pause_for p s m) m 1_000_000 with
  | Paused -> ()
  | _ -> Alcotest.fail "no pause"

let line_of (source : string) (text : string) : int =
  let rec go i = function [] -> Alcotest.fail text | l :: rest -> if contains l text then i else go (i + 1) rest in
  go 1 (String.split_on_char '\n' source)

let tests =
  Testo.categorize "Pascal"
    [
      Testo.create "the debugger: Wirth's queens paused at a breakpoint, its calls and watches" (fun () ->
          let src = disk "QUEENS.PAS" in
          let p, m = debug src in
          step p m Trace_into;
          Alcotest.(check int) "stopped at the main begin" (line_of src "  for i := 1 to 8 do a[i] := true;" - 1) (Pdebug.line p m);
          let bp = line_of src "x[j] := i;" in
          step p m (Continue [ bp ]);
          step p m (Continue [ bp ]);
          Alcotest.(check int) "the breakpoint" bp (Pdebug.line p m);
          check "j" "2" (Pdebug.watch p m "j");
          check "i" "3" (Pdebug.watch p m "I");
          check "x, one queen placed" "(1,0,0,0,0,0,0,0)" (Pdebug.watch p m "x");
          check "a[1], its row taken" "FALSE" (Pdebug.watch p m "a[1]");
          check "c[j - i] by a variable" "TRUE" (Pdebug.watch p m "c[j]");
          check "unknown" "Unknown identifier: nope" (Pdebug.watch p m "nope");
          let fs = Pdebug.frames p m in
          Alcotest.(check (list string)) "the calls" [ "TRY(2)"; "TRY(1)"; "QUEENS" ] (List.map (Pdebug.call p m) fs);
          Alcotest.(check (list int)) "every TRY's static link: the main frame" [ 0; 0 ] (List.map (fun (f : Pdebug.frame) -> f.static_link) (List.filteri (fun i _ -> i < 2) fs));
          Alcotest.(check bool) "the dynamic link: the caller's frame" true ((List.hd fs).dynamic_link = (List.nth fs 1).base));
      Testo.create "the debugger: F8 steps over a call, F7 goes into it; a watch through a static link" (fun () ->
          let src = disk "SCOPES.PAS" in
          let p, m = debug src in
          step p m Trace_into;
          step p m Trace_into;
          Alcotest.(check int) "sum(10)" (line_of src "  sum(10);") (Pdebug.line p m);
          step p m Step_over;
          Alcotest.(check int) "stepped over: sum(100)" (line_of src "  sum(100)") (Pdebug.line p m);
          step p m Trace_into;
          Alcotest.(check (list string)) "into sum" [ "SUM(100)"; "SCOPES" ] (List.map (Pdebug.call p m) (Pdebug.frames p m));
          step p m (To_line (line_of src "if k > 1 then add(k - 1)"));
          step p m (To_line (line_of src "if k > 1 then add(k - 1)"));
          check "total, sum's, seen from add" "199" (Pdebug.watch p m "total");
          check "k" "99" (Pdebug.watch p m "k"));
      Testo.create "the debugger: a breakpoint in a loop's body, hit at each turn" (fun () ->
          let src = program ~decls:"var i: integer;" "i := 0;\nwhile i < 3 do\n  i := i + 1;\nwriteln(i)" in
          let p, m = debug src in
          let hits = ref [] in
          (try
             while true do
               step p m (Continue [ 6 ]);
               hits := Pdebug.watch p m "i" :: !hits
             done
           with _ -> ());
          Alcotest.(check (list string)) "three turns" [ "0"; "1"; "2" ] (List.rev !hits));
      Testo.create "the lexer: Pascal_lexer.mli's example" (fun () ->
          let toks = Pascal_lexer.tokens "x := a[1] + 'z'; { done }" in
          check "tokens" "x ':=' a '[' 1 ']' '+' 'z' ';' the end of the text"
            (String.concat " " (List.map (fun (t : Pascal_lexer.token) -> Pascal_lexer.show t.kind) toks));
          check "case doesn't count" "begin writeln" (String.concat " " (List.map (fun (t : Pascal_lexer.token) -> Pascal_lexer.show t.kind) (List.filteri (fun i _ -> i < 2) (Pascal_lexer.tokens "BEGIN WriteLn")))));
      Testo.create "the code of x := a + b * 2: Pcode.mli's example, reverse Polish" (fun () ->
          let c = code (program ~decls:"var x, a, b: integer;" "x := a + b * 2") in
          Alcotest.(check (list string)) "code" [ "ujp 1"; "ent 7"; "lod 0,5"; "lod 0,6"; "ldc 2"; "mpi"; "adi"; "str 0,4"; "stp" ]
            (List.map (fun s -> String.concat " " (List.filter (( <> ) "") (String.split_on_char ' ' s))) c));
      Testo.create "the disk: every program's output" (fun () ->
          check "hello" "Hello, world!\n" (run (disk "HELLO.PAS"));
          check "fact" " 7! =  5040" (last_line (run (disk "FACT.PAS")));
          check "sieve" "46 primes below 200" (last_line (run (disk "SIEVE.PAS")));
          check "hanoi" "7 moves" (last_line (run (disk "HANOI.PAS")));
          let q = run (disk "QUEENS.PAS") in
          check "queens: Wirth's first solution" "  1  5  8  6  3  7  2  4" (List.hd (String.split_on_char '\n' q));
          check "queens: all of them" "92 solutions" (last_line q);
          check "scopes" "1 + ... + 10 = 55\n1 + ... + 100 = 5050\n" (run (disk "SCOPES.PAS"));
          check "parity" "7 is odd: TRUE\n10 is odd: FALSE\n" (run (disk "PARITY.PAS"));
          check "points" "area: 12\ncorner 3 is now at (14, 23)\narea: 12\n" (run (disk "POINTS.PAS"));
          (* 1, 2, 3...: the number of tries is the secret itself *)
          let g = run ~answers:(List.init 100 (fun i -> string_of_int (i + 1))) (disk "GUESS.PAS") in
          Alcotest.(check bool) g true (contains (last_line g) "Right, in "));
      Testo.create "static links: the inner procedure reaches the outer's variable one level out" (fun () ->
          let c = code (disk "SCOPES.PAS") in
          Alcotest.(check bool) "lod 1,5" true (List.mem "lod 1,5" c);
          Alcotest.(check bool) "str 1,5" true (List.mem "str 1,5" c));
      Testo.create "the compiler's first error, where it is" (fun () ->
          check "a semicolon" "Error at 3:1: ';' expected, not begin\n" (run "program T;\nvar x: integer\nbegin\nx := 1\nend.\n");
          check "unknown" "Error at 3:1: Unknown identifier y\n" (run "program T;\nbegin\ny := 1\nend.\n");
          check "types" "Error at 4:10: Type mismatch: integer expected, not boolean\n" (run (program ~decls:"var x: integer;" "x := 1 + (2 = 2)"));
          check "forward, never given" "Error at 2:11: p was declared forward, but its body never came\n"
            (run "program T;\nprocedure p; forward;\nbegin p end.\n"));
      Testo.create "run-time errors, Turbo Pascal's numbers" (fun () ->
          check "201" "Runtime error 201 at line 5: Range check error" (last_line (run (program ~decls:"var a: array[1..3] of integer; i: integer;" "i := 4;\na[i] := 0")));
          check "200" "Runtime error 200 at line 5: Division by zero" (last_line (run (program ~decls:"var i: integer;" "i := 0;\ni := 1 div i")));
          check "202" "Runtime error 202 at line 2: Stack overflow"
            (last_line (run "program T;\nprocedure p; begin p end;\nbegin p end.\n"));
          check "a subrange" "Runtime error 201 at line 5: Range check error" (last_line (run (program ~decls:"var d: 1..6;" "d := 6;\nd := d + 1"))));
      Testo.create "16 bits: maxint + 1 wraps around" (fun () ->
          check "wrap" "-32768\n" (run (program ~decls:"var i: integer;" "i := maxint;\nwriteln(i + 1)")));
      Testo.create "reading: two numbers on a line, readln skipping the rest, a character" (fun () ->
          let src = program ~decls:"var a, b: integer; c: char;" "read(a, b); readln;\nread(c);\nwriteln(a + b, c)" in
          check "read" "3 4 junk\nxyz\n7x\n" (run ~answers:[ "3 4 junk"; "xyz" ] src);
          (* the question printed before the wait for its answer *)
          check "lazy" "? 5\n5\n" (run ~answers:[ "5" ] (program ~decls:"var a: integer;" "write('? '); readln(a); writeln(a)")));
      Testo.create "arrays are values: copied when assigned (stm) and passed (ldm); a[i, j]" (fun () ->
          let src =
            program ~decls:"type v = array[1..3] of integer;\nvar a, b: v; m: array[1..2, 1..3] of integer; i, j: integer;\nprocedure zero(x: v); begin x[1] := 0 end;"
              "a[1] := 5; b := a; b[1] := 7; zero(a);\nfor i := 1 to 2 do for j := 1 to 3 do m[i, j] := 10 * i + j;\nwriteln(a[1], ' ', b[1], ' ', m[2, 3], ' ', m[1, 2])"
          in
          check "copies" "5 7 23 12\n" (run src));
      Testo.create "case, with Turbo's else" (fun () ->
          let src =
            program ~decls:"var i: integer;"
              "for i := 1 to 4 do\n case i of\n 1: write('one ');\n 2, 3: write('few ')\n else write('many')\n end;\nwriteln"
          in
          check "case" "one few few many\n" (run src));
    ]
