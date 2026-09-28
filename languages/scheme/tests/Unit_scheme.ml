(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_scheme.mli *)

module E = Scheme_eval

(* the value of the last form of [text], written *)
let run ?(style = Scheme.Write) (text : string) : string =
  match E.eval_all (E.create ()) text with Ok v, _ -> Scheme.print style v | Error e, _ -> "error: " ^ e.message

(* the error, and the text it points at *)
let error_at (text : string) : string =
  match E.eval_all (E.create ()) text with
  | Ok v, _ -> "no error, " ^ Scheme.print Write v
  | Error { message; at = Some sp }, _ -> Printf.sprintf "%s [%s]" message (String.sub text sp.start (sp.stop - sp.start))
  | Error { message; at = None }, _ -> message

let check = Alcotest.(check string)

let tests =
  Testo.categorize "Scheme"
    [
      Testo.create "arithmetic, lists, strings" (fun () ->
          check "sum" "6" (run "(+ 1 2 3)");
          check "reals" "2.5" (run "(/ 5 2)");
          check "exact" "3" (run "(/ 6 2)");
          check "a real prints as one" "2.0" (run "(* 1.0 2)");
          check "list" "(1 2 3)" (run "(cons 1 (list 2 3))");
          check "dotted" "(1 . 2)" (run "(cons 1 2)");
          check "strings" {|"ab"|} (run {|(string-append "a" "b")|});
          check "#f is not '()" "#t" (run "(and (not #f) (null? '()) (if '() #t #f))");
          check "equal?" "#t" (run "(equal? (list 1 (list 2)) '(1 (2)))"));
      Testo.create "lexical scope: a closure keeps its variables" (fun () ->
          check "a counter" "3"
            (run "(define (make-counter) (let ([n 0]) (lambda () (set! n (+ n 1)) n))) (define c (make-counter)) (c) (c) (define d (make-counter)) (d) (c)");
          (* Lisp_eval.mli's example the other way round: search-it sees
             the exact of its text, not of its caller *)
          check "not dynamic" "\"exact\"" (run "(define exact #t) (define (search-it) (if exact \"exact\" \"any case\")) (let ([exact #f]) (search-it))"));
      Testo.create "the derived forms" (fun () ->
          check "let*" "3" (run "(let* ([x 1] [y (+ x 1)]) (+ x y))");
          check "named let" "55" (run "(let loop ([i 1] [acc 0]) (if (> i 10) acc (loop (+ i 1) (+ acc i))))");
          check "letrec" "#t" (run "(letrec ([ev? (lambda (n) (if (= n 0) #t (od? (- n 1))))] [od? (lambda (n) (if (= n 0) #f (ev? (- n 1))))]) (ev? 10))");
          check "internal defines" "12" (run "(define (f x) (define y (* x 2)) (define (g) (+ y x)) (g)) (f 4)");
          check "cond" "big" (run "(define (size n) (cond [(< n 10) 'small] [(< n 100) 'medium] [else 'big])) (size 500)");
          check "cond =>-less test clause" "3" (run "(cond [(memv 3 '(1 2)) 'no] [(car '(3))])");
          check "case" "vowel" (run "(case (string-ref \"abc\" 0) [(#\\a #\\e) 'vowel] [else 'consonant])");
          check "quasiquote" "(1 2 3 4)" (run "(define xs '(2 3)) `(1 ,@xs ,(+ 2 2))");
          check "or keeps its value" "3" (run "(or #f 3 (car '()))"));
      Testo.create "the prelude: map, filter, fold, sort" (fun () ->
          check "map" "(2 4 6)" (run "(map (lambda (x) (* 2 x)) '(1 2 3))");
          check "map, two lists" "(5 7 9)" (run "(map + '(1 2 3) '(4 5 6))");
          check "filter" "(2 4)" (run "(filter even? '(1 2 3 4))");
          check "foldl" "(3 2 1)" (run "(foldl cons '() '(1 2 3))");
          check "foldr" "(1 2 3)" (run "(foldr cons '() '(1 2 3))");
          check "sort" "(1 2 3 5 8)" (run "(sort '(5 3 8 1 2) <)");
          check "build-list" "(0 1 4 9)" (run "(build-list 4 (lambda (i) (* i i)))"));
      Testo.create "tail calls: a loop of a hundred thousand, in constant space" (fun () ->
          check "counted down" "done" (run "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (loop 100000)"));
      Testo.create "call/cc: an escape, and a continuation called twice" (fun () ->
          check "escape from a loop" "3"
            (run "(call/cc (lambda (return) (for-each (lambda (x) (if (> x 2) (return x) #f)) '(1 2 3 4)) 'none))");
          (* in one expression: a top-level form's continuation ends with
             that form, in Racket too *)
          check "re-entered" "(1 2 3)"
            (run "(let ([k #f] [seen '()]) (let ([n (+ 1 (call/cc (lambda (c) (set! k c) 0)))]) (set! seen (cons n seen)) (if (< n 3) (k n) (reverse seen))))"));
      Testo.create "define-struct, and the two printing styles" (fun () ->
          let text = "(define-struct posn (x y)) (define p (make-posn 1 2)) (list (posn-x p) (posn? p) p '() #t 'a)" in
          check "Scheme's" "(1 #t #(struct:posn 1 2) () #t a)" (run text);
          check "the teaching languages'" "(list 1 true (make-posn 1 2) empty true 'a)" (run ~style:Constructor text));
      Testo.create "errors: DrScheme's words, and the text at fault" (fun () ->
          check "car" "car: expects argument of type <pair>; given 5 [(car 5)]" (error_at "(car 5)");
          check "undefined" "reference to an undefined identifier: y [y]" (error_at "(define x 1) (+ x y)");
          check "not a procedure" "procedure application: expected procedure, given: 1; arguments were: 2 [(1 2)]" (error_at "(1 2)");
          check "arity" "f: expects 1 argument, given 2: 1 2 [(f 1 2)]" (error_at "(define (f x) x) (f 1 2)");
          check "a selector on the wrong thing" "posn-x: expects argument of type <struct:posn>; given 3 [(posn-x 3)]" (error_at "(define-struct posn (x y)) (posn-x 3)");
          check "cond fell through" "cond: all question results were false [(cond [#f 1])]" (error_at "(cond [#f 1])");
          check "syntax" "define: expected an expression for the function's body, but nothing's there [(define (f x))]" (error_at "(define (f x))");
          check "read" "read: expected a ) to close (, found ] []]" (error_at "(+ 1 ]"));
      Testo.create "display, random, fuel" (fun () ->
          let st = E.create () in
          (match E.eval_all st {|(display "hi") (newline) (display 42)|} with
          | Ok _, st -> check "printed" "hi\n42" (fst (E.take_output st))
          | Error e, _ -> Alcotest.fail e.message);
          check "random, in range" "#t" (run "(andmap (lambda (i) (< -1 (random 6) 6)) (build-list 50 (lambda (i) i)))");
          let e = Scheme_syntax.top (fst (Sexpr_read.read Scheme "(let loop () (loop))" 0)) in
          match E.run ~fuel:1000 (E.start st e) with
          | E.Running, st -> Alcotest.(check bool) "stopped, still alive" true (E.steps st >= 1000)
          | _ -> Alcotest.fail "an endless loop ended");
      Testo.create "big-bang waits for its host, which calls the handlers" (fun () ->
          let st = E.create () in
          match E.eval_all st "(define (tick w) (+ w 1))" with
          | Error e, _ -> Alcotest.fail e.message
          | Ok _, st -> (
              let e = Scheme_syntax.top (fst (Sexpr_read.read Scheme "(* 10 (big-bang 0 [on-tick tick]))" 0)) in
              match E.run (E.start st e) with
              | E.World w, st -> (
                  let tick = List.assoc "on-tick" w.handlers in
                  let w1, st = E.call st tick [ w.init ] in
                  let w1 = match w1 with Ok v -> v | Error e -> Alcotest.fail e.message in
                  check "a tick" "1" (Scheme.print Write w1);
                  match E.run (E.resume st w1) with E.Done v, _ -> check "the last world, returned" "10" (Scheme.print Write v) | _ -> Alcotest.fail "not done")
              | _ -> Alcotest.fail "no world"));
      Testo.create "images: their sizes" (fun () ->
          check "beside" "(30 20)" (run "(define i (beside (circle 10 \"solid\" \"red\") (square 10 'outline 'blue))) (list (image-width i) (image-height i))");
          check "above" "(20 30)" (run "(define i (above (circle 10 \"solid\" \"red\") (square 10 \"solid\" \"blue\"))) (list (image-width i) (image-height i))");
          check "a scene" "(100 60)" (run "(define s (place-image (circle 5 \"solid\" \"red\") 10 10 (empty-scene 100 60))) (list (image-width s) (image-height s))"));
    ]
