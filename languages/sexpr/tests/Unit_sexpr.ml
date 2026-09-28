(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_sexpr.mli *)

let check = Alcotest.(check string)
let read d s = Sexpr.to_string (fst (Sexpr_read.read d s 0))

let error_at d s : string =
  match Sexpr_read.read d s 0 with _ -> "no error" | exception Sexpr_read.Error (msg, pos) -> Printf.sprintf "%s at %d" msg pos

let tests =
  Testo.categorize "Sexpr"
    [
      Testo.create "Sexpr_read.mli's example, and the spans" (fun () ->
          let x, pos = Sexpr_read.read Emacs "(defun double (x) (+ x x)) ; twice" 0 in
          check "value" "(defun double (x) (+ x x))" (Sexpr.to_string x);
          Alcotest.(check int) "just after" 26 pos;
          Alcotest.(check (pair int int)) "its span" (0, 26) (x.span.start, x.span.stop);
          match x.datum with
          | List ([ _; name; _; body ], None) ->
              Alcotest.(check (pair int int)) "double" (7, 13) (name.span.start, name.span.stop);
              Alcotest.(check (pair int int)) "the body" (18, 25) (body.span.start, body.span.stop)
          | _ -> Alcotest.fail "not a list of four");
      Testo.create "both dialects: quote, dotted lists, strings" (fun () ->
          check "quote" "'(a . b)" (read Scheme "'(a . b)");
          check "its span includes the quote" "0-3" (let x, _ = Sexpr_read.read Scheme " 'a" 0 in Printf.sprintf "%d-%d" (x.span.start - 1) x.span.stop);
          check "dotted" "(1 2 . 3)" (read Emacs "(1 2 . 3)");
          check "a string" {|"say \"hi\""|} (read Scheme {|"say \"hi\""|}));
      Testo.create "Emacs: characters and keys" (fun () ->
          check "?a ?\\n ?\\C-a" "(#\\a #\\\n #\\\001)" (read Emacs {|(?a ?\n ?\C-a)|});
          check "keys" (Printf.sprintf "%S" "\x18\x13\x1bx") (read Emacs {|"\C-x\C-s\M-x"|});
          check "#'" "(function car)" (read Emacs "#'car");
          check "no brackets: a symbol" "[a" (read Emacs "[a b]"));
      Testo.create "Scheme: booleans, characters, brackets, vectors" (fun () ->
          check "booleans" "(#t #f #t #f)" (read Scheme "(#t #f #true #false)");
          check "characters" "(#\\a #\\  #\\( 10)" (read Scheme {|(#\a #\space #\( 10)|});
          check "brackets" "(let ((x 1)) x)" (read Scheme "(let ([x 1]) x)");
          check "decimals" "(1.5 2000 -3 x1)" (read Scheme "(1.5 2e3 -3 x1)");
          check "a vector" "#(1 2)" (read Scheme "#(1 2)");
          check "quasiquote" "(quasiquote (a (unquote b) (unquote-splicing c)))" (read Scheme "`(a ,b ,@c)"));
      Testo.create "Scheme: comments" (fun () ->
          check "#| nested |#" "(a b)" (read Scheme "#| one #| two |# |# (a #;(gone) b)");
          check "only blank" "true" (string_of_bool (Sexpr_read.only_blank Scheme "  #| x |# ; y\n #;z " 0)));
      Testo.create "errors, where they are" (fun () ->
          check "cut short" "end of input in a list at 0" (error_at Scheme "(a (b)");
          check "a ] for a (" "expected a ) to close (, found ] at 3" (error_at Scheme "(a ]");
          check "a stray )" "unexpected ) at 2" (error_at Emacs "  )"));
    ]
