(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_lisp.mli *)

module L = Lisp_eval

(* the value of the last of [text]'s forms, printed *)
let run ?(st = L.create ()) (text : string) : string =
  let v, _ = List.fold_left (fun (_, st) form -> L.eval st form) (Lisp.nil, st) (Lisp_read.read_all text) in
  Lisp.print v

let error_of (text : string) : string =
  match run text with v -> "no error, " ^ v | exception L.Error e -> L.error_message e

let check = Alcotest.(check string)

let tests =
  Testo.categorize "Lisp"
    [
      Testo.create "the reader: Lisp_read.mli's example, and what it reads" (fun () ->
          let v, pos = Lisp_read.read "(defun double (x) (+ x x)) ; twice" 0 in
          check "value" "(defun double (x) (+ x x))" (Lisp.print v);
          Alcotest.(check int) "just after" 26 pos;
          check "quote" "'(a . b)" (Lisp.print (fst (Lisp_read.read "'(a . b)" 0)));
          check "dotted list" "(1 2 . 3)" (Lisp.print (fst (Lisp_read.read "(1 . (2 . 3))" 0)));
          check "a string" {|"say \"hi\""|} (Lisp.print (fst (Lisp_read.read {|"say \"hi\""|} 0)));
          check "characters are integers" "(97 10 1 32)" (Lisp.print (fst (Lisp_read.read {|(?a ?\n ?\C-a ?\s)|} 0)));
          check "keys" (Lisp.print (Lisp.Str "\x18\x13\x1bx")) (Lisp.print (fst (Lisp_read.read {|"\C-x\C-s\M-x"|} 0))));
      Testo.create "eval: arithmetic, lists, strings, format" (fun () ->
          check "sum" "6" (run "(+ 1 2 3)");
          check "nested" "14" (run "(+ 2 (* 3 4))");
          check "list" "(1 2 3)" (run "(cons 1 (list 2 3))");
          check "nil" "nil" (run "(cdr '(1))");
          check "concat" {|"foobar"|} (run {|(concat "foo" (substring "xbar" 1))|});
          check "format" {|"x=3 \"s\" s"|} (run {|(format "x=%d %S %s" 3 "s" "s")|});
          check "mapcar" "(2 4 6)" (run "(mapcar (lambda (x) (* 2 x)) '(1 2 3))"));
      Testo.create "defun, recursion, let and let*" (fun () ->
          check "fact" "3628800" (run "(defun fact (n) (if (= n 0) 1 (* n (fact (1- n))))) (fact 10)");
          check "let is parallel" "(2 1)" (run "(setq a 1 b 2) (let ((a b) (b a)) (list a b))");
          check "let* is sequential" "(1 1)" (run "(let* ((a 1) (b a)) (list a b))");
          check "&optional and &rest" "(1 nil (3 4))" (run "(defun f (a &optional b &rest c) (list a b c)) (list (car (f 1)) (nth 1 (f 1)) (nth 2 (f 1 2 3 4)))"));
      Testo.create "dynamic scope: Lisp_eval.mli's search-it" (fun () ->
          let defs = {|(defvar exact t) (defun search-it () (if exact "exact" "any case"))|} in
          check "the let's binding, seen by the callee" {|"any case"|} (run (defs ^ "(let ((exact nil)) (search-it))"));
          check "and put back after" {|"exact"|} (run (defs ^ "(let ((exact nil)) (search-it)) (search-it)"));
          check "defvar keeps a value set before it" "1" (run "(setq v 1) (defvar v 2) v"));
      Testo.create "macros of the prelude: when, unless, dolist, dotimes, push" (fun () ->
          check "when" "2" (run "(when t 1 2)");
          check "unless" "nil" (run "(unless t 1)");
          check "dolist" "(3 2 1)" (run "(setq r nil) (dolist (x '(1 2 3)) (push x r)) r");
          check "dotimes" "10" (run "(setq s 0) (dotimes (i 5) (setq s (+ s i))) s");
          check "a macro of our own" "4" (run "(defmacro twice (e) (list '+ e e)) (twice 2)"));
      Testo.create "errors: their messages as Emacs writes them" (fun () ->
          check "void variable" "Symbol's value as variable is void: nope" (error_of "nope");
          check "void function" "Symbol's function definition is void: nope" (error_of "(nope)");
          check "wrong type" "Wrong type argument: integerp, \"a\"" (error_of "(+ 1 \"a\")");
          check "error" "no 3" (error_of {|(error "no %d" 3)|});
          check "arity" "Wrong number of arguments: (lambda (x) x), 2" (error_of "((lambda (x) x) 1 2)"));
      Testo.create "condition-case: what came before kept, the lets inside undone" (fun () ->
          check "caught" {|"caught: boom"|} (run {|(condition-case e (error "boom") (error (concat "caught: " (car (cdr e)))))|});
          check "the setq before the error kept" "1"
            (run "(setq x 0) (condition-case nil (progn (setq x 1) (car 1)) (error nil)) x");
          check "the let inside undone" "0" (run "(setq y 0) (condition-case nil (let ((y 5)) (car 1)) (error nil)) y");
          check "only its own signal" "Arithmetic error" (error_of "(condition-case nil (/ 1 0) (void-variable 1))");
          (* reading a file of forms until end-of-file, as eval-buffer does *)
          check "read until the end" "3"
            (run {|(setq text "(setq n 1) (setq n (+ n 2))" pos 0)
                   (condition-case nil
                       (while t
                         (let ((r (read-from-string (substring text pos))))
                           (eval (car r))
                           (setq pos (+ pos (cdr r)))))
                     (end-of-file nil))
                   n|}));
      Testo.create "fuel stops a loop that never ends" (fun () ->
          match run ~st:(L.create ~fuel:10_000 ()) "(while t)" with
          | _ -> Alcotest.fail "it ended?"
          | exception L.Error e -> check "message" "Lisp ran too long (a real Emacs would wait for C-g)" (Lisp_eval.error_message e));
      Testo.create "interactive specs and documentation" (fun () ->
          let st = L.create () in
          let _, st = L.eval st (fst (Lisp_read.read {|(defun hello (n) "Say hello N times." (interactive "p") n)|} 0)) in
          check "spec" {|"p"|} (Option.fold ~none:"none" ~some:Lisp.print (L.interactive_spec st (Lisp.Sym "hello")));
          check "doc" "Say hello N times." (Option.value (L.documentation st (Lisp.Sym "hello")) ~default:"none");
          check "not a command" "none" (Option.fold ~none:"none" ~some:Lisp.print (L.interactive_spec st (Lisp.Sym "car"))));
    ]
