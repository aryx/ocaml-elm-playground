(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lisp_prelude.mli *)

let text =
  {|
(defmacro when (cond &rest body)
  (list 'if cond (cons 'progn body)))

(defmacro unless (cond &rest body)
  (cons 'if (cons cond (cons nil body))))

(defmacro push (x place)
  (list 'setq place (list 'cons x place)))

(defmacro pop (place)
  (list 'prog1 (list 'car place) (list 'setq place (list 'cdr place))))

;; (dolist (x list) body...): body with x each element in turn
(defmacro dolist (spec &rest body)
  (list 'let (list (list '--dolist-tail-- (car (cdr spec))) (car spec))
        (list 'while '--dolist-tail--
              (list 'setq (car spec) '(car --dolist-tail--))
              (cons 'progn body)
              '(setq --dolist-tail-- (cdr --dolist-tail--)))))

;; (dotimes (i n) body...): body with i from 0 to n - 1
(defmacro dotimes (spec &rest body)
  (list 'let (list (list '--dotimes-limit-- (car (cdr spec))) (list (car spec) 0))
        (list 'while (list '< (car spec) '--dotimes-limit--)
              (cons 'progn body)
              (list 'setq (car spec) (list '1+ (car spec))))))

(defun mapc (f list) (dolist (x list) (funcall f x)) list)
(defun cadr (x) (car (cdr x)))
(defun cddr (x) (cdr (cdr x)))
(defun zerop (n) (= n 0))
|}
