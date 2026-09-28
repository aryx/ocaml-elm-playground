(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scheme_prelude.mli *)

let text =
  {|
(define empty '())
(define true #t)
(define false #f)
(define pi 3.141592653589793)

(define (map1 f l)
  (if (null? l) '() (cons (f (car l)) (map1 f (cdr l)))))

(define (map f l . ls)
  (if (null? ls)
      (map1 f l)
      (let loop ([ls (cons l ls)])
        (if (null? (car ls))
            '()
            (cons (apply f (map1 car ls)) (loop (map1 cdr ls)))))))

(define (for-each f l)
  (if (null? l) (void) (begin (f (car l)) (for-each f (cdr l)))))

(define (filter keep? l)
  (cond [(null? l) '()]
        [(keep? (car l)) (cons (car l) (filter keep? (cdr l)))]
        [else (filter keep? (cdr l))]))

(define (foldl f acc l)
  (if (null? l) acc (foldl f (f (car l) acc) (cdr l))))

(define (foldr f acc l)
  (if (null? l) acc (f (car l) (foldr f acc (cdr l)))))

(define (andmap ok? l)
  (or (null? l) (and (ok? (car l)) (andmap ok? (cdr l)))))

(define (ormap ok? l)
  (and (pair? l) (or (ok? (car l)) (ormap ok? (cdr l)))))

(define (build-list n f)
  (let loop ([i (- n 1)] [acc '()])
    (if (< i 0) acc (loop (- i 1) (cons (f i) acc)))))

; merge sort: split in two, sort each, merge
(define (sort l less?)
  (define (merge a b)
    (cond [(null? a) b]
          [(null? b) a]
          [(less? (car b) (car a)) (cons (car b) (merge a (cdr b)))]
          [else (cons (car a) (merge (cdr a) b))]))
  (define (split l a b)
    (if (null? l) (cons a b) (split (cdr l) b (cons (car l) a))))
  (if (or (null? l) (null? (cdr l)))
      l
      (let ([halves (split l '() '())])
        (merge (sort (car halves) less?) (sort (cdr halves) less?)))))
|}
