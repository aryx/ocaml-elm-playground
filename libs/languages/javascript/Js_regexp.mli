(* Js_regexp: JavaScript's regular expressions, the part pages use -- a
   pattern read into a tree, matched by backtracking.

   (notes_javascript.md.) Hacker News' script counts its stories with
   s.match(/[0-9]+/); a page's scripts split, test and replace with
   them. What is read:

     x  \.  \/  \n  \t  \uXXXX     a character (a byte: the strings are
                                   UTF-8, as Js_value says)
     .                             any but a newline
     [abc] [a-z] [^0-9]            a set, ranges, its complement
     \d \w \s  \D \W \S            digits, word characters, spaces, and not
     ^ $                           the start and the end (of a line, with m)
     \b \B                         a word's edge, and not
     (x) (?:x)                     a group, captured (its number counted
                                   from its "(") or not
     x|y                           either
     x* x+ x? x{n} x{n,} x{n,m}    repeated, greedy; lazy with a ? after

   and the flags g (Js_builtins': every match), i (case ignored), m
   (^ and $ at each line).

   **Matching by backtracking**: each part of the pattern is tried at a
   position with "what comes after" as a continuation; a repetition
   takes as many as it can (greedy) and gives them back one by one when
   what follows fails -- Henry Spencer's way, Perl's, every browser's
   (theirs compile to bytecode). Its cost can be exponential on
   pathological patterns ({|(a*)*b|}); a step budget stops those.

     /a(b+)c/ on "xabbbcx": tried at 0 (fails at x), at 1: a, then b+
     takes bbb, c matches: [1, 6), group 1 [2, 5)

   Not read: lookaheads (?=...) and lookbehinds, backreferences \1,
   named groups, the u, s and y flags, classes of Unicode.

   Reference: ECMA-262 5.1, section 15.10 (RegExp); Russ Cox, "Regular
   Expression Matching Can Be Simple And Fast" (2007: why backtracking,
   and its worst case); Kernighan and Pike, "The Practice of
   Programming", chapter 9 (a matcher in 30 lines). *)

type t

(* the pattern compiled with its flags ("gim"); Error: why it cannot be *)
val compile : string -> string -> (t, string) result

val source : t -> string
val flags : t -> string
val global : t -> bool

(* the number of capturing groups *)
val groups : t -> int

(* [exec re s from]: the first match at or after [from], its span, and
 * each group's, if it took part: index 0 the whole match *)
val exec : t -> string -> int -> (int * int) option array option
