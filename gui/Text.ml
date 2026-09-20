(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Text.mli *)

(* a byte whose top bits are 10 continues the character before it *)
let continues s i = Char.code s.[i] land 0xC0 = 0x80

let rec start_of_char s i =
  if i <= 0 then 0 else if continues s i then start_of_char s (i - 1) else i

let prev_char s i = if i <= 0 then 0 else start_of_char s (i - 1)

let next_char s i =
  let n = String.length s in
  let rec go i = if i >= n || not (continues s i) then i else go (i + 1) in
  if i >= n then n else go (i + 1)

let chars s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let j = next_char s i in
      go j (String.sub s i (j - i) :: acc)
  in
  go 0 []

let column s i = List.length (chars (String.sub s 0 (min i (String.length s))))

let byte_of_column s col =
  let rec go i col = if col <= 0 || i >= String.length s then i else go (next_char s i) (col - 1) in
  go 0 col

let edit ~typed ~pressed text caret =
  let caret = max 0 (min (String.length text) caret) in
  let text, caret =
    if typed = "" then (text, caret)
    else
      ( String.sub text 0 caret ^ typed ^ String.sub text caret (String.length text - caret),
        caret + String.length typed )
  in
  let text, caret =
    if pressed "Backspace" && caret > 0 then
      let from = prev_char text caret in
      (String.sub text 0 from ^ String.sub text caret (String.length text - caret), from)
    else (text, caret)
  in
  let text, caret =
    if pressed "Delete" && caret < String.length text then
      let upto = next_char text caret in
      (String.sub text 0 caret ^ String.sub text upto (String.length text - upto), caret)
    else (text, caret)
  in
  let caret = if pressed "ArrowLeft" then prev_char text caret else caret in
  let caret = if pressed "ArrowRight" then next_char text caret else caret in
  let caret = if pressed "Home" then 0 else caret in
  let caret = if pressed "End" then String.length text else caret in
  (text, caret)
