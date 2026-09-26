(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The Smalltalk under node (see dune): each check prints a line, and a
 * failure makes the exit code 1. The expected values are the native
 * tests' (Unit_smalltalk.ml): the ones where an int's size shows --
 * SmallInteger's edges, the large integers, the tags, the image's
 * varints. *)

module I = St_interp

let failures = ref 0

let () =
  let vm = St_boot.boot () in
  let print text = match I.evaluate vm text with Ok v -> I.print_string vm v | Error e -> "error: " ^ e in
  let check text expected =
    let got = print text in
    Printf.printf "%s %s = %s\n" (if got = expected then "ok  " else "FAIL") text got;
    if got <> expected then incr failures
  in
  check "3 + 4 * 2" "14";
  check "1073741823 + 1" "1073741824";
  check "(1073741823 + 1) class" "LargePositiveInteger";
  check "-1073741824 class" "SmallInteger";
  check "-1073741824 - 1" "-1073741825";
  check "32768 * 32768" "1073741824";
  check "46341 * 46341" "2147488281";
  check "1 bitShift: 29" "536870912";
  check "(1 bitShift: 40) printString" "'1099511627776'";
  check "100 factorial printString size" "158";
  check "20 factorial" "2432902008176640000";
  check "(10 raisedTo: 20) // (10 raisedTo: 8)" "1000000000000";
  check "(1/3) + (2/3) = 1" "true";
  check "-7 \\\\ 2" "1";
  check "1e10" "10000000000";
  check "16r7FFFFFFF" "2147483647";
  check "1.0e10 truncated" "10000000000";
  check "#(5 3 8 1 2) asSortedCollection asArray" "#(1 2 3 5 8)";
  check "| d | d := Dictionary new. 1 to: 100 do: [:i | d at: i put: i * i]. d at: 77" "5929";
  (* the image: saved and loaded, the same bytes *)
  let m = I.memory vm in
  let image = St_image.save m in
  let vm2 = St_image.load_vm image in
  let same = St_image.save (I.memory vm2) = image in
  Printf.printf "%s the image saved again, the same bytes\n" (if same then "ok  " else "FAIL");
  if not same then incr failures;
  if !failures > 0 then exit 1
