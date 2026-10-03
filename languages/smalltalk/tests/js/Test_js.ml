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
  (* Squeak's kernel: closures *)
  let vm = St_boot.boot ~kernel:St_kernel.squeak () in
  let print text = match I.evaluate vm text with Ok v -> I.print_string vm v | Error e -> "error: " ^ e in
  let check text expected =
    let got = print text in
    Printf.printf "%s %s = %s\n" (if got = expected then "ok  " else "FAIL") text got;
    if got <> expected then incr failures
  in
  check "| fact | fact := [:n | n < 2 ifTrue: [1] ifFalse: [n * (fact value: n - 1)]]. fact value: 20" "2432902008176640000";
  check "(#(1 2 3) collect: [:i | [i * 10]]) collect: [:b | b value]" "#(10 20 30)";
  (* colour: a pixel of 32 bits is a negative int here (St_colorblt.mli) *)
  check "| f | f := Form extent: 4 @ 4 depth: 32. f fillColor: Color white. f fillColor: (Color red alpha: 1/2). f colorAt: 1 @ 1"
    "Color(255 127 127)";
  check "| f | f := Form extent: 4 @ 4 depth: 32. f fillColor: Color red. f reverse. f colorAt: 1 @ 1" "Color(0 255 255 alpha 0)";
  check "| f | f := Form extent: 40 @ 20 depth: 32. f fillColor: Color white. f drawString: 'A' at: 0 @ 0. f colorAt: 5 @ 2" "Color(0 0 0)";
  (* Morphic: a cycle, a morph drawn; the hand's shadow, blended *)
  check
    "| f w r | f := Form extent: 100 @ 80 depth: 32. w := PasteUpMorph on: f. r := RectangleMorph new. r color: Color green. w addMorph: r. r position: 20 @ 20. w doOneCycle. f colorAt: 30 @ 30"
    "Color(0 255 0)";
  check
    "| f w r | f := Form extent: 100 @ 80 depth: 32. w := PasteUpMorph on: f. r := EllipseMorph new. w hand attachMorph: r. w doOneCycle. Array with: (f colorAt: 25 @ 20) with: (f colorAt: 52 @ 42)"
    "#(Color(255 255 0) Color(142 142 142))";
  (* the tools: evaluation by a DoIt method, the Browser's source *)
  check
    "| w t | w := PasteUpMorph on: (Form extent: 300 @ 200 depth: 32). t := Workspace open submorphs first. t contents: '(1 bitShift: 40) + 1'. t printIt. t contents"
    "'(1 bitShift: 40) + 1 1099511627777'";
  check "(Morph sourceCodeAt: #drawOn:) copyFrom: 1 to: 15" "'drawOn: aCanvas'";
  if !failures > 0 then exit 1
