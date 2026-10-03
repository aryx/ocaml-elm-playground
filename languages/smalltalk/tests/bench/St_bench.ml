(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_bench.mli *)

module I = St_interp

(* a name, the expression, its answer printed *)
let workloads =
  [
    ("sends", "27 benchFib", "196418");
    ("blocks", "| s | s := 0. 1 to: 200000 do: [:i | s := s + (#(1 2 3) inject: 0 into: [:a :b | a + b])]. s", "1200000");
    ( "arrays",
      "| flags n | n := 0. 1 to: 20 do: [:r | flags := Array new: 8190. 1 to: 8190 do: [:i | flags at: i put: true]. n := 0. 1 to: 8190 do: [:i | (flags at: i) \
       ifTrue: [| k | k := i + i + 1. n := n + 1. (i + k) to: 8190 by: k do: [:j | flags at: j put: false]]]]. n",
      "1899" );
    ("dictionary", "| d | d := Dictionary new. 1 to: 30000 do: [:i | d at: i put: i * i]. d size", "30000");
    ("large", "300 factorial printString size", "615");
    ("pen", "Display fillWhite. Pen new dragon: 13", "a Pen");
  ]

let fib = "benchFib\n\t^self < 2 ifTrue: [self] ifFalse: [(self - 1) benchFib + (self - 2) benchFib]"

(* BitBlt alone: a 640 by 400 rectangle of the Display xor-ed with a
 * Form put anywhere, the definition then what runs *)
let blit () =
  let form w h : St_bitblt.form = { bits = Bytes.make ((w + 15) / 16 * 2 * h) '\165'; w; h; stride = (w + 15) / 16 * 2 } in
  let dest = form 800 600 and source = form 700 500 in
  List.iter
    (fun (name, simple, times) ->
      let t0 = Sys.time () in
      for i = 1 to times do
        St_bitblt.blit ~simple ~dest ~source:(Some source) ~halftone:None ~rule:6 ~dx:(40 + (i land 7)) ~dy:50 ~sx:3 ~sy:7
          (40 + (i land 7), 50, 680, 450)
      done;
      let dt = Sys.time () -. t0 in
      Printf.printf "blit      %-11s %10d pixels    %6.2f s %6.1f M pixels/s\n%!" name (times * 640 * 400) dt
        (float_of_int (times * 640 * 400) /. 1e6 /. dt))
    [ ("a pixel", true, 20); ("a byte", false, 400) ]

let () =
  let only = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  let has (name : string) =
    let n = String.length only in
    let rec at i = i + n <= String.length name && (String.sub name i n = only || at (i + 1)) in
    n = 0 || at 0
  in
  if has "blit" then blit ();
  List.iter
    (fun (kernel, files) ->
      let vm = St_boot.boot ~kernel:files () in
      let m = I.memory vm in
      ignore (St_compile.compile_and_install m ~cls:(St_boot.class_named m "Integer") ~category:"bench" fib);
      I.flush_cache vm;
      List.iter
        (fun (name, text, expected) ->
          if has name then begin
            let before = I.bytecodes_run vm and t0 = Sys.time () in
            let got = match I.evaluate vm ~budget:2_000_000_000 text with Ok v -> I.print_string vm v | Error e -> "error: " ^ e in
            let dt = Sys.time () -. t0 and n = I.bytecodes_run vm - before in
            Printf.printf "%-9s %-11s %10d bytecodes %6.2f s %6.1f M/s%s\n%!" kernel name n dt
              (float_of_int n /. 1e6 /. dt)
              (if got = expected then "" else "  WRONG: " ^ got)
          end)
        workloads)
    [ ("blue book", St_kernel.files); ("squeak", St_kernel.squeak) ]
