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

(* MiniMorphic's cycle (kernel/morphic/MiniMorphic.st): n atoms
 * bouncing, the bytecodes and the time of a cycle -- what a frame
 * costs when Smalltalk draws the screen *)
let morphic (atoms : int) =
  let vm = St_boot.boot ~kernel:St_kernel.mini_morphic () in
  let run text = match I.evaluate vm ~budget:2_000_000_000 text with Ok _ -> () | Error e -> print_endline ("error: " ^ e) in
  run (Printf.sprintf "Smalltalk at: #W put: (WorldMorph bouncingAtoms: %d). W doOneCycle" atoms);
  let cycles = 200 in
  let before = I.bytecodes_run vm and blits = St_bitblt.changes () and t0 = Sys.time () in
  run (Printf.sprintf "%d timesRepeat: [W doOneCycle]" cycles);
  let dt = Sys.time () -. t0 and n = I.bytecodes_run vm - before in
  Printf.printf "morphic   %3d atoms   %10d bytecodes a cycle %6.2f ms a cycle %5d blits %6.1f M/s\n%!" atoms (n / cycles)
    (dt *. 1000. /. float_of_int cycles)
    ((St_bitblt.changes () - blits) / cycles)
    (float_of_int n /. 1e6 /. dt)

(* BitBlt in colour alone (St_colorblt.mli), at 32 bits: a 640 by 400
 * rectangle filled with a colour, a Form stored, the definition then
 * what runs; and a Form blended, a pixel at a time either way *)
let colour () =
  let form w h : St_colorblt.form = { bits = Bytes.make (4 * w * h) '\165'; w; h; stride = 4 * w; depth = 32 } in
  let dest = form 800 600 and source = form 700 500 and pixel = form 1 1 in
  List.iter
    (fun (name, simple, source, halftone, rule, times) ->
      let t0 = Sys.time () in
      for i = 1 to times do
        St_colorblt.blit ~simple ~dest ~source ~map:None ~halftone ~rule ~dx:(40 + (i land 7)) ~dy:50 ~sx:3 ~sy:7
          (40 + (i land 7), 50, 680, 450)
      done;
      let dt = Sys.time () -. t0 in
      Printf.printf "colour    %-18s %10d pixels    %6.2f s %6.1f M pixels/s\n%!" name (times * 640 * 400) dt
        (float_of_int (times * 640 * 400) /. 1e6 /. dt))
    [
      ("fill, a pixel", true, None, Some pixel, 3, 40);
      ("fill, a row", false, None, Some pixel, 3, 2000);
      ("store, a pixel", true, Some source, None, 3, 40);
      ("store, a row", false, Some source, None, 3, 2000);
      ("blend", false, Some source, None, 24, 40);
    ]

(* Squeak's text (kernel/squeak/Text.st): the font drawn from Hershey's
 * strokes, then strings of 40 characters on a Form of 32 bits *)
let text () =
  let vm = St_boot.boot ~kernel:St_kernel.squeak () in
  let timed name text =
    let before = I.bytecodes_run vm and t0 = Sys.time () in
    (match I.evaluate vm ~budget:2_000_000_000 text with Ok _ -> () | Error e -> print_endline ("error: " ^ e));
    Printf.printf "text      %-18s %10d bytecodes %6.2f ms\n%!" name (I.bytecodes_run vm - before) ((Sys.time () -. t0) *. 1000.)
  in
  timed "the font" "StrikeFont default";
  timed "100 strings of 40"
    "| f | f := Form extent: 400 @ 20 depth: 32. 100 timesRepeat: [f drawString: 'The quick brown fox jumps over the lazy d' at: 0 @ 0]"

(* Squeak's Morphic (kernel/squeak/Morphic.st), as [morphic] for
 * MiniMorphic: n AtomMorphs, ellipses, bouncing in a world of 800 by
 * 600 at 32 bits; and a window of text redrawn whole *)
let morphs (atoms : int) =
  let vm = St_boot.boot ~kernel:St_kernel.squeak () in
  let run text = match I.evaluate vm ~budget:2_000_000_000 text with Ok _ -> () | Error e -> print_endline ("error: " ^ e) in
  run "Smalltalk at: #W put: (PasteUpMorph on: (Form extent: 800 @ 600 depth: 32))";
  run
    (Printf.sprintf
       "1 to: %d do: [:i | | a | a := AtomMorph new. a velocity: (i \\\\ 7 - 3 * 2 + 1) @ (i \\\\ 5 - 2 * 2 + 1). W addMorph: a. a position: (i * 37 \\\\ 780) @ (i * 53 \\\\ 580)]. W doOneCycle"
       atoms);
  let timed name cycles text =
    let before = I.bytecodes_run vm and blits = St_bitblt.changes () and t0 = Sys.time () in
    run (Printf.sprintf "%d timesRepeat: [%s]" cycles text);
    let dt = Sys.time () -. t0 and n = I.bytecodes_run vm - before in
    Printf.printf "morphs    %-22s %10d bytecodes a cycle %6.2f ms a cycle %5d blits\n%!" name (n / cycles)
      (dt *. 1000. /. float_of_int cycles)
      ((St_bitblt.changes () - blits) / cycles)
  in
  timed (Printf.sprintf "%d atoms" atoms) 200 "W doOneCycle";
  if atoms = 10 then begin
    run
      "| win t | W submorphs copy do: [:m | m delete]. win := SystemWindow new. t := TextMorph new. t contents: ((1 to: 20) inject: '' into: [:s :i | s, 'The quick brown fox jumps over the lazy d', (String with: (Character value: 13))]). win addMorph: t frame: (0 @ 0 corner: 1 @ 1). W addMorph: win. win position: 50 @ 50; extent: 500 @ 400. W doOneCycle";
    timed "a window, 20 lines" 20 "W restoreDisplay. W doOneCycle";
    timed "nothing changed" 200 "W doOneCycle"
  end

(* Squeak's tools (kernel/squeak/Tools.st): what the Browser's
 * gestures cost, each followed by the cycle that redraws what it
 * damaged *)
let tools () =
  let keys = Queue.create () in
  let host = { St_boot.quiet_host with keyboard = (fun () -> Queue.take_opt keys) } in
  let vm = St_boot.boot ~host ~kernel:St_kernel.squeak () in
  let run text = match I.evaluate vm ~budget:2_000_000_000 text with Ok _ -> () | Error e -> print_endline ("error: " ^ e) in
  let timed name text =
    let before = I.bytecodes_run vm and t0 = Sys.time () in
    run (text ^ ". W doOneCycle");
    Printf.printf "tools     %-26s %10d bytecodes %7.2f ms\n%!" name (I.bytecodes_run vm - before) ((Sys.time () -. t0) *. 1000.)
  in
  run "Smalltalk at: #W put: (PasteUpMorph on: (Form extent: 800 @ 600 depth: 32)). StrikeFont default. W doOneCycle";
  timed "the Browser opened" "Smalltalk at: #B put: Browser open";
  timed "a category picked" "B categoryList selectItem: 'Morphic-Kernel'";
  timed "a class picked" "B classList selectItem: #Morph";
  timed "a protocol picked" "B protocolList selectItem: 'changing'";
  timed "a selector picked" "B selectorList selectItem: #position:";
  timed "the method accepted" "B codePane accept";
  run "W hand keyboardFocus: B codePane. W doOneCycle";
  Queue.add (Char.code 'x') keys;
  timed "a character typed" "3";
  Queue.add 13 keys;
  timed "a return typed" "3";
  timed "the world redrawn" "W restoreDisplay";
  timed "print it" "B codePane contents: '100 factorial printString size'. B codePane printIt"

let () =
  let only = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  let has (name : string) =
    let n = String.length only in
    let rec at i = i + n <= String.length name && (String.sub name i n = only || at (i + 1)) in
    n = 0 || at 0
  in
  if has "blit" then blit ();
  if has "colour" then colour ();
  if has "text" then text ();
  if has "morphic" then List.iter morphic [ 10; 50; 200 ];
  if has "morphs" then List.iter morphs [ 10; 50; 200 ];
  if has "tools" then tools ();
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
