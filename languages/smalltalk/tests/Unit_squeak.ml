(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_squeak.mli *)

module M = St_memory
module I = St_interp

let check = Alcotest.(check string)

(* the two systems, each booted once *)
let squeak = lazy (St_boot.boot ~kernel:St_kernel.squeak ())
let blue_book = lazy (St_boot.boot ())

let print_in (vm : I.vm) (text : string) : string =
  match I.evaluate vm text with Ok v -> I.print_string vm v | Error e -> "error: " ^ e

let print (text : string) : string = print_in (Lazy.force squeak) text

(* a method's bytecodes, as St_bytecode lists them *)
let listing (src : string) : string =
  let m = I.memory (Lazy.force squeak) in
  let meth = St_compile.compile m ~cls:M.nil ~source:src (St_parse.parse_method src) in
  St_bytecode.disassemble ~show_literal:(fun i -> "literal " ^ string_of_int i) (St_bytecode.bytecodes m meth)
  |> List.map (fun (pc, s) -> string_of_int pc ^ " " ^ String.concat " " (List.filter (( <> ) "") (String.split_on_char ' ' s)))
  |> String.concat "|"

(* the blocks of the two systems disagree on these, and only these *)
let recursive = "| fact | fact := [:n | n < 2 ifTrue: [1] ifFalse: [n * (fact value: n - 1)]]. fact value: 5"
let made_in_a_loop = "(#(1 2 3) collect: [:i | [i * 10]]) collect: [:b | b value]"
let two_counters = "| mk a b | mk := [| n | n := 0. [n := n + 1]]. a := mk value. b := mk value. a value. a value. b value. a value + b value"

(* the Blue Book kernel's tests (Unit_smalltalk.ml), whose answers a
 * change of blocks must not change *)
let same_in_both =
  [
    "3 + 4 * 2"; "#(3 1 2) inject: 0 into: [:a :b | a + b]"; "(1073741823 + 1) class"; "3 perform: #* with: 4";
    "#(1 2 3 4) detect: [:x | x > 2]"; "#(1 2) detect: [:x | x > 2] ifNone: [#none]"; "3 frobnicate";
    "100 factorial printString size"; "(1/3) + (2/3) = 1"; "(10 raisedTo: 20) + 3 \\\\ (10 raisedTo: 11)";
    "((1 to: 10) select: [:i | i even]) asArray"; "(#(5 3 8 1 2) asSortedCollection: [:a :b | a >= b]) asArray";
    "| d | d := Dictionary new. 1 to: 100 do: [:i | d at: i put: i * i]. d size"; "#(1 2 2 3 3 3) asBag occurrencesOf: 3";
    "| s | s := WriteStream on: String new. #(1 2 3) do: [:x | s print: x] separatedBy: [s nextPutAll: ', ']. s contents";
    "| i sum | i := 0. sum := 0. [i < 10] whileTrue: [i := i + 1. sum := sum + i]. sum"; "[:x | x] value: 1 value: 2";
    "| n inc | n := 0. inc := [n := n + 1]. inc value. inc value. n"; "[] printString"; "[:a :b | a] numArgs";
    "| f | f := Form extent: 16 @ 16. f fillBlack; reverse. f pixelAt: 5 @ 9"; "Display fillWhite. Pen new dragon: 6";
    "SmallInteger withAllSuperclasses"; "Metaclass class class == Metaclass";
  ]

let tests =
  Testo.categorize "Squeak"
    [
      Testo.create "closures: a block is a BlockClosure, a value of it a context of its own" (fun () ->
          check "its class" "BlockClosure" (print "[] class");
          check "the Blue Book's" "BlockContext" (print_in (Lazy.force blue_book) "[] class");
          check "a block calling itself" "120" (print recursive);
          check "which the Blue Book's cannot" "error: Message not understood: *" (print_in (Lazy.force blue_book) recursive);
          check "blocks made in a loop keep their own i" "#(10 20 30)" (print made_in_a_loop);
          check "the Blue Book's share it" "#(30 30 30)" (print_in (Lazy.force blue_book) made_in_a_loop);
          check "two counters, an n each" "5" (print two_counters);
          check "the Blue Book's, one n" "9" (print_in (Lazy.force blue_book) two_counters);
          check "a block in a block" "13" (print "| a | a := 10. (([:x | [:y | x + y + a]] value: 1) value: 2)"));
      Testo.create "closures: a value that never changes is copied (St_compile.mli)" (fun () ->
          check "listing"
            "0 16 push temporary 0|1 143 17 0 4 push a closure of 1 arguments copying 1, to 9|5 16 push temporary 0|6 17 push \
             temporary 1|7 176 send +|8 125 block return top|9 124 return top"
            (listing "adder: n ^[:x | x + n]"));
      Testo.create "closures: one that changes lives in a temp vector, shared" (fun () ->
          check "listing"
            "0 138 1 push a new Array of 1|2 104 pop into temporary 0|3 117 push 0|4 142 0 0 pop into temporary 0 of the vector \
             in 0|7 16 push temporary 0|8 143 16 0 9 push a closure of 0 arguments copying 1, to 21|12 140 0 0 push temporary 0 \
             of the vector in 0|15 118 push 1|16 176 send +|17 141 0 0 store into temporary 0 of the vector in 0|20 125 block \
             return top|21 124 return top"
            (listing "counter | n | n := 0. ^[n := n + 1]");
          check "a block's own vector, reached by the block inside" "#(4 9)"
            (print "| mk b | mk := [:x | | t | t := x. [:y | t := t + y]]. b := mk value: 1. Array with: (b value: 3) with: (b value: 5)"));
      Testo.create "closures: to:do: is a loop, or a send when a block holds its variable" (fun () ->
          check "a loop: no block made" "false"
            (string_of_bool
               (let l = listing "sum: n | s | s := 0. 1 to: n do: [:i | s := s + i]. ^s" in
                List.exists (fun line -> List.mem "closure" (String.split_on_char ' ' line)) (String.split_on_char '|' l)));
          check "summed" "5050" (print "| sum | sum := 0. 1 to: 100 do: [:i | sum := sum + i]. sum");
          check "each turn its own i" "OrderedCollection (1 2 3 )"
            (print "| bs | bs := OrderedCollection new. 1 to: 3 do: [:i | bs add: [i]]. bs collect: [:b | b value]");
          check "the Blue Book's, one i" "OrderedCollection (4 4 4 )"
            (print_in (Lazy.force blue_book) "| bs | bs := OrderedCollection new. 1 to: 3 do: [:i | bs add: [i]]. bs collect: [:b | b value]");
          (* the trap left: a temporary of an inlined loop's body is the
           * method's, one for every turn *)
          check "a while loop's temporary is shared" "OrderedCollection (2 2 2 )"
            (print "| i r | i := 0. r := OrderedCollection new. [i < 3] whileTrue: [| t | t := i. r add: [t]. i := i + 1]. r collect: [:b | b value]"));
      Testo.create "closures: ^ returns from the home, a dead home cannot return" (fun () ->
          let vm = Lazy.force squeak in
          let m = I.memory vm in
          let obj = St_boot.class_named m "Object" in
          ignore (St_compile.compile_and_install m ~cls:obj ~category:"tests" "escape ^[:x | ^x]");
          ignore (St_compile.compile_and_install m ~cls:obj ~category:"tests" "firstOver: n in: c c do: [:x | c do: [:y | x * y > n ifTrue: [^x @ y]]]. ^nil");
          I.flush_cache vm;
          check "out of two blocks" "2@3" (print "nil firstOver: 5 in: #(1 2 3)");
          check "a dead home" "error: Context cannot return" (print "(3 escape) value: 4");
          check "the home" "true" (print "| c | c := thisContext. [[thisContext home == c] value] value"));
      Testo.create "the Blue Book's kernel, compiled with closures: the same answers" (fun () ->
          List.iter (fun text -> check text (print_in (Lazy.force blue_book) text) (print text)) same_in_both);
      Testo.create "the debugger and the collector over closures" (fun () ->
          let vm = St_boot.boot ~kernel:St_kernel.squeak () in
          let m = I.memory vm in
          let src = "#(1 2) do: [:x | [:y | y halt] value: x]" in
          let p = I.spawn_method vm (St_compile.compile_doit m ~receiver_class:(M.class_of m M.nil) src) M.nil in
          I.run vm p ~budget:100_000;
          check "the stack" "SmallInteger(Object)>>halt | [] in UndefinedObject>>DoIt | [] in UndefinedObject>>DoIt | Array(SequenceableCollection)>>do: | UndefinedObject>>DoIt"
            (String.concat " | " (List.map (fun (f : St_debug.frame) -> f.label) (St_debug.frames vm p)));
          I.terminate vm p;
          ignore (print_in vm "Smalltalk at: #Kept put: ([:n | | t | t := n. [t := t + 1]] value: 40)");
          ignore (print_in vm "1 to: 1000 do: [:i | [:x | Array new: x] value: 10]");
          let before = M.live m in
          I.collect vm;
          Alcotest.(check bool) "garbage freed" true (M.live m < before - 900);
          check "a closure kept, its vector too" "42" (print_in vm "Kept value. Kept value");
          let vm2 = St_image.load_vm (St_image.save m) in
          check "and in a saved image" "43" (print_in vm2 "Kept value");
          check "which still compiles closures" "BlockClosure" (print_in vm2 "[] class"));
    ]
