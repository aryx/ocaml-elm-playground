(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_smalltalk.mli *)

module M = St_memory
module I = St_interp

let check = Alcotest.(check string)

(* one system for the tests that only read it, booted once *)
let shared = lazy (St_boot.boot ())

(* what "print it" shows *)
let print ?(vm = Lazy.force shared) (text : string) : string =
  match I.evaluate vm text with Ok v -> I.print_string vm v | Error e -> "error: " ^ e

let tokens (text : string) : string =
  St_lexer.tokenize text |> Array.to_list |> List.map (fun (t : St_lexer.token) -> St_lexer.to_string t.kind) |> String.concat "  "

let parse (text : string) : string =
  match St_parse.parse_doit text with
  | m -> String.concat ". " (List.map St_ast.show_stmt m.body)
  | exception St_parse.Error (pos, msg) -> Printf.sprintf "error at %d: %s" pos msg

let tests =
  Testo.categorize "Smalltalk"
    [
      Testo.create "the lexer: St_lexer.mli's example and its three traps" (fun () ->
          check "example" "Name x  Assign  Array_start  Int 1  Char a  Rparen  Keyword at:  Int 2  Period  Caret  Name x  Eof"
            (tokens "x := #(1 $a) at: 2. ^x");
          check "x:=1 is not a keyword" "Name x  Assign  Int 1  Eof" (tokens "x:=1");
          check "3-4 subtracts" "Int 3  Binary -  Int 4  Eof" (tokens "3-4");
          check "3 - -4" "Int 3  Binary -  Int -4  Eof" (tokens "3 - -4");
          check "the underscore assigns" "Name a  Assign  Name b  Eof" (tokens "a_b");
          check "literals" "Int 31  Int 10  Symbol at:put:  Symbol +  Symbol two words  String \"it's\"  Float 1500  Eof"
            (tokens "16r1F 2r1010 #at:put: #+ #'two words' 'it''s' 1.5e3");
          check "a large one" "Large  Eof" (tokens "1e20");
          check "comments dropped" "Int 1  Eof" (tokens "\"a comment\" 1"));
      Testo.create "the parser: precedence, cascades, blocks, errors" (fun () ->
          check "binary, left to right" "((3 + 4) * 2)" (parse "3 + 4 * 2");
          check "unary, then binary, then keyword" "(a at: (i + 1) put: (b sqrt))" (parse "a at: i + 1 put: b sqrt");
          check "a cascade" "(Transcript show: 'a'; cr)" (parse "Transcript show: 'a'; cr");
          check "a block" "[:x | | t | t := (x + 1). ^t]" (parse "[:x | | t | t := x + 1. ^t]");
          check "a literal array" "#(1 $a 'x' #foo #at:put: #(2 3) nil)" (parse "#(1 $a 'x' foo at:put: (2 3) nil)");
          check "an error, where it is" "error at 6: Nothing more expected" (parse "3 + 4 5");
          check "a method's selector" "at:put:" (Option.get (St_parse.selector_of "at: i put: x ^x")));
      Testo.create "the compiler: the Blue Book's Rectangle>>center, byte for byte" (fun () ->
          let vm = Lazy.force shared in
          let m = I.memory vm in
          let rect = St_boot.class_named m "Rectangle" in
          let meth = Option.get (St_class.local_method m rect (M.symbol m "center")) in
          check "bytes" "0 1 176 119 185 124"
            (String.concat " " (List.map string_of_int (List.init 6 (fun i -> Char.code (Bytes.get (St_bytecode.bytecodes m meth) i)))));
          check "evaluated" "2@3" (print "(Rectangle origin: 0@0 corner: 4@6) center"));
      Testo.create "the compiler: ifTrue:ifFalse: and whileTrue: become jumps" (fun () ->
          let vm = Lazy.force shared in
          let m = I.memory vm in
          let src = "sign: x ^x > 0 ifTrue: ['pos'] ifFalse: ['neg']" in
          let meth = St_compile.compile m ~cls:M.nil ~source:src (St_parse.parse_method src) in
          let show i = M.string_of m (St_bytecode.literal m meth i) in
          let code = St_bytecode.disassemble ~show_literal:(fun i -> "'" ^ show i ^ "'") (St_bytecode.bytecodes m meth) in
          check "listing"
            "0 16        push temporary 0|1 117       push 0|2 179       send >|3 153       jump on false to 6|4 32        push 'pos'|5 144       jump to 7|6 33        push 'neg'|7 124       return top"
            (String.concat "|" (List.map (fun (pc, s) -> string_of_int pc ^ " " ^ s) code));
          check "a loop" "55" (print "| i sum | i := 0. sum := 0. [i < 10] whileTrue: [i := i + 1. sum := sum + i]. sum");
          check "to:do:" "5050" (print "| sum | sum := 0. 1 to: 100 do: [:i | sum := sum + i]. sum"));
      Testo.create "the object memory: the metaclass knot" (fun () ->
          check "3's class's class" "SmallInteger class" (print "3 class class");
          check "a metaclass's class" "true" (print "3 class class class == Metaclass");
          check "the knot" "true" (print "Metaclass class class == Metaclass");
          check "Object class superclass" "Class" (print "Object class superclass");
          check "the chain" "OrderedCollection (SmallInteger Integer Number Magnitude Object )"
            (print "SmallInteger withAllSuperclasses"));
      Testo.create "the object memory: become:, and the collector" (fun () ->
          let vm = St_boot.boot () in
          check "swapped" "#(#b #a)" (print ~vm "| a b | a := 'a' asSymbol -> 1. b := 'b' asSymbol -> 2. a become: b. Array with: a key with: b key");
          check "a collection grown in place" "20"
            (print ~vm "| c d | c := OrderedCollection new. d := c. 1 to: 20 do: [:i | c add: i]. d size");
          let m = I.memory vm in
          ignore (print ~vm "1 to: 1000 do: [:i | Array new: 10]");
          let before = M.live m in
          I.collect vm;
          Alcotest.(check bool) "garbage freed" true (M.live m < before - 900));
      Testo.create "the interpreter: sends, primitives failing into Smalltalk" (fun () ->
          check "3 + 4" "7" (print "3 + 4");
          check "binary precedence" "14" (print "3 + 4 * 2");
          check "inject:into:" "6" (print "#(3 1 2) inject: 0 into: [:a :b | a + b]");
          check "overflow into LargePositiveInteger" "1073741824" (print "1073741823 + 1");
          check "its class" "LargePositiveInteger" (print "(1073741823 + 1) class");
          check "and back" "SmallInteger" (print "(1073741823 + 1 - 1) class");
          check "a float" "3.5" (print "3 + 0.5");
          check "perform:" "12" (print "3 perform: #* with: 4"));
      Testo.create "the interpreter: non-local return, cannotReturn:, doesNotUnderstand:" (fun () ->
          check "^ out of a block" "3" (print "#(1 2 3 4) detect: [:x | x > 2]");
          check "detect:ifNone:" "#none" (print "#(1 2) detect: [:x | x > 2] ifNone: [#none]");
          check "not understood" "error: Message not understood: frobnicate" (print "3 frobnicate");
          let vm = Lazy.force shared in
          let m = I.memory vm in
          let obj = St_boot.class_named m "Object" in
          ignore (St_compile.compile_and_install m ~cls:obj ~category:"tests" "escape ^[:x | ^x]");
          I.flush_cache vm;
          check "a dead home" "error: Context cannot return" (print "(3 escape) value: 4"));
      Testo.create "the kernel: numbers" (fun () ->
          check "100 factorial printString size" "158" (print "100 factorial printString size");
          check "20 factorial" "2432902008176640000" (print "20 factorial");
          check "exact fractions" "true" (print "(1/3) + (2/3) = 1");
          check "a fraction" "3/4" (print "(1/2) + (1/4)");
          check "coercion" "7/2" (print "3 + (1/2)");
          check "floor division" "-4" (print "-7 // 2");
          check "its remainder" "1" (print "-7 \\\\ 2");
          check "large division" "1000000000000" (print "(10 raisedTo: 20) // (10 raisedTo: 8)");
          check "large remainder" "3" (print "(10 raisedTo: 20) + 3 \\\\ (10 raisedTo: 11)");
          check "gcd" "6" (print "(2 raisedTo: 40) * 3 gcd: 18");
          check "negative large" "-1099511627776" (print "(2 raisedTo: 40) negated");
          check "comparison" "true" (print "(2 raisedTo: 40) > (2 raisedTo: 39)");
          check "sqrt" "'1.4142135623730'" (print "2 sqrt printString copyFrom: 1 to: 15");
          check "points" "4@6" (print "(1@2) + (3@4)"));
      Testo.create "the kernel: collections and streams" (fun () ->
          check "select:" "#(2 4 6 8 10)" (print "((1 to: 10) select: [:i | i even]) asArray");
          check "collect:" "#(1 4 9)" (print "#(1 2 3) collect: [:x | x * x]");
          check "sorted" "SortedCollection (1 2 3 5 8 )" (print "#(5 3 8 1 2) asSortedCollection");
          check "sorted by a block" "#(8 5 3 2 1)" (print "(#(5 3 8 1 2) asSortedCollection: [:a :b | a >= b]) asArray");
          check "a dictionary" "3" (print "| d | d := Dictionary new. d at: #a put: 1; at: #b put: 2. (d at: #a) + (d at: #b)");
          check "many keys" "100" (print "| d | d := Dictionary new. 1 to: 100 do: [:i | d at: i put: i * i]. d size");
          check "a set" "3" (print "#(1 2 2 3 3 3) asSet size");
          check "a bag" "3" (print "#(1 2 2 3 3 3) asBag occurrencesOf: 3");
          check "strings" "'hello world'" (print "'hello' , ' ' , 'world'");
          check "a stream" "'1, 2, 3'"
            (print "| s | s := WriteStream on: String new. #(1 2 3) do: [:x | s print: x] separatedBy: [s nextPutAll: ', ']. s contents");
          check "reversed" "'olleh'" (print "'hello' reversed");
          check "upper" "'HELLO'" (print "'hello' asUppercase");
          check "a symbol" "#foo:bar:" (print "'foo:bar:' asSymbol");
          check "an interval" "(1 to: 5)" (print "1 to: 5"));
      Testo.create "the debugger: not understood, defined, restarted, finished" (fun () ->
          let vm = St_boot.boot () in
          let m = I.memory vm in
          let p = I.spawn_method vm (St_compile.compile_doit m ~receiver_class:(M.class_of m M.nil) "10 fib + 1") M.nil in
          I.run vm p ~budget:100_000;
          check "stopped" "Message not understood: fib" (match p.state with I.Suspended l -> l | _ -> "not suspended");
          check "the stack" "SmallInteger(Object)>>doesNotUnderstand: | UndefinedObject>>DoIt"
            (String.concat " | " (List.map (fun (f : St_debug.frame) -> f.label) (St_debug.frames vm p)));
          let cls, sel, sender = Option.get (St_debug.not_understood vm p) in
          check "the class and selector" "SmallInteger fib" (St_class.name m cls ^ " " ^ sel);
          check "a template" "fib\n\t\"A method for fib, to write here.\"\n\t^self" (St_debug.template sel);
          ignore
            (St_compile.compile_and_install m ~cls:(St_boot.class_named m "Integer") ~category:"fun"
               "fib\n\tself < 2 ifTrue: [^self].\n\t^(self - 1) fib + (self - 2) fib");
          I.flush_cache vm;
          Alcotest.(check bool) "restarted" true (St_debug.restart vm p sender);
          St_debug.proceed p;
          I.run vm p ~budget:1_000_000;
          check "finished" "56" (match p.state with I.Finished v -> I.print_string vm v | _ -> "not finished"));
      Testo.create "the debugger: stepping, a send at a time" (fun () ->
          let vm = St_boot.boot () in
          let m = I.memory vm in
          let src = "| a | self halt. a := 3 + 4. a := a * 2. ^a" in
          let p = I.spawn_method vm (St_compile.compile_doit m ~receiver_class:(M.class_of m M.nil) src) M.nil in
          I.run vm p ~budget:100_000;
          let doit = List.nth (St_debug.frames vm p) 1 in
          check "halted in" "UndefinedObject>>DoIt" doit.label;
          let next () =
            match St_debug.frames ~stepping:true vm p with
            | f :: _ -> ( match f.highlight with Some (a, b) -> String.sub f.source a (b - a) | None -> "-")
            | [] -> "none"
          in
          St_debug.step vm p doit.ctx;
          check "before +" "3 + 4" (next ());
          St_debug.step vm p doit.ctx;
          check "before *" "a * 2" (next ());
          check "a's value" "7"
            (I.print_string vm (List.assoc "a" (St_debug.variables vm (List.hd (St_debug.frames vm p)).ctx)));
          St_debug.proceed p;
          I.run vm p ~budget:100_000;
          check "the answer" "14" (match p.state with I.Finished v -> I.print_string vm v | _ -> "not finished"));
      Testo.create "the image: saved, loaded, the world kept" (fun () ->
          let vm = St_boot.boot () in
          let m = I.memory vm in
          ignore (St_compile.compile_and_install m ~cls:(St_boot.class_named m "Integer") ~category:"fun" "double ^self * 2");
          ignore (print ~vm "Smalltalk at: #Answer put: 21 double");
          let image = St_image.save m in
          let vm2 = St_image.load_vm image in
          check "the same bytes again" "true" (string_of_bool (St_image.save (I.memory vm2) = image));
          check "a global" "42" (print ~vm:vm2 "Answer");
          check "a method" "84" (print ~vm:vm2 "Answer double");
          check "the kernel" "158" (print ~vm:vm2 "100 factorial printString size"));
      Testo.create "BitBlt and Pen" (fun () ->
          let vm = St_boot.boot () in
          check "a black form" "1" (print ~vm "| f | f := Form extent: 16 @ 16. f fillBlack. f pixelAt: 3 @ 3");
          check "a white one" "0" (print ~vm "| f | f := Form extent: 16 @ 16. f pixelAt: 3 @ 3");
          check "xor" "0" (print ~vm "| f | f := Form extent: 16 @ 16. f fillBlack; reverse. f pixelAt: 5 @ 9");
          check "the pen goes up" "#(1 0)"
            (print ~vm "| p | Display fillWhite. p := Pen new. p go: 50. Array with: (Display pixelAt: 400 @ 260) with: (Display pixelAt: 410 @ 260)");
          check "the dragon" "a Pen" (print ~vm "Display fillWhite. Pen new dragon: 6"));
    ]
