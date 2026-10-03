(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tools.mli *)

open Testutil_morphic

let check = Alcotest.(check string)

(* a text's returns shown as bars *)
let bars (s : string) : string = String.map (fun c -> if c = '\r' then '|' else c) s

let tests =
  Testo.categorize "Squeak tools"
    [
      Testo.create "a text: a selection by the mouse, replaced by what is typed; the arrows up and down" (fun () ->
          let w = boot () in
          ignore (print w "Smalltalk at: #T put: TextMorph new. W addMorph: T. T position: 50 @ 50. T contents: 'Hello world', (String with: (Character value: 13)), 'second'");
          ignore (print w "Smalltalk at: #X put: [:n | 53 + (StrikeFont default widthOfString: ('Hello world' copyFrom: 1 to: n)) + 1]");
          let x n = int_of_string (print w (Printf.sprintf "X value: %d" n)) in
          move w (x 6) 55 4;
          move w (x 11) 55 4;
          move w (x 11) 55 0;
          check "dragged over a word" "world" (print w "T selection");
          check "it is drawn over the highlight" "Color(179 204 255)" (colour w (x 6 + 1) 53);
          typed w "there";
          check "typed over it" "Hello there|second" (bars (print w "T contents"));
          typed w "\031";
          check "an arrow down: the same place a line below, or its end" "18" (print w "T cursor");
          typed w "\030\030";
          check "up, and up again stays on the first" "true" (print w "T cursor <= 11"));
      Testo.create "the Workspace: print it, do it, the compiler's complaint; from its menu" (fun () ->
          let w = boot () in
          ignore (print w "Smalltalk at: #T put: Workspace open submorphs first");
          check "a window with a text" "#TextMorph" (print w "T class name");
          ignore (print w "W doOneCycle");
          click w (int_of_string (print w "T bounds left + 20")) (int_of_string (print w "T bounds top + 20"));
          typed w "3 + 4 * 2";
          check "print it: the line's answer after it, selected" "3 + 4 * 2 14| 14"
            (print w "T printIt. T contents, '|', T selection");
          typed w "\b";
          check "a backspace takes the answer away" "3 + 4 * 2" (print w "T contents");
          typed w "\rSmalltalk at: #Z put: 6 * 7";
          check "do it: evaluated, nothing shown" "42" (print w "T doIt. Smalltalk at: #Z");
          typed w "\r3 +";
          check "what does not compile: why, after it" "true" (print w "T printIt. T selection size > 3 and: [(T contents occurrencesOf: $+) = 2]");
          (* the yellow button's menu, its second item *)
          ignore (print w "T contents: '100 factorial printString size'");
          let tx = int_of_string (print w "T bounds left + 20") and ty = int_of_string (print w "T bounds top + 8") in
          click ~button:2 w tx ty;
          check "a menu" "true" (print w "W submorphs first isMenu");
          ignore (print w "Smalltalk at: #P put: (W submorphs first submorphs at: 2) bounds origin + 3");
          click w (int_of_string (print w "P x")) (int_of_string (print w "P y"));
          check "print it, from the menu" "100 factorial printString size 158" (print w "T contents");
          (* an error: the cycle that ran it stops there, the next ones go on *)
          ignore (print w "T contents: 'nil foo'");
          click ~button:2 w tx ty;
          ignore (print w "Smalltalk at: #P put: (W submorphs first submorphs at: 2) bounds origin + 3");
          let px = int_of_string (print w "P x") and py = int_of_string (print w "P y") in
          move w px py 4;
          set_mouse w px py 0;
          check "an error in what is evaluated stops the cycle" "error: Message not understood: foo" (print w "W doOneCycle");
          check "and the next cycle goes on: the menu gone, the text as it was" "nil foo 0"
            (print w "W doOneCycle. T contents, ' ', (W submorphs select: [:m | m isMenu]) size printString");
          typed w "x";
          check "and takes what is typed" "true" (print w "T contents includes: $x"));
      Testo.create "the Transcript's window shows what is shown, at the next cycle" (fun () ->
          let w = boot () in
          ignore (print w "Smalltalk at: #T put: Transcript open submorphs first. W doOneCycle");
          ignore (print w "Transcript show: 'Hello'; cr; show: 3 + 4");
          check "not before the cycle" "" (print w "T contents");
          check "its step" "Hello|7" (bars (print w "W doOneCycle. T contents")));
      Testo.create "what the host needs: an error said in the Transcript, the Display's pixels" (fun () ->
          let w = boot () in
          check "no window yet" "0" (print w "(W submorphs select: [:m | m isKindOf: SystemWindow]) size");
          ignore (print w "Transcript showError: 'Message not understood: foo'. W doOneCycle");
          check "said, in a window opened for it" "Message not understood: foo|"
            (bars (print w "(W submorphs detect: [:m | m isKindOf: SystemWindow]) submorphs first contents"));
          check "a second one: the same window" "1"
            (print w "Transcript showError: 'Halt'. (W submorphs select: [:m | m isKindOf: SystemWindow]) size");
          (* red, green, blue, alpha, a row after the other *)
          let pixels text =
            let vm = St_boot.boot ~kernel:St_kernel.squeak () in
            match St_interp.evaluate vm text with
            | Ok f -> (
                match St_colorblt.rgba (St_interp.memory vm) f with
                | Some (w, h, b) -> Printf.sprintf "%dx%d %s" w h (String.concat " " (List.init (Bytes.length b) (fun i -> string_of_int (Char.code (Bytes.get b i)))))
                | None -> "not a Form")
            | Error e -> e
          in
          check "32 bits: the alpha last" "2x1 255 128 0 255 0 0 0 0"
            (pixels "| f | f := Form extent: 2 @ 1 depth: 32. f fill: (0 @ 0 corner: 1 @ 1) color: (Color r: 1 g: 1/2 b: 0). f");
          check "8 bits: by the palette" "1x1 0 0 255 255" (pixels "| f | f := Form extent: 1 @ 1 depth: 8. f fillColor: Color blue. f");
          check "1 bit: black and white" "2x1 0 0 0 255 255 255 255 255" (pixels "| f | f := Form extent: 2 @ 1. f fill: (0 @ 0 corner: 1 @ 1) rule: 15. f"));
      Testo.create "the Inspector: an object's fields, and a text where self is the object" (fun () ->
          let w = boot () in
          ignore (print w "Smalltalk at: #I put: (Inspector openOn: 3 @ 4)");
          check "its fields" "#('self' 'x' 'y')" (print w "I fieldList items");
          check "self, printed" "3@4" (print w "I valuePane contents");
          check "a field picked" "4" (print w "I fieldList select: 3. I valuePane contents");
          check "self is the point: its variables are there" "x * y 12" (print w "I valuePane contents: 'x * y'. I valuePane printIt. I valuePane contents");
          check "an Array's elements by number" "#('self' '1' '2')" (print w "(Inspector openOn: #(7 8)) fieldList items");
          check "inspect opens one" "3" (print w "#foo inspect. (W submorphs select: [:m | m isKindOf: SystemWindow]) size");
          (* the halo's handle, at the bottom left *)
          ignore (print w "W submorphs copy do: [:m | m delete]. Smalltalk at: #R put: RectangleMorph new. W addMorph: R. R position: 300 @ 200");
          click ~button:1 w 310 210;
          click w 293 247;
          check "the halo's inspect handle: an Inspector on the morph" "RectangleMorph"
            (print w "((W submorphs detect: [:m | m isKindOf: SystemWindow]) submorphs detect: [:m | m isKindOf: StringMorph]) contents"));
      Testo.create "the Browser: the system's classes and methods, from the system itself" (fun () ->
          let w = boot ~size:(700, 450) () in
          ignore (print w "Smalltalk at: #B put: Browser open. W doOneCycle");
          check "the categories, Morphic's among them" "true" (print w "B categoryList items includes: 'Morphic-Kernel'");
          check "a category's classes" "#(#Morph #HandMorph #PasteUpMorph)" (print w "B categoryList selectItem: 'Morphic-Kernel'. B classList items");
          check "a class: its definition" "Object subclass: #Morph true"
            (print w
               "B classList selectItem: #Morph. (B codePane contents copyFrom: 1 to: 23), ' ', (B codePane contents includesSubstring: 'instanceVariableNames: ''bounds owner submorphs color''') printString");
          check "its protocols" "true" (print w "B protocolList items includes: 'drawing'");
          check "a protocol's selectors" "#(#drawOn: #fullDrawOn:)" (print w "B protocolList selectItem: 'drawing'. B selectorList items");
          check "a method's source" "drawOn: aCanvas"
            (print w "B selectorList selectItem: #drawOn:. B codePane contents copyFrom: 1 to: 15");
          check "the class side" "true"
            (print w "B switchSide. B protocolList selectItem: 'instance creation'. B selectorList items includes: #new");
          check "a method's lines end with returns" "1"
            (print w "B selectorList selectItem: #new. (B codePane contents occurrencesOf: (Character value: 13)) printString");
          (* a click on the first list's third row *)
          ignore (print w "Smalltalk at: #P put: B categoryList bounds origin + (10 @ (StrikeFont default height * 2 + 6))");
          click w (int_of_string (print w "P x")) (int_of_string (print w "P y"));
          check "a click picks a row: the third shown, the list scrolled to Morphic's" "true"
            (print w
               "(B categoryList selectedItem ~= 'Morphic-Kernel') and: [B classList items notEmpty and: [B classList items = (B classNamesIn: B categoryList selectedItem) asArray]]"));
      Testo.create "the Browser's accept: Morph>>drawOn: changed, every morph drawn its new way" (fun () ->
          let w = boot ~size:(760, 480) () in
          ignore (print w "Smalltalk at: #M put: Morph new. W addMorph: M. M position: 700 @ 440. Smalltalk at: #B put: Browser open. W doOneCycle");
          check "a morph, blue" "Color(0 0 255)" (colour w 710 450);
          ignore (print w "B categoryList selectItem: 'Morphic-Kernel'. B classList selectItem: #Morph. B protocolList selectItem: 'drawing'. B selectorList selectItem: #drawOn:");
          ignore
            (print w
               "B codePane contents: 'drawOn: aCanvas', (String with: (Character value: 13)), '\taCanvas fillRectangle: bounds color: Color red'. B codePane accept. W restoreDisplay. W doOneCycle");
          check "accepted: it is red, with no other change" "Color(255 0 0)" (colour w 710 450);
          check "the list still shows it picked" "#drawOn:" (print w "B selectorList selectedItem");
          check "what does not compile is not installed: why, in the text" "true"
            (print w "B codePane contents: 'drawOn: aCanvas ^^'. B codePane accept. B codePane selection size > 0");
          check "still red" "Color(255 0 0)" (ignore (print w "W restoreDisplay. W doOneCycle"); colour w 710 450);
          check "a new method, in the protocol picked" "#(#drawOn: #fullDrawOn: #twice)"
            (print w "B codePane contents: 'twice ^2'. B codePane accept. B selectorList items");
          check "and understood" "2" (print w "M twice");
          check "a class's definition accepted: a new instance variable" "true"
            (print w
               "B categoryList selectItem: 'Morphic-Demo'. B classList selectItem: #AtomMorph. B codePane contents: 'EllipseMorph subclass: #AtomMorph instanceVariableNames: ''velocity mass'' classVariableNames: '''' poolDictionaries: '''' category: ''Morphic-Demo'''. B codePane accept. (AtomMorph instanceVariableNames includes: 'mass') and: [B classList selectedItem == #AtomMorph and: [B codePane contents includesSubstring: 'velocity mass']]"));
    ]
