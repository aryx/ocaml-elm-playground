(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_etoys.mli *)

open Testutil_morphic

let check = Alcotest.(check string)

(* a Point an expression answers, for the mouse *)
let point (w : world) (text : string) : int * int =
  ignore (print w ("Smalltalk at: #P put: (" ^ text ^ ")"));
  (int_of_string (print w "P x rounded"), int_of_string (print w "P y rounded"))

(* a world with a car C at 100 @ 100 *)
let boot_car () : world =
  let w = boot () in
  ignore (print w "Smalltalk at: #C put: CarMorph new. W addMorph: C. C position: 100 @ 100. W doOneCycle");
  w

let tests =
  Testo.categorize "Squeak Etoys"
    [
      Testo.create "forward: and turn:, Logo's turtle on any morph" (fun () ->
          let w = boot_car () in
          check "heading 0 is upwards" "100@90" (print w "C forward: 10. C position");
          check "90 is to the right" "110@90" (print w "C turn: 90. C forward: 10. C position");
          check "turns add up, around 360" "20" (print w "C turn: 290. C heading");
          (* 72 steps of 4 turning 5 degrees: a circle. Rounding the
           * place at each step would lose the third of a pixel a
           * step goes sideways *)
          check "forward 4, turn 5, 72 times: back where it was" "110@90 20"
            (print w "72 timesRepeat: [C forward: 4. C turn: 5]. C position printString, ' ', C heading printString");
          (* 4 / (2 sin 2.5 degrees) is a radius of 45.8; in whole pixels *)
          check "half way round, it was a diameter away" "91"
            (print w "| p | p := C position. 36 timesRepeat: [C forward: 4. C turn: 5]. ((C position dist: p) + 0.5) truncated");
          check "any morph: an ellipse too" "0@-7"
            (print w "| e | e := EllipseMorph new. e forward: 7. e position");
          check "its name for the tiles" "Car Ellipse" (print w "C etoyName, ' ', EllipseMorph new etoyName"));
      Testo.create "the car, drawn turned by its heading" (fun () ->
          let w = boot_car () in
          (* its middle is 122 @ 122; the windshield 3 to 10 pixels towards the nose *)
          check "heading 0: the windshield above its middle, the body below" "Color(255 255 255) Color(255 0 0)"
            (colour w 122 115 ^ " " ^ colour w 122 132);
          ignore (print w "C heading: 180. W doOneCycle");
          check "heading 180: below" "Color(255 0 0) Color(255 255 255)" (colour w 122 115 ^ " " ^ colour w 122 129);
          ignore (print w "C heading: 90. W doOneCycle");
          check "heading 90: to the right, and the body lying down" "Color(255 255 255) Color(204 204 204)"
            (colour w 129 122 ^ " " ^ colour w 122 136));
      Testo.create "the viewer: what the car has, as it is now; what it can do, done once by its !" (fun () ->
          let w = boot_car () in
          ignore (print w "Smalltalk at: #V put: C openViewer. W doOneCycle");
          check "its window" "Car viewer" (print w "(V submorphs detect: [:m | (m isKindOf: StringMorph) and: [m contents = 'Car viewer']]) contents");
          check "its two phrases" "2" (print w "V phrases size");
          let values () = print w "((V submorphs select: [:m | m isKindOf: UpdatingStringMorph]) collect: [:m | m contents]) asArray" in
          check "x, y, heading, the last added in front" "#('0' '100' '100')" (values ());
          (* the ! before 'Car forward by 5' *)
          let x, y = point w "(V submorphs detect: [:m | (m isKindOf: SimpleButtonMorph) and: [m submorphs notEmpty and: [m bounds top = (V phrases detect: [:p | p argument value = 5 and: [p bounds top < (V phrases inject: 0 into: [:a :q | a max: q bounds top])]]) bounds top]]]) bounds origin + 3" in
          click w x y;
          check "forward by 5, once: its y shown at the next cycle" "#('0' '95' '100')" (values ()));
      Testo.create "a script: a phrase dragged out of the viewer, another dropped into it, ticking" (fun () ->
          let w = boot_car () in
          ignore (print w "Smalltalk at: #V put: C openViewer. V position: 150 @ 20. W doOneCycle");
          ignore (print w "Smalltalk at: #Forward put: (V phrases detect: [:p | p bounds top = (V phrases inject: 9999 into: [:a :q | a min: q bounds top])])");
          ignore (print w "Smalltalk at: #Turn put: (V phrases detect: [:p | p ~~ Forward])");
          (* the forward phrase, by its words *)
          let x, y = point w "Forward bounds origin + 6" in
          move w x y 4;
          check "a copy in the hand: the viewer keeps its own" "true 2" (print w "W hand submorphs first isPhraseTile printString, ' ', V phrases size printString");
          move w 60 220 4;
          move w 60 220 0;
          check "put down on the world: a script, the phrase in it" "1 false"
            (print w "Smalltalk at: #S put: (W submorphs detect: [:m | m isKindOf: ScriptEditorMorph]). S phrases size printString, ' ', S isTicking printString");
          (* the turn phrase, dropped into the script *)
          let x, y = point w "Turn bounds origin + 6" in
          move w x y 4;
          let sx, sy = point w "S bounds corner - 6" in
          move w sx sy 4;
          move w sx sy 0;
          check "dropped into it: under the first" "2 true"
            (print w "S phrases size printString, ' ', (S phrases last bounds top > S phrases first bounds top) printString");
          check "nothing runs while it is paused" "100@100" (print w "W doOneCycle. C position");
          (* its number: a click, then typed over *)
          let nx, ny = point w "S phrases first argument bounds origin + 2" in
          click w nx ny;
          typed w "12";
          check "a number typed over the 5" "12" (print w "S phrases first argument value");
          let bx, by = point w "(S submorphs detect: [:m | m isKindOf: SimpleButtonMorph]) bounds origin + 3" in
          click w bx by;
          check "its button: ticking" "true" (print w "S isTicking");
          (* the cycle of the click did them once already: 12 up, then 12 at 5 degrees *)
          check "each cycle, forward 12 and turn 5" "101@76 10" (print w "W doOneCycle. C position printString, ' ', C heading printString");
          (* the forward phrase taken out of the script *)
          let x, y = point w "S phrases first bounds origin + 6" in
          move w x y 4;
          move w 300 250 4;
          check "a phrase taken out: the script goes on with the rest" "1" (print w "S phrases size");
          check "the car only turns" "true" (print w "| p | p := C position. W doOneCycle. C position = p and: [C heading > 10]"));
      Testo.create "the halo's viewer handle; a morph dropped where nothing wants it lands in the world" (fun () ->
          let w = boot_car () in
          click ~button:1 w 122 122;
          let x, y = point w "(W submorphs detect: [:m | m isHalo]) submorphs last bounds origin + 5" in
          click w x y;
          check "a viewer of the car" "true" (print w "(W submorphs detect: [:m | m isKindOf: ViewerMorph]) target == C");
          (* the car dropped on the viewer: a window does not take it *)
          ignore (print w "(W submorphs detect: [:m | m isKindOf: ViewerMorph]) position: 180 @ 150. W doOneCycle");
          move w 122 122 4;
          let vx, vy = point w "(W submorphs detect: [:m | m isKindOf: ViewerMorph]) bounds origin + (60 @ 100)" in
          move w vx vy 4;
          move w vx vy 0;
          check "in the world, in front" "true" (print w "C owner == W and: [W submorphs first == C]"));
    ]
