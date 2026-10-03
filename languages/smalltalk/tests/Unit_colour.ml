(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_colour.mli *)

module I = St_interp
module B = St_colorblt

let check = Alcotest.(check string)
let squeak = lazy (St_boot.boot ~kernel:St_kernel.squeak ())

(* what an expression prints; a String, without its quotes *)
let print (text : string) : string =
  match I.evaluate (Lazy.force squeak) ~budget:50_000_000 text with
  | Ok v ->
      let s = I.print_string (Lazy.force squeak) v in
      let n = String.length s in
      if n >= 2 && s.[0] = '\'' && s.[n - 1] = '\'' then String.sub s 1 (n - 2) else s
  | Error e -> "error: " ^ e

(* a pixel of 32 bits, as its four bytes *)
let argb (p : int) : string =
  Printf.sprintf "%d %d %d %d" ((p lsr 24) land 255) ((p lsr 16) land 255) ((p lsr 8) land 255) (p land 255)

let tests =
  Testo.categorize "Squeak colour"
    [
      Testo.create "the rules on a pixel: St_colorblt.mli's worked examples" (fun () ->
          let red_glass = 0x80FF0000 and white = 0xFFFFFFFF and blue = 0xFF0000FF in
          check "red of alpha 128 over white: pink" "255 255 127 127" (argb (B.combine ~rule:24 ~depth:32 red_glass white));
          check "opaque: the source" "255 0 0 255" (argb (B.combine ~rule:24 ~depth:32 blue white));
          check "alpha 0: the destination" "255 255 255 255" (argb (B.combine ~rule:24 ~depth:32 0x00FF0000 white));
          check "paint: 0 is transparent" "7 9" (Printf.sprintf "%d %d" (B.combine ~rule:25 ~depth:8 0 7) (B.combine ~rule:25 ~depth:8 9 7));
          check "store" "255 0 0 255" (argb (B.combine ~rule:3 ~depth:32 blue white));
          check "reverse, twice: back" "255 0 0 255" (argb (B.combine ~rule:6 ~depth:32 white (B.combine ~rule:6 ~depth:32 white blue))));
      Testo.create "BitBlt in colour: a row at a time is the definition, a pixel at a time" (fun () ->
          let rng = Random.State.make [| 1996 |] in
          let int n = Random.State.int rng n in
          let new_form depth w h : B.form =
            let stride = B.stride ~depth w in
            { bits = Bytes.init (stride * h) (fun _ -> Char.chr (int 256)); w; h; stride; depth }
          in
          for trial = 1 to 4000 do
            let depth = if int 2 = 0 then 8 else 32 in
            let dest = new_form depth (1 + int 40) (1 + int 12) in
            let source, map =
              match int 5 with
              | 0 -> (None, None)
              | 1 -> (Some dest, None)
              | 2 -> (Some (new_form 1 (1 + int 40) (1 + int 12)), Some [| 0; 0x80C04020 land if depth = 8 then 255 else -1 |])
              | _ -> (Some (new_form depth (1 + int 40) (1 + int 12)), None)
            in
            let halftone = match int 3 with 0 -> Some (new_form depth 1 1) | 1 -> Some (new_form depth 3 2) | _ -> None in
            (* stores mostly, what a row at a time does; then every rule *)
            let rule = match int 4 with 0 -> [| 24; 25; 6; 1; 7; 12 |].(int 6) | _ -> 3 in
            let rule = if rule = 24 && depth = 8 then 25 else rule in
            let dx = int 44 - 2 and dy = int 14 - 1 in
            let sx = int 6 and sy = int 4 in
            let x0 = max dx 0 and y0 = max dy 0 in
            let x1 = min (dx + int 44) dest.w and y1 = min (dy + int 14) dest.h in
            (* inside the source too, as copy_bits clips *)
            let x1, y1 =
              match source with None -> (x1, y1) | Some f -> (min x1 (dx - sx + f.w), min y1 (dy - sy + f.h))
            in
            let run simple =
              let d = { dest with bits = Bytes.copy dest.bits } in
              let source = match source with Some f when f == dest -> Some d | s -> s in
              B.blit ~simple ~dest:d ~source ~map ~halftone ~rule ~dx ~dy ~sx ~sy (x0, y0, x1, y1);
              Bytes.to_string d.bits
            in
            if run true <> run false then
              Alcotest.failf "trial %d: depth %d, rule %d, %dx%d at %d,%d from %d,%d, rectangle %d,%d to %d,%d" trial depth rule
                dest.w dest.h dx dy sx sy x0 y0 x1 y1
          done);
      Testo.create "a Color: its parts, its pixel at each depth" (fun () ->
          check "from 0 to 1" "Color(255 128 0)" (print "Color r: 1 g: 1/2 b: 0");
          check "and back" "128/255" (print "(Color r: 1 g: 1/2 b: 0) green");
          check "glass" "Color(255 0 0 alpha 128)" (print "Color red alpha: 1/2");
          check "equal" "true" (print "Color red = (Color r: 1 g: 0 b: 0)");
          check "32 bits: alpha, red, green, blue" "ByteArray (255 255 128 0 )" (print "(Color r: 1 g: 1/2 b: 0) asBytes");
          check "8 bits: 1 + 36 * 5 + 6 * 3 + 0" "199" (print "(Color r: 1 g: 1/2 b: 0) index");
          check "the palette's colour is the nearest" "Color(255 153 0)" (print "Color fromIndex: 199");
          check "0 is transparent" "0" (print "Color transparent index");
          check "yellow is light, blue dark" "#(false true)" (print "Array with: Color yellow isDark with: Color blue isDark"));
      Testo.create "Forms with a depth: filled, read, blended" (fun () ->
          check "the Blue Book's Forms have one bit" "1" (print "(Form extent: 16 @ 16) depth");
          check "32 bits: four bytes a pixel" "a Form(10x5x32) 200" (print "| f | f := Form extent: 10 @ 5 depth: 32. f printString, ' ', f bits size printString");
          check "8 bits: rows padded to 4 bytes" "60" (print "(Form extent: 10 @ 5 depth: 8) bits size");
          ignore (print "Smalltalk at: #F put: (Form extent: 10 @ 5 depth: 32). F fillColor: Color white");
          check "filled" "Color(255 255 255)" (print "F colorAt: 9 @ 4");
          check "a rectangle of it" "Color(255 0 0) Color(255 255 255)"
            (print "F fill: (2 @ 1 corner: 4 @ 3) color: Color red. (F colorAt: 3 @ 2) printString, ' ', (F colorAt: 4 @ 2) printString");
          check "glass over white: pink; over red: red" "Color(255 127 127) Color(255 0 0)"
            (print "F fill: (3 @ 0 corner: 6 @ 5) color: (Color red alpha: 1/2). (F colorAt: 5 @ 2) printString, ' ', (F colorAt: 3 @ 2) printString");
          check "8 bits: the palette's nearest" "Color(255 153 0)"
            (print "| f | f := Form extent: 10 @ 5 depth: 8. f fillColor: (Color r: 1 g: 1/2 b: 0). f colorAt: 7 @ 3");
          check "a rule of bits still works: reversed" "Color(0 255 255 alpha 0)"
            (print "| f | f := Form extent: 4 @ 4 depth: 32. f fillColor: Color red. f reverse. f colorAt: 1 @ 1"));
      Testo.create "a Form drawn on one of another depth: through a colour map" (fun () ->
          ignore (print "Smalltalk at: #F put: (Form extent: 20 @ 20 depth: 32). F fillColor: Color yellow");
          check "8 on 32: by the palette, 0 transparent" "Color(0 0 255) Color(255 255 0)"
            (print "| s | s := Form extent: 8 @ 8 depth: 8. s fill: (0 @ 0 corner: 4 @ 8) color: Color blue. s displayOn: F at: 2 @ 2. (F colorAt: 3 @ 3) printString, ' ', (F colorAt: 7 @ 3) printString");
          check "1 on 32: its black, in black" "Color(0 0 0) Color(255 255 0)"
            (print "| s | s := Form extent: 8 @ 8. s fill: (0 @ 0 corner: 4 @ 8) rule: 15. s displayOn: F at: 10 @ 10. (F colorAt: 11 @ 11) printString, ' ', (F colorAt: 15 @ 11) printString");
          check "clipped to the source: nothing past its width" "Color(255 255 0)" (print "F colorAt: 18 @ 11");
          check "32 on 8: refused" "error: A Form of more colours cannot be drawn on one of fewer"
            (print "F displayOn: (Form extent: 4 @ 4 depth: 8) at: 0 @ 0");
          check "the Blue Book's BitBlt, untouched" "1" (print "| f | f := Form extent: 16 @ 16. f fillBlack. f pixelAt: 3 @ 3"));
      Testo.create "a font: Hershey's strokes drawn into a strike" (fun () ->
          (* 33 pixels high: a pixel a unit, Text.st's worked example *)
          ignore (print "Smalltalk at: #Font put: (StrikeFont height: 33)");
          check "A is 18 wide, from I to [" "18" (print "Font widthOf: $A");
          check "it starts after the 32 glyphs before it" "true" (print "(Font leftOf: $A) = ((32 to: 64) inject: 0 into: [:sum :c | sum + (Font widthOf: (Character value: c))])");
          (* the apex at (0, -12), the bar from (-5, 2) to (5, 2), a foot at (-8, 9): 9 and 16 added *)
          check "its apex, its bar, its left foot; not its inside" "1110"
            (print "| at | at := [:x :y | (Font glyphs pixelAt: (Font leftOf: $A) + x @ y) printString]. (at value: 9 value: 4), (at value: 9 value: 18), (at value: 1 value: 25), (at value: 9 value: 12)");
          check "the baseline" "25" (print "Font ascent");
          check "a string's width" "true" (print "(Font widthOfString: 'AB') = ((Font widthOf: $A) + (Font widthOf: $B))");
          check "the default: 17 pixels, drawn once" "true" (print "StrikeFont default == StrikeFont default and: [StrikeFont default height = 17]"));
      Testo.create "a string drawn: a BitBlt a character, the colour a map" (fun () ->
          ignore (print "Smalltalk at: #Font put: (StrikeFont height: 33). Smalltalk at: #F put: (Form extent: 100 @ 40 depth: 32). F fillColor: Color white");
          check "answers where it ends: 10, A's 18, B's 21" "49" (print "F drawString: 'AB' at: 10 @ 4 font: Font color: Color red");
          check "A's apex red, the paper white around" "Color(255 0 0) Color(255 255 255)"
            (print "(F colorAt: 19 @ 8) printString, ' ', (F colorAt: 12 @ 8) printString");
          check "ink of glass" "Color(127 127 255)"
            (print "F fillColor: Color white. F drawString: 'A' at: 10 @ 4 font: Font color: (Color blue alpha: 1/2). F colorAt: 19 @ 8");
          check "at 8 bits" "Color(0 0 255)"
            (print "| f | f := Form extent: 100 @ 40 depth: 8. f drawString: 'A' at: 10 @ 4 font: Font color: Color blue. f colorAt: 19 @ 8");
          check "at 1 bit: black on white, white on black" "10"
            (print "| f a | f := Form extent: 100 @ 40. f drawString: 'A' at: 10 @ 4 font: Font color: Color black. a := f pixelAt: 19 @ 8. f fillBlack. f drawString: 'A' at: 10 @ 4 font: Font color: Color white. a printString, (f pixelAt: 19 @ 8) printString"));
    ]
