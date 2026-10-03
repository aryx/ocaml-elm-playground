(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_highlight_st.mli *)

module H = Highlight_code

let check = Alcotest.(check string)

(* each name, keyword and symbol with its category *)
let names (src : string) : string =
  Highlight_st.categorize src
  |> List.filter (fun (text, _) -> text <> "" && (let c = text.[0] in (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '^'))
  |> List.map (fun (text, c) -> text ^ ":" ^ H.show c)
  |> String.concat " "

(* a file: a class, then its methods *)
let file =
  {st|"A comment."!

Object subclass: #Morph
	instanceVariableNames: 'bounds owner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic'!

!Morph methodsFor: 'drawing'!
drawOn: aCanvas
	"What it looks like."
	| r |
	r := bounds.
	^aCanvas fill: r color: Color red!

at: i put: x
	owner == nil ifTrue: [^self].
	#(1 two $c) do: [:each | | t | t := each. x add: t]! !

!Morph class methodsFor: 'instance creation'!
new
	<primitive: 70>
	^super new initialize! !
|st}

let tests =
  Testo.categorize "Highlight_st"
    [
      Testo.create "the worked example: a method's pattern, its names" (fun () ->
          check "Highlight_st.mli's"
            "drawOn::Def_function aCanvas:Parameter r:Local r:Local bounds:Field ^:Keyword_control aCanvas:Parameter fill::Normal r:Local \
             color::Normal Color:Global red:Normal"
            (names "Object subclass: #Morph instanceVariableNames: 'bounds owner'!\n!Morph methodsFor: 'drawing'!\ndrawOn: aCanvas\n\t| r |\n\tr := bounds.\n\t^aCanvas fill: r color: Color red! !"
             |> fun s ->
             (* from the method on *)
             let i = String.index s 'd' in
             let rec find i = if String.sub s i 7 = "drawOn:" then i else find (i + 1) in
             let j = find i in
             String.sub s j (String.length s - j)));
      Testo.create "the chunks of a file: a class defined, the methods' line, each method" (fun () ->
          let cats = Highlight_st.categorize file in
          let cat text = H.show (List.assoc text cats) in
          check "the file's comment" "Comment" (cat "\"A comment.\"");
          check "the class defined" "Def_type" (cat "Morph");
          check "the line that opens its methods" "Comment_section" (cat "methodsFor:");
          check "a keyword pattern: both keywords, both arguments" "at::Def_function i:Parameter put::Def_function x:Parameter"
            (String.concat " " (List.map (fun t -> t ^ ":" ^ cat t) [ "at:"; "i"; "put:"; "x" ]));
          check "a field of the class, a pseudo-variable, a control message" "Field Keyword Keyword_control"
            (String.concat " " [ cat "owner"; cat "self"; cat "ifTrue:" ]);
          check "a literal array's names are symbols; a block's argument and temporary" "Constructor Parameter Local"
            (String.concat " " [ cat "two"; cat "each"; cat "t" ]);
          check "a primitive" "Attribute" (cat "primitive:");
          check "a method of the class itself has no fields" "new:Def_function"
            (List.find_map (fun (t, c) -> if t = "new" && c = H.Def_function then Some "new:Def_function" else None) cats |> Option.value ~default:"none"));
      Testo.create "what a file defines, the classes it names, where its names are bound" (fun () ->
          let an = Highlight_st.analyze file in
          check "its class, its methods as Class>>selector" "Morph Morph class>>new Morph>>at:put: Morph>>drawOn:"
            (String.concat " " (List.sort compare (List.map (fun (d : H.definition) -> d.dname) an.definitions)));
          check "the class's place: its name after the #" "2:18"
            (List.find_map (fun (d : H.definition) -> if d.dname = "Morph" then Some (Printf.sprintf "%d:%d" d.dline d.dcol) else None) an.definitions
             |> Option.value ~default:"");
          check "the classes named and not defined here" "Color Object"
            (String.concat " " (List.sort_uniq compare (List.map (fun (r : H.reference) -> r.rname) an.references)));
          (* aCanvas: bound in the pattern (line 9), used on line 13 *)
          let uses =
            List.filter (fun (o : H.occurrence) -> o.bound_at = (9, 8)) an.occurrences |> List.map (fun (o : H.occurrence) -> o.line) |> List.sort compare
          in
          Alcotest.(check (list int)) "an argument's uses" [ 9; 13 ] uses;
          Alcotest.(check int) "a line of spans a line of the file" (List.length (String.split_on_char '\n' file)) (Array.length an.spans));
      Testo.create "the kernels' files: read whole, every method found" (fun () ->
          List.iter
            (fun (name, src) ->
              let an = Highlight_st.analyze src in
              let methods = List.length (List.filter (fun (d : H.definition) -> d.dspace = H.Value) an.definitions) in
              (* as many as the compiler finds: a chunk file's methods *)
              let expected =
                List.fold_left (fun n item -> match item with St_chunk.Methods m -> n + List.length m.methods | St_chunk.Doit _ -> n) 0 (St_chunk.read src)
              in
              Alcotest.(check int) (name ^ ": its methods") expected methods;
              (* a span never past its line's end *)
              let lines = Array.of_list (String.split_on_char '\n' src) in
              Array.iteri
                (fun y spans ->
                  List.iter
                    (fun (s : H.span) ->
                      if s.col + String.length s.text > String.length lines.(y) then Alcotest.failf "%s, line %d: a span past the line" name (y + 1))
                    spans)
                an.spans)
            St_kernel.squeak;
          let morphic = List.assoc "squeak/Morphic.st" St_kernel.squeak in
          let cats = Highlight_st.categorize morphic in
          Alcotest.(check bool) "Morphic.st: bounds is a field, somewhere" true (List.mem ("bounds", H.Field) cats));
    ]
