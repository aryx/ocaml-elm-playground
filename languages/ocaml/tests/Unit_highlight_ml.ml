(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_highlight_ml.mli *)

(* each name (not the punctuation) with its category *)
let show (src : string) : string =
  Highlight_ml.categorize (Lexer_ml.tokens src)
  |> List.filter (fun ((t : Token_ml.t), _) -> t.kind <> Operator && t.kind <> Punctuation)
  |> List.map (fun ((t : Token_ml.t), c) -> t.text ^ ":" ^ Highlight_code.show c)
  |> String.concat " "

let check (what : string) (src : string) (expected : string) : unit = Alcotest.(check string) what expected (show src)

let tests =
  Testo.categorize "Highlight_ml"
    [
      Testo.create "the worked example" (fun () ->
          check "Highlight_ml.mli's" "let move (p : point) ~dx = Point.add p dx"
            "let:Keyword move:Def_function p:Parameter point:Type ~dx:Label Point:Module add:Global p:Parameter dx:Parameter");
      Testo.create "definitions" (fun () ->
          check "a value, a function by fun" "let v = 1\nlet f = fun x -> x"
            "let:Keyword v:Def_value 1:Number let:Keyword f:Def_function fun:Keyword_control x:Parameter x:Parameter";
          check "a local let" "let f x =\n  let y = x in y"
            "let:Keyword f:Def_function x:Parameter let:Keyword y:Local x:Parameter in:Keyword y:Local";
          check "a let in a struct is at the top" "module M = struct\n  let g () = 1\nend"
            "module:Keyword_module M:Def_module struct:Keyword_module let:Keyword g:Def_function 1:Number end:Keyword_module";
          check "val in a signature" "val f : int -> t\nval v : t"
            "val:Keyword f:Def_function int:Type t:Type val:Keyword v:Def_value t:Type");
      Testo.create "types" (fun () ->
          check "a variant" "type 'a t = A of 'a | B"
            "type:Keyword 'a:Type_var t:Def_type A:Constructor of:Keyword 'a:Type_var B:Constructor";
          check "a record: fields and types" "type r = { x : int; mutable y : float }"
            "type:Keyword r:Def_type x:Field int:Type mutable:Keyword y:Field float:Type";
          check "and" "type a = int\nand b = a" "type:Keyword a:Def_type int:Type and:Keyword b:Def_type a:Type");
      (* claude: what the tree (Parse_ml) says that the neighbours could
       * not: fields, and the scopes *)
      Testo.create "fields" (fun () ->
          check "read, written, built, matched" "let f p = p.x + p.M.y; p.x <- 1; { p with x = 2 }; match p with { x; _ } -> x"
            "let:Keyword f:Def_function p:Parameter p:Parameter x:Field p:Parameter M:Module y:Field p:Parameter x:Field 1:Number p:Parameter with:Keyword_control x:Field 2:Number match:Keyword_control p:Parameter with:Keyword_control x:Field _:Normal x:Local");
      Testo.create "scopes" (fun () ->
          check "a case's names are its own" "let f x = match x with Some y -> y | None -> x"
            "let:Keyword f:Def_function x:Parameter match:Keyword_control x:Parameter with:Keyword_control Some:Constructor y:Local y:Local None:Constructor x:Parameter";
          check "a parameter shadowed by a local" "let f x = let x = 1 in x"
            "let:Keyword f:Def_function x:Parameter let:Keyword x:Local 1:Number in:Keyword x:Local";
          check "a local out of its scope is not one" "let f () = (let y = 1 in y) + y"
            "let:Keyword f:Def_function let:Keyword y:Local 1:Number in:Keyword y:Local y:Normal";
          check "fun inside, and a label" "let f l = List.map (fun v ~k -> v + k) l"
            "let:Keyword f:Def_function l:Parameter List:Module map:Global fun:Keyword_control v:Parameter ~k:Label v:Parameter k:Parameter l:Parameter");
      Testo.create "capabilities" (fun () ->
          check "Cap and caps" "let f (caps : < Cap.stdout ; .. >) = Cap.x caps"
            "let:Keyword f:Def_function caps:Capability Cap:Capability stdout:Capability Cap:Capability x:Capability caps:Capability");
      Testo.create "comments" (fun () ->
          check "a banner's title"
            "(*****)\n(* Model *)\n(*****)\n(* plain *)"
            "(*****):Comment_section (* Model *):Comment_section (*****):Comment_section (* plain *):Comment");
      Testo.create "lines" (fun () ->
          let ls = Highlight_ml.lines "let x = (* a\nb *) 1\n" in
          Alcotest.(check int) "3 lines" 3 (Array.length ls);
          Alcotest.(check (list (pair int string))) "the second: the comment's end, then 1"
            [ (0, "b *)"); (5, "1") ]
            (List.map (fun (s : Highlight_code.span) -> (s.col, s.text)) ls.(1)));
    ]
