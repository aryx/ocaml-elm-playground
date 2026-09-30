(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_highlight_c.mli *)

(* the tokens, as "Kind text" separated by two spaces *)
let tokens (src : string) : string =
  Lexer_c.tokens src |> List.map (fun (t : Token_c.t) -> Token_c.show_kind t.kind ^ " " ^ t.text) |> String.concat "  "

(* each name with its category *)
let names (src : string) : string =
  Highlight_c.categorize (Lexer_c.tokens src)
  |> List.filter (fun ((t : Token_c.t), _) -> t.kind = Ident || t.kind = Keyword || t.kind = Directive)
  |> List.map (fun ((t : Token_c.t), c) -> t.text ^ ":" ^ Highlight_code.show c)
  |> String.concat " "

(* a file whose items all parse *)
let whole (what : string) (src : string) : unit =
  Alcotest.(check int) what 0 (List.length (Parse_c.parse (Lexer_c.tokens src)).skipped)

let check f what src expected = Alcotest.(check string) what expected (f src)

(* claude: a one-line source's bindings: each binding's column, then its
 * places' (itself and its uses), by column *)
let bindings (src : string) : string =
  let occs = (Highlight_c.analyze src).occurrences in
  let groups = List.sort_uniq compare (List.map (fun (o : Highlight_code.occurrence) -> snd o.bound_at) occs) in
  List.map
    (fun b ->
      let cols = List.sort compare (List.filter_map (fun (o : Highlight_code.occurrence) -> if snd o.bound_at = b then Some o.col else None) occs) in
      Printf.sprintf "%d: %s" b (String.concat " " (List.map string_of_int cols)))
    groups
  |> String.concat ", "

let tests =
  Testo.categorize "Highlight_c"
    [
      Testo.create "the lexer's worked example" (fun () ->
          check tokens "Lexer_c.mli's" "#define N 10 /* max */\nstatic int f(char *s) { return s[0] == 'a'; }"
            "Directive #define  Ident N  Int 10  Comment /* max */  Keyword static  Keyword int  Ident f  Punctuation (  Keyword char  Operator *  Ident s  Punctuation )  Punctuation {  Keyword return  Ident s  Punctuation [  Int 0  Punctuation ]  Operator ==  Char 'a'  Punctuation ;  Punctuation }");
      Testo.create "the preprocessor's lines" (fun () ->
          check tokens "#include's file, a continued line, # in a macro"
            "#include <u.h>\n#define S(x) \\\n  #x\ny"
            "Directive #include  String <u.h>  Directive #define  Ident S  Punctuation (  Ident x  Punctuation )  Operator #  Ident x  Ident y";
          Alcotest.(check (list bool)) "pp to the line's end" [ true; true; false ]
            (List.map (fun (t : Token_c.t) -> t.pp) (Lexer_c.tokens "#define N\n1")));
      Testo.create "the highlighter's worked example" (fun () ->
          check names "Highlight_c.mli's" "static int f(Proc *p) { int n = p->len; return n; }"
            "static:Keyword int:Type f:Def_function Proc:Type p:Parameter int:Type n:Local p:Parameter len:Field return:Keyword_control n:Local");
      Testo.create "definitions" (fun () ->
          check names "typedef, struct, enum, global, prototype"
            "typedef struct Foo Foo;\nstruct Foo { int x; Lock; };\nenum { Qdir, Qfile };\nint count;\nvoid g(int);"
            "typedef:Keyword struct:Keyword Foo:Type Foo:Def_type struct:Keyword Foo:Def_type int:Type x:Field Lock:Type enum:Keyword Qdir:Constructor Qfile:Constructor int:Type count:Def_value void:Type g:Def_function int:Type";
          check names "a #define's name and parameters" "#define MAX(a, b) ((a) > (b) ? (a) : c)"
            "#define:Keyword_module MAX:Def_function a:Parameter b:Parameter a:Parameter b:Parameter a:Parameter c:Normal");
      Testo.create "scopes" (fun () ->
          check names "a local in its block only" "void f(int a) { { int b = a; } b = 1; }"
            "void:Type f:Def_function int:Type a:Parameter int:Type b:Local a:Parameter b:Normal";
          check names "for's own, a label" "void f(void) { for (int i = 0; i < 9; i++) goto out; out: ; }"
            "void:Type f:Def_function void:Type for:Keyword_control int:Type i:Local i:Local i:Local goto:Keyword_control out:Label out:Label");
      Testo.create "bindings" (fun () ->
          check bindings "a parameter, a local, a block's own a" "int f(int a) { int b = a; { int a = b; return a; } return a; }"
            "4: 4, 10: 10 23 58, 19: 19 36, 32: 32 46";
          check bindings "a #define's parameters" "#define MAX(a, b) ((a) > (b) ? (a) : c)" "8: 8, 12: 12 20 32, 15: 15 26");
      Testo.create "top-level bindings" (fun () ->
          check bindings "the definition, not the prototype"
            "static int g(int); int f(void) { return g(1); } static int g(int a) { return a; }" "23: 23, 59: 11 40 59, 65: 65 77";
          (* claude: typedef struct P P; with P's body in the file: the type
           * is the struct's, its uses bound to the body (the author: the
           * typedef is C's quirk, struct Window what matters) *)
          check bindings "typedef struct P P: the struct's" "struct P { int x; }; typedef struct P P; P *new(struct P *p) { return p; }"
            "7: 7 36 38 41 55, 44: 44, 58: 58 70";
          check bindings "typedef struct P Q: apart" "struct P { int x; }; typedef struct P Q; Q *new(struct P *p) { return p; }"
            "7: 7 36 55, 38: 38 41, 44: 44, 58: 58 70");
      Testo.create "fields" (fun () ->
          check names "read, written, designated" "void f(S *s) { s->a.b = 1; S t = { .c = 2 }; }"
            "void:Type f:Def_function S:Type s:Parameter s:Parameter a:Field b:Field S:Type t:Local c:Field");
      Testo.create "types not declared: the shapes" (fun () ->
          whole "a name then a name, stars then a name, a cast"
            "void f(void) { Proc *p; Chan c; p = (Proc*)x; n = (ulong)-1; q = (Qid){0}; }";
          whole "K&R" "main(argc, argv)\nchar **argv;\n{ return 0; }");
      Testo.create "macros" (fun () ->
          whole "ARGBEGIN, a statement's prefix, a loop, no semicolon"
            "void main(int argc, char **argv) {\n  ARGBEGIN{ case 'v': v++; break; }ARGEND\n  DBG print(\"x\");\n  TLOOP(i) { f(i); }\n  USED(argc)\n  return;\n}";
          whole "#if 0 and #else: one branch read" "#if 0\nint f( {\n#else\nint g(void);\n#endif\n#ifdef X\nint h(\n#else\nint h(void);\n#endif\n#ifdef X\n) { }\n#endif";
          whole "at the top, and in a declaration" "STUB(tanh)\nvoid fatal(char*) Noreturn;\nchar *s = PREFIX \"x\" SUFFIX;")
    ]
