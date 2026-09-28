(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Highlight_c.mli *)

open Highlight_code

(*****************************************************************************)
(* From the tokens *)
(*****************************************************************************)

let control_keywords = [ "if"; "else"; "while"; "for"; "do"; "switch"; "case"; "default"; "return"; "break"; "continue"; "goto" ]
let type_keywords = [ "void"; "char"; "short"; "int"; "long"; "float"; "double"; "signed"; "unsigned"; "_Bool"; "__signed__" ]

(* a banner comment: /*****...*/ or //*****... *)
let is_banner (t : Token_c.t) : bool =
  t.kind = Comment
  && String.length t.text >= 7
  && (String.sub t.text 0 7 = "/******" || String.sub t.text 0 7 = "//*****")

(* a token's category from its kind alone *)
let of_kind (t : Token_c.t) : category =
  match t.kind with
  | Comment -> Comment
  | Keyword ->
      if List.mem t.text control_keywords then Keyword_control
      else if List.mem t.text type_keywords then Type
      else Keyword
  | Ident -> if Token_c.is_constant t.text then Constructor else Normal
  | Int | Float -> Number
  | Char | String -> String
  | Operator -> Operator
  | Punctuation -> Punctuation
  | Directive -> Keyword_module
  | Error -> Error

(*****************************************************************************)
(* From the tree *)
(*****************************************************************************)

(* claude: what the tree says of a name (Parse_c, Ast_c): C's scopes (a
 * parameter until its function ends, a local from its declaration to its
 * block's end), the fields, the definitions, the types. A token index to
 * its category; what is not in it keeps the first guess. *)
open Ast_c

(* the names in scope: parameters and locals, and their binding's token *)
type env = (string * (category * int)) list

(* claude: and [binds], a name's token to its binding's (a binding's to
 * its own), as Highlight_ml's *)
let resolve (file : file) : (int, category) Hashtbl.t * (int, int) Hashtbl.t =
  let out = Hashtbl.create 1024 in
  let binds = Hashtbl.create 256 in
  let mark (n : name) (c : category) = if n.tok >= 0 then Hashtbl.replace out n.tok c in
  (* [n] bound as [c], in scope in [env] from now on *)
  let bound (n : name) (c : category) (env : env) : env =
    if n.tok >= 0 then Hashtbl.replace binds n.tok n.tok;
    (n.text, (c, n.tok)) :: env
  in
  let use (n : name) (b : int) = if n.tok >= 0 && b >= 0 then Hashtbl.replace binds n.tok b in
  (* the file's constants: its enums', its #defines' without parameters *)
  let constants = Hashtbl.create 64 in
  List.iter (fun d -> if d.mparams = None then Hashtbl.replace constants d.mname.text ()) file.defines;
  let rec ty (env : env) (t : ty) =
    match t with
    | Tbase -> ()
    | Tname n -> mark n Type
    | Tstruct (tag, fields) ->
        Option.iter (fun n -> mark n (if fields = None then Type else Def_type)) tag;
        Option.iter (List.iter (decl env Field)) fields
    | Tenum (tag, cs) ->
        Option.iter (fun n -> mark n (if cs = None then Type else Def_type)) tag;
        Option.iter
          (List.iter (fun (n, v) ->
               mark n Constructor;
               Hashtbl.replace constants n.text ();
               Option.iter (expr env) v))
          cs
    | Tptr t -> ty env t
    | Tarray (t, e) -> ty env t; Option.iter (expr env) e
    | Tfunc (t, ps) -> ty env t; List.iter (decl env Parameter) ps
    | Ttypeof e -> expr env e
  (* a declared name, as [c]; its type; its initializer *)
  and decl (env : env) (c : category) (d : decl) =
    Option.iter (fun n -> mark n c) d.dname;
    ty env d.dty;
    Option.iter (init env) d.dinit
  and init env (i : init) =
    match i with
    | Iexpr e -> expr env e
    | Ilist items ->
        List.iter
          (fun (ds, i) ->
            List.iter (function Dfield n -> mark n Field | Dindex e -> expr env e) ds;
            init env i)
          items
  and expr (env : env) (e : expr) =
    match e with
    | Econst -> ()
    | Eident n -> (
        match List.assoc_opt n.text env with
        | Some (c, b) ->
            mark n c;
            use n b
        | None -> mark n (if Hashtbl.mem constants n.text || Token_c.is_constant n.text then Constructor else Normal))
    | Efield (e, n) -> expr env e; mark n Field
    | Ecall (f, args) -> expr env f; List.iter (expr env) args
    | Ecast (t, e) -> ty env t; expr env e
    | Etype t -> ty env t
    | Ecompound (t, i) -> ty env t; init env i
    | Estmt ss -> stmts env ss
    | Emisc es -> List.iter (expr env) es
  (* a block's statements: a declaration's names in scope after it *)
  and stmts (env : env) (ss : stmt list) = ignore (List.fold_left stmt env ss)
  and stmt (env : env) (s : stmt) : env =
    match s with
    | Sexpr e -> expr env e; env
    | Sdecl (sp, ds) ->
        ty env sp.base;
        List.fold_left
          (fun env d ->
            let c = if sp.typedef then Def_type else Local in
            (* its initializer sees it: int n = sizeof n *)
            let env = match d.dname with Some n when not sp.typedef -> bound n c env | _ -> env in
            decl env c d;
            env)
          env ds
    | Sblock ss -> stmts env ss; env
    | Sfor (first, es, body) ->
        let inner = match first with Some s -> stmt env s | None -> env in
        List.iter (expr inner) es;
        ignore (stmt inner body);
        env
    | Snest (es, ss) ->
        List.iter (expr env) es;
        List.iter (fun s -> ignore (stmt env s)) ss;
        env
    | Slabel (n, s) -> mark n Label; stmt env s
    | Sgoto n -> mark n Label; env
  in
  (* a function's parameters: its declarator's outermost Tfunc's *)
  let params_of (t : ty) : decl list = match t with Tfunc (_, ps) -> ps | _ -> [] in
  let names (ds : decl list) : env = List.fold_right (fun d env -> match d.dname with Some n -> bound n Parameter env | None -> env) ds [] in
  let item (it : item) =
    match it with
    | Ifunc (sp, d, kr, body) ->
        ty [] sp.base;
        decl [] Def_function d;
        List.iter (decl [] Parameter) kr;
        stmts (names (params_of d.dty) @ names kr) body
    | Idecl (sp, ds) ->
        ty [] sp.base;
        List.iter
          (fun d ->
            let c = if sp.typedef then Def_type else match d.dty with Tfunc _ -> Def_function | _ -> Def_value in
            decl [] c d)
          ds
    | Imacro e -> expr [] e
  in
  List.iter
    (fun d ->
      mark d.mname (if d.mparams = None then Def_value else Def_function);
      let ps = Option.value d.mparams ~default:[] in
      let env = List.fold_left (fun env p -> mark p Parameter; bound p Parameter env) [] ps in
      List.iter
        (fun (n : name) ->
          match List.assoc_opt n.text env with
          | Some (_, b) ->
              mark n Parameter;
              use n b
          | None -> ())
        d.mbody)
    file.defines;
  List.iter item file.items;
  (out, binds)

(* the kinds' categories, and over them what the tree says of the names;
 * a banner, and the title between two, a section *)
let categorize_bound (toks : Token_c.t list) : (Token_c.t * category) list * (int, int) Hashtbl.t =
  let tree, binds = resolve (Parse_c.parse toks) in
  let all = Array.of_list toks in
  let n = Array.length all in
  ( Array.to_list
      (Array.mapi
         (fun i (t : Token_c.t) ->
           let c =
             match Hashtbl.find_opt tree i with
             | Some c when t.kind = Ident -> c
             | _ ->
                 if is_banner t || (t.kind = Comment && i > 0 && is_banner all.(i - 1) && i + 1 < n && is_banner all.(i + 1)) then Comment_section
                 else of_kind t
           in
           (t, c))
         all),
    binds )

let categorize (toks : Token_c.t list) : (Token_c.t * category) list = fst (categorize_bound toks)

(* claude: rev_map and rev, not List.map (Highlight_ml.lines says why) *)
let analyze (src : string) : span list array * occurrence list =
  let toks = Lexer_c.tokens src in
  let cats, binds = categorize_bound toks in
  ( Highlight_code.lines src (List.rev (List.rev_map (fun ((t : Token_c.t), c) -> (t.line, t.col, t.text, c)) cats)),
    Highlight_code.occurrences (Array.of_list (List.map (fun (t : Token_c.t) -> (t.line, t.col, t.text)) toks)) binds )

let lines (src : string) : span list array = fst (analyze src)
