(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Parse_c.mli *)

open Ast_c

(*****************************************************************************)
(* The tokens the grammar reads *)
(*****************************************************************************)

(* the file's tokens but the comments and the directives' lines; [idx]:
 * each one's index in the file's tokens, a name's [tok]; [types]: the
 * names known to be types *)
type st = {
  toks : Token_c.t array;
  idx : int array;
  mutable pos : int;
  mutable skipped : (int * int) list;
  types : (string, unit) Hashtbl.t;
}

exception Parse_error

(* the types every file knows without its headers: Plan 9's (u.h), C's
 * and POSIX's commonest, OCaml's runtime's; and any name ending in _t *)
let known_types =
  [ "uchar"; "ushort"; "uint"; "ulong"; "vlong"; "uvlong"; "schar"; "u8int"; "u16int"; "u32int"; "u64int";
    "s8int"; "s16int"; "s32int"; "s64int"; "uintptr"; "intptr"; "usize"; "Rune"; "va_list"; "jmp_buf";
    "FILE"; "bool"; "value"; "intnat"; "uintnat"; "mlsize_t"; "tag_t"; "header_t"; "asize_t" ]

let is_known (st : st) (s : string) : bool =
  Hashtbl.mem st.types s || (String.length s > 2 && String.sub s (String.length s - 2) 2 = "_t")

(* a directive's word: define in "# define" *)
let word (t : Token_c.t) : string = String.trim (String.sub t.text 1 (String.length t.text - 1))

(* claude: the directives read apart; what is kept is the code of the
 * first branch of each #if (the second of an #if 0) *)
let reading (all : Token_c.t array) : st * define list =
  let n = Array.length all in
  let keep = ref [] and defines = ref [] in
  (* the end of the directive at [j]: its last token, excluded *)
  let directive_end j =
    let k = ref (j + 1) in
    while !k < n && all.(!k).pp && all.(!k).kind <> Directive do incr k done;
    !k
  in
  (* its tokens after its word, but the comments, with their indexes *)
  let args j = List.filter (fun k -> all.(k).kind <> Comment) (List.init (directive_end j - j - 1) (fun k -> j + 1 + k)) in
  (* from [j], to the directive that ends the branch we are in: its #else
   * or #elif (unless [to_endif]) or #endif, nested #ifs counted *)
  let branch_end j ~to_endif =
    let depth = ref 0 and k = ref j and found = ref None in
    while !found = None && !k < n do
      let t = all.(!k) in
      (if t.kind = Directive then
         match word t with
         | "if" | "ifdef" | "ifndef" -> incr depth
         | "endif" -> if !depth = 0 then found := Some !k else decr depth
         | "else" | "elif" when !depth = 0 && not to_endif -> found := Some !k
         | _ -> ());
      incr k
    done;
    match !found with Some k -> k | None -> n
  in
  let name_at k = { text = all.(k).text; tok = k } in
  let i = ref 0 in
  while !i < n do
    let t = all.(!i) in
    match t.kind with
    | Comment -> incr i
    | Directive -> (
        let e = directive_end !i in
        let a = args !i in
        match word t with
        | "define" ->
            (match a with
            | m :: rest when all.(m).kind = Ident || all.(m).kind = Keyword ->
                (* NAME(x, y), the parenthesis touching the name: parameters *)
                let params, body =
                  match rest with
                  | p :: rest when all.(p).text = "(" && all.(p).offset = all.(m).offset + String.length all.(m).text ->
                      let rec split ps = function
                        | k :: rest when all.(k).text = ")" -> (List.rev ps, rest)
                        | k :: rest -> split (if all.(k).kind = Ident then name_at k :: ps else ps) rest
                        | [] -> (List.rev ps, [])
                      in
                      let ps, body = split [] rest in
                      (Some ps, body)
                  | _ -> (None, rest)
                in
                let body = List.filter_map (fun k -> if all.(k).kind = Ident then Some (name_at k) else None) body in
                defines := { mname = name_at m; mparams = params; mbody = body } :: !defines
            | _ -> ());
            i := e
        | ("if" | "ifdef") as w
          when (w = "if" && List.map (fun k -> all.(k).text) a = [ "0" ]) || List.exists (fun k -> all.(k).text = "__cplusplus") a ->
            (* read the other branch: past this one's #else or #elif line, or its #endif *)
            let k = branch_end e ~to_endif:false in
            i := if k < n then directive_end k else n
        | "else" | "elif" ->
            (* the branch before was read: this one is not *)
            let k = branch_end e ~to_endif:true in
            i := if k < n then directive_end k else n
        | _ -> i := e)
    | _ ->
        if not t.pp then keep := (!i, t) :: !keep;
        incr i
  done;
  let kept = Array.of_list (List.rev !keep) in
  let types = Hashtbl.create 64 in
  List.iter (fun s -> Hashtbl.replace types s ()) known_types;
  ({ toks = Array.map snd kept; idx = Array.map fst kept; pos = 0; skipped = []; types }, List.rev !defines)

let at (st : st) (k : int) : Token_c.t option = if st.pos + k < Array.length st.toks then Some st.toks.(st.pos + k) else None
let text_at st k = match at st k with Some t -> t.text | None -> ""
let eof st = st.pos >= Array.length st.toks
let advance st = st.pos <- st.pos + 1

(* a keyword, an operator or a punctuation, not a name spelled so *)
let is_at st k s = match at st k with Some t -> t.text = s && t.kind <> Ident | None -> false
let is st s = is_at st 0 s
let accept st s = if is st s then (advance st; true) else false
let expect st s = if not (accept st s) then raise Parse_error
let kind_at st k (kind : Token_c.kind) = match at st k with Some t -> t.kind = kind | None -> false
let kind_is st kind = kind_at st 0 kind

let name st : name =
  match at st 0 with
  | Some ({ kind = Ident; _ } as t) ->
      let n = { text = t.text; tok = st.idx.(st.pos) } in
      advance st;
      n
  | _ -> raise Parse_error

(* the line of the token before: a macro without its semicolon ends one *)
let prev_line st = if st.pos > 0 then st.toks.(st.pos - 1).line else 0
let new_line st = match at st 0 with Some t -> t.line > prev_line st | None -> true

(* past a (...), its nested ones counted *)
let skip_parens st =
  expect st "(";
  let depth = ref 1 in
  while !depth > 0 && not (eof st) do
    if is st "(" then incr depth else if is st ")" then decr depth;
    advance st
  done

(* gcc's: __attribute__((...)), __asm__("name") after a declarator *)
let skip_attributes st =
  while
    match at st 0 with
    | Some { kind = Keyword; text = "__attribute__" | "__asm__" | "__asm" | "asm" | "__declspec"; _ } -> true
    | _ -> false
  do
    advance st;
    ignore (accept st "volatile" || accept st "__volatile__");
    if is st "(" then skip_parens st
  done

(* ... and a macro standing for one: void fatal(char* ) Noreturn;,
 * register value *sp SP_REG; *)
let after_declarator st =
  skip_attributes st;
  if kind_is st Ident && List.exists (is_at st 1) [ ";"; ","; "=" ] then advance st

(*****************************************************************************)
(* Which names are types *)
(*****************************************************************************)

let base_types = [ "void"; "char"; "short"; "int"; "long"; "float"; "double"; "signed"; "unsigned"; "_Bool"; "__signed__" ]
let qualifiers = [ "const"; "volatile"; "restrict"; "__restrict"; "__const"; "__volatile__" ]
let storage = [ "static"; "extern"; "auto"; "register"; "inline"; "__inline"; "__inline__"; "__extension__"; "typedef" ]

(* a keyword that starts a type (in a cast, sizeof's parentheses) *)
let type_keyword (s : string) : bool =
  List.mem s base_types || List.mem s qualifiers || List.mem s [ "struct"; "union"; "enum"; "typeof"; "__typeof__" ]

(* ... or a declaration *)
let spec_keyword (s : string) : bool = type_keyword s || List.mem s storage || List.mem s [ "__attribute__"; "__declspec" ]

(* from [k], past stars and qualifiers: where they end *)
let past_stars st k =
  let k = ref k in
  while is_at st !k "*" || List.mem (text_at st !k) qualifiers do incr k done;
  !k

(* claude: whether the name at [k] is a type in the specifiers being
 * read ([base]: a type already read in them). Known, it is one unless
 * what follows makes it a value (x = , x.f, x[i]); not known, only when
 * nothing else could follow: a name (Proc p, CAMLprim value f), stars
 * then a declared name or nothing (Proc *p;, Proc** ), a function
 * pointer's declarator (Proc ( *f)(int)) *)
let names_type st (k : int) ~(has_base : bool) : bool =
  let next = at st (k + 1) in
  let next_is s = is_at st (k + 1) s in
  let next_kind kind = kind_at st (k + 1) kind in
  let fn_pointer () = next_is "(" && is_at st (k + 2) "*" && (kind_at st (k + 3) Ident && is_at st (k + 4) ")") && (is_at st (k + 5) "(" || is_at st (k + 5) "[") in
  if has_base then next_kind Ident
  else if is_known st (text_at st k) then
    next_kind Ident
    || (match next with Some { kind = Keyword; text; _ } -> spec_keyword text | _ -> false)
    || next_is "*" || next_is ")" || next_is "," || next_is ";" || fn_pointer ()
  else
    next_kind Ident
    || (match next with Some { kind = Keyword; text; _ } -> spec_keyword text | _ -> false)
    || (next_is "*"
       &&
       let j = past_stars st (k + 1) in
       is_at st j ")" || is_at st j ","
       || (kind_at st j Ident && List.exists (is_at st (j + 1)) [ ";"; ","; "="; "["; ")"; "("; ":" ])
       || (is_at st j "(" && is_at st (j + 1) "*"))
    || fn_pointer ()

(* a declaration starts here, in a block *)
let decl_start st : bool =
  match at st 0 with
  | Some { kind = Keyword; text; _ } -> spec_keyword text
  | Some { kind = Ident; _ } -> (not (is_at st 1 ":")) && names_type st 0 ~has_base:false && not (is_at st 1 ")" || is_at st 1 ",")
  | _ -> false

(* a type in parentheses here, at "(": a cast, sizeof(T) *)
let type_in_parens st : bool =
  match at st 1 with
  | Some { kind = Keyword; text; _ } -> type_keyword text
  | Some { kind = Ident; text; _ } -> (
      let j = past_stars st 2 in
      (j > 2 && is_at st j ")")
      || (match at st 2 with Some { kind = Keyword; text; _ } -> type_keyword text | _ -> false)
      || (is_known st text && (is_at st 2 ")" || (is_at st 2 "(" && is_at st 3 "*")))
      ||
      (* (T) before an operand that cannot follow a parenthesized value *)
      is_at st 2 ")"
      &&
      match at st 3 with
      | Some { kind = Ident | Int | Float | Char | String; _ } -> true
      | Some { kind = Keyword; text = "sizeof"; _ } -> true
      | Some { kind = Operator; text = "!" | "~"; _ } -> true
      (* (Qid){ ... }: a compound literal *)
      | Some { kind = Punctuation; text = "{"; _ } -> true
      | _ -> false)
  | _ -> false

(*****************************************************************************)
(* Declarations *)
(*****************************************************************************)

let rec specifiers st : spec * bool =
  let typedef = ref false and base = ref None and any = ref false in
  let continue = ref true in
  while !continue do
    match at st 0 with
    | Some { kind = Keyword; text; _ } when spec_keyword text ->
        any := true;
        (match text with
        | "typedef" -> advance st; typedef := true
        | "struct" | "union" -> advance st; base := Some (struct_body st)
        | "enum" -> advance st; base := Some (enum_body st)
        | "typeof" | "__typeof__" ->
            advance st;
            let is_type = type_in_parens st in
            expect st "(";
            let e = if is_type then Etype (type_name st) else expr st in
            expect st ")";
            base := Some (Ttypeof e)
        | "__attribute__" | "__declspec" -> skip_attributes st
        | _ ->
            advance st;
            if List.mem text base_types && !base = None then base := Some Tbase)
    | Some { kind = Ident; _ } when names_type st 0 ~has_base:(!base <> None) ->
        any := true;
        base := Some (Tname (name st))
    | _ -> continue := false
  done;
  ({ typedef = !typedef; base = Option.value !base ~default:Tbase }, !any)

(* struct or union, after the keyword: its tag, its fields *)
and struct_body st : ty =
  skip_attributes st;
  let tag = if kind_is st Ident then Some (name st) else None in
  if accept st "{" then begin
    let fields = ref [] in
    while not (is st "}" || eof st) do
      if accept st ";" then ()
      else if kind_is st Ident && is_at st 1 ";" then begin
        (* Plan 9's anonymous member: Lock; *)
        fields := { dname = None; dty = Tname (name st); dinit = None } :: !fields;
        advance st
      end
      else begin
        let sp, _ = specifiers st in
        if accept st ";" then fields := { dname = None; dty = sp.base; dinit = None } :: !fields
        else begin
          let one () =
            let d =
              if is st ":" then { dname = None; dty = sp.base; dinit = None }
              else
                let n, f = declarator st ~abstract:false in
                { dname = n; dty = f sp.base; dinit = None }
            in
            (* a bit field's width *)
            let d = if accept st ":" then { d with dinit = Some (Iexpr (cond st)) } else d in
            skip_attributes st;
            fields := d :: !fields
          in
          one ();
          while accept st "," do one () done;
          expect st ";"
        end
      end
    done;
    expect st "}";
    skip_attributes st;
    Tstruct (tag, Some (List.rev !fields))
  end
  else if tag = None then raise Parse_error
  else Tstruct (tag, None)

and enum_body st : ty =
  skip_attributes st;
  let tag = if kind_is st Ident then Some (name st) else None in
  if accept st "{" then begin
    let cs = ref [] in
    while not (is st "}" || eof st) do
      let n = name st in
      let v = if accept st "=" then Some (cond st) else None in
      cs := (n, v) :: !cs;
      if not (is st "}") then expect st ","
    done;
    expect st "}";
    Tenum (tag, Some (List.rev !cs))
  end
  else if tag = None then raise Parse_error
  else Tenum (tag, None)

(* claude: a declarator, the name inside its type -- *p, a[10], ( *f)(int)
 * -- as the name and the function that builds its type from the
 * specifiers': the stars first, then the suffixes ([], (params)), then
 * what the parentheses hold, outermost:
 *
 *   int ( *f)(int)    f: the star, on (int) on int  = pointer to function
 *   int *f(int)       f: (int) on the star on int   = function returning a pointer
 *
 * [abstract]: the name may be missing (a parameter's type alone, a cast) *)
and declarator st ~(abstract : bool) : name option * (ty -> ty) =
  let stars = ref 0 in
  while is st "*" || is st "^" || List.mem (text_at st 0) qualifiers || is st "__attribute__" do
    if is st "*" || is st "^" then (advance st; incr stars) else if is st "__attribute__" then skip_attributes st else advance st
  done;
  let n, inner =
    if kind_is st Ident then (Some (name st), Fun.id)
    else if is st "(" && ((not abstract) || is_at st 1 "*" || is_at st 1 "^" || is_at st 1 "(" || is_at st 1 "[") then begin
      advance st;
      let n, f = declarator st ~abstract in
      expect st ")";
      (n, f)
    end
    else if abstract then (None, Fun.id)
    else raise Parse_error
  in
  let suffixes = ref [] in
  let continue = ref true in
  while !continue do
    if accept st "[" then begin
      let e = if is st "]" then None else Some (expr st) in
      expect st "]";
      suffixes := `Array e :: !suffixes
    end
    else if accept st "(" then begin
      let ps = params st in
      expect st ")";
      suffixes := `Func ps :: !suffixes
    end
    else if kind_is st Ident && is_at st 1 "(" && is_at st 2 "(" then begin
      (* GNU's f PARAMS ((int a)), the parameters for ANSI compilers only *)
      advance st;
      advance st;
      advance st;
      let ps = params st in
      expect st ")";
      expect st ")";
      suffixes := `Func ps :: !suffixes
    end
    else continue := false
  done;
  let suffixes = List.rev !suffixes in
  let build base =
    let t = ref base in
    for _ = 1 to !stars do t := Tptr !t done;
    inner (List.fold_right (fun s t -> match s with `Array e -> Tarray (t, e) | `Func ps -> Tfunc (t, ps)) suffixes !t)
  in
  (n, build)

(* a function's parameters, inside its parentheses: types and names, or
 * K&R's names alone, or (void), or (fmt, ...) *)
and params st : decl list =
  let out = ref [] in
  if not (is st ")") then begin
    let one () =
      if not (accept st "...") then begin
        let sp, _ = specifiers st in
        let n, f = declarator st ~abstract:true in
        skip_attributes st;
        out := { dname = n; dty = f sp.base; dinit = None } :: !out
      end
    in
    one ();
    while accept st "," do one () done
  end;
  List.rev !out

(* a type alone: in a cast, sizeof's parentheses, a macro's argument *)
and type_name st : ty =
  let sp, any = specifiers st in
  let base = if any then sp.base else Tname (name st) in
  let _, f = declarator st ~abstract:true in
  f base

(* after the specifiers: the declarators, their initializers; a
 * typedef's names become types *)
and declarators st (sp : spec) : decl list =
  let out = ref [] in
  let one () =
    let n, f = declarator st ~abstract:false in
    after_declarator st;
    let d = { dname = n; dty = f sp.base; dinit = (if accept st "=" then Some (init st) else None) } in
    (match n with Some n when sp.typedef -> Hashtbl.replace st.types n.text () | _ -> ());
    out := d :: !out
  in
  one ();
  while accept st "," do one () done;
  List.rev !out

and init st : init = if is st "{" then braces st else Iexpr (assign st)

(* { .x = 1, [2] = 3, 4, } *)
and braces st : init =
  expect st "{";
  let out = ref [] in
  while not (is st "}" || eof st) do
    let ds = ref [] in
    let continue = ref true in
    while !continue do
      if is st "." && kind_at st 1 Ident then (advance st; ds := Dfield (name st) :: !ds)
      else if accept st "[" then begin
        let e = cond st in
        if accept st "..." then ignore (cond st);
        expect st "]";
        ds := Dindex e :: !ds
      end
      else if !ds = [] && kind_is st Ident && is_at st 1 ":" then begin
        (* gcc's old x: 1 *)
        ds := [ Dfield (name st) ];
        advance st;
        continue := false
      end
      else continue := false
    done;
    if !ds <> [] then ignore (accept st "=");
    out := (List.rev !ds, init st) :: !out;
    if not (is st "}") then expect st ","
  done;
  expect st "}";
  Ilist (List.rev !out)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* C's binary operators, from the loosest: || && | ^ & == < << + * *)
and precedence (t : Token_c.t) : int option =
  if t.kind <> Operator then None
  else
    match t.text with
    | "||" -> Some 1
    | "&&" -> Some 2
    | "|" -> Some 3
    | "^" -> Some 4
    | "&" -> Some 5
    | "==" | "!=" -> Some 6
    | "<" | ">" | "<=" | ">=" -> Some 7
    | "<<" | ">>" -> Some 8
    | "+" | "-" -> Some 9
    | "*" | "/" | "%" -> Some 10
    | _ -> None

and expr st : expr =
  let e = assign st in
  if is st "," then begin
    let es = ref [ e ] in
    while accept st "," do es := assign st :: !es done;
    Emisc (List.rev !es)
  end
  else e

and assign st : expr =
  let l = cond st in
  match at st 0 with
  | Some { kind = Operator; text = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="; _ } ->
      advance st;
      Emisc [ l; assign st ]
  | _ -> l

and cond st : expr =
  let c = binary st 1 in
  if accept st "?" then begin
    (* gcc's a ?: b *)
    let a = if is st ":" then Econst else expr st in
    expect st ":";
    Emisc [ c; a; assign st ]
  end
  else c

(* precedence climbing: a loop along a chain, a recursion only for a
 * tighter operator's right side *)
and binary st (min : int) : expr =
  let l = ref (unary st) in
  let continue = ref true in
  while !continue do
    match Option.bind (at st 0) precedence with
    | Some p when p >= min ->
        advance st;
        l := Emisc [ !l; binary st (p + 1) ]
    | _ -> continue := false
  done;
  !l

and unary st : expr =
  match at st 0 with
  | Some { kind = Operator; text = "-" | "+" | "!" | "~" | "*" | "&" | "++" | "--" | "&&"; _ } ->
      advance st;
      unary st
  | Some { kind = Keyword; text = "sizeof" | "signof" | "__alignof__"; _ } ->
      advance st;
      if is st "(" && type_in_parens st then begin
        advance st;
        let t = type_name st in
        expect st ")";
        Etype t
      end
      else unary st
  | Some { kind = Keyword; text = "__extension__"; _ } ->
      advance st;
      unary st
  | Some { kind = Punctuation; text = "("; _ } when type_in_parens st ->
      advance st;
      let t = type_name st in
      expect st ")";
      if is st "{" then Ecompound (t, braces st) else Ecast (t, unary st)
  | _ -> postfix st (primary st)

and postfix st (e : expr) : expr =
  let e = ref e in
  let continue = ref true in
  while !continue do
    if accept st "[" then begin
      let i = expr st in
      expect st "]";
      e := Emisc [ !e; i ]
    end
    else if accept st "(" then begin
      let args = ref [] in
      if not (is st ")") then begin
        args := [ arg st ];
        while accept st "," do args := arg st :: !args done
      end;
      expect st ")";
      e := Ecall (!e, List.rev !args)
    end
    else if (is st "." || is st "->") && kind_at st 1 Ident then begin
      advance st;
      e := Efield (!e, name st)
    end
    else if is st "++" || is st "--" then advance st
    else continue := false
  done;
  !e

(* a call's argument, or a macro's that is a type: va_arg(l, int),
 * va_arg(l, Foo* ) *)
and arg st : expr =
  let type_arg =
    match at st 0 with
    | Some { kind = Keyword; text; _ } -> type_keyword text
    | Some { kind = Ident; text; _ } ->
        let j = past_stars st 1 in
        (is_known st text && (is_at st 1 ")" || is_at st 1 "," || j > 1)) || (j > 1 && (is_at st j ")" || is_at st j ","))
    | _ -> false
  in
  if type_arg then Etype (type_name st) else assign st

and primary st : expr =
  match at st 0 with
  | Some { kind = Ident; text; _ } when not (Token_c.is_constant text && kind_at st 1 String) -> Eident (name st)
  | Some { kind = Int | Float | Char; _ } ->
      advance st;
      Econst
  | Some { kind = String | Ident; _ } ->
      advance st;
      (* "a" "b", and "a" PRId64 "b", PREFIX "x" SUFFIX: a macro
       * standing for a string *)
      while
        kind_is st String
        || kind_is st Ident
           && (kind_at st 1 String || (Token_c.is_constant (text_at st 0) && List.exists (is_at st 1) [ ";"; ","; ")" ]))
      do
        advance st
      done;
      Econst
  | Some { kind = Punctuation; text = "("; _ } ->
      advance st;
      if is st "{" then begin
        (* gcc's ({ ... }) *)
        let ss = block st in
        expect st ")";
        Estmt ss
      end
      else begin
        let e = expr st in
        expect st ")";
        e
      end
  | _ -> raise Parse_error

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

and paren_expr st : expr =
  expect st "(";
  let e = expr st in
  expect st ")";
  e

and declaration st : stmt =
  let sp, _ = specifiers st in
  if accept st ";" then Sdecl (sp, [])
  else begin
    let ds = declarators st sp in
    expect st ";";
    Sdecl (sp, ds)
  end

and stmt st : stmt =
  match at st 0 with
  | Some { kind = Punctuation; text = "{"; _ } -> Sblock (block st)
  | Some { kind = Punctuation; text = ";"; _ } ->
      advance st;
      Snest ([], [])
  | Some { kind = Keyword; text; _ } when not (spec_keyword text) -> (
      advance st;
      match text with
      | "if" ->
          let c = paren_expr st in
          let a = stmt st in
          Snest ([ c ], if accept st "else" then [ a; stmt st ] else [ a ])
      | "while" | "switch" ->
          let c = paren_expr st in
          Snest ([ c ], [ stmt st ])
      | "do" ->
          let body = stmt st in
          expect st "while";
          let c = paren_expr st in
          expect st ";";
          Snest ([ c ], [ body ])
      | "for" ->
          expect st "(";
          let first =
            if accept st ";" then None
            else if decl_start st then Some (declaration st)
            else begin
              let e = expr st in
              expect st ";";
              Some (Sexpr e)
            end
          in
          let c = if is st ";" then [] else [ expr st ] in
          expect st ";";
          let step = if is st ")" then [] else [ expr st ] in
          expect st ")";
          Sfor (first, c @ step, stmt st)
      | "return" ->
          if accept st ";" then Snest ([], [])
          else begin
            let e = expr st in
            expect st ";";
            Snest ([ e ], [])
          end
      | "break" | "continue" ->
          expect st ";";
          Snest ([], [])
      | "goto" ->
          let n = name st in
          expect st ";";
          Sgoto n
      | "case" ->
          let e = cond st in
          (* gcc's case 1 ... 3: *)
          let e = if accept st "..." then Emisc [ e; cond st ] else e in
          expect st ":";
          Snest ([ e ], [])
      | "default" ->
          expect st ":";
          Snest ([], [])
      | "asm" | "__asm__" | "__asm" ->
          ignore (accept st "volatile" || accept st "__volatile__");
          skip_parens st;
          expect st ";";
          Snest ([], [])
      | _ ->
          (* sizeof x; and the like: an expression after all *)
          st.pos <- st.pos - 1;
          expr_stmt st)
  | Some { kind = Ident; _ } when is_at st 1 ":" ->
      let n = name st in
      advance st;
      Slabel (n, Snest ([], []))
  | _ when decl_start st -> (
      let start = st.pos in
      try declaration st
      with Parse_error when st.toks.(start).kind = Ident ->
        (* DBG print(...);: not a declaration but a macro before a
         * statement, #define DBG if(debug) *)
        st.pos <- start;
        let e = Eident (name st) in
        Snest ([ e ], [ stmt st ]))
  | _ -> expr_stmt st

(* claude: an expression and its semicolon, or a macro used as a
 * statement: ARGBEGIN{ ... }, FOO(x) { ... }, FOO(x) stmt; on the same
 * line, or FOO(x) and ARGEND alone at a line's end, no semicolon *)
and expr_stmt st : stmt =
  if kind_is st Ident && is_at st 1 "{" then begin
    let e = Eident (name st) in
    Snest ([ e ], [ stmt st ])
  end
  else begin
    let e = expr st in
    if accept st ";" then Sexpr e
    else
      match e with
      | (Ecall _ | Eident _) when is st "{" -> Snest ([ e ], [ stmt st ])
      (* a macro's label: Instruct(STOP): *)
      | Ecall _ when accept st ":" -> Snest ([ e ], [])
      | (Ecall _ | Eident _) when eof st || is st "}" || new_line st -> Sexpr e
      | Ecall _ -> Snest ([ e ], [ stmt st ])
      | _ -> raise Parse_error
  end

(* claude: a statement that does not parse is skipped to its semicolon
 * (or its block's end), not the function: the scopes stay right around
 * it *)
and block st : stmt list =
  expect st "{";
  let out = ref [] in
  while not (is st "}" || eof st) do
    let start = st.pos in
    match stmt st with
    | s -> out := s :: !out
    | exception Parse_error ->
        st.pos <- start;
        skip_stmt st;
        st.skipped <- (st.idx.(start), if eof st then max_int else st.idx.(st.pos)) :: st.skipped
  done;
  expect st "}";
  List.rev !out

(* past the statement at point: to its ; or past its block, at its depth,
 * or to the } of the block around it *)
and skip_stmt st =
  let depth = ref 0 and continue = ref true and first = ref true in
  while !continue && not (eof st) do
    if is st "{" then (incr depth; advance st)
    else if is st "}" then
      if !depth = 0 then (if !first then advance st; continue := false)
      else begin
        decr depth;
        advance st;
        if !depth = 0 then continue := false
      end
    else if is st ";" && !depth = 0 then (advance st; continue := false)
    else advance st;
    first := false
  done

(*****************************************************************************)
(* The file *)
(*****************************************************************************)

(* claude: FOO(...) at the top, not followed by a body ({, or K&R's
 * parameters' declarations): a macro used, FOO(x); or FOO(x) alone on
 * its line -- not a function declared with no type, which C89 allowed
 * and no one writes *)
let macro_use st : bool =
  kind_is st Ident && is_at st 1 "("
  &&
  let depth = ref 0 and j = ref 1 and continue = ref true in
  while !continue && st.pos + !j < Array.length st.toks do
    if is_at st !j "(" then incr depth else if is_at st !j ")" then (decr depth; if !depth = 0 then continue := false);
    if !continue then incr j
  done;
  (not !continue)
  &&
  match at st (!j + 1) with
  | None -> true
  | Some { text = "{"; kind = Punctuation; _ } -> false
  | Some { kind = Keyword; text; _ } -> not (spec_keyword text)
  | Some { kind = Ident; text; _ } -> not (is_known st text || names_type st (!j + 1) ~has_base:false)
  | Some _ -> true

let macro st : item =
  let e = expr st in
  ignore (accept st ";");
  Imacro e

(* a macro used, or a declaration; or a macro after all, when what looked
 * like a K&R function's head is not one: STUB(tanh) then a line
 * starting with a type *)
let rec item st : item =
  let start = st.pos in
  if macro_use st then macro st
  else
    try declaration_item st
    with Parse_error when start + 1 < Array.length st.toks && st.toks.(start).kind = Ident && st.toks.(start + 1).text = "(" ->
      st.pos <- start;
      macro st

and declaration_item st : item =
  let start = st.pos in
  let sp, any = specifiers st in
  if accept st ";" then Idecl (sp, [])
  else begin
    let n, f = declarator st ~abstract:false in
    after_declarator st;
    let d = { dname = n; dty = f sp.base; dinit = None } in
    match d.dty with
    | Tfunc _ when is st "{" -> Ifunc (sp, d, [], block st)
    | Tfunc _ when decl_start st ->
        (* K&R: f(a, b) int a; char *b; { ... } *)
        let kr = ref [] in
        while not (is st "{" || eof st) do
          match declaration st with Sdecl (_, ds) -> kr := List.rev_append ds !kr | _ -> ()
        done;
        Ifunc (sp, d, List.rev !kr, block st)
    | _ ->
        let d = if accept st "=" then { d with dinit = Some (init st) } else d in
        (match n with Some n when sp.typedef -> Hashtbl.replace st.types n.text () | _ -> ());
        let rest = if accept st "," then declarators st sp else [] in
        if (not any) && rest = [] && (is st ";" || new_line st) then begin
          (* no type: a macro used at the top, FOO(x); or FOO(x) *)
          st.pos <- start;
          let e = expr st in
          ignore (accept st ";");
          Imacro e
        end
        else begin
          expect st ";";
          Idecl (sp, d :: rest)
        end
  end

(* the tokens of an item that did not parse: to the next one, a token at
 * column 0 after a ; or a } *)
let skip_item st =
  advance st;
  while
    (not (eof st))
    &&
    match (at st 0, st.toks.(st.pos - 1)) with
    | Some t, prev -> not (t.col = 0 && t.text <> "{" && t.text <> "}" && (prev.text = ";" || prev.text = "}"))
    | None, _ -> false
  do
    advance st
  done

let parse (tokens : Token_c.t list) : file =
  let st, defines = reading (Array.of_list tokens) in
  let out = ref [] in
  while not (eof st) do
    if not (accept st ";") then begin
      let start = st.pos in
      match item st with
      | it -> out := it :: !out
      | exception Parse_error ->
          st.pos <- start;
          skip_item st;
          st.skipped <- (st.idx.(start), if eof st then max_int else st.idx.(st.pos)) :: st.skipped
    end
  done;
  { items = List.rev !out; defines; skipped = List.rev st.skipped }
