(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Parse_ml.mli *)

open Ast_ml

(*****************************************************************************)
(* The tokens the grammar reads *)
(*****************************************************************************)

(* the file's tokens but the comments, the directives and the attributes
 * ([@...], [@@...], [%...], skipped whole: brackets counted); [idx]: each
 * one's index in the file's tokens, a name's [tok] *)
type st = { toks : Token_ml.t array; idx : int array; mutable pos : int; mutable skipped : (int * int) list }

exception Parse_error

let opens (t : Token_ml.t) = t.kind = Punctuation && List.mem t.text [ "["; "[|"; "[<"; "[>"; "[@"; "[@@"; "[@@@"; "[%"; "[%%" ]
let closes (t : Token_ml.t) = t.kind = Punctuation && (t.text = "]" || t.text = "|]")

let rec reading (all : Token_ml.t array) : st =
  let keep = ref [] in
  let n = Array.length all in
  let i = ref 0 in
  while !i < n do
    let t = all.(!i) in
    (match t.kind with
    | Comment | Directive -> incr i
    | Punctuation when List.mem t.text [ "[@"; "[@@"; "[@@@"; "[%"; "[%%" ] ->
        (* to its bracket *)
        let depth = ref 1 in
        incr i;
        while !i < n && !depth > 0 do
          if opens all.(!i) then incr depth else if closes all.(!i) then decr depth;
          incr i
        done
    | Operator when glued t.text <> None ->
        (* ..> is .. then >, :< : then <, ::! :: then !: OCaml's lexer
         * glues the symbols, its grammar and ours read them apart *)
        let a, b = Option.get (glued t.text) in
        keep := (!i, { t with text = b; col = t.col + String.length a }) :: (!i, { t with text = a }) :: !keep;
        incr i
    | _ ->
        keep := (!i, t) :: !keep;
        incr i);
  done;
  let kept = Array.of_list (List.rev !keep) in
  { toks = Array.map snd kept; idx = Array.map fst kept; pos = 0; skipped = [] }

and glued (s : string) : (string * string) option =
  let n = String.length s in
  let starts p = n > String.length p && String.sub s 0 (String.length p) = p in
  if starts ".." then Some ("..", String.sub s 2 (n - 2))
  else if starts "::" then Some ("::", String.sub s 2 (n - 2))
  else if starts ":" && not (List.mem s [ "::"; ":="; ":>" ]) then Some (":", String.sub s 1 (n - 1))
  else None

let at (st : st) (k : int) : Token_ml.t option =
  if st.pos + k < Array.length st.toks then Some st.toks.(st.pos + k) else None

let text_at st k = match at st k with Some t -> t.text | None -> ""
let eof st = st.pos >= Array.length st.toks
let advance st = st.pos <- st.pos + 1

(* a keyword, an operator or a punctuation, not a name spelled so *)
let is_at st k s =
  match at st k with
  | Some t -> t.text = s && (t.kind = Keyword || t.kind = Operator || t.kind = Punctuation)
  | None -> false

let is st s = is_at st 0 s
let accept st s = if is st s then (advance st; true) else false
let expect st s = if not (accept st s) then raise Parse_error
let kind_is_at st k kind = match at st k with Some t -> t.kind = kind | None -> false
let kind_is st kind = kind_is_at st 0 kind

let name st : name =
  match at st 0 with
  | Some t ->
      let n = { text = t.text; tok = st.idx.(st.pos) } in
      advance st;
      n
  | None -> raise Parse_error

let lident st = if kind_is st Lident then name st else raise Parse_error
let uident st = if kind_is st Uident then name st else raise Parse_error

(* a name bound but with no token of its own to colour: ~x, punned, whose
 * token is the label's *)
let unseen (text : string) : name = { text; tok = -1 }

let label_name (label : string) : string =
  let s = String.sub label 1 (String.length label - 1) in
  if String.length s > 0 && s.[String.length s - 1] = ':' then String.sub s 0 (String.length s - 1) else s

let has_colon (label : string) = String.length label > 0 && label.[String.length label - 1] = ':'

(* M.N. then [last]: the modules of a path, the dots consumed *)
let modules st : name list =
  let ms = ref [] in
  while kind_is st Uident && is_at st 1 "." && (kind_is_at st 2 Uident || kind_is_at st 2 Lident) do
    ms := name st :: !ms;
    advance st
  done;
  List.rev !ms

(* a type's or a value's path: M.N.x *)
let lpath st : longid =
  let ms = modules st in
  ms @ [ lident st ]

(* a module's path: M.N *)
let upath st : longid =
  let ms = modules st in
  ms @ [ uident st ]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let rec ty st : ty =
  (* type a b. t, 'a 'b. t: polymorphic *)
  if is st "type" then begin
    advance st;
    let ns = ref [] in
    while kind_is st Lident do ns := name st :: !ns done;
    expect st ".";
    Tpoly (List.rev !ns, ty st)
  end
  else if kind_is st Type_var && (is_at st 1 "." || (kind_is_at st 1 Type_var && vars_then_dot st)) then begin
    let ns = ref [] in
    while kind_is st Type_var do ns := name st :: !ns done;
    expect st ".";
    Tpoly (List.rev !ns, ty st)
  end
  else begin
    (* a label: ~x:t, ?x:t (the lexer's), x:t *)
    (match at st 0 with
    | Some { kind = Label; _ } -> advance st
    | Some { kind = Lident; _ } when is_at st 1 ":" -> advance st; advance st
    | _ -> ());
    let t = tuple_ty st in
    if accept st "->" then Tarrow (t, ty st) else t
  end

and vars_then_dot st =
  let k = ref 0 in
  while kind_is_at st !k Type_var do incr k done;
  is_at st !k "."

and tuple_ty st =
  let t = app_ty st in
  if is st "*" then begin
    let ts = ref [ t ] in
    while accept st "*" do ts := app_ty st :: !ts done;
    Ttuple (List.rev !ts)
  end
  else t

(* int list option: constructors applied, postfix *)
and app_ty st =
  let t = ref (atom_ty st) in
  while kind_is st Lident || (kind_is st Uident && is_at st 1 ".") || (is st "#" && not (is_at st 1 "(")) do
    if accept st "#" then t := Tconstr (lpath st, [ !t ]) else t := Tconstr (lpath st, [ !t ])
  done;
  !t

and atom_ty st : ty =
  match at st 0 with
  | Some { kind = Type_var; _ } -> Tvar (name st)
  | Some { kind = Lident; text = "_"; _ } -> advance st; Tany
  | Some { kind = Lident; _ } | Some { kind = Uident; _ } -> Tconstr (lpath st, [])
  | Some { kind = Punctuation; text = "#"; _ } -> advance st; Tconstr (lpath st, [])
  | Some { text = "("; kind = Punctuation; _ } ->
      advance st;
      if accept st "module" then begin
        let p = upath st in
        (* with type constraints *)
        while not (is st ")") && not (eof st) do advance st done;
        expect st ")";
        Tpackage p
      end
      else begin
        let first = ty st in
        if accept st "," then begin
          let ts = ref [ first ] in
          ts := ty st :: !ts;
          while accept st "," do ts := ty st :: !ts done;
          expect st ")";
          (* (a, b) t *)
          Tconstr (lpath st, List.rev !ts)
        end
        else (expect st ")"; first)
      end
  | Some { text = "<"; kind = Operator; _ } ->
      advance st;
      let fields = ref [] in
      while not (is st ">") && not (eof st) do
        if accept st ".." then ()
        else if accept st ";" then ()
        else if kind_is st Lident && is_at st 1 ":" then begin
          let m = name st in
          advance st;
          fields := (m, ty st) :: !fields
        end
        else fields := (unseen "", ty st) :: !fields
      done;
      expect st ">";
      Tobject (List.rev !fields)
  | Some { text = ("[" | "[<" | "[>"); kind = Punctuation; _ } ->
      advance st;
      let tags = ref [] and inherited = ref [] in
      while not (is st "]") && not (eof st) do
        if accept st "|" || accept st ">" then ()
        else if accept st "`" then begin
          let tag = name st in
          let args = ref [] in
          if accept st "of" then begin
            args := [ ty st ];
            while accept st "&" do args := ty st :: !args done
          end;
          tags := (tag, List.rev !args) :: !tags
        end
        else inherited := ty st :: !inherited
      done;
      expect st "]";
      Tvariant (List.rev !tags, List.rev !inherited)
  | _ -> raise Parse_error

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)

let starts_simple_pattern st =
  match at st 0 with
  | Some { kind = Lident | Uident | Int | Float | Char | String; _ } -> true
  | Some { kind = Punctuation; text = "(" | "[" | "[|" | "{" | "`" | "#"; _ } -> true
  | Some { kind = Keyword; text = "true" | "false"; _ } -> true
  | Some { kind = Operator; text = "-"; _ } -> true
  | _ -> false

(* p as x, q and p as x :: r: after an alias, a tuple or a list goes on *)
let rec pattern st : pat =
  let p = ref (or_pattern st) in
  while accept st "as" do p := Palias (!p, lident st) done;
  if accept st "," then Ptuple [ !p; pattern st ]
  else if accept st "::" then Ptuple [ !p; pattern st ]
  else !p

and or_pattern st =
  let p = ref (tuple_pattern st) in
  while is st "|" do
    advance st;
    p := Por (!p, tuple_pattern st)
  done;
  !p

and tuple_pattern st =
  let p = cons_pattern st in
  if is st "," then begin
    let ps = ref [ p ] in
    while accept st "," do ps := cons_pattern st :: !ps done;
    Ptuple (List.rev !ps)
  end
  else p

and cons_pattern st =
  let p = app_pattern st in
  if accept st "::" then Ptuple [ p; cons_pattern st ] else p

and app_pattern st : pat =
  if accept st "lazy" || accept st "exception" then Pinner (app_pattern st)
  else if kind_is st Uident && not (is_at st 1 "." && is_at st 2 "(") then begin
    let c = upath st in
    if starts_simple_pattern st then Pconstr (c, Some (simple_pattern st)) else Pconstr (c, None)
  end
  else if is st "`" then begin
    advance st;
    let tag = name st in
    if starts_simple_pattern st then Pvariant (tag, Some (simple_pattern st)) else Pvariant (tag, None)
  end
  else simple_pattern st

and simple_pattern st : pat =
  match at st 0 with
  | Some { kind = Lident; text = "_"; _ } -> advance st; Pany
  | Some { kind = Lident; _ } -> Pvar (name st)
  | Some { kind = Int | Float | String; _ } -> advance st; Pconst
  | Some { kind = Char; _ } ->
      advance st;
      if accept st ".." then advance st;
      Pconst
  | Some { kind = Operator; text = "-"; _ } -> advance st; advance st; Pconst
  | Some { kind = Keyword; text = "true" | "false"; _ } -> advance st; Pconst
  | Some { kind = Uident; _ } ->
      let ms = modules st in
      let last = uident st in
      if is st "." && is_at st 1 "(" then begin
        (* M.(p) *)
        advance st;
        advance st;
        let p = pattern st in
        expect st ")";
        Popen (ms @ [ last ], p)
      end
      else Pconstr (ms @ [ last ], None)
  | Some { kind = Punctuation; text = "`"; _ } ->
      advance st;
      Pvariant (name st, None)
  | Some { kind = Punctuation; text = "#"; _ } ->
      advance st;
      ignore (lpath st);
      Pany
  | Some { kind = Punctuation; text = "("; _ } ->
      advance st;
      if accept st ")" then Pconst
      else if accept st "module" then begin
        let m = uident st in
        if accept st ":" then ignore (modtype st);
        expect st ")";
        Pmodule m
      end
      else if kind_is st Operator && is_at st 1 ")" || (kind_is st Keyword && is_at st 1 ")") then begin
        (* ( + ): an operator defined *)
        let op = name st in
        expect st ")";
        Pvar op
      end
      else begin
        let p = pattern st in
        let p = if accept st ":" then Pconstraint (p, ty st) else p in
        expect st ")";
        p
      end
  | Some { kind = Punctuation; text = ("[" | "[|") as o; _ } ->
      advance st;
      let close = if o = "[" then "]" else "|]" in
      let ps = ref [] in
      while not (is st close) && not (eof st) do
        ps := pattern st :: !ps;
        ignore (accept st ";")
      done;
      expect st close;
      Plist (List.rev !ps)
  | Some { kind = Punctuation; text = "{"; _ } ->
      advance st;
      let fields = ref [] in
      while not (is st "}") && not (eof st) do
        if kind_is st Lident && (at st 0 |> Option.map (fun (t : Token_ml.t) -> t.text)) = Some "_" then advance st
        else begin
          let f = lpath st in
          if accept st ":" then ignore (ty st);
          let p = if accept st "=" then Some (pattern st) else None in
          fields := (f, p) :: !fields
        end;
        ignore (accept st ";")
      done;
      expect st "}";
      Precord (List.rev !fields)
  | _ -> raise Parse_error

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* an operator's precedence (the higher, the tighter) and whether it is
 * right-associative; None: not an infix operator (it stops an
 * expression: then, ->, |, in, ...) *)
and infix st : (int * bool) option =
  match at st 0 with
  | Some { kind = Operator; text; _ } -> (
      match text with
      | "<-" | ":=" -> Some (1, true)
      | "||" -> Some (3, true)
      | "&&" | "&" -> Some (4, true)
      | "::" -> Some (7, true)
      | "!=" -> Some (5, false)
      | "->" | "|" | ":" | ":>" | "." | ".." | "=>" | "~" | "?" | "!" -> None
      | _ -> (
          match text.[0] with
          | '*' when String.length text >= 2 && text.[1] = '*' -> Some (10, true)
          | '*' | '/' | '%' -> Some (9, false)
          | '+' | '-' -> Some (8, false)
          | '@' | '^' -> Some (6, true)
          | '=' | '<' | '>' | '|' | '&' | '$' -> Some (5, false)
          | _ -> None))
  | Some { kind = Keyword; text; _ } -> (
      match text with
      | "or" -> Some (3, true)
      | "mod" | "land" | "lor" | "lxor" -> Some (9, false)
      | "lsl" | "lsr" | "asr" -> Some (10, true)
      | _ -> None)
  | Some { kind = Punctuation; text = ","; _ } -> Some (2, false)
  | _ -> None

(* let*, and+: a binding operator, as a value in ( let* ) *)
and binding_op st =
  match at st 0 with
  | Some { kind = Keyword; text; _ } -> String.length text > 3 && (String.sub text 0 3 = "let" || String.sub text 0 3 = "and")
  | _ -> false

and starts_atom st =
  match at st 0 with
  | Some { kind = Lident | Uident | Int | Float | Char | String; _ } -> true
  | Some { kind = Punctuation; text = "(" | "[" | "[|" | "{" | "`"; _ } -> true
  | Some { kind = Keyword; text = "true" | "false" | "begin" | "while" | "for" | "new" | "object"; _ } -> true
  | Some { kind = Operator; text; _ } -> text.[0] = '!' || (text.[0] = '~' && String.length text > 1 && text <> "~")
  | _ -> false

(* where an expression can start: an atom, a construct, a minus *)
and starts_expr st =
  starts_atom st
  || (match at st 0 with
     | Some { kind = Keyword; text; _ } ->
         List.mem text [ "let"; "fun"; "function"; "match"; "try"; "if"; "assert"; "lazy" ]
         || (String.length text > 3 && String.sub text 0 3 = "let")
     | Some { kind = Operator; text = "-" | "-." | "+" | "+."; _ } -> true
     | Some { kind = Label; _ } -> false
     | _ -> false)

(* e1; e2; e3: a loop, not a recursion a statement *)
and seq_expr st : expr =
  let e = expr st 0 in
  if is st ";" then begin
    let es = ref [ e ] in
    while accept st ";" && starts_expr st do es := expr st 0 :: !es done;
    Eseq (List.rev !es)
  end
  else e

(* [min]: the loosest operator this expression may take (0: all but ;) *)
and expr st (min : int) : expr =
  match open_construct st with
  | Some e -> e
  | None ->
      let lhs = ref (unary st) in
      let continue = ref true in
      while !continue do
        match infix st with
        | Some (p, right) when p >= min ->
            if is st "," then begin
              let es = ref [ !lhs ] in
              while accept st "," do es := expr st 3 :: !es done;
              lhs := Etuple (List.rev !es)
            end
            else begin
              let op = name st in
              let rhs = expr st (if right then p else p + 1) in
              lhs :=
                match (op.text, !lhs) with
                | "<-", Efield (e, f) -> Esetfield (e, f, rhs)
                | _ -> Eapply (Eident [ op ], [ !lhs; rhs ])
            end
        | _ -> continue := false
      done;
      !lhs

(* let, fun, function, match, try, if: as far right as they can go *)
and open_construct st : expr option =
  match at st 0 with
  | Some { kind = Keyword; text = "let"; _ } -> Some (let_expr st)
  | Some { kind = Keyword; text; _ } when String.length text > 3 && String.sub text 0 3 = "let" ->
      (* let*, let+ *)
      advance st;
      let bs = bindings st in
      expect st "in";
      Some (Eletop (bs, seq_expr st))
  | Some { kind = Keyword; text = "fun"; _ } ->
      advance st;
      if is st "(" && is_at st 1 "type" then begin
        advance st;
        advance st;
        let n = lident st in
        while kind_is st Lident do advance st done;
        expect st ")";
        let ps = params st in
        if accept st ":" then ignore (tuple_ty st);
        expect st "->";
        Some (Enewtype (n, Efun (ps, seq_expr st)))
      end
      else begin
        let ps = params st in
        if accept st ":" then ignore (tuple_ty st);
        expect st "->";
        Some (Efun (ps, seq_expr st))
      end
  | Some { kind = Keyword; text = "function"; _ } ->
      advance st;
      Some (Efunction (cases st))
  | Some { kind = Keyword; text = ("match" | "try"); _ } ->
      advance st;
      let e = seq_expr st in
      expect st "with";
      Some (Ematch (e, cases st))
  | Some { kind = Keyword; text = "if"; _ } ->
      advance st;
      let c = expr st 0 in
      expect st "then";
      let a = expr st 1 in
      let b = if accept st "else" then Some (expr st 1) else None in
      Some (Eif (c, a, b))
  | _ -> None

and let_expr st : expr =
  advance st;
  if accept st "open" then begin
    ignore (accept st "!");
    let m = modexpr st in
    expect st "in";
    Eopen (m, seq_expr st)
  end
  else if accept st "module" then begin
    let n = uident st in
    let args = functor_params st in
    let mt = if accept st ":" then Some (modtype st) else None in
    expect st "=";
    let m = modexpr st in
    let m = match mt with Some t -> Mconstraint (m, t) | None -> m in
    let m = if args = [] then m else Mfunctor (args, m) in
    expect st "in";
    Eletmodule (n, m, seq_expr st)
  end
  else if accept st "exception" then begin
    ignore (constr st);
    expect st "in";
    seq_expr st
  end
  else begin
    let recursive = accept st "rec" in
    let bs = bindings st in
    expect st "in";
    Elet (recursive, bs, seq_expr st)
  end

and bindings st : binding list =
  let bs = ref [ binding st ] in
  while accept st "and" do bs := binding st :: !bs done;
  List.rev !bs

(* f x y : t = e, (a, b) = e, x : t = e, () = e *)
and binding st : binding =
  let function_name =
    match at st 0 with
    | Some { kind = Lident; text; _ } when text <> "_" -> starts_param_at st 1
    | Some { kind = Punctuation; text = "("; _ } ->
        (kind_is_at st 1 Operator || kind_is_at st 1 Keyword) && is_at st 2 ")" && starts_param_at st 3
    | _ -> false
  in
  if function_name then begin
    let n =
      if accept st "(" then (let n = name st in expect st ")"; n) else name st
    in
    let ps = params st in
    let t = if accept st ":" then Some (ty st) else None in
    expect st "=";
    { bpat = Pvar n; bparams = ps; bty = t; bbody = seq_expr st }
  end
  else begin
    let p = pattern st in
    let t = if accept st ":" then Some (ty st) else None in
    expect st "=";
    { bpat = p; bparams = []; bty = t; bbody = seq_expr st }
  end

and starts_param_at st k =
  match at st k with
  | Some { kind = Lident | Label; _ } -> true
  | Some { kind = Punctuation; text = "(" | "[" | "{" | "`" | "#"; _ } -> true
  | Some { kind = Operator; text = "~" | "?"; _ } -> true
  | Some { kind = Uident; _ } -> true
  | Some { kind = Int | Char | String; _ } -> true
  | _ -> false

(* a function's parameters, to the = or the -> *)
and params st : param list =
  let ps = ref [] in
  while (not (is st "=")) && (not (is st "->")) && (not (is st ":")) && starts_param_at st 0 do
    ps := param st :: !ps
  done;
  List.rev !ps

and param st : param =
  match at st 0 with
  | Some { kind = Label; text; _ } ->
      advance st;
      let x = label_name text in
      if has_colon text then
        if text.[0] = '?' && is st "(" then optional st else (simple_pattern st, None)
      else (Pvar (unseen x), None)
  | Some { kind = Operator; text = "~"; _ } ->
      advance st;
      expect st "(";
      let x = lident st in
      if accept st ":" then ignore (ty st);
      expect st ")";
      (Pvar x, None)
  | Some { kind = Operator; text = "?"; _ } ->
      advance st;
      optional st
  | Some { kind = Punctuation; text = "("; _ } when is_at st 1 "type" ->
      advance st;
      advance st;
      while kind_is st Lident do advance st done;
      expect st ")";
      (Pany, None)
  | _ -> (simple_pattern st, None)

(* ?(x = 1), ?(x : int = 1), ?x:(y = 1) *)
and optional st : param =
  expect st "(";
  let p = pattern st in
  let p = if accept st ":" then Pconstraint (p, ty st) else p in
  let d = if accept st "=" then Some (expr st 0) else None in
  expect st ")";
  (p, d)

and cases st : case list =
  ignore (accept st "|");
  let cs = ref [ case st ] in
  while accept st "|" do cs := case st :: !cs done;
  List.rev !cs

and case st : case =
  let p = pattern st in
  let g = if accept st "when" then Some (expr st 0) else None in
  expect st "->";
  { cpat = p; guard = g; cbody = seq_expr st }

(* -e, -.e *)
and unary st : expr =
  match at st 0 with
  | Some { kind = Operator; text = "-" | "-." | "+" | "+."; _ } ->
      let o = name st in
      Eapply (Eident [ o ], [ expr st 11 ])
  | _ -> application st

(* f a b, C a, `A a, assert a, lazy a *)
and application st : expr =
  if accept st "assert" || accept st "lazy" then Emisc [ postfix st ]
  else
    let f = postfix st in
    match f with
    | Econstr (c, None) when starts_atom st -> Econstr (c, Some (postfix st))
    | Evariant (tag, None) when starts_atom st -> Evariant (tag, Some (postfix st))
    | _ ->
        if starts_atom st || kind_is st Label then begin
          let args = ref [] in
          while starts_atom st || kind_is st Label do args := argument st :: !args done;
          Eapply (f, List.rev !args)
        end
        else f

and argument st : expr =
  match at st 0 with
  | Some { kind = Label; text; _ } ->
      advance st;
      if has_colon text then postfix st else Econst
  | _ -> postfix st

(* an atom and what follows it: e.x, e.M.x, e.(i), e.[i], e#m *)
and postfix st : expr =
  let e = ref (atom st) in
  let continue = ref true in
  while !continue do
    if is st "." && (kind_is_at st 1 Lident || kind_is_at st 1 Uident) then begin
      advance st;
      e := Efield (!e, lpath st)
    end
    else if is st "." && (is_at st 1 "(" || is_at st 1 "[" || is_at st 1 "{") then begin
      advance st;
      let close = match text_at st 0 with "(" -> ")" | "[" -> "]" | _ -> "}" in
      advance st;
      let i = seq_expr st in
      expect st close;
      e := Eapply (!e, [ i ])
    end
    else if is st "#" && kind_is_at st 1 Lident then begin
      advance st;
      advance st;
      e := Emisc [ !e ]
    end
    else if is st "#" && is_at st 1 "#" then begin
      (* js_of_ocaml's o##m, o##.p *)
      advance st;
      advance st;
      ignore (accept st ".");
      if kind_is st Lident then advance st;
      e := Emisc [ !e ]
    end
    else continue := false
  done;
  !e

and atom st : expr =
  match at st 0 with
  | Some { kind = Lident; _ } -> Eident [ name st ]
  | Some { kind = Int | Float | Char | String; _ } -> advance st; Econst
  | Some { kind = Keyword; text = "true" | "false"; _ } -> advance st; Econst
  | Some { kind = Uident; _ } ->
      let ms = modules st in
      if kind_is st Lident then Eident (ms @ [ name st ])
      else begin
        let last = uident st in
        if is st "." && is_at st 1 "(" && (kind_is_at st 2 Operator || kind_is_at st 2 Keyword) && is_at st 3 ")" then begin
          (* Stdlib.( + ): an operator of a module *)
          advance st;
          advance st;
          let op = name st in
          advance st;
          Eident (ms @ [ last; op ])
        end
        else if is st "." && (is_at st 1 "(" || is_at st 1 "[" || is_at st 1 "[|" || is_at st 1 "{") then begin
          (* M.(e), M.[e], M.{e}: a local open *)
          advance st;
          let m = Mident (ms @ [ last ]) in
          if is st "(" then begin
            advance st;
            let e = if is st ")" then Econst else seq_expr st in
            expect st ")";
            Eopen (m, e)
          end
          else Eopen (m, atom st)
        end
        else Econstr (ms @ [ last ], None)
      end
  | Some { kind = Punctuation; text = "`"; _ } ->
      advance st;
      Evariant (name st, None)
  | Some { kind = Operator; text; _ } when text.[0] = '!' || text.[0] = '~' ->
      let op = name st in
      Eapply (Eident [ op ], [ postfix st ])
  | Some { kind = Punctuation; text = "("; _ } ->
      advance st;
      if accept st ")" then Econst
      else if accept st "module" then begin
        let m = modexpr st in
        if accept st ":" then ignore (modtype st);
        expect st ")";
        Epack m
      end
      else if (kind_is st Operator || (kind_is st Keyword && (infix st <> None || binding_op st))) && is_at st 1 ")" then begin
        let op = name st in
        advance st;
        Eident [ op ]
      end
      else begin
        let e = seq_expr st in
        let e =
          if accept st ":" then Econstraint (e, ty st)
          else if accept st ":>" then Econstraint (e, ty st)
          else e
        in
        let e = if accept st ":>" then Econstraint (e, ty st) else e in
        expect st ")";
        e
      end
  | Some { kind = Keyword; text = "begin"; _ } ->
      advance st;
      if accept st "end" then Econst
      else begin
        let e = seq_expr st in
        expect st "end";
        e
      end
  | Some { kind = Punctuation; text = ("[" | "[|") as o; _ } ->
      advance st;
      let close = if o = "[" then "]" else "|]" in
      let es = ref [] in
      while not (is st close) && not (eof st) do
        es := expr st 1 :: !es;
        if not (accept st ";") && not (is st close) then raise Parse_error
      done;
      expect st close;
      Elist (List.rev !es)
  | Some { kind = Punctuation; text = "{"; _ } -> record st
  | Some { kind = Keyword; text = "while"; _ } ->
      advance st;
      let c = seq_expr st in
      expect st "do";
      let b = if is st "done" then Econst else seq_expr st in
      expect st "done";
      Ewhile (c, b)
  | Some { kind = Keyword; text = "for"; _ } ->
      advance st;
      let i = lident st in
      expect st "=";
      let a = seq_expr st in
      if not (accept st "to") then expect st "downto";
      let b = seq_expr st in
      expect st "do";
      let body = if is st "done" then Econst else seq_expr st in
      expect st "done";
      Efor (i, a, b, body)
  | Some { kind = Keyword; text = "new"; _ } ->
      advance st;
      (* new%js, js_of_ocaml's *)
      (match at st 0 with Some { kind = Operator; text; _ } when text.[0] = '%' -> advance st; if kind_is st Lident then advance st | _ -> ());
      ignore (lpath st);
      Emisc []
  | Some { kind = Keyword; text = "object"; _ } ->
      skip_to_end st;
      Emisc []
  | _ -> raise Parse_error

(* { x = a; M.y; z : t = b }, { e with x = a } *)
and record st : expr =
  advance st;
  let fields_first =
    let k = ref 0 in
    while kind_is_at st !k Uident && is_at st (!k + 1) "." do k := !k + 2 done;
    kind_is_at st !k Lident && (is_at st (!k + 1) "=" || is_at st (!k + 1) ";" || is_at st (!k + 1) "}" || is_at st (!k + 1) ":")
  in
  let base = if fields_first then None else (let b = postfix st in expect st "with"; Some b) in
  let fields = ref [] in
  while not (is st "}") && not (eof st) do
    let f = lpath st in
    if accept st ":" then ignore (ty st);
    let v = if accept st "=" then Some (expr st 1) else None in
    fields := (f, v) :: !fields;
    if not (accept st ";") && not (is st "}") then raise Parse_error
  done;
  expect st "}";
  Erecord (base, List.rev !fields)

(* object ... end, and what nests in it *)
and skip_to_end st =
  let depth = ref 0 in
  let continue = ref true in
  while !continue && not (eof st) do
    (match at st 0 with
    | Some { kind = Keyword; text = "object" | "struct" | "sig" | "begin"; _ } -> incr depth
    | Some { kind = Keyword; text = "end"; _ } -> decr depth; if !depth = 0 then continue := false
    | _ -> ());
    advance st
  done

(*****************************************************************************)
(* Modules *)
(*****************************************************************************)

and functor_params st : (name * modtype option) list =
  let ps = ref [] in
  while is st "(" do
    advance st;
    if accept st ")" then ()
    else begin
      let n = uident st in
      let t = if accept st ":" then Some (modtype st) else None in
      expect st ")";
      ps := (n, t) :: !ps
    end
  done;
  List.rev !ps

and modexpr st : modexpr =
  let m =
    match at st 0 with
    | Some { kind = Keyword; text = "struct"; _ } ->
        advance st;
        let is_ = items st ~stop:"end" in
        expect st "end";
        Mstruct is_
    | Some { kind = Keyword; text = "functor"; _ } ->
        advance st;
        let ps = functor_params st in
        expect st "->";
        Mfunctor (ps, modexpr st)
    | Some { kind = Punctuation; text = "("; _ } ->
        advance st;
        if accept st "val" then begin
          let e = expr st 0 in
          if accept st ":" then ignore (modtype st);
          expect st ")";
          Munpack e
        end
        else begin
          let m = modexpr st in
          let m = if accept st ":" then Mconstraint (m, modtype st) else m in
          expect st ")";
          m
        end
    | Some { kind = Uident; _ } -> Mident (upath st)
    | _ -> raise Parse_error
  in
  let m = ref m in
  while is st "(" do
    advance st;
    if accept st ")" then ()
    else begin
      let arg = modexpr st in
      let arg = if accept st ":" then Mconstraint (arg, modtype st) else arg in
      expect st ")";
      m := Mapply (!m, arg)
    end
  done;
  !m

and modtype st : modtype =
  let t =
    match at st 0 with
    | Some { kind = Keyword; text = "sig"; _ } ->
        advance st;
        let is_ = items st ~stop:"end" in
        expect st "end";
        MTsig is_
    | Some { kind = Keyword; text = "functor"; _ } ->
        advance st;
        let ps = functor_params st in
        expect st "->";
        MTfunctor (ps, modtype st)
    | Some { kind = Keyword; text = "module"; _ } ->
        advance st;
        expect st "type";
        expect st "of";
        MTtypeof (modexpr st)
    | Some { kind = Punctuation; text = "("; _ } ->
        let save = st.pos in
        (try
           let ps = functor_params st in
           expect st "->";
           MTfunctor (ps, modtype st)
         with Parse_error ->
           st.pos <- save;
           advance st;
           let t = modtype st in
           expect st ")";
           t)
    | Some { kind = Uident; _ } -> MTident (lpath_or_upath st)
    | _ -> raise Parse_error
  in
  if accept st "with" then begin
    let cs = ref [] in
    let one () =
      if accept st "type" then begin
        while kind_is st Type_var || is st "(" || is st "," || is st ")" do advance st done;
        let p = lpath st in
        if not (accept st "=") then expect st ":=";
        ignore (accept st "private");
        cs := (p, ty st) :: !cs
      end
      else begin
        expect st "module";
        ignore (upath st);
        if not (accept st "=") then expect st ":=";
        ignore (upath st)
      end
    in
    one ();
    while accept st "and" do one () done;
    MTwith (t, List.rev !cs)
  end
  else t

(* a module type's path: M.S (its last an uppercase name) *)
and lpath_or_upath st = let ms = modules st in if kind_is st Lident then ms @ [ name st ] else ms @ [ uident st ]

(*****************************************************************************)
(* Items *)
(*****************************************************************************)

and constr st : constr =
  let c = if kind_is st Uident then name st else (let n = name st in if is st ")" then advance st; n) in
  if accept st "of" then begin
    if is st "{" then { cname = c; cargs = []; crecord = fields st; cres = None }
    else begin
      let ts = ref [ app_ty st ] in
      while accept st "*" do ts := app_ty st :: !ts done;
      let res = if accept st ":" then Some (ty st) else None in
      { cname = c; cargs = List.rev !ts; crecord = []; cres = res }
    end
  end
  else if accept st ":" then begin
    if is st "{" then begin
      let fs = fields st in
      expect st "->";
      { cname = c; cargs = []; crecord = fs; cres = Some (ty st) }
    end
    else { cname = c; cargs = []; crecord = []; cres = Some (ty st) }
  end
  else if accept st "=" then (ignore (upath st); { cname = c; cargs = []; crecord = []; cres = None })
  else { cname = c; cargs = []; crecord = []; cres = None }

and fields st : field list =
  expect st "{";
  let fs = ref [] in
  while not (is st "}") && not (eof st) do
    ignore (accept st "mutable");
    let f = lident st in
    expect st ":";
    fs := { fname = f; fty = ty st } :: !fs;
    if not (accept st ";") && not (is st "}") then raise Parse_error
  done;
  expect st "}";
  List.rev !fs

and typedecl st : typedecl =
  (* its parameters: 'a, ('a, 'b), +'a, _ *)
  let tparams = ref [] in
  let param () =
    ignore (accept st "+" || accept st "-" || accept st "!" || accept st "+!" || accept st "-!");
    if kind_is st Type_var then tparams := name st :: !tparams
    else if kind_is st Lident && text_at st 0 = "_" then advance st
    else raise Parse_error
  in
  if kind_is st Type_var || is st "+" || is st "-" || (kind_is st Lident && text_at st 0 = "_" && kind_is_at st 1 Lident) then param ()
  else if is st "(" then begin
    advance st;
    param ();
    while accept st "," do param () done;
    expect st ")"
  end;
  let n = lpath st |> List.rev |> List.hd in
  let manifest = ref None and kind = ref Kabstract in
  let kind_after_eq () =
    ignore (accept st "private");
    if is st "{" then kind := Krecord (fields st)
    else if accept st ".." then kind := Kopen
    else if is st "|" || (kind_is st Uident && not (is_at st 1 ".")) || (is st "(" && is_at st 1 ")") || (is st "[" && is_at st 1 "]") then begin
      ignore (accept st "|");
      let cs = ref [ constr st ] in
      while accept st "|" do cs := constr st :: !cs done;
      kind := Kvariant (List.rev !cs)
    end
    else begin
      manifest := Some (ty st);
      if accept st "=" then begin
        ignore (accept st "private");
        if is st "{" then kind := Krecord (fields st)
        else if accept st ".." then kind := Kopen
        else begin
          ignore (accept st "|");
          let cs = ref [ constr st ] in
          while accept st "|" do cs := constr st :: !cs done;
          kind := Kvariant (List.rev !cs)
        end
      end
    end
  in
  if accept st "=" then kind_after_eq ()
  else if accept st "+=" then begin
    ignore (accept st "private");
    ignore (accept st "|");
    let cs = ref [ constr st ] in
    while accept st "|" do cs := constr st :: !cs done;
    kind := Kvariant (List.rev !cs)
  end
  else if accept st ":=" then manifest := Some (ty st);
  while accept st "constraint" do
    ignore (ty st);
    expect st "=";
    ignore (ty st)
  done;
  { tname = n; tparams = List.rev !tparams; tmanifest = !manifest; tkind = !kind }

and item st : item =
  match at st 0 with
  | Some { kind = Keyword; text = "let"; _ } when not (is_at st 1 "open" || is_at st 1 "module" || is_at st 1 "exception") ->
      advance st;
      let recursive = accept st "rec" in
      let bs = bindings st in
      if accept st "in" then Ieval (Elet (recursive, bs, seq_expr st)) else Ilet (recursive, bs)
  | Some { kind = Keyword; text = "type"; _ } ->
      advance st;
      ignore (accept st "nonrec");
      let ds = ref [ typedecl st ] in
      while accept st "and" do ds := typedecl st :: !ds done;
      Itype (List.rev !ds)
  | Some { kind = Keyword; text = "exception"; _ } ->
      advance st;
      Iexception (constr st)
  | Some { kind = Keyword; text = "external"; _ } ->
      advance st;
      let n = if accept st "(" then (let n = name st in expect st ")"; n) else lident st in
      expect st ":";
      let t = ty st in
      expect st "=";
      while kind_is st String do advance st done;
      Iexternal (n, t)
  | Some { kind = Keyword; text = "val"; _ } ->
      advance st;
      let n = if accept st "(" then (let n = name st in expect st ")"; n) else lident st in
      expect st ":";
      Ival (n, ty st)
  | Some { kind = Keyword; text = "module"; _ } ->
      advance st;
      if accept st "type" then begin
        ignore (accept st "of");
        let n = if kind_is st Uident then name st else lident st in
        if accept st "=" || accept st ":=" then Imodtype (n, Some (modtype st)) else Imodtype (n, None)
      end
      else begin
        ignore (accept st "rec");
        let one () =
          (* module _ : S = M, a check that M has S *)
          let n = if kind_is st Lident && text_at st 0 = "_" then name st else uident st in
          let args = functor_params st in
          let mt = if accept st ":" then Some (modtype st) else None in
          if accept st "=" || accept st ":=" then begin
            let m = modexpr st in
            let m = match mt with Some t -> Mconstraint (m, t) | None -> m in
            Imodule (n, if args = [] then m else Mfunctor (args, m))
          end
          else
            match mt with
            | Some t -> Imodsig (n, if args = [] then t else MTfunctor (args, t))
            | None -> raise Parse_error
        in
        let first = one () in
        while accept st "and" do ignore (one ()) done;
        first
      end
  | Some { kind = Keyword; text = "open"; _ } ->
      advance st;
      ignore (accept st "!");
      Iopen (modexpr st)
  | Some { kind = Keyword; text = "include"; _ } ->
      advance st;
      (match at st 0 with
      | Some { kind = Keyword; text = "sig" | "functor"; _ } | Some { kind = Keyword; text = "module"; _ } -> ignore (modtype st); Iinclude (Mstruct [])
      | _ -> Iinclude (modexpr st))
  | Some { kind = Keyword; text = "class"; _ } ->
      skip_item st;
      Iclass []
  | _ -> Ieval (seq_expr st)

(* the tokens of an item that did not parse, or a class: to the next
 * one, the next keyword starting an item at the same column or left of it *)
and skip_item st =
  let col = match at st 0 with Some t -> t.col | None -> 0 in
  advance st;
  let continue = ref true in
  while !continue && not (eof st) do
    match at st 0 with
    | Some { kind = Keyword; text = "let" | "type" | "module" | "open" | "include" | "exception" | "external" | "val" | "class" | "end"; col = c; _ }
      when c <= col ->
        continue := false
    | Some { kind = Punctuation; text = ";;"; _ } -> continue := false
    | _ -> advance st
  done

(* a module's or a signature's items, to [stop] or the end: an item that
 * does not parse is skipped, not the file *)
and items st ~(stop : string) : item list =
  let out = ref [] in
  let continue = ref true in
  while !continue do
    while accept st ";;" do () done;
    if eof st || is st stop then continue := false
    else begin
      let start = st.pos in
      match item st with
      | it -> out := it :: !out
      | exception Parse_error ->
          st.pos <- start;
          skip_item st;
          st.skipped <- (st.idx.(start), if eof st then max_int else st.idx.(st.pos)) :: st.skipped
    end
  done;
  List.rev !out

let parse (tokens : Token_ml.t list) : file =
  let st = reading (Array.of_list tokens) in
  (* an "end" at the top, the rest of a module that did not parse: past it *)
  let all = ref [] in
  while not (eof st) do
    all := List.rev_append (items st ~stop:"end") !all;
    if not (eof st) then begin
      let start = st.pos in
      advance st;
      st.skipped <- (st.idx.(start), if eof st then max_int else st.idx.(st.pos)) :: st.skipped
    end
  done;
  { items = List.rev !all; skipped = List.rev st.skipped }
