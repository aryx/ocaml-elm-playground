(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Jsonnet_parse.mli *)

open Jsonnet_ast
module L = Jsonnet_lexer

exception Bad of int * string

let resolve ~(from : string) (p : string) : string =
  let joined = if String.length p > 0 && p.[0] = '/' then p else Filename.concat (Filename.dirname from) p in
  let absolute = String.length joined > 0 && joined.[0] = '/' in
  let parts =
    List.fold_left
      (fun acc part ->
        match (part, acc) with
        | ("" | "."), _ -> acc
        | "..", x :: rest when x <> ".." -> rest
        | _ -> part :: acc)
      [] (String.split_on_char '/' joined)
  in
  (if absolute then "/" else "") ^ String.concat "/" (List.rev parts)

(* the binary operators' levels, the lowest first *)
let levels =
  [ [ "||" ]; [ "&&" ]; [ "|" ]; [ "^" ]; [ "&" ]; [ "=="; "!=" ]; [ "<"; "<="; ">"; ">="; "in" ]; [ "<<"; ">>" ]; [ "+"; "-" ]; [ "*"; "/"; "%" ] ]

let parse ~(path : string) (text : string) : (expr, int * string) result =
  match L.tokenize text with
  | exception L.Error (line, msg) -> Error (line, msg)
  | tokens -> (
      let toks = ref tokens in
      let peek () : L.token = match !toks with t :: _ -> t | [] -> { kind = Eof; line = 0 } in
      let peek2 () : L.kind = match !toks with _ :: t :: _ -> t.kind | _ -> Eof in
      let next () = let t = peek () in (toks := match !toks with _ :: r -> r | [] -> []); t in
      let fail (t : L.token) = raise (Bad (t.line, "unexpected " ^ L.to_string t.kind)) in
      let is k = (peek ()).kind = k in
      let expect k = let t = next () in if t.kind <> k then raise (Bad (t.line, Printf.sprintf "expected %s, not %s" (L.to_string k) (L.to_string t.kind))) in
      let ident () = match next () with { kind = Id x; _ } -> x | t -> fail t in
      let string_lit () = match next () with { kind = String s; _ } -> s | t -> fail t in
      (* items up to [close], separated by commas, maybe one at the end *)
      let items close item =
        let out = ref [] in
        while not (is close) do
          out := item () :: !out;
          if not (is close) then expect (Punct ",")
        done;
        ignore (next ());
        List.rev !out
      in
      let rec expr () : expr =
        let t = peek () in
        match t.kind with
        | Keyword "local" ->
            ignore (next ());
            let binds = ref [ bind () ] in
            while is (Punct ",") do ignore (next ()); binds := bind () :: !binds done;
            expect (Punct ";");
            Local (List.rev !binds, expr ())
        | Keyword "if" ->
            ignore (next ());
            let c = expr () in
            expect (Keyword "then");
            let a = expr () in
            let b = if is (Keyword "else") then (ignore (next ()); Some (expr ())) else None in
            If (c, a, b)
        | Keyword "function" ->
            ignore (next ());
            let ps = params () in
            Function (ps, expr ())
        | Keyword "assert" ->
            ignore (next ());
            let c = expr () in
            let msg = if is (Op ":") then (ignore (next ()); Some (expr ())) else None in
            expect (Punct ";");
            At (t.line, Assert (c, msg, expr ()))
        | Keyword "error" -> ignore (next ()); At (t.line, Error (expr ()))
        | _ -> binary 0
      (* x = e, or f(x) = e: a function *)
      and bind () =
        let x = ident () in
        if is (Punct "(") then (let ps = params () in expect (Op "="); (x, Function (ps, expr ())))
        else (expect (Op "="); (x, expr ()))
      and params () =
        expect (Punct "(");
        items (Punct ")") (fun () ->
            let x = ident () in
            if is (Op "=") then (ignore (next ()); (x, Some (expr ()))) else (x, None))
      and binary level =
        if level >= List.length levels then unary ()
        else
          let ops = List.nth levels level in
          let left = ref (binary (level + 1)) in
          let op () = match (peek ()).kind with Op o when List.mem o ops -> Some o | Keyword "in" when List.mem "in" ops -> Some "in" | _ -> None in
          let continue = ref true in
          while !continue do
            match op () with
            | Some o ->
                let t = next () in
                if o = "in" && is (Keyword "super") && (match peek2 () with Punct ("." | "[") -> false | _ -> true) then begin
                  ignore (next ());
                  left := At (t.line, In_super !left)
                end
                else left := At (t.line, Binary (o, !left, binary (level + 1)))
            | None -> continue := false
          done;
          !left
      and unary () =
        match (peek ()).kind with
        | Op (("-" | "+" | "!" | "~") as o) ->
            let t = next () in
            At (t.line, Unary (o, unary ()))
        | _ -> postfix (primary ())
      and postfix e =
        let t = peek () in
        match t.kind with
        | Punct "." ->
            ignore (next ());
            postfix (At (t.line, Field (e, ident ())))
        | Punct "[" ->
            ignore (next ());
            postfix (At (t.line, index e))
        | Punct "(" ->
            ignore (next ());
            let args = items (Punct ")") (fun () -> match (peek ()).kind, peek2 () with Id x, Op "=" -> ignore (next ()); ignore (next ()); `Named (x, expr ()) | _ -> `Pos (expr ())) in
            if is (Keyword "tailstrict") then ignore (next ());
            let pos = List.filter_map (function `Pos e -> Some e | `Named _ -> None) args in
            let named = List.filter_map (function `Named (x, e) -> Some (x, e) | `Pos _ -> None) args in
            postfix (At (t.line, Call (e, pos, named)))
        | Punct "{" ->
            ignore (next ());
            postfix (At (t.line, Binary ("+", e, obj ())))
        | _ -> e
      (* after e[: an index, or a slice *)
      and index e =
        let colon () = is (Op ":") in
        let a = if colon () || is (Op "::") then None else Some (expr ()) in
        if is (Punct "]") then begin
          ignore (next ());
          match a with Some i -> Index (e, i) | None -> fail (peek ())
        end
        else begin
          let b, c =
            if is (Op "::") then (ignore (next ()); (None, if is (Punct "]") then None else Some (expr ())))
            else begin
              expect (Op ":");
              let b = if colon () || is (Punct "]") then None else Some (expr ()) in
              let c = if colon () then (ignore (next ()); if is (Punct "]") then None else Some (expr ())) else None in
              (b, c)
            end
          in
          expect (Punct "]");
          Slice (e, a, b, c)
        end
      and primary () : expr =
        let t = next () in
        match t.kind with
        | Keyword "null" -> Null
        | Keyword "true" -> Bool true
        | Keyword "false" -> Bool false
        | Keyword "self" -> Self
        | Punct "$" -> Dollar
        | String s -> Str s
        | Number n -> Num n
        | Id x -> At (t.line, Var x)
        | Punct "(" ->
            let e = expr () in
            expect (Punct ")");
            e
        | Punct "{" -> obj ()
        | Punct "[" -> array ()
        | Keyword "super" -> (
            match (next ()).kind with
            | Punct "." -> At (t.line, Super_field (ident ()))
            | Punct "[" ->
                let i = expr () in
                expect (Punct "]");
                At (t.line, Super_index i)
            | _ -> fail t)
        | Keyword "import" -> At (t.line, Import (resolve ~from:path (string_lit ())))
        | Keyword "importstr" -> At (t.line, Importstr (resolve ~from:path (string_lit ())))
        | Keyword ("local" | "if" | "function" | "assert" | "error") ->
            toks := t :: !toks;
            expr ()
        | _ -> fail t
      and array () =
        if is (Punct "]") then (ignore (next ()); Array [])
        else begin
          let first = expr () in
          if is (Keyword "for") then begin
            let cs = comps () in
            expect (Punct "]");
            Array_comp (first, cs)
          end
          else begin
            if is (Punct ",") then ignore (next ());
            Array (first :: items (Punct "]") expr)
          end
        end
      (* for x in e, then for and if, up to what closes them *)
      and comps () =
        let rec more acc =
          match (peek ()).kind with
          | Keyword "for" ->
              ignore (next ());
              let x = ident () in
              expect (Keyword "in");
              more (For (x, expr ()) :: acc)
          | Keyword "if" ->
              ignore (next ());
              more (If_comp (expr ()) :: acc)
          | _ -> List.rev acc
        in
        more []
      (* after {: the members, or a comprehension *)
      and obj () : expr =
        let members = ref [] in
        let comp = ref None in
        while not (is (Punct "}")) do
          let m = member () in
          (match m with
          | Field_m (Computed k, false, Default, v) when is (Keyword "for") -> comp := Some (k, v, comps ())
          | _ -> members := m :: !members);
          if not (is (Punct "}")) then expect (Punct ",")
        done;
        ignore (next ());
        match !comp with
        | None -> Object (List.rev !members)
        | Some (k, v, cs) ->
            let locals = List.map (function Local_m (x, e) -> (x, e) | _ -> raise (Bad ((peek ()).line, "a comprehension has one field"))) (List.rev !members) in
            Object_comp (locals, k, v, cs)
      and member () : member =
        let t = peek () in
        match t.kind with
        | Keyword "local" ->
            ignore (next ());
            let x, e = bind () in
            Local_m (x, e)
        | Keyword "assert" ->
            ignore (next ());
            let c = expr () in
            let msg = if is (Op ":") then (ignore (next ()); Some (expr ())) else None in
            Assert_m (c, msg)
        | _ ->
            let name =
              match (next ()).kind with
              | Id x -> Fixed x
              | String s -> Fixed s
              | Punct "[" ->
                  let e = expr () in
                  expect (Punct "]");
                  Computed e
              | _ -> fail t
            in
            let method_ = if is (Punct "(") then Some (params ()) else None in
            let plus, vis =
              match (next ()).kind with
              | Op ":" -> (false, Default)
              | Op "::" -> (false, Hidden)
              | Op ":::" -> (false, Visible)
              | Op "+:" -> (true, Default)
              | Op "+::" -> (true, Hidden)
              | Op "+:::" -> (true, Visible)
              | _ -> fail t
            in
            let v = expr () in
            Field_m (name, plus, vis, match method_ with Some ps -> Function (ps, v) | None -> v)
      in
      match
        let e = expr () in
        if not (is Eof) then fail (peek ());
        e
      with
      | e -> Ok e
      | exception Bad (line, msg) -> Error (line, msg))
