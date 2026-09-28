(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Json.mli *)

type t = Null | Bool of bool | Number of float | String of string | Array of t list | Object of (string * t) list

exception Bad of int * string

(* value := object | array | string | number | - number | true | false | null
 * object := { (string : value ,)* }   array := [ (value ,)* ] *)
let parse (text : string) : (t, string) result =
  match Js_lexer.tokenize text with
  | exception Js_lexer.Error (line, msg) -> Error (Printf.sprintf "line %d: %s" line msg)
  | tokens -> (
      let toks = ref tokens in
      let peek () : Js_lexer.token = match !toks with t :: _ -> t | [] -> { kind = Eof; line = 0; newline_before = false } in
      let next () = let t = peek () in (toks := match !toks with _ :: r -> r | [] -> []); t in
      let fail (t : Js_lexer.token) = raise (Bad (t.line, "unexpected " ^ Js_lexer.to_string t.kind)) in
      let expect p = let t = next () in if t.kind <> Punct p then fail t in
      (* the items up to [close], a comma after each but maybe the last *)
      let items close item =
        let out = ref [] in
        while (peek ()).kind <> Punct close do
          out := item () :: !out;
          if (peek ()).kind <> Punct close then expect ","
        done;
        ignore (next ());
        List.rev !out
      in
      let rec value () : t =
        let t = next () in
        match t.kind with
        | Punct "{" ->
            Object
              (items "}" (fun () ->
                   let k = next () in
                   match k.kind with
                   | String s -> expect ":"; (s, value ())
                   | _ -> fail k))
        | Punct "[" -> Array (items "]" value)
        | Punct "-" -> ( match (next ()).kind with Number n -> Number (-.n) | _ -> fail t)
        | String s -> String s
        | Number n -> Number n
        | Keyword "true" -> Bool true
        | Keyword "false" -> Bool false
        | Keyword "null" -> Null
        | _ -> fail t
      in
      match
        let v = value () in
        let t = next () in
        if t.kind <> Eof then fail t;
        v
      with
      | v -> Ok v
      | exception Bad (line, msg) -> Error (Printf.sprintf "line %d: %s" line msg))

let member (k : string) (v : t) : t option = match v with Object fields -> List.assoc_opt k fields | _ -> None
