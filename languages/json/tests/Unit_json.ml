(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_json.mli *)

let rec show (v : Json.t) : string =
  match v with
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number n -> Printf.sprintf "%g" n
  | String s -> Printf.sprintf "%S" s
  | Array vs -> "[" ^ String.concat "; " (List.map show vs) ^ "]"
  | Object fs -> "{" ^ String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ show v) fs) ^ "}"

let check what text expected =
  Alcotest.(check string) what expected (match Json.parse text with Ok v -> show v | Error e -> "error: " ^ e)

let tests =
  Testo.categorize "Json"
    [
      Testo.create "the worked example" (fun () ->
          check "Json.mli's" {|{ "colors": { "kernel": "#e08030" }, "depth": 2, "x": [true, null] }|}
            {|{colors={kernel="#e08030"}; depth=2; x=[true; null]}|});
      Testo.create "jsonnet's leniencies" (fun () ->
          check "comments, a trailing comma, a negative number, an escape"
            "// the colours\n{ \"a\": [-1.5, \"x\\ny\",], /* b */ }" {|{a=[-1.5; "x\ny"]}|});
      Testo.create "errors" (fun () ->
          check "a key not a string" "{ a: 1 }" "error: line 1: unexpected Name a";
          check "something after" "1 2" "error: line 1: unexpected Number 2";
          check "unclosed" "[1," "error: line 1: unexpected Eof");
    ]
