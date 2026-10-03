(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Highlight_st.mli *)

module H = Highlight_code

(*****************************************************************************)
(* The tokens *)
(*****************************************************************************)

type kind =
  | Comment (* "..." *)
  | String (* '...' *)
  | Char (* $a *)
  | Hash (* the # of a symbol or of a literal array *)
  | Symbol (* what follows it: foo, at:put:, + *)
  | Number
  | Ident
  | Keyword (* at: *)
  | Binary
  | Assign
  | Caret
  | Bang (* a chunk's end *)
  | Punct of char (* ( ) [ ] | . ; : *)

type token = { kind : kind; text : string; line : int (* from 1 *); col : int; mutable cat : H.category }

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_binary c = String.contains "+-*/\\<>=~@%&?," c

(* every token of a file, comments included; what is not one (spaces,
 * a character that starts nothing) is skipped. Never fails: a file
 * being typed is still drawn. *)
let scan (src : string) : token array =
  let n = String.length src in
  let out = ref [] in
  let line = ref 1 and line_start = ref 0 in
  let emit kind i j l c = out := { kind; text = String.sub src i (j - i); line = l; col = c; cat = H.Normal } :: !out in
  (* past a quoted text, its quote doubled inside *)
  let quoted q i =
    let j = ref (i + 1) and fin = ref false in
    while (not !fin) && !j < n do
      if src.[!j] = q then if !j + 1 < n && src.[!j + 1] = q then j := !j + 2 else fin := true else incr j
    done;
    min n (!j + 1)
  in
  let ident i =
    let j = ref i in
    while !j < n && (is_letter src.[!j] || is_digit src.[!j] || src.[!j] = '_') do incr j done;
    !j
  in
  let i = ref 0 in
  while !i < n do
    let c = src.[!i] and l = !line and col = !i - !line_start in
    let next =
      if c = '\n' then begin
        incr line;
        line_start := !i + 1;
        !i + 1
      end
      else if c = ' ' || c = '\t' || c = '\r' then !i + 1
      else if c = '"' || c = '\'' then begin
        let j = quoted c !i in
        emit (if c = '"' then Comment else String) !i j l col;
        (* the lines it goes over *)
        for k = !i to j - 1 do
          if src.[k] = '\n' then begin
            incr line;
            line_start := k + 1
          end
        done;
        j
      end
      else if c = '$' && !i + 1 < n then (emit Char !i (!i + 2) l col; !i + 2)
      else if c = '#' then begin
        emit Hash !i (!i + 1) l col;
        let s = !i + 1 in
        (* a symbol's name, its colons too: at:put: *)
        let j = ref s in
        if s < n && is_letter src.[s] then begin
          j := ident s;
          while !j < n && src.[!j] = ':' && not (!j + 1 < n && src.[!j + 1] = '=') do
            j := ident (!j + 1)
          done
        end
        else while !j < n && is_binary src.[!j] do incr j done;
        if !j > s then emit Symbol s !j l (col + 1);
        !j
      end
      else if is_letter c then begin
        let j = ident !i in
        if j < n && src.[j] = ':' && not (j + 1 < n && src.[j + 1] = '=') then (emit Keyword !i (j + 1) l col; j + 1)
        else (emit Ident !i j l col; j)
      end
      else if is_digit c then begin
        (* 16r1F, 3.14, 1e10: digits, letters for a radix, a point followed by a digit *)
        let j = ref !i in
        while !j < n && (is_digit src.[!j] || is_letter src.[!j] || (src.[!j] = '.' && !j + 1 < n && is_digit src.[!j + 1])) do incr j done;
        emit Number !i !j l col;
        !j
      end
      else if c = ':' && !i + 1 < n && src.[!i + 1] = '=' then (emit Assign !i (!i + 2) l col; !i + 2)
      else if c = '_' then (emit Assign !i (!i + 1) l col; !i + 1)
      else if c = '^' then (emit Caret !i (!i + 1) l col; !i + 1)
      else if c = '!' then (emit Bang !i (!i + 1) l col; !i + 1)
      else if String.contains "()[]|.;:" c then (emit (Punct c) !i (!i + 1) l col; !i + 1)
      else if is_binary c then begin
        let j = ref !i in
        while !j < n && is_binary src.[!j] do incr j done;
        emit Binary !i !j l col;
        !j
      end
      else !i + 1
    in
    i := next
  done;
  Array.of_list (List.rev !out)

(*****************************************************************************)
(* The categories *)
(*****************************************************************************)

let pseudo = [ "self"; "super"; "nil"; "true"; "false"; "thisContext" ]

(* the messages that are control: what the compiler makes jumps of, and
 * the collections' loops *)
let control =
  [ "ifTrue:"; "ifFalse:"; "and:"; "or:"; "whileTrue:"; "whileFalse:"; "to:"; "do:"; "by:"; "timesRepeat:"; "reverseDo:";
    "collect:"; "select:"; "detect:"; "ifNone:"; "inject:"; "into:" ]

let subclass_keywords = [ "subclass:"; "variableSubclass:"; "variableByteSubclass:" ]

(* a string's words: instanceVariableNames: 'bounds owner' *)
let words (quoted : string) : string list =
  let s = if String.length quoted >= 2 then String.sub quoted 1 (String.length quoted - 2) else "" in
  String.split_on_char ' ' (String.map (fun c -> if c = '\t' || c = '\n' then ' ' else c) s) |> List.filter (( <> ) "")

type result = {
  tokens : token array;
  binds : (int, int) Hashtbl.t; (* a name's token to its binding's *)
  defs : (int * string * H.space) list; (* a token, the name defined there *)
  refs : int list; (* the classes named and not defined here *)
  headers : (int * int) list; (* the lines that open a class's methods: their first and last tokens *)
}

let run (src : string) : result =
  let t = scan src in
  let n = Array.length t in
  let binds = Hashtbl.create 256 and defs = ref [] and refs = ref [] and headers = ref [] in
  (* the kinds' own categories *)
  Array.iter
    (fun tok ->
      tok.cat <-
        (match tok.kind with
        | Comment -> H.Comment
        | String | Char -> H.String
        | Hash | Symbol -> H.Constructor
        | Number -> H.Number
        | Binary | Assign -> H.Operator
        | Caret -> H.Keyword_control
        | Bang | Punct _ -> H.Punctuation
        | Keyword -> if List.mem tok.text control then H.Keyword_control else H.Normal
        | Ident -> H.Normal))
    t;
  (* the classes the file defines: their token, superclass and instance
   * variables. Found first: a method may come before its class *)
  let classes : (string, int * string * string list) Hashtbl.t = Hashtbl.create 32 in
  for i = 0 to n - 4 do
    if t.(i).kind = Ident && t.(i + 1).kind = Keyword && List.mem t.(i + 1).text subclass_keywords && t.(i + 2).kind = Hash && t.(i + 3).kind = Symbol
    then begin
      let ivars = if i + 5 < n && t.(i + 4).text = "instanceVariableNames:" && t.(i + 5).kind = String then words t.(i + 5).text else [] in
      Hashtbl.replace classes t.(i + 3).text (i + 3, t.(i).text, ivars);
      t.(i + 3).cat <- H.Def_type
    end
  done;
  Hashtbl.iter
    (fun name (i, _, _) ->
      Hashtbl.replace binds i i;
      defs := (i, name, H.Type) :: !defs)
    classes;
  (* a class's instance variables, its superclasses' in the file too *)
  let rec fields (cls : string) (depth : int) : string list =
    match Hashtbl.find_opt classes cls with Some (_, super, ivars) when depth < 50 -> ivars @ fields super (depth + 1) | _ -> []
  in
  (* a stretch of tokens that is code: a method's body, or a chunk
   * evaluated. [env]: the names bound there *)
  let code (lo : int) (hi : int) (env : (string, H.category * int) Hashtbl.t) (ivars : string list) : unit =
    let declare i cat =
      t.(i).cat <- cat;
      Hashtbl.replace env t.(i).text (cat, i);
      Hashtbl.replace binds i i
    in
    let depth = ref 0 (* inside #( ... ) *) and temps = ref true and i = ref lo in
    while !i < hi do
      let tok = t.(!i) in
      let was_temps = !temps in
      if tok.kind <> Comment then temps := false;
      (match tok.kind with
      | _ when !depth > 0 -> (
          (* a literal array: its names are symbols *)
          match tok.kind with
          | Punct '(' -> incr depth
          | Punct ')' -> decr depth
          | Ident | Keyword -> tok.cat <- H.Constructor
          | _ -> ())
      | Hash when !i + 1 < hi && t.(!i + 1).kind = Punct '(' ->
          depth := 1;
          incr i
      | Punct '[' ->
          (* a block's arguments, then its bar *)
          while !i + 2 < hi && t.(!i + 1).kind = Punct ':' && t.(!i + 2).kind = Ident do
            declare (!i + 2) H.Parameter;
            i := !i + 2
          done;
          if !i + 1 < hi && t.(!i + 1).kind = Punct '|' && t.(!i).kind = Ident then incr i;
          temps := true
      | Punct '|' when was_temps ->
          (* temporaries, to the next bar *)
          incr i;
          while !i < hi && t.(!i).kind = Ident do
            declare !i H.Local;
            incr i
          done
      | Binary when tok.text = "<" && !i + 3 < hi && t.(!i + 1).text = "primitive:" && t.(!i + 3).text = ">" ->
          for k = !i to !i + 3 do t.(k).cat <- H.Attribute done;
          i := !i + 3
      | Ident -> (
          let name = tok.text in
          match Hashtbl.find_opt env name with
          | Some (cat, b) ->
              tok.cat <- cat;
              Hashtbl.replace binds !i b
          | None ->
              if List.mem name pseudo then tok.cat <- H.Keyword
              else if name.[0] >= 'A' && name.[0] <= 'Z' then begin
                tok.cat <- H.Global;
                match Hashtbl.find_opt classes name with Some (b, _, _) -> Hashtbl.replace binds !i b | None -> refs := !i :: !refs
              end
              else if List.mem name ivars then tok.cat <- H.Field)
      | _ -> ());
      incr i
    done
  in
  (* a method: its pattern (a name; an operator and its argument;
   * keywords, each with its argument), then its body *)
  let method_ (cls : string) (meta : bool) (lo : int) (hi : int) : unit =
    let env = Hashtbl.create 16 in
    let param i =
      if i < hi && t.(i).kind = Ident then begin
        t.(i).cat <- H.Parameter;
        Hashtbl.replace env t.(i).text (H.Parameter, i);
        Hashtbl.replace binds i i
      end
    in
    let selector, body =
      if lo >= hi then ("", lo)
      else
        match t.(lo).kind with
        | Ident ->
            t.(lo).cat <- H.Def_function;
            (t.(lo).text, lo + 1)
        (* the bar is an operator too: Boolean's | *)
        | Binary | Punct '|' ->
            t.(lo).cat <- H.Def_function;
            param (lo + 1);
            (t.(lo).text, lo + 2)
        | Keyword ->
            let i = ref lo and sel = ref "" in
            while !i + 1 < hi && t.(!i).kind = Keyword && t.(!i + 1).kind = Ident do
              t.(!i).cat <- H.Def_function;
              sel := !sel ^ t.(!i).text;
              param (!i + 1);
              i := !i + 2
            done;
            (!sel, !i)
        | _ -> ("", lo)
    in
    if selector <> "" then defs := (lo, (cls ^ if meta then " class>>" else ">>") ^ selector, H.Value) :: !defs;
    code (min body hi) hi env (if meta then [] else fields cls 0)
  in
  (* the chunks, between the bangs *)
  let bangs = List.filter (fun i -> t.(i).kind = Bang) (List.init n Fun.id) in
  let bounds =
    let rec go lo = function [] -> [ (lo, n) ] | b :: rest -> (lo, b) :: go (b + 1) rest in
    go 0 bangs
  in
  let current = ref None in
  List.iter
    (fun (lo, hi) ->
      let code_tokens = List.filter (fun i -> t.(i).kind <> Comment) (List.init (hi - lo) (fun k -> lo + k)) in
      match (!current, code_tokens) with
      | Some _, [] -> current := None (* "! !": the methods' end *)
      | Some (cls, meta), _ -> method_ cls meta lo hi
      | None, _ -> (
          (* !Morph methodsFor: 'drawing'!  or  !Morph class methodsFor: ... *)
          let texts = List.map (fun i -> t.(i).text) code_tokens in
          match texts with
          | [ cls; "methodsFor:"; _ ] | [ cls; "class"; "methodsFor:"; _ ] ->
              current := Some (cls, List.length texts = 4);
              List.iter (fun i -> t.(i).cat <- H.Comment_section) code_tokens;
              headers := (List.hd code_tokens, List.nth code_tokens (List.length code_tokens - 1)) :: !headers;
              (match Hashtbl.find_opt classes cls with
              | Some (b, _, _) -> Hashtbl.replace binds (List.hd code_tokens) b
              | None -> refs := List.hd code_tokens :: !refs)
          | _ -> code lo hi (Hashtbl.create 4) []))
    bounds;
  { tokens = t; binds; defs = !defs; refs = !refs; headers = !headers }

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let categorize (src : string) : (string * H.category) list =
  Array.to_list (Array.map (fun tok -> (tok.text, tok.cat)) (run src).tokens)

(* a line that opens a class's methods is one span, as a banner is one
 * comment: a code map writes it large as a section's title *)
let spans (src : string) (r : result) : H.span list array =
  let lines = Array.of_list (String.split_on_char '\n' src) in
  let merged = Hashtbl.create 64 in
  List.iter
    (fun (first, last) ->
      let a = r.tokens.(first) and b = r.tokens.(last) in
      if a.line = b.line then begin
        for i = first + 1 to last do Hashtbl.replace merged i None done;
        Hashtbl.replace merged first (Some (String.sub lines.(a.line - 1) a.col (b.col + String.length b.text - a.col)))
      end)
    r.headers;
  H.lines src
    (List.filter_map
       (fun (i, tok) ->
         match Hashtbl.find_opt merged i with
         | Some None -> None
         | Some (Some text) -> Some (tok.line, tok.col, text, tok.cat)
         | None -> Some (tok.line, tok.col, tok.text, tok.cat))
       (List.mapi (fun i tok -> (i, tok)) (Array.to_list r.tokens)))

let lines (src : string) : H.span list array = spans src (run src)

let analyze (src : string) : H.analysis =
  let r = run src in
  let places = Array.map (fun tok -> (tok.line, tok.col, tok.text)) r.tokens in
  {
    spans = spans src r;
    occurrences = H.occurrences places r.binds;
    definitions = List.rev_map (fun (i, name, space) -> H.definition ~name places i space 3) r.defs;
    references = List.rev_map (fun i -> H.reference places i [] H.Type) r.refs;
    opens = [];
    includes = [];
  }
