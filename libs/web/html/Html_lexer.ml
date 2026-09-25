(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Html_lexer.mli *)

type attribute = string * string

type token =
  | Doctype of string
  | Start_tag of {
      name : string;
      attributes : attribute list;
      extensions : attribute list;
      origin : Dtd.origin;
      self_closing : bool;
    }
  | End_tag of string
  | Text of string
  | Comment of string

(*****************************************************************************)
(* The machine's memory *)
(*****************************************************************************)

(* the page, the tokens so far (the last first), and the text read since
 * the last token, raw (its entities decoded when it is emitted) *)
type t = { s : string; n : int; mutable tokens : token list; text : Buffer.t }

(* a tag being read *)
type tag = { end_tag : bool; name : string; mutable attributes : attribute list; mutable self_closing : bool }

let emit_text ?(decode = true) (t : t) : unit =
  if Buffer.length t.text > 0 then (
    let raw = Buffer.contents t.text in
    t.tokens <- Text (if decode then Entities.decode raw else raw) :: t.tokens;
    Buffer.clear t.text)

let emit (t : t) (token : token) : unit =
  emit_text t;
  t.tokens <- token :: t.tokens

let peek (t : t) (i : int) : char option = if i < t.n then Some t.s.[i] else None
let is_letter (c : char) : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_space (c : char) : bool = c = ' ' || c = '\n' || c = '\t' || c = '\012'

(* the index of the first character from [i] satisfying [p], or n *)
let until (t : t) (i : int) (p : char -> bool) : int =
  let j = ref i in
  while !j < t.n && not (p t.s.[!j]) do incr j done;
  !j

let skip_spaces (t : t) (i : int) : int = until t i (fun c -> not (is_space c))

(* the elements whose content is not HTML: no tags in it until their end
 * tag; RAWTEXT keeps the entities too, RCDATA decodes them *)
let rawtext = [ "script"; "style" ]
let rcdata = [ "title"; "textarea" ]

(* [prefix] at [i], ignoring case *)
let at (t : t) (i : int) (prefix : string) : bool =
  let n = String.length prefix in
  i + n <= t.n && String.lowercase_ascii (String.sub t.s i n) = prefix

(*****************************************************************************)
(* The states *)
(*****************************************************************************)

(* each state is a function of where the machine is reading; a
 * transition is a tail call *)

let rec data (t : t) (i : int) : unit =
  if i >= t.n then emit_text t
  else if t.s.[i] = '<' then tag_open t i
  else (
    Buffer.add_char t.text t.s.[i];
    data t (i + 1))

(* at a '<' *)
and tag_open (t : t) (i : int) : unit =
  match peek t (i + 1) with
  | Some c when is_letter c -> tag_name t ~end_tag:false (i + 1)
  | Some '/' -> end_tag_open t (i + 2)
  | Some '!' -> markup_declaration t (i + 2)
  | Some '?' -> bogus_comment t (i + 1)
  | _ ->
      (* a '<' that starts nothing: text *)
      Buffer.add_char t.text '<';
      data t (i + 1)

(* after "</" *)
and end_tag_open (t : t) (i : int) : unit =
  match peek t i with
  | Some c when is_letter c -> tag_name t ~end_tag:true i
  | Some '>' -> (* "</>": nothing *) data t (i + 1)
  | None ->
      Buffer.add_string t.text "</";
      emit_text t
  | Some _ -> bogus_comment t i

and tag_name (t : t) ~(end_tag : bool) (i : int) : unit =
  let stop = until t i (fun c -> is_space c || c = '/' || c = '>') in
  let tag = { end_tag; name = String.lowercase_ascii (String.sub t.s i (stop - i)); attributes = []; self_closing = false } in
  before_attribute_name t tag stop

and before_attribute_name (t : t) (tag : tag) (i : int) : unit =
  let i = skip_spaces t i in
  match peek t i with
  | None -> (* cut off by the end: dropped *) emit_text t
  | Some '>' -> emit_tag t tag (i + 1)
  | Some '/' -> self_closing_start_tag t tag i
  | Some _ -> attribute_name t tag i

and attribute_name (t : t) (tag : tag) (i : int) : unit =
  (* a '=' first is part of the name, as the spec says *)
  let stop = until t (i + 1) (fun c -> is_space c || c = '/' || c = '>' || c = '=') in
  let name = String.lowercase_ascii (String.sub t.s i (stop - i)) in
  after_attribute_name t tag name stop

and after_attribute_name (t : t) (tag : tag) (name : string) (i : int) : unit =
  let i = skip_spaces t i in
  match peek t i with
  | Some '=' -> before_attribute_value t tag name (i + 1)
  | _ ->
      (* no value: <hr noshade> *)
      add_attribute tag name "";
      before_attribute_name t tag i

and before_attribute_value (t : t) (tag : tag) (name : string) (i : int) : unit =
  let i = skip_spaces t i in
  match peek t i with
  | Some (('"' | '\'') as quote) -> (
      let stop = until t (i + 1) (fun c -> c = quote) in
      match peek t stop with
      | None -> emit_text t (* cut off: dropped *)
      | Some _ ->
          add_attribute tag name (Entities.decode (String.sub t.s (i + 1) (stop - i - 1)));
          before_attribute_name t tag (stop + 1))
  | Some '>' ->
      add_attribute tag name "";
      emit_tag t tag (i + 1)
  | _ ->
      (* unquoted: up to a space or '>' *)
      let stop = until t i (fun c -> is_space c || c = '>') in
      add_attribute tag name (Entities.decode (String.sub t.s i (stop - i)));
      before_attribute_name t tag stop

(* at a '/' in a tag: "/>" ends a self-closing one, another '/' is
 * nothing *)
and self_closing_start_tag (t : t) (tag : tag) (i : int) : unit =
  match peek t (i + 1) with
  | Some '>' ->
      tag.self_closing <- true;
      emit_tag t tag (i + 2)
  | _ -> before_attribute_name t tag (i + 1)

(* the tag read, [i] after its '>'; the elements whose content is not
 * HTML switch the machine to reading it as text *)
and emit_tag (t : t) (tag : tag) (i : int) : unit =
  if tag.end_tag then (
    emit t (End_tag tag.name);
    data t i)
  else (
    (* claude: the names' origins (Dtd): a Netscape element keeps all
     * its attributes; a core one's Netscape attributes go apart *)
    let origin = Dtd.element_origin tag.name in
    let attributes, extensions =
      match origin with
      | Netscape -> (List.rev tag.attributes, [])
      | Core -> List.partition (fun a -> Dtd.attribute_origin tag.name a = Core) (List.rev tag.attributes)
    in
    emit t (Start_tag { name = tag.name; attributes; extensions; origin; self_closing = tag.self_closing });
    if List.mem tag.name rawtext then raw_text t tag.name ~decode:false i
    else if List.mem tag.name rcdata then raw_text t tag.name ~decode:true i
    else data t i)

(* RAWTEXT and RCDATA: text up to "</name" followed by a space, '/' or
 * '>', ignoring case *)
and raw_text (t : t) (name : string) ~(decode : bool) (i : int) : unit =
  let rec find j =
    if j >= t.n then None
    else if
      t.s.[j] = '<'
      && at t (j + 1) ("/" ^ name)
      && match peek t (j + 2 + String.length name) with Some c -> is_space c || c = '/' || c = '>' | None -> false
    then Some j
    else find (j + 1)
  in
  match find i with
  | Some j ->
      Buffer.add_substring t.text t.s i (j - i);
      emit_text ~decode t;
      end_tag_open t (j + 2)
  | None ->
      Buffer.add_substring t.text t.s i (t.n - i);
      emit_text ~decode t

(* after "<!" *)
and markup_declaration (t : t) (i : int) : unit =
  if at t i "--" then comment t (i + 2)
  else if at t i "doctype" then doctype t (i + 7)
  else bogus_comment t i

(* after "<!--" *)
and comment (t : t) (i : int) : unit =
  if at t i ">" then (
    emit t (Comment "");
    data t (i + 1))
  else if at t i "->" then (
    emit t (Comment "");
    data t (i + 2))
  else
    let rec close j = if j + 3 > t.n then None else if at t j "-->" then Some j else close (j + 1) in
    match close i with
    | Some j ->
        emit t (Comment (String.sub t.s i (j - i)));
        data t (j + 3)
    | None -> emit t (Comment (String.sub t.s i (t.n - i)))

and doctype (t : t) (i : int) : unit =
  let stop = until t i (fun c -> c = '>') in
  emit t (Doctype (String.trim (String.sub t.s i (stop - i))));
  data t (stop + 1)

(* "<?...>", "<!...>": up to the '>', kept as a comment *)
and bogus_comment (t : t) (i : int) : unit =
  let stop = until t i (fun c -> c = '>') in
  emit t (Comment (String.sub t.s i (stop - i)));
  data t (stop + 1)

(* a name given twice keeps its first value *)
and add_attribute (tag : tag) (name : string) (value : string) : unit =
  if not (List.mem_assoc name tag.attributes) then tag.attributes <- (name, value) :: tag.attributes

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* CR LF and a lone CR as LF: the spec's preprocessing of the input *)
let normalize_newlines (s : string) : string =
  if not (String.contains s '\r') then s
  else
    let b = Buffer.create (String.length s) in
    String.iteri
      (fun i c ->
        if c = '\r' then (if not (i + 1 < String.length s && s.[i + 1] = '\n') then Buffer.add_char b '\n')
        else Buffer.add_char b c)
      s;
    Buffer.contents b

let tokenize (text : string) : token list =
  let s = normalize_newlines text in
  let t = { s; n = String.length s; tokens = []; text = Buffer.create 256 } in
  data t 0;
  List.rev t.tokens

let attribute (name : string) (attributes : attribute list) : string option = List.assoc_opt name attributes

(* a string between double quotes, escaped as the notes write it: only
 * the quote, the backslash and the control characters (UTF-8 kept) *)
let quote (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let to_string (token : token) : string =
  match token with
  | Doctype d -> "Doctype " ^ quote d
  | Start_tag { name; attributes; extensions; origin; self_closing } ->
      let list attributes = String.concat "; " (List.map (fun (n, v) -> n ^ " = " ^ quote v) attributes) in
      Printf.sprintf "Start_tag %s [%s]%s%s" (quote name) (list attributes)
        (match (origin, extensions) with
        | Netscape, _ -> " {Netscape}"
        | Core, [] -> ""
        | Core, extensions -> " {Netscape: " ^ list extensions ^ "}")
        (if self_closing then " /" else "")
  | End_tag name -> "End_tag " ^ quote name
  | Text s -> "Text " ^ quote s
  | Comment s -> "Comment " ^ quote s
