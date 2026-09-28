(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mime.mli *)

(*****************************************************************************)
(* Content types *)
(*****************************************************************************)

(* cut at the semicolons that are not in quotes *)
let split_params (s : string) : string list =
  let parts = ref [] and start = ref 0 and quoted = ref false in
  String.iteri
    (fun i c ->
      if c = '"' then quoted := not !quoted
      else if c = ';' && not !quoted then (
        parts := String.sub s !start (i - !start) :: !parts;
        start := i + 1))
    s;
  List.rev (String.sub s !start (String.length s - !start) :: !parts)

let parameters (s : string) : string * (string * string) list =
  match split_params s with
  | [] -> ("", [])
  | kind :: params ->
      ( String.lowercase_ascii (String.trim kind),
        List.filter_map
          (fun p ->
            match String.index_opt p '=' with
            | Some i -> Some (String.lowercase_ascii (String.trim (String.sub p 0 i)), Mail.unquote (String.sub p (i + 1) (String.length p - i - 1)))
            | None -> None)
          params )

let content_type ?(digest = false) (m : Mail.t) : string * (string * string) list =
  match Mail.get m "content-type" with Some v -> parameters v | None -> ((if digest then "message/rfc822" else "text/plain"), [])

let starts (prefix : string) (s : string) : bool = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(*****************************************************************************)
(* Multipart *)
(*****************************************************************************)

(* The body cut at the lines "--boundary": what is before the first is
 * the preamble (for the mail readers of 1992, which did not know MIME:
 * "This is a multi-part message in MIME format."), what is after
 * "--boundary--" the epilogue, both skipped. The line break before a
 * boundary line belongs to it, not to the part. *)
let parts (m : Mail.t) : Mail.t list =
  let kind, params = content_type m in
  match List.assoc_opt "boundary" params with
  | Some b when starts "multipart/" kind ->
      let delimiter = "--" ^ b and close = "--" ^ b ^ "--" in
      let finish current acc = match current with Some lines -> Mail.parse (String.concat "\n" (List.rev lines)) :: acc | None -> acc in
      let rec go lines current acc =
        match lines with
        | [] -> List.rev (finish current acc)
        | l :: rest ->
            let l' = String.trim l in
            if l' = close then List.rev (finish current acc)
            else if l' = delimiter then go rest (Some []) (finish current acc)
            else go rest (Option.map (fun ls -> l :: ls) current) acc
      in
      go (String.split_on_char '\n' m.body) None []
  | _ -> []

(*****************************************************************************)
(* The encodings *)
(*****************************************************************************)

let hex (c : char) : int option =
  match c with '0' .. '9' -> Some (Char.code c - 48) | 'A' .. 'F' -> Some (Char.code c - 55) | 'a' .. 'f' -> Some (Char.code c - 87) | _ -> None

(* [q]: the header's Q encoding, where "_" is a space *)
let qp_decode ~(q : bool) (s : string) : string =
  let b = Buffer.create (String.length s) and n = String.length s in
  let rec go i =
    if i < n then
      match s.[i] with
      | '=' when i + 1 < n && s.[i + 1] = '\n' -> go (i + 2) (* a soft line break *)
      | '=' when i + 2 < n && hex s.[i + 1] <> None && hex s.[i + 2] <> None ->
          Buffer.add_char b (Char.chr ((Option.get (hex s.[i + 1]) * 16) + Option.get (hex s.[i + 2])));
          go (i + 3)
      | '_' when q ->
          Buffer.add_char b ' ';
          go (i + 1)
      | c ->
          Buffer.add_char b c;
          go (i + 1)
  in
  go 0;
  Buffer.contents b

let quoted_printable_decode = qp_decode ~q:false

let quoted_printable_encode (s : string) : string =
  let b = Buffer.create (String.length s) and col = ref 0 and n = String.length s in
  String.iteri
    (fun i c ->
      if c = '\n' then (
        Buffer.add_char b '\n';
        col := 0)
      else
        let at_end = i + 1 = n || s.[i + 1] = '\n' in
        (* a space at a line's end would be eaten by a mail server: encoded *)
        let token =
          if (c >= '!' && c <= '~' && c <> '=') || ((c = ' ' || c = '\t') && not at_end) then String.make 1 c else Printf.sprintf "=%02X" (Char.code c)
        in
        if !col + String.length token > 75 then (
          Buffer.add_string b "=\n";
          col := 0);
        Buffer.add_string b token;
        col := !col + String.length token)
    s;
  Buffer.contents b

let decoded (m : Mail.t) : string =
  match Option.map String.lowercase_ascii (Mail.get m "content-transfer-encoding") with
  | Some "base64" -> Base64.decode m.body
  | Some "quoted-printable" -> quoted_printable_decode m.body
  | _ -> m.body

(* ISO-8859-1's 256 characters are Unicode's first 256: a byte above
 * 127 is two bytes of UTF-8 *)
let to_utf8 (charset : string) (s : string) : string =
  match String.lowercase_ascii charset with
  | "iso-8859-1" | "latin1" | "iso-8859-15" | "windows-1252" ->
      let b = Buffer.create (String.length s) in
      String.iter
        (fun c ->
          let k = Char.code c in
          if k < 128 then Buffer.add_char b c
          else (
            Buffer.add_char b (Char.chr (0xC0 lor (k lsr 6)));
            Buffer.add_char b (Char.chr (0x80 lor (k land 0x3F)))))
        s;
      Buffer.contents b
  | _ -> s

(*****************************************************************************)
(* Encoded words *)
(*****************************************************************************)

(* "=?charset?Q?text?=" (or ?B?) decoded, if [s] is one *)
let word (s : string) : string option =
  let n = String.length s in
  if n > 6 && starts "=?" s && String.sub s (n - 2) 2 = "?=" then
    match String.split_on_char '?' (String.sub s 2 (n - 4)) with
    | [ charset; enc; text ] -> (
        match String.lowercase_ascii enc with
        | "q" -> Some (to_utf8 charset (qp_decode ~q:true text))
        | "b" -> Some (to_utf8 charset (Base64.decode text))
        | _ -> None)
    | _ -> None
  else None

(* word by word: the space between two encoded words is dropped
 * (RFC 2047, 6.2), so that a long word can be cut in two *)
let decode_words (s : string) : string =
  let words = String.split_on_char ' ' s in
  let rec go prev_encoded = function
    | [] -> []
    | w :: rest -> (
        match word w with
        | Some d -> (if prev_encoded then d else " " ^ d) :: go true rest
        | None -> (" " ^ w) :: go false rest)
  in
  let s' = String.concat "" (go false words) in
  if s' = "" then s' else String.sub s' 1 (String.length s' - 1)

let encode_words (s : string) : string =
  if String.for_all (fun c -> Char.code c < 128) s then s
  else
    let b = Buffer.create (String.length s * 2) in
    String.iter
      (fun c ->
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | ',' | '-' | '!' -> Buffer.add_char b c
        | ' ' -> Buffer.add_char b '_'
        | c -> Buffer.add_string b (Printf.sprintf "=%02X" (Char.code c)))
      s;
    "=?utf-8?Q?" ^ Buffer.contents b ^ "?="

(*****************************************************************************)
(* The leaves *)
(*****************************************************************************)

type leaf = { mime : string; filename : string option; data : string; part : Mail.t }

let rec leaves_of ~(digest : bool) (m : Mail.t) : leaf list =
  let kind, params = content_type ~digest m in
  if starts "multipart/" kind then List.concat_map (leaves_of ~digest:(kind = "multipart/digest")) (parts m)
  else
    let disposition = Option.map parameters (Mail.get m "content-disposition") in
    let filename =
      match Option.bind disposition (fun (_, ps) -> List.assoc_opt "filename" ps) with
      | Some f -> Some (decode_words f)
      | None -> Option.map decode_words (List.assoc_opt "name" params)
    in
    let data = decoded m in
    let data = if starts "text/" kind then to_utf8 (Option.value (List.assoc_opt "charset" params) ~default:"us-ascii") data else data in
    [ { mime = kind; filename; data; part = m } ]

let leaves (m : Mail.t) : leaf list = leaves_of ~digest:false m

let is_attachment (l : leaf) : bool =
  l.filename <> None
  || (match Mail.get l.part "content-disposition" with Some d -> fst (parameters d) = "attachment" | None -> false)
  || not (starts "text/" l.mime || l.mime = "message/rfc822")

let rec text (m : Mail.t) : string =
  let readable = List.filter (fun l -> not (is_attachment l)) (leaves m) in
  (* multipart/alternative's HTML is read only when there is no plain text *)
  let plain = List.filter (fun l -> l.mime = "text/plain" || l.mime = "message/rfc822") readable in
  let chosen = if plain <> [] then plain else readable in
  String.concat "\n"
    (List.map
       (fun l ->
         if l.mime = "message/rfc822" then
           let inner = Mail.parse l.data in
           let h name = decode_words (Option.value (Mail.get inner name) ~default:"") in
           Printf.sprintf "----- From: %s\n----- Subject: %s\n\n%s" (h "from") (h "subject") (text inner)
         else l.data)
       chosen)

let attachments (m : Mail.t) : leaf list = List.filter is_attachment (leaves m)

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let text_part (s : string) : Mail.t =
  if String.for_all (fun c -> Char.code c < 128) s then Mail.make [ ("Content-Type", "text/plain; charset=us-ascii") ] s
  else Mail.make [ ("Content-Type", "text/plain; charset=utf-8"); ("Content-Transfer-Encoding", "quoted-printable") ] (quoted_printable_encode s)

let type_of_filename (name : string) : string =
  let ext = match String.rindex_opt name '.' with Some i -> String.lowercase_ascii (String.sub name (i + 1) (String.length name - i - 1)) | None -> "" in
  match ext with
  | "png" -> "image/png"
  | "gif" -> "image/gif"
  | "jpg" | "jpeg" -> "image/jpeg"
  | "txt" -> "text/plain"
  | "mbox" -> "application/mbox"
  | _ -> "application/octet-stream"

(* base64 in lines of 76, as RFC 2045 wants them *)
let base64_lines (s : string) : string =
  let e = Base64.encode s in
  let n = String.length e in
  String.concat "\n" (List.init ((n + 75) / 76) (fun i -> String.sub e (i * 76) (min 76 (n - (i * 76))))) ^ "\n"

let attachment ~(filename : string) (data : string) : Mail.t =
  Mail.make
    [
      ("Content-Type", Printf.sprintf "%s; name=\"%s\"" (type_of_filename filename) filename);
      ("Content-Transfer-Encoding", "base64");
      ("Content-Disposition", Printf.sprintf "attachment; filename=\"%s\"" filename);
    ]
    (base64_lines data)

let multipart ~(boundary : string) (parts : Mail.t list) : (string * string) list * string =
  let chomp s = if s <> "" && s.[String.length s - 1] = '\n' then String.sub s 0 (String.length s - 1) else s in
  ( [ ("MIME-Version", "1.0"); ("Content-Type", Printf.sprintf "multipart/mixed; boundary=\"%s\"" boundary) ],
    "This is a multi-part message in MIME format.\n"
    ^ String.concat "" (List.map (fun p -> "--" ^ boundary ^ "\n" ^ chomp (Mail.to_string p) ^ "\n") parts)
    ^ "--" ^ boundary ^ "--\n" )
