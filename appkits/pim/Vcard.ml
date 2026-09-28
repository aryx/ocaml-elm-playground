(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Vcard.mli *)

type name = { family : string; given : string; additional : string; prefix : string; suffix : string }
type phone = { number : string; kinds : string list }
type email = { address : string; kinds : string list }

type address = {
  street : string;
  locality : string;
  region : string;
  code : string;
  country : string;
  kinds : string list;
}

type card = {
  uid : string;
  name : name;
  full_name : string;
  org : string;
  title : string;
  phones : phone list;
  emails : email list;
  addresses : address list;
  birthday : Civil.date option;
  note : string;
}

let no_name = { family = ""; given = ""; additional = ""; prefix = ""; suffix = "" }

let make (full_name : string) : card =
  { uid = ""; name = no_name; full_name; org = ""; title = ""; phones = []; emails = []; addresses = [];
    birthday = None; note = "" }

(*****************************************************************************)
(* Values *)
(*****************************************************************************)

let structured (v : string) : string list =
  (* cut at each ';' not escaped, skipping over the escapes *)
  let n = String.length v in
  let rec go i start acc =
    if i >= n then List.rev (String.sub v start (n - start) :: acc)
    else if v.[i] = '\\' then go (i + 2) start acc
    else if v.[i] = ';' then go (i + 1) (i + 1) (String.sub v start (i - start) :: acc)
    else go (i + 1) start acc
  in
  go 0 0 [] |> List.map (fun s -> String.trim (Ics.unescape s))

(* the part [i] of a structured value, "" if it has fewer *)
let part (parts : string list) (i : int) : string = Option.value ~default:"" (List.nth_opt parts i)

let kinds (params : (string * string) list) : string list =
  List.concat_map
    (fun (p, v) -> if p = "TYPE" then String.split_on_char ',' v |> List.map String.lowercase_ascii else [])
    params

(* "1815-12-10" or "18151210", a time after a T ignored *)
let birthday (v : string) : Civil.date option =
  let v = match String.index_opt v 'T' with Some i -> String.sub v 0 i | None -> v in
  let digits = String.concat "" (String.split_on_char '-' v) in
  if String.length digits <> 8 then None
  else
    match
      (int_of_string_opt (String.sub digits 0 4), int_of_string_opt (String.sub digits 4 2),
       int_of_string_opt (String.sub digits 6 2))
    with
    | Some year, Some month, Some day ->
        let d : Civil.date = { year; month; day } in
        if Civil.is_valid d then Some d else None
    | _ -> None

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

(* a property read into the card *)
let property (c : card) ((name, params, v) : string * (string * string) list * string) : card =
  match name with
  | "UID" -> { c with uid = Ics.unescape v }
  | "FN" -> { c with full_name = Ics.unescape v }
  | "N" ->
      let p = structured v in
      { c with name = { family = part p 0; given = part p 1; additional = part p 2; prefix = part p 3; suffix = part p 4 } }
  | "ORG" -> { c with org = String.concat ", " (List.filter (fun s -> s <> "") (structured v)) }
  | "TITLE" -> { c with title = Ics.unescape v }
  | "TEL" -> { c with phones = c.phones @ [ { number = Ics.unescape v; kinds = kinds params } ] }
  | "EMAIL" -> { c with emails = c.emails @ [ { address = Ics.unescape v; kinds = kinds params } ] }
  | "ADR" ->
      let p = structured v in
      { c with
        addresses =
          c.addresses
          @ [ { street = part p 2; locality = part p 3; region = part p 4; code = part p 5; country = part p 6;
                kinds = kinds params } ] }
  | "BDAY" -> { c with birthday = birthday v }
  | "NOTE" -> { c with note = Ics.unescape v }
  | _ -> c

let of_string (s : string) : card list =
  let lines = Ics.unfold s |> List.filter_map Ics.content_line in
  let is name want v = name = want && String.uppercase_ascii v = "VCARD" in
  (* [inside]: the card being read, if between BEGIN and END *)
  List.fold_left
    (fun (cards, inside) ((name, _, v) as p) ->
      match inside with
      | None -> if is name "BEGIN" v then (cards, Some (make "")) else (cards, None)
      | Some c -> if is name "END" v then (c :: cards, None) else (cards, Some (property c p)))
    ([], None) lines
  |> fst |> List.rev

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let to_string (cards : card list) : string =
  let line name value = [ name ^ ":" ^ value ] in
  let text name s = if s = "" then [] else line name (Ics.escape s) in
  let typed name (ks : string list) value =
    line (if ks = [] then name else name ^ ";TYPE=" ^ String.concat "," ks) value
  in
  let parts l = String.concat ";" (List.map Ics.escape l) in
  let card (c : card) =
    List.concat
      [ line "BEGIN" "VCARD"; line "VERSION" "3.0"; text "UID" c.uid;
        (* both required by 3.0 *)
        line "N" (parts [ c.name.family; c.name.given; c.name.additional; c.name.prefix; c.name.suffix ]);
        line "FN" (Ics.escape (if c.full_name = "" then String.trim (c.name.given ^ " " ^ c.name.family) else c.full_name));
        text "ORG" c.org; text "TITLE" c.title;
        List.concat_map (fun (p : phone) -> typed "TEL" p.kinds (Ics.escape p.number)) c.phones;
        List.concat_map (fun (e : email) -> typed "EMAIL" e.kinds (Ics.escape e.address)) c.emails;
        List.concat_map
          (fun (a : address) -> typed "ADR" a.kinds (parts [ ""; ""; a.street; a.locality; a.region; a.code; a.country ]))
          c.addresses;
        (match c.birthday with Some d -> line "BDAY" (Civil.to_string d) | None -> []);
        text "NOTE" c.note; line "END" "VCARD" ]
  in
  List.concat_map card cards |> List.map Ics.fold |> List.map (fun l -> l ^ "\r\n") |> String.concat ""

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

let display_name (c : card) : string =
  if c.full_name <> "" then c.full_name else String.trim (c.name.given ^ " " ^ c.name.family)

let compare_by_name (a : card) (b : card) : int =
  let key (c : card) =
    String.lowercase_ascii (if c.name.family <> "" then c.name.family ^ "\000" ^ c.name.given else display_name c)
  in
  compare (key a) (key b)
