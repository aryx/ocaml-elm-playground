(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Entities.mli *)

(*****************************************************************************)
(* The names *)
(*****************************************************************************)

(* HTML 2.0's Latin-1 set, in order: the characters 160 to 255 *)
let latin_1 =
  [|
    "nbsp"; "iexcl"; "cent"; "pound"; "curren"; "yen"; "brvbar"; "sect"; "uml"; "copy"; "ordf"; "laquo"; "not"; "shy";
    "reg"; "macr"; "deg"; "plusmn"; "sup2"; "sup3"; "acute"; "micro"; "para"; "middot"; "cedil"; "sup1"; "ordm"; "raquo";
    "frac14"; "frac12"; "frac34"; "iquest"; "Agrave"; "Aacute"; "Acirc"; "Atilde"; "Auml"; "Aring"; "AElig"; "Ccedil";
    "Egrave"; "Eacute"; "Ecirc"; "Euml"; "Igrave"; "Iacute"; "Icirc"; "Iuml"; "ETH"; "Ntilde"; "Ograve"; "Oacute";
    "Ocirc"; "Otilde"; "Ouml"; "times"; "Oslash"; "Ugrave"; "Uacute"; "Ucirc"; "Uuml"; "Yacute"; "THORN"; "szlig";
    "agrave"; "aacute"; "acirc"; "atilde"; "auml"; "aring"; "aelig"; "ccedil"; "egrave"; "eacute"; "ecirc"; "euml";
    "igrave"; "iacute"; "icirc"; "iuml"; "eth"; "ntilde"; "ograve"; "oacute"; "ocirc"; "otilde"; "ouml"; "divide";
    "oslash"; "ugrave"; "uacute"; "ucirc"; "uuml"; "yacute"; "thorn"; "yuml";
  |]

(* HTML 4's special set (32), then its symbols (124), and XHTML's apos *)
let others =
  [
    (* special *)
    ("quot", 34); ("amp", 38); ("lt", 60); ("gt", 62); ("OElig", 338); ("oelig", 339); ("Scaron", 352); ("scaron", 353);
    ("Yuml", 376); ("circ", 710); ("tilde", 732); ("ensp", 8194); ("emsp", 8195); ("thinsp", 8201); ("zwnj", 8204);
    ("zwj", 8205); ("lrm", 8206); ("rlm", 8207); ("ndash", 8211); ("mdash", 8212); ("lsquo", 8216); ("rsquo", 8217);
    ("sbquo", 8218); ("ldquo", 8220); ("rdquo", 8221); ("bdquo", 8222); ("dagger", 8224); ("Dagger", 8225);
    ("permil", 8240); ("lsaquo", 8249); ("rsaquo", 8250); ("euro", 8364);
    (* symbols: Latin, Greek *)
    ("fnof", 402); ("Alpha", 913); ("Beta", 914); ("Gamma", 915); ("Delta", 916); ("Epsilon", 917); ("Zeta", 918);
    ("Eta", 919); ("Theta", 920); ("Iota", 921); ("Kappa", 922); ("Lambda", 923); ("Mu", 924); ("Nu", 925); ("Xi", 926);
    ("Omicron", 927); ("Pi", 928); ("Rho", 929); ("Sigma", 931); ("Tau", 932); ("Upsilon", 933); ("Phi", 934);
    ("Chi", 935); ("Psi", 936); ("Omega", 937); ("alpha", 945); ("beta", 946); ("gamma", 947); ("delta", 948);
    ("epsilon", 949); ("zeta", 950); ("eta", 951); ("theta", 952); ("iota", 953); ("kappa", 954); ("lambda", 955);
    ("mu", 956); ("nu", 957); ("xi", 958); ("omicron", 959); ("pi", 960); ("rho", 961); ("sigmaf", 962); ("sigma", 963);
    ("tau", 964); ("upsilon", 965); ("phi", 966); ("chi", 967); ("psi", 968); ("omega", 969); ("thetasym", 977);
    ("upsih", 978); ("piv", 982);
    (* punctuation, letterlike symbols, arrows *)
    ("bull", 8226); ("hellip", 8230); ("prime", 8242); ("Prime", 8243); ("oline", 8254); ("frasl", 8260);
    ("weierp", 8472); ("image", 8465); ("real", 8476); ("trade", 8482); ("alefsym", 8501); ("larr", 8592);
    ("uarr", 8593); ("rarr", 8594); ("darr", 8595); ("harr", 8596); ("crarr", 8629); ("lArr", 8656); ("uArr", 8657);
    ("rArr", 8658); ("dArr", 8659); ("hArr", 8660);
    (* mathematics *)
    ("forall", 8704); ("part", 8706); ("exist", 8707); ("empty", 8709); ("nabla", 8711); ("isin", 8712);
    ("notin", 8713); ("ni", 8715); ("prod", 8719); ("sum", 8721); ("minus", 8722); ("lowast", 8727); ("radic", 8730);
    ("prop", 8733); ("infin", 8734); ("ang", 8736); ("and", 8743); ("or", 8744); ("cap", 8745); ("cup", 8746);
    ("int", 8747); ("there4", 8756); ("sim", 8764); ("cong", 8773); ("asymp", 8776); ("ne", 8800); ("equiv", 8801);
    ("le", 8804); ("ge", 8805); ("sub", 8834); ("sup", 8835); ("nsub", 8836); ("sube", 8838); ("supe", 8839);
    ("oplus", 8853); ("otimes", 8855); ("perp", 8869); ("sdot", 8901);
    (* technical, shapes, cards *)
    ("lceil", 8968); ("rceil", 8969); ("lfloor", 8970); ("rfloor", 8971); ("lang", 9001); ("rang", 9002); ("loz", 9674);
    ("spades", 9824); ("clubs", 9827); ("hearts", 9829); ("diams", 9830);
    (* XHTML 1.0 *)
    ("apos", 39);
  ]

let table : (string, int) Hashtbl.t =
  let h = Hashtbl.create 256 in
  Array.iteri (fun i name -> Hashtbl.replace h name (160 + i)) latin_1;
  List.iter (fun (name, code) -> Hashtbl.replace h name code) others;
  h

let count = Hashtbl.length table

(*****************************************************************************)
(* Characters *)
(*****************************************************************************)

let utf_8 (code : int) : string =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int code);
  Buffer.contents b

let lookup (name : string) : string option = Option.map utf_8 (Hashtbl.find_opt table name)

let of_code (code : int) : string =
  if code >= 0x80 && code <= 0x9F then utf_8 (Charset.windows_1252 code)
  else if Uchar.is_valid code && code <> 0 then utf_8 code
  else utf_8 0xFFFD

(*****************************************************************************)
(* A text *)
(*****************************************************************************)

let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_alnum c = is_digit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

(* the longest run from [i] of characters satisfying [p] *)
let span (s : string) (i : int) (p : char -> bool) : int =
  let j = ref i in
  while !j < String.length s && p s.[!j] do incr j done;
  !j

(* the reference at [s.[i] = '&']: its character and where the text
 * goes on, or None when it is not one *)
let reference (s : string) (i : int) : (string * int) option =
  let n = String.length s in
  let semicolon j = if j < n && s.[j] = ';' then j + 1 else j in
  if i + 1 < n && s.[i + 1] = '#' then
    (* a number, its ";" optional (the WHATWG's parse error, forgiven) *)
    let hex = i + 2 < n && (s.[i + 2] = 'x' || s.[i + 2] = 'X') in
    let start = if hex then i + 3 else i + 2 in
    let stop = span s start (if hex then is_hex else is_digit) in
    if stop = start || stop - start > 8 then None
    else
      let code = int_of_string ((if hex then "0x" else "") ^ String.sub s start (stop - start)) in
      Some (of_code code, semicolon stop)
  else
    let stop = span s (i + 1) is_alnum in
    if stop < n && s.[stop] = ';' then
      match lookup (String.sub s (i + 1) (stop - i - 1)) with Some c -> Some (c, stop + 1) | None -> None
    else None

let decode (s : string) : string =
  if not (String.contains s '&') then s
  else
    let b = Buffer.create (String.length s) in
    let rec go (i : int) =
      match String.index_from_opt s i '&' with
      | None -> Buffer.add_substring b s i (String.length s - i)
      | Some j -> (
          Buffer.add_substring b s i (j - i);
          match reference s j with
          | Some (c, next) ->
              Buffer.add_string b c;
              go next
          | None ->
              Buffer.add_char b '&';
              go (j + 1))
    in
    go 0;
    Buffer.contents b
