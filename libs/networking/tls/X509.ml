(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See X509.mli *)

type public_key = Rsa of Bignum.t * Bignum.t | Ec of Ecdsa.curve * string | Other of string

type t = {
  der : string;
  tbs : string;
  issuer : string;
  subject : string;
  common_name : string;
  not_before : float;
  not_after : float;
  key : public_key;
  algorithm : string;
  signature : string;
  names : string list;
  ca : bool;
}

let ( let* ) = Result.bind
let one (s : string) : (Asn1.t, string) result = Result.map fst (Asn1.parse s 0)

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

(* a Name's CommonName (2.5.4.3), if it has one *)
let common_name (name : Asn1.t) : string =
  List.concat_map Asn1.children (Asn1.children name) (* the SETs of attributes *)
  |> List.find_map (fun attr -> match Asn1.children attr with [ oid; v ] when Asn1.oid oid = "2.5.4.3" -> Some (Asn1.text v) | _ -> None)
  |> Option.value ~default:""

let public_key (spki : Asn1.t) : public_key =
  match Asn1.children spki with
  | [ alg; key ] -> (
      match Asn1.children alg with
      | oid :: params -> (
          match (Asn1.oid oid, params) with
          | "1.2.840.113549.1.1.1", _ -> (
              match Result.map Asn1.children (one (Asn1.bit_string key)) with
              | Ok [ n; e ] -> Rsa (Asn1.integer n, Asn1.integer e)
              | _ -> Other "bad RSA key")
          | "1.2.840.10045.2.1", [ curve ] -> (
              match Asn1.oid curve with
              | "1.2.840.10045.3.1.7" -> Ec (Ecdsa.p256, Asn1.bit_string key)
              | "1.3.132.0.34" -> Ec (Ecdsa.p384, Asn1.bit_string key)
              | other -> Other other)
          | other, _ -> Other other)
      | [] -> Other "")
  | _ -> Other ""

(* the extensions we read: subjectAltName's names, basic constraints *)
let extensions (exts : Asn1.t list) : string list * bool =
  List.fold_left
    (fun (names, ca) ext ->
      match Asn1.children ext with
      | oid :: rest -> (
          let value = match List.rev rest with v :: _ -> Asn1.value_of v | [] -> "" in
          match Asn1.oid oid with
          | "2.5.29.17" -> (
              match one value with
              | Ok seq -> (names @ List.filter_map (fun (g : Asn1.t) -> if g.tag = 0x82 then Some g.value else None) (Asn1.children seq), ca)
              | Error _ -> (names, ca))
          | "2.5.29.19" -> (
              match Result.map Asn1.children (one value) with
              | Ok ((b : Asn1.t) :: _) when b.tag = 0x01 -> (names, b.value <> "\000")
              | _ -> (names, ca))
          | _ -> (names, ca))
      | [] -> (names, ca))
    ([], false) exts

let parse (der : string) : (t, string) result =
  let* cert = one der in
  match Asn1.children cert with
  | [ tbs; alg; sigv ] -> (
      let fields = Asn1.children tbs in
      (* the version, [0], is optional (v1): skip it if there *)
      let fields = match fields with (v : Asn1.t) :: rest when v.tag = 0xa0 -> rest | l -> l in
      match fields with
      | _serial :: _sigalg :: issuer :: validity :: subject :: spki :: rest -> (
          let exts = List.concat_map (fun (x : Asn1.t) -> if x.tag = 0xa3 then List.concat_map Asn1.children (Asn1.children x) else []) rest in
          let names, ca = extensions exts in
          match (Asn1.children validity, Asn1.children alg) with
          | [ nb; na ], oid :: _ -> (
              match (Asn1.time nb, Asn1.time na) with
              | Some not_before, Some not_after ->
                  Ok
                    {
                      der; tbs = tbs.raw; issuer = issuer.raw; subject = subject.raw; common_name = common_name subject; not_before; not_after;
                      key = public_key spki; algorithm = Asn1.oid oid; signature = Asn1.bit_string sigv; names; ca;
                    }
              | _ -> Error "a certificate's dates unreadable")
          | _ -> Error "a certificate's validity or algorithm unreadable")
      | _ -> Error "a certificate with too few fields")
  | _ -> Error "not a certificate"

(*****************************************************************************)
(* Signatures *)
(*****************************************************************************)

(* an ECDSA signature's DER: SEQUENCE { r, s } *)
let ecdsa (curve : Ecdsa.curve) (point : string) ~(hash : string) (signature : string) : bool =
  match Result.map Asn1.children (one signature) with
  | Ok [ r; s ] -> Ecdsa.verify curve ~public:point ~hash ~r:(Asn1.integer r) ~s:(Asn1.integer s)
  | _ -> false

let signed_by ~(issuer : t) (cert : t) : bool =
  let rsa h = match issuer.key with Rsa (n, e) -> Rsa.verify_pkcs1 ~n ~e h ~message:cert.tbs ~signature:cert.signature | _ -> false in
  let ec digest = match issuer.key with Ec (c, p) -> ecdsa c p ~hash:(digest cert.tbs) cert.signature | _ -> false in
  match cert.algorithm with
  | "1.2.840.113549.1.1.11" -> rsa Rsa.Sha256
  | "1.2.840.113549.1.1.12" -> rsa Rsa.Sha384
  | "1.2.840.113549.1.1.13" -> rsa Rsa.Sha512
  | "1.2.840.10045.4.3.2" -> ec Sha256.digest
  | "1.2.840.10045.4.3.3" -> ec Sha512.digest384
  | "1.2.840.10045.4.3.4" -> ec Sha512.digest
  | _ -> false

let verify_scheme (cert : t) ~(scheme : int) ~(message : string) ~(signature : string) : bool =
  match (scheme, cert.key) with
  | 0x0403, Ec (c, p) -> ecdsa c p ~hash:(Sha256.digest message) signature
  | 0x0503, Ec (c, p) -> ecdsa c p ~hash:(Sha512.digest384 message) signature
  | 0x0804, Rsa (n, e) -> Rsa.verify_pss ~n ~e Rsa.Sha256 ~message ~signature
  | 0x0805, Rsa (n, e) -> Rsa.verify_pss ~n ~e Rsa.Sha384 ~message ~signature
  | 0x0806, Rsa (n, e) -> Rsa.verify_pss ~n ~e Rsa.Sha512 ~message ~signature
  | 0x0401, Rsa (n, e) -> Rsa.verify_pkcs1 ~n ~e Rsa.Sha256 ~message ~signature
  | 0x0501, Rsa (n, e) -> Rsa.verify_pkcs1 ~n ~e Rsa.Sha384 ~message ~signature
  | _ -> false

(*****************************************************************************)
(* Names, and the path *)
(*****************************************************************************)

(* "*.wikipedia.org" covers "en.wikipedia.org", not "wikipedia.org" nor
   "a.b.wikipedia.org" *)
let matches (pattern : string) (host : string) : bool =
  let pattern = String.lowercase_ascii pattern and host = String.lowercase_ascii host in
  if String.length pattern > 2 && String.sub pattern 0 2 = "*." then
    match String.index_opt host '.' with
    | Some i -> String.sub host (i + 1) (String.length host - i - 1) = String.sub pattern 2 (String.length pattern - 2) && i > 0
    | None -> false
  else pattern = host

let names_host (cert : t) (host : string) : bool =
  let names = if cert.names = [] then [ cert.common_name ] else cert.names in
  List.exists (fun p -> matches p host) names

let verify ~(trust : t list) ~(now : float) ~(host : string) (chain : t list) : (unit, string) result =
  let dated (c : t) = now >= c.not_before && now <= c.not_after in
  let name (c : t) = if c.common_name <> "" then c.common_name else "a certificate" in
  match chain with
  | [] -> Error "no certificate"
  | leaf :: others ->
      if not (names_host leaf host) then Error (Printf.sprintf "the certificate is for %s, not %s" (String.concat ", " (if leaf.names = [] then [ leaf.common_name ] else leaf.names)) host)
      else
        (* up from [c]: a trusted root that signed it, or else one of the
           certificates sent that is a CA and did *)
        let rec up (c : t) (depth : int) (unused : t list) =
          if not (dated c) then Error (name c ^ ": out of its dates")
          else if depth > 8 then Error "a chain too long"
          else
            match List.find_opt (fun (r : t) -> r.subject = c.issuer && signed_by ~issuer:r c) trust with
            | Some root -> if dated root then Ok () else Error (name root ^ ": a root out of its dates")
            | None -> (
                match List.find_opt (fun (i : t) -> i.subject = c.issuer && i.der <> c.der) unused with
                | Some i when not i.ca -> Error (name i ^ " is not allowed to sign certificates")
                | Some i when not (signed_by ~issuer:i c) -> Error (name c ^ ": its signature does not check")
                | Some i -> up i (depth + 1) (List.filter (fun (x : t) -> x != i) unused)
                | None -> Error (name c ^ ": issued by no one we trust"))
        in
        up leaf 0 others
