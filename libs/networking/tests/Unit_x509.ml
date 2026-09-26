(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_x509.mli *)

(* 2026-09-26 00:34 UTC, when the chains were captured *)
let captured = 1790382858.

let read (file : string) : string = In_channel.with_open_bin file In_channel.input_all
let certs (file : string) : X509.t list = List.map (fun der -> match X509.parse der with Ok c -> c | Error e -> Alcotest.fail (file ^ ": " ^ e)) (Pem.certificates (read file))
let roots () = certs "tls/roots.pem"
let ok = Alcotest.(check (result unit string))
let refused name r = Alcotest.(check bool) name true (Result.is_error r)

let tests =
  Testo.categorize "X509"
    [
      Testo.create "DER: a SEQUENCE, an OID" (fun () ->
          match Asn1.parse "\x30\x06\x02\x01\x05\x01\x01\xff" 0 with
          | Ok (seq, 8) ->
              (match Asn1.children seq with
              | [ i; b ] ->
                  Alcotest.(check string) "5" "5" (Bignum.to_hex (Asn1.integer i));
                  Alcotest.(check string) "true" "\xff" b.value
              | _ -> Alcotest.fail "two children");
              let oid = Result.get_ok (Asn1.parse "\x06\x08\x2a\x86\x48\x86\xf7\x0d\x01\x01" 0) |> fst in
              Alcotest.(check string) "rsadsi" "1.2.840.113549.1.1" (Asn1.oid oid)
          | _ -> Alcotest.fail "not parsed");
      Testo.create "the roots: parsed, and each signed by itself" (fun () ->
          let rs = roots () in
          Alcotest.(check (list string)) "their names" [ "GTS Root R4"; "GTS Root R1"; "ISRG Root X2"; "USERTrust ECC Certification Authority" ]
            (List.map (fun (c : X509.t) -> c.common_name) rs);
          List.iter (fun (r : X509.t) -> Alcotest.(check bool) (r.common_name ^ ", self-signed") true (X509.signed_by ~issuer:r r && r.ca)) rs);
      Testo.create "four real chains, at their date" (fun () ->
          List.iter
            (fun (file, host) -> ok (file ^ " for " ^ host) (Ok ()) (X509.verify ~trust:(roots ()) ~now:captured ~host (certs file)))
            [ ("tls/gmail.pem", "pop.gmail.com"); ("tls/google-rsa.pem", "www.google.com"); ("tls/wikipedia.pem", "en.wikipedia.org");
              ("tls/github.pem", "github.com") ]);
      Testo.create "refused: another host, later, a byte changed, no roots" (fun () ->
          let trust = roots () and chain = certs "tls/gmail.pem" in
          refused "smtp.gmail.com is not pop.gmail.com" (X509.verify ~trust ~now:captured ~host:"smtp.gmail.com" chain);
          refused "two years later" (X509.verify ~trust ~now:(captured +. (2. *. 365. *. 86400.)) ~host:"pop.gmail.com" chain);
          refused "no roots" (X509.verify ~trust:[] ~now:captured ~host:"pop.gmail.com" chain);
          (* the leaf's last signature byte changed *)
          let leaf = List.hd chain in
          let bad = { leaf with signature = String.mapi (fun i c -> if i = String.length leaf.signature - 1 then Char.chr (Char.code c lxor 1) else c) leaf.signature } in
          refused "a byte changed" (X509.verify ~trust ~now:captured ~host:"pop.gmail.com" (bad :: List.tl chain));
          (* a wildcard covers one label *)
          let wiki = certs "tls/wikipedia.pem" in
          refused "a.b.wikipedia.org" (X509.verify ~trust ~now:captured ~host:"a.b.wikipedia.org" wiki);
          ok "fr.wikipedia.org" (Ok ()) (X509.verify ~trust ~now:captured ~host:"fr.wikipedia.org" wiki));
    ]
