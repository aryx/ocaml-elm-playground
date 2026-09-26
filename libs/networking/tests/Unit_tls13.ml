(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tls13.mli *)

let unhex (s : string) : string = String.init (String.length s / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))
let hex (s : string) : string = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))
let same name expected actual = Alcotest.(check string) name expected (hex actual)
let r = unhex

(* a message's body, less its 4-byte header *)
let body (msg : string) : string = String.sub msg 4 (String.length msg - 4)

let tests =
  Testo.categorize "Tls13"
    [
      Testo.create "RFC 8448: the shared secret and the key schedule" (fun () ->
          let shared = X25519.scalar_mult (r Rfc8448.client_private) (r Rfc8448.server_public) in
          let hs = Tls13.handshake_secret shared in
          same "handshake secret" Rfc8448.handshake_secret hs;
          let hello = r Rfc8448.client_hello ^ r Rfc8448.server_hello in
          same "c hs traffic" Rfc8448.c_hs_traffic (Tls13.derive_secret hs "c hs traffic" hello);
          same "s hs traffic" Rfc8448.s_hs_traffic (Tls13.derive_secret hs "s hs traffic" hello);
          same "master secret" Rfc8448.master_secret (Tls13.master_secret hs);
          let k = Tls13.traffic_keys Aes128_gcm (r Rfc8448.s_hs_traffic) in
          same "server handshake key" Rfc8448.server_hs_key k.key;
          same "server handshake iv" Rfc8448.server_hs_iv k.iv;
          let k = Tls13.traffic_keys Aes128_gcm (r Rfc8448.c_hs_traffic) in
          same "client handshake key" Rfc8448.client_hs_key k.key;
          same "client handshake iv" Rfc8448.client_hs_iv k.iv);
      Testo.create "RFC 8448: the server's flight, opened and checked" (fun () ->
          let record = r Rfc8448.server_flight_record in
          let k = Tls13.traffic_keys Aes128_gcm (r Rfc8448.s_hs_traffic) in
          (match Tls13.open_record k (String.sub record 0 5) (String.sub record 5 (String.length record - 5)) with
          | Some ((22, payload), _) -> same "EncryptedExtensions, Certificate, CertificateVerify, Finished" Rfc8448.server_flight_payload payload
          | _ -> Alcotest.fail "not opened");
          (* CertificateVerify: RSA-PSS over the transcript up to the Certificate *)
          let cert_msg = body (r Rfc8448.certificate) in
          (* request context (1 byte, empty), the list (3), the entry's length (3) *)
          let len = (Char.code cert_msg.[4] lsl 16) lor (Char.code cert_msg.[5] lsl 8) lor Char.code cert_msg.[6] in
          let cert = Result.get_ok (X509.parse (String.sub cert_msg 7 len)) in
          let transcript = r Rfc8448.client_hello ^ r Rfc8448.server_hello ^ r Rfc8448.encrypted_extensions ^ r Rfc8448.certificate in
          let cv = body (r Rfc8448.certificate_verify) in
          let scheme = (Char.code cv.[0] lsl 8) lor Char.code cv.[1] in
          Alcotest.(check int) "rsa_pss_rsae_sha256" 0x0804 scheme;
          let content = String.make 64 ' ' ^ "TLS 1.3, server CertificateVerify" ^ "\000" ^ Sha256.digest transcript in
          Alcotest.(check bool) "the signature checks" true (X509.verify_scheme cert ~scheme ~message:content ~signature:(String.sub cv 4 (String.length cv - 4)));
          (* the server's Finished *)
          let transcript = transcript ^ r Rfc8448.certificate_verify in
          same "server Finished" (hex (body (r Rfc8448.server_finished))) (Tls13.finished (r Rfc8448.s_hs_traffic) transcript));
      Testo.create "RFC 8448: the client's Finished, the application's keys" (fun () ->
          let transcript =
            String.concat "" (List.map r [ Rfc8448.client_hello; Rfc8448.server_hello; Rfc8448.encrypted_extensions; Rfc8448.certificate; Rfc8448.certificate_verify; Rfc8448.server_finished ])
          in
          let fin = Tls13.finished (r Rfc8448.c_hs_traffic) transcript in
          same "client Finished" (hex (body (r Rfc8448.client_finished))) fin;
          let record, _ = Tls13.seal (Tls13.traffic_keys Aes128_gcm (r Rfc8448.c_hs_traffic)) 22 (r Rfc8448.client_finished) in
          same "its record" Rfc8448.client_finished_record record;
          let master = r Rfc8448.master_secret in
          same "c ap traffic" Rfc8448.c_ap_traffic (Tls13.derive_secret master "c ap traffic" transcript);
          same "s ap traffic" Rfc8448.s_ap_traffic (Tls13.derive_secret master "s ap traffic" transcript));
      Testo.create "RFC 8448: application data and close_notify" (fun () ->
          let client = Tls13.traffic_keys Aes128_gcm (r Rfc8448.c_ap_traffic) in
          same "client ap key" Rfc8448.client_ap_key client.key;
          let record, client = Tls13.seal client 23 (r Rfc8448.client_data) in
          same "the client's data" Rfc8448.client_data_record record;
          let alert, _ = Tls13.seal client 21 "\001\000" in
          same "its close_notify" Rfc8448.client_alert_record alert;
          (* the server's: a NewSessionTicket first (its record 0), then the data *)
          let server = Tls13.traffic_keys Aes128_gcm (r Rfc8448.s_ap_traffic) in
          let open_ k rec_ = Tls13.open_record k (String.sub rec_ 0 5) (String.sub rec_ 5 (String.length rec_ - 5)) in
          match open_ server (r Rfc8448.server_ticket_record) with
          | Some ((22, ticket), server) -> (
              Alcotest.(check int) "a NewSessionTicket" 4 (Char.code ticket.[0]);
              match open_ server (r Rfc8448.server_data_record) with
              | Some ((23, data), _) -> same "the server's data" Rfc8448.client_data data
              | _ -> Alcotest.fail "the data not opened")
          | _ -> Alcotest.fail "the ticket not opened");
    ]
