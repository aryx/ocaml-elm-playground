(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tls13.mli *)

(*****************************************************************************)
(* Bytes *)
(*****************************************************************************)

let u8 (n : int) : string = String.make 1 (Char.chr (n land 0xff))
let u16 (n : int) : string = u8 (n lsr 8) ^ u8 n
let u24 (n : int) : string = u8 (n lsr 16) ^ u16 n
let get8 (s : string) (i : int) : int = Char.code s.[i]
let get16 (s : string) (i : int) : int = (get8 s i lsl 8) lor get8 s (i + 1)
let get24 (s : string) (i : int) : int = (get8 s i lsl 16) lor get16 s (i + 1)

(* a vector: its length in [n] bytes, then its bytes *)
let vec8 (s : string) : string = u8 (String.length s) ^ s
let vec16 (s : string) : string = u16 (String.length s) ^ s

(*****************************************************************************)
(* The key schedule *)
(*****************************************************************************)

let hash = Sha256.digest
let hash_len = 32
let hmac = Hmac.sha256

let hkdf_expand_label (secret : string) ~(label : string) ~(context : string) (length : int) : string =
  Hkdf.expand ~hmac secret ~info:(u16 length ^ vec8 ("tls13 " ^ label) ^ vec8 context) length

let derive_secret (secret : string) (label : string) (transcript : string) : string =
  hkdf_expand_label secret ~label ~context:(hash transcript) hash_len

let zeros = String.make hash_len '\000'
let early_secret = Hkdf.extract ~hmac ~salt:zeros zeros
let handshake_secret (shared : string) : string = Hkdf.extract ~hmac ~salt:(derive_secret early_secret "derived" "") shared
let master_secret (hs : string) : string = Hkdf.extract ~hmac ~salt:(derive_secret hs "derived" "") zeros

type cipher = Chacha20_poly1305 | Aes128_gcm
type keys = { cipher : cipher; key : string; iv : string; seq : int }

let traffic_keys (cipher : cipher) (secret : string) : keys =
  let key_len = match cipher with Chacha20_poly1305 -> 32 | Aes128_gcm -> 16 in
  { cipher; key = hkdf_expand_label secret ~label:"key" ~context:"" key_len; iv = hkdf_expand_label secret ~label:"iv" ~context:"" 12; seq = 0 }

let finished (secret : string) (transcript : string) : string =
  hmac (hkdf_expand_label secret ~label:"finished" ~context:"" hash_len) (hash transcript)

(*****************************************************************************)
(* The records *)
(*****************************************************************************)

(* the IV xored with the record's number, on its last 8 bytes *)
let nonce (k : keys) : string = String.mapi (fun i c -> if i < 4 then c else Char.chr (Char.code c lxor ((k.seq lsr (8 * (11 - i))) land 0xff))) k.iv

let seal (k : keys) (content_type : int) (data : string) : string * keys =
  let inner = data ^ u8 content_type in
  let header = u8 23 ^ u16 0x0303 ^ u16 (String.length inner + 16) in
  let aead = match k.cipher with Chacha20_poly1305 -> Chacha20_poly1305.seal | Aes128_gcm -> Gcm.seal in
  (header ^ aead ~key:k.key ~nonce:(nonce k) ~aad:header inner, { k with seq = k.seq + 1 })

let open_record (k : keys) (header : string) (body : string) : ((int * string) * keys) option =
  let aead = match k.cipher with Chacha20_poly1305 -> Chacha20_poly1305.open_ | Aes128_gcm -> Gcm.open_ in
  match aead ~key:k.key ~nonce:(nonce k) ~aad:header body with
  | None -> None
  | Some inner ->
      (* the padding's zeros, then the real type *)
      let rec last i = if i < 0 then None else if inner.[i] <> '\000' then Some i else last (i - 1) in
      Option.map (fun i -> ((get8 inner i, String.sub inner 0 i), { k with seq = k.seq + 1 })) (last (String.length inner - 1))

(*****************************************************************************)
(* The client *)
(*****************************************************************************)

type state = Handshaking | Open | Closed | Failed of string
type waiting = Server_hello | Encrypted_extensions | Certificate | Certificate_verify | Server_finished | Done

type t = {
  secret : string;
  verify : X509.t list -> (unit, string) result;
  state : state;
  waiting : waiting;
  inbox : string; (* bytes not yet a whole record *)
  pending : string; (* handshake bytes not yet a whole message *)
  transcript : string;
  cipher : cipher option;
  hs : string; (* the handshake secret *)
  client_secret : string; (* this phase's traffic secrets *)
  server_secret : string;
  read_keys : keys option;
  write_keys : keys option;
  chain : X509.t list;
  certificate_request : string option; (* its context, if the server asked for our certificate *)
  app : Buffer.t;
}

let hello_retry = "\xcf\x21\xad\x74\xe5\x9a\x61\x11\xbe\x1d\x8c\x02\x1e\x65\xb8\x91\xc2\xa2\x11\x16\x7a\xbb\x8c\x5e\x07\x9e\x09\xe2\xc8\xa8\x33\x9c"

let client_hello ~(host : string) ~(random : string) ~(public : string) ~(session_id : string) : string =
  let ext typ data = u16 typ ^ vec16 data in
  let extensions =
    String.concat ""
      [
        ext 0x0000 (vec16 (u8 0 ^ vec16 host)) (* server_name *);
        ext 0x000a (vec16 (u16 0x001d)) (* supported_groups: x25519 *);
        ext 0x000d (vec16 (String.concat "" (List.map u16 [ 0x0403; 0x0503; 0x0804; 0x0805; 0x0806; 0x0401; 0x0501; 0x0601 ])))
        (* signature_algorithms *);
        ext 0x002b (vec8 (u16 0x0304)) (* supported_versions: 1.3 *);
        ext 0x0033 (vec16 (u16 0x001d ^ vec16 public)) (* key_share *);
      ]
  in
  let body = u16 0x0303 ^ random ^ vec8 session_id ^ vec16 (u16 0x1303 ^ u16 0x1301) ^ vec8 "\000" ^ vec16 extensions in
  u8 1 ^ u24 (String.length body) ^ body

let client ~(host : string) ~(random : string) ~(secret : string) ~(session_id : string) ~(verify : X509.t list -> (unit, string) result) : t * string =
  let hello = client_hello ~host ~random ~public:(X25519.public_key secret) ~session_id in
  ( {
      secret; verify; state = Handshaking; waiting = Server_hello; inbox = ""; pending = ""; transcript = hello; cipher = None; hs = "";
      client_secret = ""; server_secret = ""; read_keys = None; write_keys = None; chain = []; certificate_request = None; app = Buffer.create 4096;
    },
    u8 22 ^ u16 0x0301 ^ vec16 hello )

let fail (t : t) (why : string) : t = if t.state = Handshaking || t.state = Open then { t with state = Failed why } else t

(* a ServerHello's extensions, as (type, data) *)
let extensions (s : string) (pos : int) : (int * string) list =
  let stop = pos + 2 + get16 s pos in
  let rec go i acc = if i + 4 > stop then List.rev acc else let len = get16 s (i + 2) in go (i + 4 + len) ((get16 s i, String.sub s (i + 4) len) :: acc) in
  go (pos + 2) []

let server_hello (t : t) (body : string) : t =
  let sid_len = get8 body 34 in
  let random = String.sub body 2 32 in
  let suite = get16 body (35 + sid_len) in
  let exts = extensions body (35 + sid_len + 3) in
  if random = hello_retry then fail t "HelloRetryRequest: the server wants another group (not supported)"
  else if List.assoc_opt 0x002b exts <> Some (u16 0x0304) then fail t "not TLS 1.3"
  else
    match (suite, List.assoc_opt 0x0033 exts) with
    | (0x1303 | 0x1301), Some share when get16 share 0 = 0x001d && get16 share 2 = 32 ->
        let cipher = if suite = 0x1303 then Chacha20_poly1305 else Aes128_gcm in
        let shared = X25519.scalar_mult t.secret (String.sub share 4 32) in
        let hs = handshake_secret shared in
        let client_secret = derive_secret hs "c hs traffic" t.transcript and server_secret = derive_secret hs "s hs traffic" t.transcript in
        {
          t with
          cipher = Some cipher; hs; client_secret; server_secret;
          read_keys = Some (traffic_keys cipher server_secret); write_keys = Some (traffic_keys cipher client_secret);
          waiting = Encrypted_extensions;
        }
    | _ -> fail t "a cipher suite or key share we did not offer"

(* the chain of a Certificate message *)
let chain_of (body : string) : (X509.t list, string) result =
  let ctx = get8 body 0 in
  let list_start = 1 + ctx + 3 and stop = 1 + ctx + 3 + get24 body (1 + ctx) in
  let rec go i acc =
    if i >= stop then Ok (List.rev acc)
    else
      let len = get24 body i in
      let der = String.sub body (i + 3) len in
      let ext_len = get16 body (i + 3 + len) in
      match X509.parse der with Ok c -> go (i + 3 + len + 2 + ext_len) (c :: acc) | Error e -> Error e
  in
  go list_start []

let certificate_verify_content (transcript : string) : string =
  String.make 64 ' ' ^ "TLS 1.3, server CertificateVerify" ^ "\000" ^ hash transcript

(* one handshake message, [msg] with its 4-byte header; the bytes to send *)
let handle (t : t) (typ : int) (msg : string) : t * string =
  let body = String.sub msg 4 (String.length msg - 4) in
  let t_after = { t with transcript = t.transcript ^ msg } in
  match (t.waiting, typ) with
  | Server_hello, 2 -> (server_hello t_after body, "")
  | Encrypted_extensions, 8 -> ({ t_after with waiting = Certificate }, "")
  | Certificate, 13 ->
      (* a CertificateRequest: we have none, and will say so with an
         empty Certificate before our Finished (RFC 8446, 4.4.2) *)
      ({ t_after with certificate_request = Some (String.sub body 1 (get8 body 0)) }, "")
  | Certificate, 11 -> (
      match chain_of body with
      | Error e -> (fail t e, "")
      | Ok [] -> (fail t "no certificate", "")
      | Ok chain -> ( match t.verify chain with Ok () -> ({ t_after with chain; waiting = Certificate_verify }, "") | Error e -> (fail t e, "")))
  | Certificate_verify, 15 ->
      let scheme = get16 body 0 and signature = String.sub body 4 (get16 body 2) in
      if X509.verify_scheme (List.hd t.chain) ~scheme ~message:(certificate_verify_content t.transcript) ~signature then
        ({ t_after with waiting = Server_finished }, "")
      else (fail t "the server's CertificateVerify does not check", "")
  | Server_finished, 20 ->
      if body <> finished t.server_secret t.transcript then (fail t "the server's Finished does not check", "")
      else
        (* our Finished, then the application's keys (over the transcript
           up to the server's Finished) *)
        let cipher = Option.get t.cipher in
        (* asked for a certificate: an empty one, in the transcript before our Finished *)
        let empty = match t.certificate_request with Some ctx -> let b = vec8 ctx ^ u24 0 in u8 11 ^ u24 (String.length b) ^ b | None -> "" in
        let fin = u8 20 ^ u24 hash_len ^ finished t.client_secret (t_after.transcript ^ empty) in
        let record, _ = seal (Option.get t.write_keys) 22 (empty ^ fin) in
        let master = master_secret t.hs in
        let client_secret = derive_secret master "c ap traffic" t_after.transcript and server_secret = derive_secret master "s ap traffic" t_after.transcript in
        ( {
            t_after with
            client_secret; server_secret;
            read_keys = Some (traffic_keys cipher server_secret); write_keys = Some (traffic_keys cipher client_secret);
            waiting = Done; state = Open;
          },
          (* a ChangeCipherSpec first, which middleboxes expect (RFC 8446, D.4) *)
          "\x14\x03\x03\x00\x01\x01" ^ record )
  | Done, 4 -> (t, "") (* a NewSessionTicket: no resumption here *)
  | Done, 24 ->
      (* KeyUpdate: the server's next keys; and ours, if it asks *)
      let cipher = Option.get t.cipher in
      let server_secret = hkdf_expand_label t.server_secret ~label:"traffic upd" ~context:"" hash_len in
      let t = { t with server_secret; read_keys = Some (traffic_keys cipher server_secret) } in
      if get8 body 0 = 1 then
        let record, _ = seal (Option.get t.write_keys) 22 (u8 24 ^ u24 1 ^ u8 0) in
        let client_secret = hkdf_expand_label t.client_secret ~label:"traffic upd" ~context:"" hash_len in
        ({ t with client_secret; write_keys = Some (traffic_keys cipher client_secret) }, record)
      else (t, "")
  | _ -> (fail t (Printf.sprintf "an unexpected handshake message (%d)" typ), "")

(* the whole handshake messages in [pending] *)
let rec messages (t : t) (out : string) : t * string =
  if String.length t.pending < 4 then (t, out)
  else
    let len = get24 t.pending 1 in
    if String.length t.pending < 4 + len then (t, out)
    else
      let msg = String.sub t.pending 0 (4 + len) in
      let t = { t with pending = String.sub t.pending (4 + len) (String.length t.pending - 4 - len) } in
      let t, more = handle t (get8 msg 0) msg in
      match t.state with Failed _ -> (t, out ^ more) | _ -> messages t (out ^ more)

let alert (t : t) (data : string) : t =
  if String.length data >= 2 && get8 data 1 = 0 then { t with state = Closed } else fail t (Printf.sprintf "the server's alert %d" (if String.length data >= 2 then get8 data 1 else -1))

(* the whole records in the inbox *)
let rec records (t : t) (out : string) : t * string =
  if String.length t.inbox < 5 then (t, out)
  else
    let typ = get8 t.inbox 0 and len = get16 t.inbox 3 in
    if String.length t.inbox < 5 + len then (t, out)
    else
      let header = String.sub t.inbox 0 5 and body = String.sub t.inbox 5 len in
      let t = { t with inbox = String.sub t.inbox (5 + len) (String.length t.inbox - 5 - len) } in
      let t, more =
        match (typ, t.read_keys) with
        | 20, _ -> (t, "") (* ChangeCipherSpec: ignored, as 1.3 says *)
        | 21, _ -> (alert t body, "")
        | 22, None -> messages { t with pending = t.pending ^ body } ""
        | 23, Some k -> (
            match open_record k header body with
            | None -> (fail t "a record that does not decrypt", "")
            | Some ((22, data), k) -> messages { t with read_keys = Some k; pending = t.pending ^ data } ""
            | Some ((23, data), k) ->
                Buffer.add_string t.app data;
                ({ t with read_keys = Some k }, "")
            | Some ((21, data), k) -> (alert { t with read_keys = Some k } data, "")
            | Some ((other, _), _) -> (fail t (Printf.sprintf "a record of type %d" other), ""))
        | _ -> (fail t (Printf.sprintf "a record of type %d, unexpected" typ), "")
      in
      match t.state with Failed _ -> (t, out ^ more) | _ -> records t (out ^ more)

let received (t : t) (bytes : string) : t * string = records { t with inbox = t.inbox ^ bytes } ""
let state (t : t) : state = t.state

let read (t : t) : t * string =
  let s = Buffer.contents t.app in
  Buffer.clear t.app;
  (t, s)

let write (t : t) (data : string) : t * string =
  match (t.state, t.write_keys) with
  | Open, Some k ->
      (* records of at most 2^14 bytes *)
      let rec go k i out =
        if i >= String.length data then ({ t with write_keys = Some k }, String.concat "" (List.rev out))
        else
          let chunk = String.sub data i (min 16384 (String.length data - i)) in
          let r, k = seal k 23 chunk in
          go k (i + String.length chunk) (r :: out)
      in
      go k 0 []
  | _ -> (t, "")

let close (t : t) : t * string =
  match (t.state, t.write_keys) with
  | Open, Some k ->
      let r, k = seal k 21 "\001\000" in
      ({ t with write_keys = Some k; state = Closed }, r)
  | _ -> (t, "")

let certificates (t : t) : X509.t list = t.chain
let cipher (t : t) : cipher option = t.cipher
