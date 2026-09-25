(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_mime.mli *)

let str = Alcotest.(check string)

let multipart =
  "Content-Type: multipart/mixed; boundary=\"xyz\"\n\
   \n\
   This is a multi-part message in MIME format.\n\
   --xyz\n\
   \n\
   Here are the photos.\n\
   --xyz\n\
   Content-Type: image/png\n\
   Content-Transfer-Encoding: base64\n\
   Content-Disposition: attachment; filename=\"picture.png\"\n\
   \n\
   iVBORw0K\n\
   GgoAAAA=\n\
   --xyz--\n\
   the epilogue\n"

let digest =
  "Content-Type: multipart/digest; boundary=\"d\"\n\
   \n\
   --d\n\
   \n\
   From: Carol <carol@tiny>\n\
   Subject: first\n\
   \n\
   one\n\
   --d--\n"

let tests =
  Testo.categorize "Mime"
    [
      Testo.create "quoted-printable, the worked example" (fun () ->
          str "decoded" "café au lait, and a line too long to be sent as it is, so cut here"
            (Mime.quoted_printable_decode "caf=C3=A9 au lait, and a line too long to be sent as it is, so=\n cut here");
          let long = String.make 100 'a' ^ " é \n" in
          let e = Mime.quoted_printable_encode long in
          Alcotest.(check bool) "lines of 76 at most" true (List.for_all (fun l -> String.length l <= 76) (String.split_on_char '\n' e));
          str "and back" long (Mime.quoted_printable_decode e));
      Testo.create "encoded words, Q and B" (fun () ->
          str "Q" "café" (Mime.decode_words "=?utf-8?Q?caf=C3=A9?=");
          str "B" "café" (Mime.decode_words "=?utf-8?B?Y2Fmw6k=?=");
          str "latin-1" "café" (Mime.decode_words "=?iso-8859-1?Q?caf=E9?=");
          str "between two, no space" "le café" (Mime.decode_words "=?utf-8?Q?le_?= =?utf-8?Q?caf=C3=A9?=");
          str "plain words kept" "Re: le café ici" (Mime.decode_words "Re: =?utf-8?Q?le_caf=C3=A9?= ici");
          str "there and back" "le café" (Mime.decode_words (Mime.encode_words "le café")));
      Testo.create "parameters" (fun () ->
          let t, ps = Mime.parameters "Multipart/Mixed; Boundary=\"x;y\"; charset=utf-8" in
          str "type" "multipart/mixed" t;
          Alcotest.(check (list (pair string string))) "params" [ ("boundary", "x;y"); ("charset", "utf-8") ] ps);
      Testo.create "a multipart: the text and a picture" (fun () ->
          let m = Mail.parse multipart in
          Alcotest.(check int) "two parts" 2 (List.length (Mime.parts m));
          str "the text" "Here are the photos." (Mime.text m);
          match Mime.attachments m with
          | [ a ] ->
              str "image/png" "image/png" a.mime;
              Alcotest.(check (option string)) "its name" (Some "picture.png") a.filename;
              str "its bytes" "\137PNG\r\n\026\n\000\000\000" a.data
          | l -> Alcotest.failf "%d attachments" (List.length l));
      Testo.create "a digest: its parts are messages" (fun () ->
          let m = Mail.parse digest in
          str "introduced" "----- From: Carol <carol@tiny>\n----- Subject: first\n\none" (Mime.text m);
          Alcotest.(check int) "nothing to save" 0 (List.length (Mime.attachments m)));
      Testo.create "written, then read back: a text and an attachment" (fun () ->
          let bytes = String.init 200 Char.chr in
          let headers, body = Mime.multipart ~boundary:"=_b" [ Mime.text_part "un café"; Mime.attachment ~filename:"a.png" bytes ] in
          let m = Mail.make (("Subject", "x") :: headers) body in
          let m = Mail.parse (Mail.to_string m) in
          str "the text" "un café" (Mime.text m);
          (match Mime.attachments m with
          | [ a ] ->
              str "its type" "image/png" a.mime;
              str "its bytes" bytes a.data
          | l -> Alcotest.failf "%d attachments" (List.length l));
          Alcotest.(check bool) "7-bit lines of 76 at most" true
            (List.for_all (fun l -> String.length l <= 76 && String.for_all (fun c -> Char.code c < 128) l) (String.split_on_char '\n' body)));
    ]
