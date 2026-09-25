(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_our_mail.mli *)

let str = Alcotest.(check string)
let inbox = Mbox.parse (List.assoc "In" Our_mail.mailboxes)
let nth n = (List.nth inbox (n - 1)).Mbox.mail
let get n name = Mime.decode_words (Option.value (Mail.get (nth n) name) ~default:"")

let tests =
  Testo.categorize "Our_mail"
    [
      Testo.create "every mailbox, there and back, the same bytes" (fun () ->
          List.iter (fun (name, text) -> str name text (Mbox.to_string (Mbox.parse text))) Our_mail.mailboxes);
      Testo.create "In's eleven messages, each with a date" (fun () ->
          Alcotest.(check int) "eleven" 11 (List.length inbox);
          List.iteri (fun i (e : Mbox.entry) -> Alcotest.(check bool) (string_of_int (i + 1)) true (Option.bind (Mail.get e.mail "date") Mail.date <> None)) inbox);
      Testo.create "the lessons" (fun () ->
          Alcotest.(check (list string)) "the thread's last references" [ "plan1@carol.tiny"; "plan2@alice.tiny"; "plan3@dave.tiny" ] (Mail.message_ids (get 5 "references"));
          str "forged: the envelope's sender" "mallory@evil.example" (Mbox.sender (List.nth inbox 5));
          str "forged: the header's" "president@whitehouse.gov" (List.hd (Mail.addresses (get 6 "from"))).mailbox;
          Alcotest.(check bool) "the digest's two messages" true (List.length (Mime.parts (nth 7)) = 2);
          (match Mime.attachments (nth 8) with
          | [ a ] -> str "a PNG" "\137PNG" (String.sub a.data 0 4)
          | l -> Alcotest.failf "%d attachments" (List.length l));
          str "an encoded-word subject" "le café de la gare" (get 9 "subject");
          Alcotest.(check bool) "quoted-printable, joined" true (Mime.text (nth 9) |> fun t -> String.length t > 0 && not (String.contains t '='));
          Alcotest.(check bool) "the From line unquoted" true
            (List.mem "From the desk of the director: the mail server stays up all" (String.split_on_char '\n' (nth 10).body)));
    ]
