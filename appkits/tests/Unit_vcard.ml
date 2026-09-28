(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/pim: Vcard *)

let t = Testo.create

(* split on the unescaped ';' first, unescape after *)
let test_structured () =
  let check s expected = Alcotest.(check (list string)) s expected (Vcard.structured s) in
  check "Lovelace;Ada;;;" [ "Lovelace"; "Ada"; ""; ""; "" ];
  check "Doe\\;Smith;John" [ "Doe;Smith"; "John" ];
  check "a\\,b;c\\\\;d" [ "a,b"; "c\\"; "d" ];
  check "" [ "" ]

(* RFC 2426, section 7: two cards, lowercase vCard in BEGIN, an address
 * folded across two lines (and continuing with a ';') *)
let rfc_example =
  String.concat "\r\n"
    [ "BEGIN:vCard"; "VERSION:3.0"; "FN:Frank Dawson"; "ORG:Lotus Development Corporation";
      "ADR;TYPE=WORK,POSTAL,PARCEL:;;6544 Battleford Drive"; " ;Raleigh;NC;27613-3502;U.S.A.";
      "TEL;TYPE=VOICE,MSG,WORK:+1-919-676-9515"; "TEL;TYPE=FAX,WORK:+1-919-676-9564";
      "EMAIL;TYPE=INTERNET,PREF:Frank_Dawson@Lotus.com"; "EMAIL;TYPE=INTERNET:fdawson@earthlink.net";
      "URL:http://home.earthlink.net/~fdawson"; "END:vCard"; ""; "BEGIN:vCard"; "VERSION:3.0"; "FN:Tim Howes";
      "ORG:Netscape Communications Corp."; "ADR;TYPE=WORK:;;501 E. Middlefield Rd.;Mountain View;";
      " CA; 94043;U.S.A."; "TEL;TYPE=VOICE,MSG,WORK:+1-415-937-3419"; "TEL;TYPE=FAX,WORK:+1-415-528-4164";
      "EMAIL;TYPE=INTERNET:howes@netscape.com"; "END:vCard"; "" ]

let test_rfc () =
  match Vcard.of_string rfc_example with
  | [ frank; tim ] ->
      Alcotest.(check string) "FN" "Frank Dawson" (Vcard.display_name frank);
      Alcotest.(check string) "ORG" "Lotus Development Corporation" frank.org;
      (match frank.addresses with
      | [ a ] ->
          Alcotest.(check (list string)) "the folded address" [ "6544 Battleford Drive"; "Raleigh"; "NC"; "27613-3502"; "U.S.A." ]
            [ a.street; a.locality; a.region; a.code; a.country ];
          Alcotest.(check (list string)) "its kinds" [ "work"; "postal"; "parcel" ] a.kinds
      | _ -> Alcotest.fail "one address");
      Alcotest.(check (list string)) "two phones" [ "+1-919-676-9515"; "+1-919-676-9564" ]
        (List.map (fun (p : Vcard.phone) -> p.number) frank.phones);
      Alcotest.(check (list string)) "the fax" [ "fax"; "work" ] (List.nth frank.phones 1).kinds;
      Alcotest.(check (list string)) "two e-mails" [ "Frank_Dawson@Lotus.com"; "fdawson@earthlink.net" ]
        (List.map (fun (e : Vcard.email) -> e.address) frank.emails);
      Alcotest.(check string) "Tim's city, and his code trimmed" "Mountain View CA 94043"
        (match tim.addresses with [ a ] -> String.concat " " [ a.locality; a.region; a.code ] | _ -> "")
  | cards -> Alcotest.failf "%d cards" (List.length cards)

(* a phone's export from before 3.0: bare types, N but no FN *)
let test_old_cards () =
  match Vcard.of_string "BEGIN:VCARD\nVERSION:2.1\nN:Hopper;Grace;Murray;Rear Admiral;\nTEL;WORK;VOICE:555-0199\nBDAY:19061209\nX-FOO:ignored\nEND:VCARD\n" with
  | [ c ] ->
      Alcotest.(check string) "the name, from N" "Grace Hopper" (Vcard.display_name c);
      Alcotest.(check string) "the prefix" "Rear Admiral" c.name.prefix;
      Alcotest.(check (list string)) "bare types" [ "work"; "voice" ] (List.hd c.phones).kinds;
      Alcotest.(check (option string)) "a basic-format birthday" (Some "1906-12-09") (Option.map Civil.to_string c.birthday)
  | cards -> Alcotest.failf "%d cards" (List.length cards)

(* the Palm's list: by family name, then given name, ignoring case *)
let test_order () =
  let card family given : Vcard.card = { (Vcard.make "") with name = { (Vcard.make "").name with family; given } } in
  let cards = [ card "lovelace" "Ada"; card "Hopper" "Grace"; card "Babbage" "Charles"; card "Hopper" "Admiral"; Vcard.make "Zuse" ] in
  Alcotest.(check (list string)) "sorted" [ "Charles Babbage"; "Admiral Hopper"; "Grace Hopper"; "Ada lovelace"; "Zuse" ]
    (List.sort Vcard.compare_by_name cards |> List.map Vcard.display_name)

(* written, then read back the same: escapes in structured parts, a
 * long note folded *)
let test_round_trip () =
  let ada : Vcard.card =
    { uid = "ada@tiny"; name = { family = "Lovelace"; given = "Ada"; additional = "King"; prefix = "Countess;"; suffix = "" };
      full_name = "Ada Lovelace"; org = "Analytical Engine Society, London"; title = "Programmer";
      phones = [ { number = "+44 20 7946 0000"; kinds = [ "home"; "voice" ] } ];
      emails = [ { address = "ada@example.org"; kinds = [ "internet" ] } ];
      addresses =
        [ { street = "12 St James's Square"; locality = "London"; region = ""; code = "SW1Y 4JH"; country = "England";
            kinds = [ "home" ] } ];
      birthday = Some { year = 1815; month = 12; day = 10 };
      note = String.concat " " (List.init 20 (fun i -> Printf.sprintf "note%d;" i)) ^ "\nsecond line" }
  in
  let cards = [ ada; Vcard.make "Just a name" ] in
  let text = Vcard.to_string cards in
  Alcotest.(check bool) "CRLF line ends" true (String.ends_with ~suffix:"END:VCARD\r\n" text);
  Alcotest.(check bool) "N's ';' escaped" true (Ics.unfold text |> List.mem "N:Lovelace;Ada;King;Countess\\;;");
  let back = Vcard.of_string text in
  Alcotest.(check bool) "read back the same" true (List.hd back = ada);
  Alcotest.(check string) "a card with only a name" "Just a name" (Vcard.display_name (List.nth back 1))

let tests =
  Testo.categorize "Vcard"
    [ t "structured values" test_structured;
      t "RFC 2426's example" test_rfc;
      t "vCard 2.1" test_old_cards;
      t "the Palm's order" test_order;
      t "written and read back" test_round_trip ]
