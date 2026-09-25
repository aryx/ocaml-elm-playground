(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyEudora: a mail client after Eudora (Steve Dorner, University of
 * Illinois, 1988, for the Macintosh; Qualcomm's from 1991, free and
 * then shareware -- the mail client of the 1990s), in its 1.x look:
 * black and white, a window of messages, a message under it
 * (plan_tiny_eudora.md).
 *
 * Eudora is the POP client: mail downloaded to your disk and read
 * there, your mailboxes plain files you own -- before IMAP and webmail
 * kept everything on a server. A mailbox is one file, an mbox
 * (Mbox.mli), and TinyEudora's are In, Out, Trash and the user's own.
 * What it shows of a message is what Mail.mli and Mime.mli read out of
 * its bytes: who, when, the size, the subject, the text part of a
 * multipart, the attachments.
 *
 *   up, down     the message before, after (as the list is sorted)
 *   click        a message; a column's title sorts by it (again: back
 *                to the order of arrival); an attachment's name saves it
 *   b            Eudora's "Blah Blah Blah" button: every header, and
 *                the mbox's "From " line, the envelope -- which is how
 *                the forged message in In is found out
 *   Delete       into the Trash (from the Trash: gone)
 *   PageDown/Up  the message scrolled, and the mouse's wheel
 *
 * and the menus: Mailbox (open one, or New...), Message (New Message,
 * Reply, Forward, Attach Document..., Delete, Blah Blah Blah),
 * Transfer (Eudora's name for moving a message to a mailbox), Special
 * (Empty Trash, Make Nickname, Nicknames). Flags: message=n, the n-th
 * message of the mailbox opened at the start; mailbox=Out, that
 * mailbox shown (In by default); compose=new|reply|forward, a
 * message begun at the start (to it); user=, who you are ("Bob
 * <bob@tiny>").
 *
 * Writing is Eudora's on a dial-up modem: a message written is not
 * sent but *queued* -- put in Out, marked Q -- and Send Queued
 * Messages would send them all at once, connecting once (the plan's
 * phase 5). A reply quotes the message with "> " under Eudora's "At
 * 12:00 PM 9/25/26, Alice wrote:", and says what it answers,
 * In-Reply-To: and References:, which is what threads a conversation;
 * the message answered is marked R (forwarded: F) when the reply is
 * queued. Bcc: stays in the queued copy until it is sent: the
 * recipients are the envelope's, not the headers' (Smtp.mli, to come).
 * The signature is added when a message is queued, as Eudora added its
 * Signature file when it sent. To:, Cc: and Bcc: take nicknames --
 * "team" -- expanded when queued: Eudora's address book, kept as
 * vCards (appkits/pim's Vcard, the one TinyPalmPilot's Address Book
 * reads), a list being a card with several addresses.
 *
 * The mailboxes are kept as they change, each an mbox file in the
 * platform's store ("eudora-In.mbox"; natively a file you can open
 * with mutt, in a browser its localStorage), and the nicknames as
 * "eudora-nicknames.vcf". The built-in ones are the defaults: a
 * mailbox never changed is not written.
 *
 * With no server, it opens on the built-in mailboxes, Our_mail --
 * messages written for what they show: a thread of five replies, a
 * forged sender, a digest of two messages, a picture attached,
 * quoted-printable French with an encoded-word subject, a ">From" line.
 *
 * What it uses: networking_mail (Mail, Mime, Mbox: reading and
 * writing), the picture decoders (Png, Gif, Jpeg, as the browsers'),
 * Stroke_text (text from the left, with Hershey's real widths), Vcard,
 * the gui toolkit's fields and text area (Text_edit), the
 * playground's store. Not the network yet: sending and Check Mail are
 * the plan's next phases. Not the File menu either: a document is
 * attached from the store by a list of its names, since Eudora's
 * File menu opened mailboxes, not documents.
 *
 * Exercises: Reply All (the Cc: kept, yourself removed); a nickname
 * edited and removed in the Nicknames pane; Eudora's "Keep copies" off,
 * a sent message not kept in Out.
 *
 * Hershey's font has no accents: "café" is decoded, in UTF-8, and
 * drawn "cafe" (ascii below). Exercise: the accents as strokes over
 * the letter.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type column = Status | Who | Date | Size | Subject
type box = { name : string; entries : Mbox.entry list }

(* a message being written *)
type draft = {
  to_ : string;
  subject : string;
  cc : string;
  bcc : string;
  body : Text_edit.t;
  attached : (string * string) list; (* each document's name and bytes *)
  in_reply_to : string option; (* the Message-ID answered *)
  references : string list;
  marks : (string * char) option; (* the original's Message-ID, and its mark once this is queued: A replied, F forwarded *)
}

(* what the lower window shows *)
type pane =
  | Reading
  | Composing of draft
  | Picking of draft * string list * int option (* Attach Document...: the store's documents *)
  | Naming of string (* Mailbox > New...: the name being typed *)
  | Nicknames

type model = {
  boxes : box list;
  box : string; (* the mailbox shown *)
  selected : int option; (* the message, in the mailbox's own order *)
  sort : column option; (* None: the order of arrival *)
  blah : bool; (* every header *)
  scroll : float; (* the message, scrolled, in pixels *)
  pane : pane;
  nicknames : Vcard.card list;
  user : Mail.address;
  (* what the store holds, to write only what changed *)
  saved : box list;
  saved_nicknames : Vcard.card list;
  said : string;
  started : bool; (* the flags and the store read *)
  was : string list;
  was_down : bool;
}

let initial : model =
  {
    boxes = List.map (fun (name, text) -> { name; entries = Mbox.parse text }) Our_mail.mailboxes;
    box = "In";
    selected = None;
    sort = None;
    blah = false;
    scroll = 0.;
    pane = Reading;
    nicknames = Vcard.of_string Our_mail.nicknames;
    user = { display = "Bob"; mailbox = "bob@tiny" };
    saved = [];
    saved_nicknames = [];
    said = "";
    started = false;
    was = [];
    was_down = false;
  }

let entries (m : model) : Mbox.entry list = match List.find_opt (fun b -> b.name = m.box) m.boxes with Some b -> b.entries | None -> []
let with_entries (name : string) (f : Mbox.entry list -> Mbox.entry list) (m : model) : model = { m with boxes = List.map (fun b -> if b.name = name then { b with entries = f b.entries } else b) m.boxes }
let current (m : model) : Mbox.entry option = Option.bind m.selected (List.nth_opt (entries m))

(*****************************************************************************)
(* A message's columns *)
(*****************************************************************************)

let header (e : Mbox.entry) (name : string) : string = Mime.decode_words (Option.value (Mail.get e.mail name) ~default:"")

(* the Status: and X-Status: headers of the Unix mailers: R read, A
   answered (Eudora's "R"eplied); F forwarded, S sent, Q queued are
   Eudora's, kept in X-Status too *)
let has (e : Mbox.entry) (name : string) (c : char) : bool = String.contains (header e name) c
let is_read (e : Mbox.entry) : bool = has e "status" 'R'
let read (e : Mbox.entry) : Mbox.entry = if is_read e then e else { e with mail = Mail.set "Status" "RO" e.mail }

let status (e : Mbox.entry) : string =
  if not (is_read e) then "*"
  else if has e "x-status" 'A' then "R"
  else if has e "x-status" 'F' then "F"
  else if has e "x-status" 'Q' then "Q"
  else if has e "x-status" 'S' then "S"
  else ""

(* in Out, who it went to; elsewhere, who it is from *)
let who (box : string) (e : Mbox.entry) : string =
  let field = if box = "Out" then "to" else "from" in
  match Mail.addresses (header e field) with a :: _ -> Mail.who a | [] -> Mbox.sender e

let date (e : Mbox.entry) : Mail.date option = Mail.date (header e "date")

(* as Eudora wrote it: "12:00 PM 9/25/26", the time where it was sent *)
let date_text (e : Mbox.entry) : string =
  match date e with
  | Some d ->
      let h = d.time.hour in
      Printf.sprintf "%d:%02d %s %d/%d/%02d" (if h mod 12 = 0 then 12 else h mod 12) d.time.minute (if h < 12 then "AM" else "PM") d.day.month d.day.day (d.day.year mod 100)
  | None -> ""

(* in K, rounded up, as Eudora counted *)
let size (e : Mbox.entry) : int = (String.length (Mail.to_string e.mail) + 1023) / 1024
let attached (e : Mbox.entry) : bool = Mime.attachments e.mail <> []

(* where [x] is in [l] *)
let position (x : int) (l : int list) : int option =
  let rec go k = function [] -> None | y :: _ when y = x -> Some k | _ :: rest -> go (k + 1) rest in
  go 0 l

(* the mailbox's messages as the list shows them: their indices *)
let order (m : model) : int list =
  let l = List.mapi (fun i e -> (i, e)) (entries m) in
  let by f = List.stable_sort (fun (_, a) (_, b) -> compare (f a) (f b)) l in
  let sorted =
    match m.sort with
    | None -> l
    | Some Status -> by status
    | Some Who -> by (fun e -> String.lowercase_ascii (who m.box e))
    | Some Date -> by (fun e -> Option.fold ~none:0. ~some:Mail.seconds (date e))
    | Some Size -> by size
    | Some Subject -> by (fun e -> String.lowercase_ascii (header e "subject"))
  in
  List.map fst sorted

(*****************************************************************************)
(* Text, from the left *)
(*****************************************************************************)

(* U+00C0 to U+00FF, the accented Latin letters, without their accents *)
let latin = "AAAAAAACEEEEIIIIDNOOOOOxOUUUUYPsaaaaaaaceeeeiiiidnooooo/ouuuuypy"

(* UTF-8 to what Hershey can draw *)
let ascii (s : string) : string =
  let b = Buffer.create (String.length s) and n = String.length s in
  let rec go i =
    if i < n then
      let c = Char.code s.[i] in
      if c < 0x80 then (
        Buffer.add_char b s.[i];
        go (i + 1))
      else if c = 0xC3 && i + 1 < n then (
        Buffer.add_char b latin.[(Char.code s.[i + 1] - 0x80) land 63];
        go (i + 2))
      else (
        (* another character: a '?', its continuation bytes skipped *)
        if c >= 0xC0 then Buffer.add_char b '?';
        go (i + 1))
  in
  go 0;
  Buffer.contents b

let look ?(bold = false) (size : float) : Style.t = { Style.plain with size; bold }
let width (look : Style.t) (s : string) : float = String.fold_left (fun w c -> w +. Stroke_text.metrics look (String.make 1 c)) 0. s

(* [s] (ASCII) drawn from [x], on the baseline [y] *)
let text ?(bold = false) ?(size = 13.) (color : color) (x : float) (y : float) (s : string) : shape list =
  let look = look ~bold size in
  let rec go x i acc =
    if i >= String.length s then acc
    else
      let c = String.make 1 s.[i] in
      go (x +. Stroke_text.metrics look c) (i + 1) (if c = " " then acc else Stroke_text.glyph color look c ~x ~baseline:y @ acc)
  in
  go x 0 []

(* [s] cut to [w] *)
let fit ?(bold = false) ?(size = 13.) (w : float) (s : string) : string =
  let look = look ~bold size in
  let rec cut n = if n <= 0 || width look (String.sub s 0 n) <= w then String.sub s 0 (max 0 n) else cut (n - 1) in
  cut (String.length s)

(* a line broken into lines of at most [w], at spaces; a word longer
   than that, cut *)
let wrap (w : float) (line : string) : string list =
  let look = look 13. in
  let rec cut s =
    if width look s <= w then [ s ]
    else
      let h = fit w s in
      let h = if h = "" then String.sub s 0 1 else h in
      h :: cut (String.sub s (String.length h) (String.length s - String.length h))
  in
  let rec go acc cur = function
    | [] -> List.rev (cur :: acc)
    | word :: rest ->
        let longer = if cur = "" then word else cur ^ " " ^ word in
        if width look longer <= w || cur = "" then go acc longer rest else go (cur :: acc) word rest
  in
  List.concat_map cut (go [] "" (String.split_on_char ' ' line))

(*****************************************************************************)
(* The layout *)
(*****************************************************************************)

let left = -490.
let right = 490.
let bar_y = 485. (* the menu bar's middle *)
let list_top = 462.
let row_h = 20.
let rows = 12
let columns_y = list_top -. 22. -. (row_h /. 2.) (* the column titles' middle *)
let first_row_top = list_top -. 22. -. row_h
let list_bottom = first_row_top -. (row_h *. float_of_int rows) -. 4.
let msg_top = list_bottom -. 12.
let msg_bottom = -462.
let line_h = 18.
let body_top = msg_top -. 22. -. 8.

(* where each column starts, and its title *)
let column_x = [ (Status, (left +. 8., "")); (Who, (left +. 30., "Who")); (Date, (left +. 230., "Date")); (Size, (left +. 380., "K")); (Subject, (left +. 425., "Subject")) ]
let column_at (x : float) : column option = List.fold_left (fun found (c, (cx, _)) -> if x >= cx -. 8. then Some c else found) None column_x

(* the list scrolled to keep the selection in sight *)
let first_shown (m : model) : int =
  let pos = match m.selected with Some i -> Option.value (position i (order m)) ~default:0 | None -> 0 in
  max 0 (pos - rows + 1)

(* a message, as what it shows, top to bottom *)
type row = Line of string * bool (* bold *) | Picture of Rgba_image.t | Attachment of Mime.leaf

let row_height = function Picture p -> (float_of_int p.height *. 2.) +. 8. | _ -> line_h

let decoded_pictures : (string, Rgba_image.t option) Hashtbl.t = Hashtbl.create 8

let picture (data : string) : Rgba_image.t option =
  match Hashtbl.find_opt decoded_pictures data with
  | Some p -> p
  | None ->
      let starts p = String.length data >= String.length p && String.sub data 0 (String.length p) = p in
      let p =
        try
          if starts "\x89PNG" then Some (Png.decode data)
          else if starts "GIF8" then Some (Gif.decode data)
          else if starts "\xFF\xD8" then Some (Jpeg.decode data)
          else None
        with _ -> None
      in
      Hashtbl.replace decoded_pictures data p;
      p

let text_w = right -. left -. 40.

let message_rows ~(blah : bool) (e : Mbox.entry) : row list =
  let headers =
    if blah then
      (* everything, as it came: the envelope first *)
      Line ("From " ^ e.envelope, true)
      :: List.concat_map (fun (f : Mail.field) -> List.map (fun l -> Line (l, false)) (wrap text_w (ascii (f.name ^ ":" ^ Mime.decode_words (Mail.unfold f.raw))))) e.mail.fields
    else
      List.filter_map
        (fun name -> match Mail.get e.mail name with Some _ -> Some (Line (ascii (name ^ ": " ^ header e name), false)) | None -> None)
        [ "From"; "To"; "Cc"; "Subject"; "Date" ]
  in
  let body = String.split_on_char '\n' (Mime.text e.mail) |> List.concat_map (fun l -> List.map (fun l -> Line (l, false)) (wrap text_w (ascii l))) in
  let attachments =
    List.concat_map
      (fun (a : Mime.leaf) -> Attachment a :: (match picture a.data with Some p -> [ Picture p ] | None -> []))
      (Mime.attachments e.mail)
  in
  headers @ [ Line ("", false) ] @ body @ (if attachments = [] then [] else Line ("", false) :: attachments)

(* each row with its top, the message scrolled *)
let placed (m : model) (e : Mbox.entry) : (row * float) list =
  let rec go y = function [] -> [] | r :: rest -> (r, y) :: go (y -. row_height r) rest in
  go (body_top +. m.scroll) (message_rows ~blah:m.blah e)

let total_height (m : model) (e : Mbox.entry) : float = List.fold_left (fun h r -> h +. row_height r) 0. (message_rows ~blah:m.blah e)

(*****************************************************************************)
(* What is done to messages *)
(*****************************************************************************)

(* the message [i] shown, and so read *)
let select (i : int option) (m : model) : model =
  let m = { m with selected = i; scroll = 0. } in
  match i with Some i -> with_entries m.box (List.mapi (fun j e -> if j = i then read e else e)) m | None -> m

(* the message [i] taken out of the mailbox shown, and the one after
   it (as sorted) shown instead *)
let take_out (m : model) : (Mbox.entry * model) option =
  match (m.selected, current m) with
  | Some i, Some e ->
      let ord = order m in
      let pos = Option.get (position i ord) in
      let next = match List.nth_opt ord (pos + 1) with Some j -> Some j | None -> if pos > 0 then List.nth_opt ord (pos - 1) else None in
      (* the indices after [i] move up one *)
      let next = Option.map (fun j -> if j > i then j - 1 else j) next in
      let m = with_entries m.box (List.filteri (fun j _ -> j <> i)) m in
      Some (e, select next { m with selected = None })
  | _ -> None

let transfer (dest : string) (m : model) : model =
  if dest = m.box then m
  else match take_out m with Some (e, m) -> { (with_entries dest (fun l -> l @ [ e ]) m) with said = "moved to " ^ dest } | None -> m

let delete (m : model) : model =
  if m.box = "Trash" then match take_out m with Some (_, m) -> { m with said = "deleted" } | None -> m else transfer "Trash" m

let open_box (name : string) (m : model) : model =
  let m = { m with box = name; sort = None; selected = None } in
  (* the newest message, as a mailbox opens at its end *)
  let n = List.length (entries m) in
  select (if n > 0 then Some (n - 1) else None) m

let save (caps : < Cap.open_out ; .. >) (a : Mime.leaf) (m : model) : model =
  let name = Option.value a.filename ~default:"attachment" in
  Playground_platform.export caps name a.data;
  { m with said = Printf.sprintf "saved %s, %d bytes" name (String.length a.data) }

(*****************************************************************************)
(* Nicknames *)
(*****************************************************************************)

(* a card's nickname: its name, lowercased, without spaces *)
let nickname (c : Vcard.card) : string = String.lowercase_ascii (String.concat "" (String.split_on_char ' ' c.full_name))

(* the nicknames of a To: replaced by their addresses *)
let expand (cards : Vcard.card list) (s : string) : string =
  Mail.addresses s
  |> List.concat_map (fun (a : Mail.address) ->
         match List.find_opt (fun c -> a.display = "" && nickname c = String.lowercase_ascii a.mailbox) cards with
         | Some c ->
             let one = List.length c.emails = 1 in
             List.map (fun (e : Vcard.email) -> { Mail.display = (if one then c.full_name else ""); mailbox = e.address }) c.emails
         | None -> [ a ])
  |> List.map Mail.address_to_string |> String.concat ", "

(* Special > Make Nickname: the sender of the message shown *)
let make_nickname (m : model) : model =
  match Option.map (fun e -> Mail.addresses (header e "from")) (current m) with
  | Some (a :: _) ->
      if List.exists (fun (c : Vcard.card) -> List.exists (fun (x : Vcard.email) -> x.address = a.mailbox) c.emails) m.nicknames then
        { m with said = a.mailbox ^ " has a nickname already" }
      else
        let c = { (Vcard.make (Mail.who a)) with emails = [ { address = a.mailbox; kinds = [] } ] } in
        { m with nicknames = m.nicknames @ [ c ]; said = "nickname " ^ nickname c }
  | _ -> m

(*****************************************************************************)
(* Composing *)
(*****************************************************************************)

let blank : draft =
  { to_ = ""; subject = ""; cc = ""; bcc = ""; body = Text_edit.of_string ""; attached = []; in_reply_to = None; references = []; marks = None }

(* the caret at the end, where a reply is typed *)
let at_end (s : string) : Text_edit.t = Text_edit.at (String.length s) (Text_edit.of_string s)

(* the message's text, each line after "> " (after ">" when it was
   quoted already, so that ">>" counts the replies) *)
let quoted (e : Mbox.entry) : string =
  let rec trim = function "" :: rest -> trim rest | l -> l in
  let lines = List.rev (trim (List.rev (String.split_on_char '\n' (Mime.text e.mail)))) in
  String.concat "\n" (List.map (fun l -> if l = "" then ">" else if l.[0] = '>' then ">" ^ l else "> " ^ l) lines)

let starts_ci (prefix : string) (s : string) : bool =
  String.length s >= String.length prefix && String.lowercase_ascii (String.sub s 0 (String.length prefix)) = prefix

let composing (d : draft) (m : model) : model =
  match m.pane with
  | Composing _ | Picking _ -> { m with said = "a message is being written: Queue or Cancel it first" }
  | _ -> { m with pane = Composing d }

let reply (m : model) : model =
  match current m with
  | None -> m
  | Some e ->
      let subject = header e "subject" in
      let subject = if starts_ci "re:" subject then subject else "Re: " ^ subject in
      let to_ = match Mail.get e.mail "reply-to" with Some r -> r | None -> header e "from" in
      let id = List.nth_opt (Mail.message_ids (header e "message-id")) 0 in
      (* what it answered, and it: References, or else In-Reply-To (RFC
         5322, 3.6.4) *)
      let before = match Mail.message_ids (header e "references") with [] -> Mail.message_ids (header e "in-reply-to") | l -> l in
      let body = Printf.sprintf "At %s, %s wrote:\n%s\n\n" (date_text e) (who "In" e) (quoted e) in
      composing
        { blank with to_; subject; body = at_end body; in_reply_to = id; references = before @ Option.to_list id; marks = Option.map (fun i -> (i, 'A')) id }
        m

let forward (m : model) : model =
  match current m with
  | None -> m
  | Some e ->
      let body = Printf.sprintf "\n\n> From: %s\n> Subject: %s\n> Date: %s\n>\n%s\n" (header e "from") (header e "subject") (header e "date") (quoted e) in
      let attached = List.map (fun (a : Mime.leaf) -> (Option.value a.filename ~default:"attachment", a.data)) (Mime.attachments e.mail) in
      let id = List.nth_opt (Mail.message_ids (header e "message-id")) 0 in
      composing { blank with subject = "Fwd: " ^ header e "subject"; body = Text_edit.of_string body; attached; marks = Option.map (fun i -> (i, 'F')) id } m

(* the wall clock, as a Date: header writes it *)
let now (computer : computer) : Mail.date =
  let t = match computer.time with Time t -> t in
  let offset = Playground_platform.utc_offset computer.time in
  let day, tod = Clock.local ~offset t in
  { day; time = { tod with second = Float.round (Float.of_int (int_of_float tod.second)) }; offset }

(* the original marked: R, or F *)
let mark ((id, c) : string * char) (m : model) : model =
  let marked (e : Mbox.entry) = if Mail.message_ids (header e "message-id") = [ id ] then { e with mail = Mail.set "X-Status" (header e "x-status" ^ String.make 1 c) e.mail } else e in
  { m with boxes = List.map (fun b -> { b with entries = List.map marked b.entries }) m.boxes }

(* The draft made a message, in Out, queued. Its body is a text part
   (quoted-printable if it is not ASCII), or a multipart with the
   documents attached; the headers are what Mail.mli reads back. *)
let queue (computer : computer) (d : draft) (m : model) : model =
  let to_ = expand m.nicknames d.to_ and cc = expand m.nicknames d.cc and bcc = expand m.nicknames d.bcc in
  if to_ = "" then { m with said = "no recipient: To: is empty" }
  else
    let date = now computer in
    let n = List.length (List.concat_map (fun b -> b.entries) m.boxes) in
    let unique = Printf.sprintf "%.0f.%d" (Mail.seconds date) n in
    let id = unique ^ "@tiny" in
    let signature = "\n-- \n" ^ Mail.who m.user ^ "\n" in
    let text = Text_edit.to_string d.body ^ signature in
    let content, body =
      if d.attached = [] then
        let p = Mime.text_part text in
        (("MIME-Version", "1.0") :: List.map (fun (f : Mail.field) -> (f.name, String.trim (Mail.unfold f.raw))) p.fields, p.body)
      else Mime.multipart ~boundary:("=_tiny_" ^ unique) (* no '@': RFC 2046's bchars *) (Mime.text_part text :: List.map (fun (name, bytes) -> Mime.attachment ~filename:name bytes) d.attached)
    in
    let some name v = if v = "" then [] else [ (name, v) ] in
    let fields =
      [ ("From", Mail.address_to_string m.user); ("To", to_) ]
      @ some "Cc" cc @ some "Bcc" bcc
      @ [ ("Subject", Mime.encode_words d.subject); ("Date", Mail.date_to_string date); ("Message-ID", "<" ^ id ^ ">") ]
      @ some "In-Reply-To" (match d.in_reply_to with Some r -> "<" ^ r ^ ">" | None -> "")
      @ some "References" (String.concat " " (List.map (fun r -> "<" ^ r ^ ">") d.references))
      @ content
      @ [ ("Status", "RO"); ("X-Status", "Q") ]
    in
    let e = { Mbox.envelope = Mbox.envelope ~sender:m.user.mailbox date; mail = Mail.make fields body } in
    let m = match d.marks with Some mk -> mark mk m | None -> m in
    let m = with_entries "Out" (fun l -> l @ [ e ]) m in
    { m with pane = Reading; said = "queued in Out: " ^ Mime.decode_words d.subject }

(*****************************************************************************)
(* The store *)
(*****************************************************************************)

type caps = < Cap.open_in ; Cap.open_out ; Cap.readdir >

let prefix = "eudora-"
let stored_name (box : string) : string = prefix ^ box ^ ".mbox"
let nicknames_name = prefix ^ "nicknames.vcf"

(* the built-in mailboxes, or what the store has of them, then the
   user's own *)
let load (caps : caps) (m : model) : model =
  let fetch = Playground_platform.fetch caps in
  let builtin = List.map (fun b -> match fetch (stored_name b.name) with Some t -> { b with entries = Mbox.parse t } | None -> b) m.boxes in
  let own =
    List.filter_map
      (fun n ->
        let p = String.length prefix and k = String.length n in
        if k > p + 5 && String.sub n 0 p = prefix && String.sub n (k - 5) 5 = ".mbox" then
          let name = String.sub n p (k - p - 5) in
          if List.exists (fun b -> b.name = name) builtin then None else Option.map (fun t -> { name; entries = Mbox.parse t }) (fetch n)
        else None)
      (Playground_platform.stored caps)
  in
  let nicknames = match fetch nicknames_name with Some t -> Vcard.of_string t | None -> m.nicknames in
  let boxes = builtin @ own in
  { m with boxes; saved = boxes; nicknames; saved_nicknames = nicknames }

(* what changed, written *)
let keep (caps : caps) (m : model) : model =
  if m.boxes == m.saved && m.nicknames == m.saved_nicknames then m
  else (
    List.iter
      (fun b ->
        match List.find_opt (fun s -> s.name = b.name) m.saved with
        | Some s when s.entries = b.entries -> ()
        | _ -> Playground_platform.store caps (stored_name b.name) (Mbox.to_string b.entries))
      m.boxes;
    if m.nicknames <> m.saved_nicknames then Playground_platform.store caps nicknames_name (Vcard.to_string m.nicknames);
    { m with saved = m.boxes; saved_nicknames = m.nicknames })

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let menu_mailbox (m : model) = ("Mailbox" :: List.map (fun b -> b.name) m.boxes) @ [ "New..." ]
let menu_message = [ "Message"; "New Message"; "Reply"; "Forward"; "Attach Document..."; "Delete"; "Blah Blah Blah" ]
let menu_transfer (m : model) = "Transfer" :: List.map (fun b -> "-> " ^ b.name) m.boxes
let menu_special = [ "Special"; "Empty Trash"; "Sort by arrival"; "Make Nickname"; "Nicknames" ]

let menu_box (i : int) : Widget.box = { Widget.x = -405. +. (float_of_int i *. 155.); y = bar_y; w = 150.; h = 28. }

let menus (caps : caps) (computer : computer) (m : model) : model =
  let chose items i = List.nth_opt items (Gui.menu_in computer (menu_box i) items 0) in
  let m =
    match chose (menu_mailbox m) 0 with
    | Some "New..." -> { m with pane = Naming "" }
    | Some name when name <> "Mailbox" -> open_box name { m with pane = (match m.pane with Reading | Nicknames | Naming _ -> Reading | p -> p) }
    | _ -> m
  in
  let m =
    match chose menu_message 1 with
    | Some "New Message" -> composing blank m
    | Some "Reply" -> reply m
    | Some "Forward" -> forward m
    | Some "Attach Document..." -> (
        match m.pane with
        | Composing d ->
            let names = List.filter (fun n -> not (String.length n > String.length prefix && String.sub n 0 (String.length prefix) = prefix)) (Playground_platform.stored caps) in
            { m with pane = Picking (d, names, None) }
        | _ -> { m with said = "attach to a message being written: New Message first" })
    | Some "Delete" -> delete m
    | Some "Blah Blah Blah" -> { m with blah = not m.blah }
    | _ -> m
  in
  let m =
    match chose (menu_transfer m) 2 with
    | Some s when String.length s > 3 && String.sub s 0 3 = "-> " -> transfer (String.sub s 3 (String.length s - 3)) m
    | _ -> m
  in
  match chose menu_special 3 with
  | Some "Empty Trash" ->
      let m = with_entries "Trash" (fun _ -> []) m in
      { (if m.box = "Trash" then { m with selected = None } else m) with said = "the Trash emptied" }
  | Some "Sort by arrival" -> { m with sort = None }
  | Some "Make Nickname" -> make_nickname m
  | Some "Nicknames" -> (match m.pane with Reading | Naming _ -> { m with pane = Nicknames } | _ -> m)
  | _ -> m

let flag (computer : computer) (name : string) : string option = List.assoc_opt name computer.flags

let clicked (caps : caps) (computer : computer) (m : model) : model =
  let x = computer.mouse.mx and y = computer.mouse.my in
  if y <= list_top -. 22. && y > first_row_top && x > left && x < right then
    (* a column's title: sorted by it, or back *)
    match column_at x with Some c -> { m with sort = (if m.sort = Some c then None else Some c) } | None -> m
  else if y <= first_row_top && y > list_bottom && x > left && x < right then
    let k = int_of_float ((first_row_top -. y) /. row_h) + first_shown m in
    match List.nth_opt (order m) k with Some i -> select (Some i) { m with pane = (if m.pane = Nicknames then Reading else m.pane) } | None -> m
  else
    match current m with
    | Some e when m.pane = Reading && y < body_top && y > msg_bottom -> (
        let hit = List.find_opt (fun (r, top) -> (match r with Attachment _ -> true | _ -> false) && y <= top && y > top -. line_h) (placed m e) in
        match hit with Some (Attachment a, _) -> save caps a m | _ -> m)
    | _ -> m

(* the composition window's rows, from the top: the fields, and the
   buttons on the right *)
let form_top = msg_top -. 22.
let form_row (i : int) : float = form_top -. 18. -. (float_of_int i *. 28.)
let field_box (i : int) : Widget.box = { Widget.x = left +. 420.; y = form_row i; w = 600.; h = 24. }
let button_box (i : int) : Widget.box = { Widget.x = right -. 90.; y = form_row i; w = 150.; h = 24. }
let body_box : Widget.box =
  let top = form_row 5 -. 22. and bottom = msg_bottom +. 8. in
  { Widget.x = 0.; y = (top +. bottom) /. 2.; w = right -. left -. 20.; h = top -. bottom }

(* the fields, the body and the buttons of the message being written *)
let compose (computer : computer) (d : draft) (m : model) : model =
  let to_ = Gui.field_in computer (field_box 0) d.to_ in
  let subject = Gui.field_in computer (field_box 2) d.subject in
  let cc = Gui.field_in computer (field_box 3) d.cc in
  let bcc = Gui.field_in computer (field_box 4) d.bcc in
  let body = Gui.text_area_in computer body_box d.body in
  let d = { d with to_; subject; cc; bcc; body } in
  let m = { m with pane = Composing d } in
  if Gui.button_in computer (button_box 0) "Queue" then queue computer d m
  else if Gui.button_in computer (button_box 2) "Cancel" then { m with pane = Reading; said = "not queued" }
  else m

(* the store's documents, one to attach *)
let pick (caps : caps) (computer : computer) (d : draft) (names : string list) (sel : int option) (m : model) : model =
  let sel = Gui.list_in computer { Widget.x = 0.; y = (msg_top +. msg_bottom) /. 2.; w = 500.; h = 400. } names sel in
  let m = { m with pane = Picking (d, names, sel) } in
  if Gui.button_in computer (button_box 0) "Attach" then
    match Option.bind sel (List.nth_opt names) with
    | Some name -> (
        match Playground_platform.fetch caps name with
        | Some bytes -> { m with pane = Composing { d with attached = d.attached @ [ (name, bytes) ] }; said = "attached " ^ name }
        | None -> m)
    | None -> { m with said = "choose a document" }
  else if Gui.button_in computer (button_box 1) "Cancel" then { m with pane = Composing d }
  else m

(* Mailbox > New...: a name, and an empty mailbox of that name *)
let naming (computer : computer) (name : string) (m : model) : model =
  let name = Gui.field_in computer (field_box 1) name in
  let m = { m with pane = Naming name } in
  let ok = name <> "" && String.for_all (fun c -> c = ' ' || c = '-' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) name in
  if Gui.button_in computer (button_box 0) "Create" || (computer.keyboard.kenter && ok) then
    if not ok then { m with said = "a name of letters, digits and spaces" }
    else if List.exists (fun b -> b.name = name) m.boxes then { m with said = name ^ " exists already" }
    else open_box name { m with boxes = m.boxes @ [ { name; entries = [] } ]; pane = Reading; said = "new mailbox " ^ name }
  else if Gui.button_in computer (button_box 1) "Cancel" then { m with pane = Reading }
  else m

(* Macintosh's black and white, for the toolkit's widgets too *)
let mac_theme = { Theme.default with background = white; face = white; face_hot = rgb 225 225 225; face_down = rgb 190 190 190; edge = black; text = black; accent = black; field_face = white; text_size = 14.; row = 28. }

let start (caps : caps) (computer : computer) (m : model) : model =
  let m = load caps { m with started = true } in
  let m = match Option.bind (flag computer "user") Mail.address with Some user -> { m with user } | None -> m in
  let m = match flag computer "mailbox" with Some name when List.exists (fun b -> b.name = name) m.boxes -> { m with box = name } | _ -> m in
  let n = List.length (entries m) in
  let m =
    match Option.bind (flag computer "message") int_of_string_opt with
    | Some k when k >= 1 && k <= n -> select (Some (k - 1)) m
    | _ -> select (if n > 0 then Some (n - 1) else None) m
  in
  match flag computer "compose" with
  | Some "new" -> composing blank m
  | Some "reply" -> reply m
  | Some "forward" -> forward m
  | _ -> m

let update (caps : caps) (computer : computer) (m : model) : model =
  Gui.set_theme mac_theme;
  let m = if m.started then m else start caps computer m in
  let m = menus caps computer m in
  let m =
    match m.pane with
    | Composing d -> compose computer d m
    | Picking (d, names, sel) -> pick caps computer d names sel m
    | Naming name -> naming computer name m
    | Nicknames -> if Gui.button_in computer (button_box 0) "Close" then { m with pane = Reading } else m
    | Reading -> m
  in
  let now = Set_.elements computer.keyboard.keys in
  let pressed key = List.mem key now && not (List.mem key m.was) in
  let down = computer.mouse.mdown && not m.was_down in
  let m = if down && not (Gui.modal ()) then clicked caps computer m else m in
  let ord = order m in
  let pos = Option.bind m.selected (fun i -> position i ord) in
  let step d = match pos with Some p -> List.nth_opt ord (max 0 (min (List.length ord - 1) (p + d))) | None -> List.nth_opt ord 0 in
  let reading = m.pane = Reading || m.pane = Nicknames in
  let m =
    if Gui.modal () || not reading then m
    else if pressed "ArrowDown" then select (step 1) m
    else if pressed "ArrowUp" then select (step (-1)) m
    else if pressed "Delete" || pressed "Backspace" then delete m
    else if pressed "b" then { m with blah = not m.blah; scroll = 0. }
    else m
  in
  (* the message scrolled: a page, or the wheel's notches *)
  let page = body_top -. msg_bottom -. line_h in
  let scroll = m.scroll +. (if pressed "PageDown" then page else 0.) -. (if pressed "PageUp" then page else 0.) +. (computer.mouse.mwheel *. -3. *. line_h) in
  let most = match current m with Some e -> max 0. (total_height m e -. page) | None -> 0. in
  keep caps { m with scroll = max 0. (min most scroll); was = now; was_down = computer.mouse.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let paper = white
let ink = black

(* a Macintosh window: a black frame, the title bar's stripes, the
   title in a white gap in them, the close box on the left *)
let window (title : string) (top : float) (bottom : float) : shape list =
  let w = right -. left and h = top -. bottom and cx = (left +. right) /. 2. in
  let tw = width (look 13.) title +. 20. in
  [ rectangle ink (w +. 2.) (h +. 2.) |> move cx ((top +. bottom) /. 2.); rectangle paper w h |> move cx ((top +. bottom) /. 2.) ]
  @ List.init 6 (fun i -> rectangle ink (w -. 8.) 1. |> move cx (top -. 5. -. (float_of_int i *. 2.5)))
  @ [ rectangle paper tw 20. |> move cx (top -. 11.); rectangle ink w 1. |> move cx (top -. 21.) ]
  @ [ rectangle paper 22. 20. |> move (left +. 18.) (top -. 11.); rectangle ink 12. 12. |> move (left +. 18.) (top -. 11.); rectangle paper 10. 10. |> move (left +. 18.) (top -. 11.) ]
  @ text ink (cx -. (tw /. 2.) +. 10.) (top -. 16.) title

let column_titles (m : model) : shape list =
  (rectangle ink (right -. left) 1. |> move 0. (first_row_top +. 1.))
  :: List.concat_map (fun (c, (x, t)) -> text ~bold:(m.sort = Some c) ~size:12. ink x (columns_y -. 5.) t) column_x

let list_row (m : model) (k : int) (i : int) (e : Mbox.entry) : shape list =
  let top = first_row_top -. (float_of_int k *. row_h) in
  let on = m.selected = Some i in
  let fg, bg = if on then (paper, ink) else (ink, paper) in
  let y = top -. 15. in
  let x c = fst (List.assoc c column_x) in
  let s = status e in
  let unread = s = "*" in
  (if on then [ rectangle bg (right -. left -. 4.) row_h |> move 0. (top -. (row_h /. 2.)) ] else [])
  @ (if unread then [ circle fg 4. |> move (x Status +. 4.) (top -. 10.) ] else text fg (x Status) y s)
  @ text ~bold:unread fg (x Who) y (fit 190. (ascii (who m.box e)))
  @ text fg (x Date) y (date_text e)
  @ text fg (x Size) y (string_of_int (size e))
  @ text ~bold:unread fg (x Subject) y (fit 390. (ascii (header e "subject")))
  @
  if attached e then
    (* a page with its corner folded: something enclosed *)
    [ rectangle fg 10. 13. |> move (right -. 14.) (top -. 10.); rectangle bg 8. 11. |> move (right -. 14.) (top -. 10.); rectangle fg 4. 1. |> move (right -. 14.) (top -. 8.); rectangle fg 4. 1. |> move (right -. 14.) (top -. 11.) ]
  else []

let message_view (m : model) (e : Mbox.entry) : shape list =
  List.concat_map
    (fun (r, top) ->
      if top > body_top +. 1. || top -. row_height r < msg_bottom then []
      else
        match r with
        | Line (s, bold) -> text ~bold ink (left +. 20.) (top -. 13.) s
        | Attachment a ->
            let label = Printf.sprintf "Attachment: %s (%s, %d bytes) -- click to save" (ascii (Option.value a.filename ~default:"attachment")) a.mime (String.length a.data) in
            text ~bold:true ink (left +. 20.) (top -. 13.) label
        | Picture p ->
            let w = float_of_int p.width *. 2. and h = float_of_int p.height *. 2. in
            [ bitmap w h p |> move (left +. 20. +. (w /. 2.)) (top -. 4. -. (h /. 2.)) ])
    (placed m e)

(* The shapes of the list and of the message are made again only when
   what they show changed -- the mailbox (the same list, ==, since an
   update that changes nothing returns it untouched), the selection,
   the order, the scroll: most frames draw the ones before *)
let list_cache : (Mbox.entry list * int option * column option * string * shape list) option ref = ref None
let message_cache : (Mbox.entry * bool * float * shape list) option ref = ref None

let list_view (m : model) : shape list =
  let l = entries m in
  match !list_cache with
  | Some (l', sel, sort, box, shapes) when l' == l && sel = m.selected && sort = m.sort && box = m.box -> shapes
  | _ ->
      let first = first_shown m in
      let shapes =
        List.concat (List.mapi (fun k i -> if k >= first && k < first + rows then list_row m (k - first) i (List.nth l i) else []) (order m))
      in
      list_cache := Some (l, m.selected, m.sort, m.box, shapes);
      shapes

let shown_message (m : model) (e : Mbox.entry) : shape list =
  match !message_cache with
  | Some (e', blah, scroll, shapes) when e' == e && blah = m.blah && scroll = m.scroll -> shapes
  | _ ->
      let shapes = message_view m e in
      message_cache := Some (e, m.blah, m.scroll, shapes);
      shapes

(* Eudora's title for a message being written: its first recipient and
   its subject, "No Recipient, No Subject" until then *)
let draft_title (d : draft) : string =
  let to_ = match Mail.addresses d.to_ with a :: _ -> Mail.who a | [] -> "No Recipient" in
  to_ ^ ", " ^ if d.subject = "" then "No Subject" else d.subject

let label (i : int) (s : string) : shape list = text ~bold:true ink (left +. 20.) (form_row i -. 5.) s

let compose_view (m : model) (d : draft) : shape list =
  window (ascii (draft_title d)) msg_top msg_bottom
  @ label 0 "To:" @ label 1 "From:" @ label 2 "Subject:" @ label 3 "Cc:" @ label 4 "Bcc:" @ label 5 "Attachments:"
  @ text ink (left +. 125.) (form_row 1 -. 5.) (ascii (Mail.address_to_string m.user))
  @ text ink (left +. 125.) (form_row 5 -. 5.) (fit 700. (ascii (String.concat ", " (List.map fst d.attached))))
  @ [ rectangle ink (right -. left) 1. |> move 0. (form_row 5 -. 16.) ]

let nicknames_view (m : model) : shape list =
  window "Nicknames" msg_top msg_bottom
  @ List.concat
      (List.mapi
         (fun i (c : Vcard.card) ->
           let y = body_top -. 13. -. (float_of_int i *. line_h) in
           text ~bold:true ink (left +. 20.) y (nickname c)
           @ text ink (left +. 160.) y (fit 600. (ascii (String.concat ", " (List.map (fun (e : Vcard.email) -> e.address) c.emails)))))
         m.nicknames)

let view (_ : computer) (m : model) : shape list =
  let th = Gui.theme () in
  let n = List.length (entries m) in
  let unread = List.length (List.filter (fun e -> not (is_read e)) (entries m)) in
  let message =
    match (m.pane, current m) with
    | Composing d, _ -> compose_view m d
    | Picking (d, _, _), _ -> window ("Attach to: " ^ ascii (draft_title d)) msg_top msg_bottom @ label 0 "A document of the store:"
    | Naming _, _ -> window "New Mailbox" msg_top msg_bottom @ label 1 "Name:"
    | Nicknames, _ -> nicknames_view m
    | Reading, Some e ->
        window (ascii (Printf.sprintf "%s, %s, %s" (who m.box e) (date_text e) (header e "subject"))) msg_top msg_bottom
        @ shown_message m e
        (* the title bar again, over the rows scrolled up under it *)
        @ [ rectangle paper (right -. left) 10. |> move 0. (body_top +. 4.) ]
    | Reading, None -> window "" msg_top msg_bottom
  in
  [ rectangle (rgb 160 160 160) 1000. 1000.; rectangle th.face 1000. 30. |> move 0. bar_y; rectangle ink 1000. 1. |> move 0. (bar_y -. 15.) ]
  @ message
  @ window (Printf.sprintf "%s  (%d messages, %d unread)" m.box n unread) list_top list_bottom
  @ column_titles m @ list_view m
  @ text ink (left +. 640.) (bar_y -. 5.) (if m.said <> "" then m.said else "TinyEudora")
  @ Gui.draw ()

let app (caps : caps) = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> caps)))
