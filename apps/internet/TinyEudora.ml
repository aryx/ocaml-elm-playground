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
 * and the menus: Mailbox (open one), Message (Delete, Blah Blah
 * Blah), Transfer (Eudora's name for moving a message to a mailbox),
 * Special (Empty Trash). Flag message=n: the n-th message of In
 * opened at the start.
 *
 * With no server, it opens on the built-in mailboxes, Our_mail --
 * messages written for what they show: a thread of five replies, a
 * forged sender, a digest of two messages, a picture attached,
 * quoted-printable French with an encoded-word subject, a ">From" line.
 *
 * What it uses: networking_mail (Mail, Mime, Mbox), the picture
 * decoders (Png, Gif, Jpeg, as the browsers'), Stroke_text (text from
 * the left, with Hershey's real widths), the playground's menus. Not
 * the network yet, nor the File menu: composing, the queue and
 * Check Mail are the plan's next phases.
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

type model = {
  boxes : box list;
  box : string; (* the mailbox shown *)
  selected : int option; (* the message, in the mailbox's own order *)
  sort : column option; (* None: the order of arrival *)
  blah : bool; (* every header *)
  scroll : float; (* the message, scrolled, in pixels *)
  said : string;
  started : bool; (* the flags read *)
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
(* Update *)
(*****************************************************************************)

let menu_mailbox (m : model) = "Mailbox" :: List.map (fun b -> b.name) m.boxes
let menu_message = [ "Message"; "Delete"; "Blah Blah Blah" ]
let menu_transfer (m : model) = "Transfer" :: List.map (fun b -> "-> " ^ b.name) m.boxes
let menu_special = [ "Special"; "Empty Trash"; "Sort by arrival" ]

let menu_box (i : int) : Widget.box = { Widget.x = -420. +. (float_of_int i *. 120.); y = bar_y; w = 114.; h = 28. }

let menus (computer : computer) (m : model) : model =
  let chose items i = List.nth_opt items (Gui.menu_in computer (menu_box i) items 0) in
  let m = match chose (menu_mailbox m) 0 with Some name when name <> "Mailbox" -> open_box name m | _ -> m in
  let m = match chose menu_message 1 with Some "Delete" -> delete m | Some "Blah Blah Blah" -> { m with blah = not m.blah } | _ -> m in
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
  | _ -> m

let flag (computer : computer) (name : string) : string option = List.assoc_opt name computer.flags

let clicked (caps : < Cap.open_out ; .. >) (computer : computer) (m : model) : model =
  let x = computer.mouse.mx and y = computer.mouse.my in
  if y <= list_top -. 22. && y > first_row_top && x > left && x < right then
    (* a column's title: sorted by it, or back *)
    match column_at x with Some c -> { m with sort = (if m.sort = Some c then None else Some c) } | None -> m
  else if y <= first_row_top && y > list_bottom && x > left && x < right then
    let k = int_of_float ((first_row_top -. y) /. row_h) + first_shown m in
    match List.nth_opt (order m) k with Some i -> select (Some i) m | None -> m
  else
    match current m with
    | Some e when y < body_top && y > msg_bottom -> (
        let hit = List.find_opt (fun (r, top) -> (match r with Attachment _ -> true | _ -> false) && y <= top && y > top -. line_h) (placed m e) in
        match hit with Some (Attachment a, _) -> save caps a m | _ -> m)
    | _ -> m

let update (caps : < Cap.open_out ; .. >) (computer : computer) (m : model) : model =
  let m =
    if m.started then m
    else
      let m = { m with started = true } in
      let n = List.length (entries m) in
      match Option.bind (flag computer "message") int_of_string_opt with
      | Some k when k >= 1 && k <= n -> select (Some (k - 1)) m
      | _ -> select (if n > 0 then Some (n - 1) else None) m
  in
  let m = menus computer m in
  let now = Set_.elements computer.keyboard.keys in
  let pressed key = List.mem key now && not (List.mem key m.was) in
  let down = computer.mouse.mdown && not m.was_down in
  let m = if down && not (Gui.modal ()) then clicked caps computer m else m in
  let ord = order m in
  let pos = Option.bind m.selected (fun i -> position i ord) in
  let step d = match pos with Some p -> List.nth_opt ord (max 0 (min (List.length ord - 1) (p + d))) | None -> List.nth_opt ord 0 in
  let m =
    if Gui.modal () then m
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
  { m with scroll = max 0. (min most scroll); was = now; was_down = computer.mouse.mdown }

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

let view (_ : computer) (m : model) : shape list =
  let th = Gui.theme () in
  let n = List.length (entries m) in
  let unread = List.length (List.filter (fun e -> not (is_read e)) (entries m)) in
  let message =
    match current m with
    | Some e ->
        window (ascii (Printf.sprintf "%s, %s, %s" (who m.box e) (date_text e) (header e "subject"))) msg_top msg_bottom
        @ shown_message m e
        (* the title bar again, over the rows scrolled up under it *)
        @ [ rectangle paper (right -. left) 10. |> move 0. (body_top +. 4.) ]
    | None -> window "" msg_top msg_bottom
  in
  [ rectangle (rgb 160 160 160) 1000. 1000.; rectangle th.face 1000. 30. |> move 0. bar_y; rectangle ink 1000. 1. |> move 0. (bar_y -. 15.) ]
  @ message
  @ window (Printf.sprintf "%s  (%d messages, %d unread)" m.box n unread) list_top list_bottom
  @ column_titles m @ list_view m
  @ text ink (left +. 500.) (bar_y -. 5.) (if m.said <> "" then m.said else "TinyEudora")
  @ Gui.draw ()

let app (caps : < Cap.open_out ; .. >) = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app caps))
