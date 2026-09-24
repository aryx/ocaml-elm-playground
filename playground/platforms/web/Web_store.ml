(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Web_store.mli *)

(* Base64, done here rather than with the browser's btoa: btoa takes a
   JavaScript string, and Marshal's bytes are not UTF-8, so they would
   not survive the trip into one *)
let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64 s =
  let n = String.length s in
  let b = Buffer.create ((n + 2) / 3 * 4) in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let i = ref 0 in
  while !i < n do
    let v = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
    for k = 0 to 3 do
      (* the last group's missing bytes are '=' *)
      if !i + k - 1 < n || k < 2 then Buffer.add_char b alphabet.[(v lsr (18 - (6 * k))) land 63] else Buffer.add_char b '='
    done;
    i := !i + 3
  done;
  Buffer.contents b

let of_base64 s =
  let value c = String.index_opt alphabet c in
  let n = String.length s in
  if n mod 4 <> 0 then None
  else
    let b = Buffer.create (n / 4 * 3) in
    let rec go i =
      if i >= n then Some (Buffer.contents b)
      else
        let v k = if s.[i + k] = '=' then Some 0 else value s.[i + k] in
        match (v 0, v 1, v 2, v 3) with
        | Some a, Some c, Some d, Some e ->
            let x = (a lsl 18) lor (c lsl 12) lor (d lsl 6) lor e in
            Buffer.add_char b (Char.chr ((x lsr 16) land 255));
            if s.[i + 2] <> '=' then Buffer.add_char b (Char.chr ((x lsr 8) land 255));
            if s.[i + 3] <> '=' then Buffer.add_char b (Char.chr (x land 255));
            go (i + 4)
        | _ -> None
    in
    go 0

let prefix = "elm-playground:"

let storage () =
  match Js_browser.Window.local_storage Js_browser.window with
  | Some st -> st
  | None -> failwith "no localStorage in this browser"

let store name bytes = Js_browser.Storage.set_item (storage ()) (prefix ^ name) (base64 bytes)
let fetch name = Option.bind (Js_browser.Storage.get_item (storage ()) (prefix ^ name)) of_base64

let stored () =
  let st = storage () in
  let p = String.length prefix in
  List.init (Js_browser.Storage.length st) (Js_browser.Storage.key st)
  |> List.filter_map (function Some k when String.length k > p && String.sub k 0 p = prefix -> Some (String.sub k p (String.length k - p)) | _ -> None)
  |> List.sort compare

let export name bytes =
  let a = Js_browser.Document.create_element Js_browser.document "a" in
  Js_browser.Element.set_attribute a "href" ("data:application/octet-stream;base64," ^ base64 bytes);
  Js_browser.Element.set_attribute a "download" name;
  Js_browser.Element.click a
