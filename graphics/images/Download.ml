(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Download.mli *)

(* from ocurl/examples/opar.ml *)
let writer accum data =
  Buffer.add_string accum data;
  String.length data

let save fname content =
  let fp = open_out_bin fname in
    Buffer.output_buffer fp content;
    close_out fp

let curl_url fname url =
  let result = Buffer.create 16384 in
  let conn = Curl.init () in
  Curl.set_writefunction conn (writer result);
  Curl.set_followlocation conn true;
  Curl.set_url conn url;
  Curl.perform conn;
  Curl.cleanup conn;
  save fname result

let is_url (src : string) : bool =
  let has_prefix p =
    String.length src >= String.length p && String.sub src 0 (String.length p) = p
  in
  has_prefix "http://" || has_prefix "https://"

let local_file ~prefix (src : string) : string =
  if is_url src then begin
    let fn = Filename.temp_file prefix (Filename.extension src) in
    curl_url fn src;
    fn
  end
  else src
