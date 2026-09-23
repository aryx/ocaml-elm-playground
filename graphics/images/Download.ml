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

(* http:// by our own client; curl is for https:// only *)
let http_url fname url =
  match Http_client.get url with
  | Ok (r : Http.response) when r.status / 100 = 2 -> Out_channel.with_open_bin fname (fun oc -> Out_channel.output_string oc r.body)
  | Ok r -> failwith (Printf.sprintf "%s: %d %s" url r.status r.reason)
  | Error msg -> failwith msg

let has_prefix (src : string) (p : string) : bool =
  String.length src >= String.length p && String.sub src 0 (String.length p) = p

let is_url (src : string) : bool =
  has_prefix src "http://" || has_prefix src "https://"

let local_file ~prefix (src : string) : string =
  if is_url src then begin
    let fn = Filename.temp_file prefix (Filename.extension src) in
    if has_prefix src "http://" then http_url fn src else curl_url fn src;
    fn
  end
  else src
