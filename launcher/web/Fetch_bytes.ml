(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Fetch_bytes.mli; moved from Tinybox_web *)

(* claude: the response's bytes as an OCaml string, built by the browser
 * a slice at a time (String.fromCharCode of 32 KB), then taken as it is
 * (Js.to_bytestring, a byte a character). The platform's fetch_web does
 * it a byte at a time (String.init over the Uint8Array): for 10 MB, 1.4 s
 * of the page frozen, most of it the garbage collector; this, a few
 * dozen ms.
 *
 *   old: String.init n (fun i -> Char.chr (Ojs.int_of_js (Ojs.array_get bytes i)))
 *)
let bytes_of_response (response : Ojs.t) : string =
  let bytes = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Uint8Array") [| response |] in
  let n = Ojs.int_of_js (Ojs.get_prop_ascii bytes "length") in
  let from_char_code = Ojs.get_prop_ascii (Ojs.get_prop_ascii Ojs.global "String") "fromCharCode" in
  let chunk = 32768 in
  let parts =
    List.init ((n + chunk - 1) / chunk) (fun k ->
        let slice = Ojs.call bytes "subarray" [| Ojs.int_to_js (k * chunk); Ojs.int_to_js (min n ((k + 1) * chunk)) |] in
        Ojs.call from_char_code "apply" [| Ojs.null; slice |])
  in
  let whole = Ojs.call (Ojs.list_to_js (fun x -> x) parts) "join" [| Ojs.string_to_js "" |] in
  (* claude: an Ojs.t is a JavaScript value as it is, as a Js.t is:
   * the two libraries' types for the same thing *)
  Js_of_ocaml.Js.to_bytestring (Obj.magic whole : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t)

(* an XMLHttpRequest for bytes, as the web platform's fetch_web *)
let get (url : string) ~(ok : string -> unit) ~(failed : string -> unit) : unit =
  let xhr = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "XMLHttpRequest") [||] in
  ignore (Ojs.call xhr "open" [| Ojs.string_to_js "GET"; Ojs.string_to_js url |]);
  Ojs.set_prop_ascii xhr "responseType" (Ojs.string_to_js "arraybuffer");
  Ojs.set_prop_ascii xhr "onload"
    (Ojs.fun_to_js 1 (fun _ ->
         let status = Ojs.int_of_js (Ojs.get_prop_ascii xhr "status") in
         if status >= 200 && status < 300 then ok (bytes_of_response (Ojs.get_prop_ascii xhr "response"))
         else failed (Printf.sprintf "not found (%d)" status)));
  Ojs.set_prop_ascii xhr "onerror" (Ojs.fun_to_js 1 (fun _ -> failed "no answer"));
  ignore (Ojs.call xhr "send" [||])
