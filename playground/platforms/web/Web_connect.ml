(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Web_connect.mli *)

(* claude: Multiplayer's net=relay in a browser: its own WebSocket
 * (what networking/unix/Relay_client.ml does by hand natively), the
 * packets as binary messages (ArrayBuffers); the relay's first one, 02
 * then a number, says which player this is. A web page has no UDP:
 * net=host and net=join are for native programs. *)
let connect (_caps : Cap.network) (role : Transport.role) : (Transport.t, string) result =
  match role with
  | Host _ | Join _ -> Error "a browser has no UDP: use net=relay"
  | Relay { host; port } -> (
      let url = Printf.sprintf "ws://%s:%d/" host port in
      match Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "WebSocket") [| Ojs.string_to_js url |] with
      | exception _ -> Error ("can't open " ^ url)
      | ws ->
          Ojs.set_prop_ascii ws "binaryType" (Ojs.string_to_js "arraybuffer");
          let inbox = ref [] and player = ref None and closed = ref false and waiting = ref [] in
          let send_now s =
            let bytes = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Uint8Array") [| Ojs.int_to_js (String.length s) |] in
            String.iteri (fun i c -> Ojs.array_set bytes i (Ojs.int_to_js (Char.code c))) s;
            ignore (Ojs.call ws "send" [| bytes |])
          in
          Ojs.set_prop_ascii ws "onclose" (Ojs.fun_to_js 1 (fun _ -> closed := true));
          (* what was sent before the connection opened, now *)
          Ojs.set_prop_ascii ws "onopen"
            (Ojs.fun_to_js 1 (fun _ ->
                 List.iter send_now (List.rev !waiting);
                 waiting := []));
          Ojs.set_prop_ascii ws "onmessage"
            (Ojs.fun_to_js 1 (fun ev ->
                 let bytes = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Uint8Array") [| Ojs.get_prop_ascii ev "data" |] in
                 let n = Ojs.int_of_js (Ojs.get_prop_ascii bytes "length") in
                 let s = String.init n (fun i -> Char.chr (Ojs.int_of_js (Ojs.array_get bytes i))) in
                 if !player = None && n = 2 && s.[0] = '\002' then player := Some (Char.code s.[1]) else inbox := s :: !inbox));
          Ok
            {
              send =
                (fun s ->
                  (* once open (readyState 1) at once; before, kept for
                   * onopen *)
                  if Ojs.int_of_js (Ojs.get_prop_ascii ws "readyState") = 1 then send_now s else waiting := s :: !waiting);
              receive =
                (fun () ->
                  let packets = List.rev !inbox in
                  inbox := [];
                  packets);
              status =
                (fun () ->
                  match (!closed, !player) with
                  | true, _ -> Printf.sprintf "%s closed the connection (full, or not running?)" url
                  | false, None -> Printf.sprintf "connecting to %s" url
                  | false, Some _ -> Printf.sprintf "through %s" url);
              player = (fun () -> !player);
            })
