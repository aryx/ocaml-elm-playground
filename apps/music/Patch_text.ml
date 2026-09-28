(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Patch_text.mli *)

type 'p knob = { name : string; control : Control.t; get : 'p -> float; put : 'p -> float -> 'p }

let knob name get put = { name; control = Knob (0., 1.); get; put }
let detune name get put = { name; control = Knob (-1., 1.); get; put }

let switch name get put =
  { name; control = Switch; get = (fun p -> Control.of_bool (get p)); put = (fun p x -> put p (Control.on x)) }

let selector name labels get put =
  { name; control = Selector labels; get = (fun p -> float_of_int (get p)); put = (fun p x -> put p (Control.index x)) }

let to_string (knobs : 'p knob list) (p : 'p) : string =
  String.concat "" (List.map (fun k -> Printf.sprintf "%s = %s\n" k.name (Control.to_string k.control (k.get p))) knobs)

let of_string (knobs : 'p knob list) ~(initial : 'p) (text : string) : ('p, string) result =
  let line (acc : ('p, string) result) (l : string) =
    match acc with
    | Error _ -> acc
    | Ok p -> (
        let l = match String.index_opt l '#' with Some i -> String.sub l 0 i | None -> l in
        match String.index_opt l '=' with
        | None -> if String.trim l = "" then acc else Error ("not \"name = value\": " ^ String.trim l)
        | Some i -> (
            let name = String.trim (String.sub l 0 i) and v = String.trim (String.sub l (i + 1) (String.length l - i - 1)) in
            match List.find_opt (fun k -> k.name = name) knobs with
            | None -> Error ("no such control: " ^ name)
            | Some k -> (
                match Control.of_string k.control v with
                | Some x -> Ok (k.put p x)
                | None -> Error (Printf.sprintf "%s: not a value: %s" name v))))
  in
  List.fold_left line (Ok initial) (String.split_on_char '\n' text)
