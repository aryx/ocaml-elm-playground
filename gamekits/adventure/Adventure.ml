(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Adventure.mli *)

type place = Room of string | Carried | Inside of string | Nowhere
type world = { here : string; places : (string * place) list; flags : string list; score : int; turns : int }

let start (room : string) (places : (string * place) list) : world = { here = room; places; flags = []; score = 0; turns = 0 }
let where (w : world) (obj : string) : place = match List.assoc_opt obj w.places with Some p -> p | None -> Nowhere

let put (obj : string) (p : place) (w : world) : world =
  { w with places = List.map (fun (o, q) -> if o = obj then (o, p) else (o, q)) w.places }

let go (room : string) (w : world) : world = { w with here = room }
let has (w : world) (flag : string) : bool = List.mem flag w.flags
let set (flag : string) (w : world) : world = if has w flag then w else { w with flags = flag :: w.flags }
let unset (flag : string) (w : world) : world = { w with flags = List.filter (( <> ) flag) w.flags }

let rec visible (w : world) (obj : string) : bool =
  match where w obj with
  | Room r -> r = w.here
  | Carried -> true
  | Inside c -> has w (c ^ " open") && visible w c
  | Nowhere -> false

let in_room (w : world) : string list = List.filter_map (fun (o, p) -> if p = Room w.here then Some o else None) w.places
let carried (w : world) : string list = List.filter_map (fun (o, p) -> if p = Carried then Some o else None) w.places

type sentence = { verb : string; obj : string option; with_ : string option }

type rule = {
  verb : string;
  obj : string option;
  with_ : string option;
  test : sentence -> world -> bool;
  act : sentence -> world -> world * string;
}

let any = Some "*"
let always (_ : sentence) (_ : world) : bool = true

let matches (pattern : string option) (o : string option) : bool =
  match (pattern, o) with Some "*", Some _ -> true | _ -> pattern = o

let run (rules : rule list) (s : sentence) (w : world) : world * string =
  let w = { w with turns = w.turns + 1 } in
  let unseen = List.filter (fun o -> not (visible w o)) (Option.to_list s.obj @ Option.to_list s.with_) in
  match unseen with
  | o :: _ -> (w, "You see no " ^ o ^ " here.")
  | [] -> (
      match
        List.find_opt (fun (r : rule) -> r.verb = s.verb && matches r.obj s.obj && matches r.with_ s.with_ && r.test s w) rules
      with
      | Some r -> r.act s w
      | None -> (w, "You can't do that."))
