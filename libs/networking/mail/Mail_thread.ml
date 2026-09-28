(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mail_thread.mli *)

type 'a tree = Node of 'a option * 'a tree list

(* a container: the essay's, mutable as its are -- the links are made
   and broken as the messages come *)
type 'a container = { mutable message : 'a option; mutable parent : 'a container option; mutable children : 'a container list }

(*****************************************************************************)
(* Subjects *)
(*****************************************************************************)

(* "Re:", "RE:", "Re[2]:", "Fwd:", "Fw:", "Aw:" (German), then spaces *)
let strip_prefix (s : string) : string option =
  let s = String.trim s in
  let lower = String.lowercase_ascii s in
  let prefixes = [ "re"; "fwd"; "fw"; "aw" ] in
  List.find_map
    (fun p ->
      let n = String.length p in
      if String.length lower > n && String.sub lower 0 n = p then
        let rest = String.sub s n (String.length s - n) in
        (* "Re[2]:" *)
        let rest = if rest <> "" && rest.[0] = '[' then match String.index_opt rest ']' with Some i -> String.sub rest (i + 1) (String.length rest - i - 1) | None -> rest else rest in
        if rest <> "" && rest.[0] = ':' then Some (String.sub rest 1 (String.length rest - 1)) else None
      else None)
    prefixes

let rec base_subject (s : string) : string = match strip_prefix s with Some rest -> base_subject rest | None -> String.lowercase_ascii (String.trim s)
let is_reply (s : string) : bool = strip_prefix s <> None

(*****************************************************************************)
(* The algorithm *)
(*****************************************************************************)

(* is [a] [b] or one of its descendants? (a link from [b] down to [a]
   would make a loop) *)
let rec below (a : 'a container) (b : 'a container) : bool = a == b || List.exists (fun c -> below a c) b.children

let unlink (c : 'a container) : unit =
  match c.parent with
  | Some p ->
      p.children <- List.filter (fun x -> x != c) p.children;
      c.parent <- None
  | None -> ()

let link ~(parent : 'a container) (child : 'a container) : unit =
  if not (below parent child) then begin
    unlink child;
    child.parent <- Some parent;
    parent.children <- parent.children @ [ child ]
  end

let threads ~(id : 'a -> string option) ~(references : 'a -> string list) ~(subject : 'a -> string) ~(date : 'a -> float) (messages : 'a list) : 'a tree list =
  let table : (string, 'a container) Hashtbl.t = Hashtbl.create 64 in
  let all = ref [] in
  let fresh () =
    let c = { message = None; parent = None; children = [] } in
    all := c :: !all;
    c
  in
  let container_of (i : string) : 'a container =
    match Hashtbl.find_opt table i with
    | Some c -> c
    | None ->
        let c = fresh () in
        Hashtbl.replace table i c;
        c
  in
  (* 1. the containers, and the links *)
  List.iter
    (fun m ->
      let c =
        match id m with
        | Some i -> (
            match Hashtbl.find_opt table i with
            | Some c when c.message = None -> c
            | Some _ -> fresh () (* the same id twice: kept apart *)
            | None -> container_of i)
        | None -> fresh ()
      in
      c.message <- Some m;
      let refs = List.filter (fun r -> Some r <> id m) (references m) in
      let chain = List.map container_of refs in
      (* each reference the child of the one before, unless it has a
         parent already *)
      let rec pairs = function
        | a :: (b :: _ as rest) ->
            if b.parent = None && not (below a b) then link ~parent:a b;
            pairs rest
        | _ -> ()
      in
      pairs chain;
      (* the message under the last of them: its own word wins *)
      match List.rev chain with
      | last :: _ when not (below last c) -> link ~parent:last c
      | _ -> unlink c)
    messages;
  (* 2. the roots *)
  let roots = List.filter (fun c -> c.parent = None) (List.rev !all) in
  (* 3. the empty containers pruned *)
  let rec prune ~(top : bool) (cs : 'a container list) : 'a container list =
    List.concat_map
      (fun c ->
        c.children <- prune ~top:false c.children;
        List.iter (fun k -> k.parent <- Some c) c.children;
        match (c.message, c.children) with
        | None, [] -> []
        | None, [ only ] when top -> [ only ]
        | None, kids when not top -> kids
        | _ -> [ c ])
      cs
  in
  let roots = prune ~top:true roots in
  List.iter (fun c -> c.parent <- None) roots;
  (* 4. the roots grouped by subject *)
  let subject_of (c : 'a container) : string =
    match c.message with Some m -> subject m | None -> ( match c.children with k :: _ -> Option.fold ~none:"" ~some:subject k.message | [] -> "")
  in
  let by_subject : (string, 'a container) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun c ->
      let s = base_subject (subject_of c) in
      if s <> "" then
        match Hashtbl.find_opt by_subject s with
        | None -> Hashtbl.replace by_subject s c
        | Some old ->
            (* the better representative: an empty one, or one that is not a reply *)
            if (c.message = None && old.message <> None) || (is_reply (subject_of old) && not (is_reply (subject_of c))) then Hashtbl.replace by_subject s c)
    roots;
  let merged =
    List.filter
      (fun c ->
        let s = base_subject (subject_of c) in
        match Hashtbl.find_opt by_subject s with
        | Some rep when rep != c && s <> "" -> (
            match (rep.message, c.message) with
            | None, None ->
                rep.children <- rep.children @ c.children;
                false
            | None, Some _ -> link ~parent:rep c; false
            | Some _, Some m when is_reply (subject m) && not (is_reply (subject_of rep)) -> link ~parent:rep c; false
            | Some _, Some _ ->
                (* two of a kind: both under a new empty container, which
                   takes [rep]'s place among the roots -- made by moving
                   [rep]'s message and replies down into a new one *)
                let moved = { message = rep.message; parent = Some rep; children = rep.children } in
                List.iter (fun k -> k.parent <- Some moved) moved.children;
                rep.message <- None;
                rep.children <- [ moved ];
                link ~parent:rep c;
                false
            | Some _, None -> link ~parent:rep c; false)
        | _ -> true)
      roots
  in
  (* 5. brothers by date; an empty container dated by its first child *)
  let rec to_tree (c : 'a container) : 'a tree = Node (c.message, sort c.children)
  and when_ (c : 'a container) : float =
    match c.message with Some m -> date m | None -> List.fold_left (fun t k -> min t (when_ k)) infinity c.children
  and sort (cs : 'a container list) : 'a tree list = List.map to_tree (List.stable_sort (fun a b -> compare (when_ a) (when_ b)) cs) in
  sort merged

let rec flatten_at (depth : int) (ts : 'a tree list) : ('a * int) list =
  List.concat_map
    (fun (Node (m, kids)) -> (match m with Some m -> [ (m, depth) ] | None -> []) @ flatten_at (depth + 1) kids)
    ts

let flatten (ts : 'a tree list) : ('a * int) list = flatten_at 0 ts

let of_mail (mail : 'a -> Mail.t) (messages : 'a list) : 'a tree list =
  let get m name = Option.value (Mail.get (mail m) name) ~default:"" in
  let references m =
    let refs = Mail.message_ids (get m "references") in
    let irt = Mail.message_ids (get m "in-reply-to") in
    refs @ List.filter (fun i -> not (List.mem i refs)) irt
  in
  threads
    ~id:(fun m -> List.nth_opt (Mail.message_ids (get m "message-id")) 0)
    ~references
    ~subject:(fun m -> Mime.decode_words (get m "subject"))
    ~date:(fun m -> match Mail.date (get m "date") with Some d -> Mail.seconds d | None -> 0.)
    messages
