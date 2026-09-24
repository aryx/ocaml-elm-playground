(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pim_memo.mli *)

(* the memo being written, by its place in the list *)
type t = { editing : int option }

let start : t = { editing = None }

let y0 = 15.
let rows = 12

let wrap (w : float) (s : string) : string list =
  let wrap_line (line : string) : string list =
    (* greedily, a word at a time; a word too long for a line alone is
     * cut at the last character that fits *)
    let rec cut_word word = if Palm.width word <= w || String.length word <= 1 then [ word ] else
        let rec fits n = if n <= 1 || Palm.width (String.sub word 0 n) <= w then max 1 n else fits (n - 1) in
        let n = fits (String.length word) in
        String.sub word 0 n :: cut_word (String.sub word n (String.length word - n))
    in
    let words = String.split_on_char ' ' line |> List.concat_map cut_word in
    let lines, current =
      List.fold_left
        (fun (lines, current) word ->
          let candidate = if current = "" then word else current ^ " " ^ word in
          if Palm.width candidate <= w || current = "" then (lines, candidate) else (current :: lines, word))
        ([], "") words
    in
    List.rev (current :: lines)
  in
  String.split_on_char '\n' s |> List.concat_map wrap_line

let list_buttons = Palm.buttons [ "New" ]
let edit_buttons = Palm.buttons [ "Done"; "Delete" ]

let set (d : Palm.data) (n : int) (memo : string) : Palm.data = { d with memos = List.mapi (fun k m -> if k = n then memo else m) d.memos }
let remove (d : Palm.data) (n : int) : Palm.data = { d with memos = List.filteri (fun k _ -> k <> n) d.memos }

let update (i : Palm.input) (d : Palm.data) (t : t) : Palm.data * t =
  match t.editing with
  | None -> (
      match Palm.tapped i list_buttons with
      | Some "New" -> ({ d with memos = d.memos @ [ "" ] }, { editing = Some (List.length d.memos) })
      | _ -> (
          match Palm.row_at ~y0 ~rows i with
          | Some r when r < List.length d.memos -> (d, { editing = Some r })
          | _ -> (d, t)))
  | Some n -> (
      let memo = Option.value ~default:"" (List.nth_opt d.memos n) in
      let memo = memo ^ i.typed ^ if i.enter then "\n" else "" in
      let memo = if i.backspace && memo <> "" then String.sub memo 0 (String.length memo - 1) else memo in
      let d = set d n memo in
      match Palm.tapped i edit_buttons with
      (* an empty memo is not kept *)
      | Some "Done" -> ((if String.trim memo = "" then remove d n else d), { editing = None })
      | Some "Delete" -> (remove d n, { editing = None })
      | _ -> (d, t))

let first_line (memo : string) : string = match String.split_on_char '\n' memo with l :: _ -> l | [] -> ""

let view ~(time : float) (d : Palm.data) (t : t) : Playground.shape list =
  match t.editing with
  | None ->
      Palm.title "Memo List"
      @ List.mapi
          (fun r memo -> Palm.text ~x:2. ~y:(y0 +. (float_of_int r *. 11.)) (Palm.fit 156. (Printf.sprintf "%d. %s" (r + 1) (first_line memo))))
          (List.filteri (fun k _ -> k < rows) d.memos)
      @ Palm.draw_buttons list_buttons
  | Some n ->
      let memo = Option.value ~default:"" (List.nth_opt d.memos n) in
      let lines = wrap 154. memo in
      (* the end, where the writing is, always in sight *)
      let skip = max 0 (List.length lines - rows) in
      let lines = List.filteri (fun k _ -> k >= skip) lines in
      let last = List.length lines - 1 in
      Palm.title (Printf.sprintf "Memo %d of %d" (n + 1) (List.length d.memos))
      @ List.concat
          (List.mapi
             (fun r line ->
               let y = y0 +. (float_of_int r *. 11.) in
               [ Palm.text ~x:3. ~y line; Palm.rect Palm.light (2., y +. 10., 156., 0.5) ]
               @ if r = last then Palm.caret ~time ~x:3. ~y line else [])
             lines)
      @ Palm.draw_buttons edit_buttons
