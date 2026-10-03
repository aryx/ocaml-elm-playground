(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_anatomy.mli *)

open Playground
open Code_map_base

(* claude: the anatomy's other plates (Code_anatomy): each file's facts,
 * found a few files a frame from afar (the whole repository's X-ray
 * opening at once), kept *)
let facts_cache : (string, Code_anatomy.facts) Hashtbl.t = Hashtbl.create 256

let facts_of (t : t) ?(until = infinity) (e : entry) : Code_anatomy.facts option =
  match Hashtbl.find_opt facts_cache e.path with
  | Some f -> Some f
  | None when Sys.time () > until -> None
  | None ->
      let public =
        if Filename.check_suffix e.path ".ml" then Option.map (fun (m : entry) -> Code_anatomy.public_names (Lazy.force m.file)) (Map_skeleton.entry_of t (e.path ^ "i")) else None
      in
      let file = Lazy.force e.file in
      (* claude: the configs' anatomy rules for this file, a line's test *)
      let test (rules : Code_guide.rule list) =
        if rules = [] then None
        else
          let refs l = if l < Array.length file.refs then List.map (fun (r : Highlight_code.reference) -> String.concat "." (r.rpath @ [ r.rname ])) file.refs.(l) else [] in
          let text l = Code_guide.line_text file l in
          Some
            (fun l ->
              List.exists
                (fun (r : Code_guide.rule) ->
                  if r.is_ref then List.exists (fun n -> n = r.text || Code_search.starts n (r.text ^ ".") || (let k = String.length r.text in String.length n > k && String.sub n (String.length n - k) k = r.text)) (refs l)
                  else Code_search.contains (text l) r.text)
                rules)
      in
      let nerves, lungs = Code_guide.senses t.guide e.path in
      let f = Code_anatomy.facts ?nerve:(test nerves) ?lung:(test lungs) file ~public in
      Hashtbl.replace facts_cache e.path f;
      Some f

let anatomy_shapes (t : t) (c : camera) : shape list =
  let a = c.a in
  let on s = List.mem s !Code_anatomy.shown in
  let col s = let r, g, b = Code_anatomy.colour s in rgb r g b in
  let tint s alpha (x0, y0, w, h) = rectangle (col s) w (Float.max 1.5 h) |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade alpha in
  match Map_paint.at_ground t c with
  | Some e ->
      (* at the ground and the street: the lines, tinted *)
      List.concat_map
        (fun (path, (g : Code_ground.t)) ->
          match Option.bind (Map_skeleton.entry_of t path) (fun x -> facts_of t x) with
          | None -> []
          | Some (fs : Code_anatomy.facts) ->
              let box l = if l < Array.length g.places then Some (Code_ground.box g l) else None in
              let lines s alpha ls = List.filter_map (fun l -> Option.map (tint s alpha) (box l)) ls in
              (if on Muscles then
                 List.concat_map (fun (a0, b0, st) -> if st < 0.35 then [] else lines Muscles (0.06 +. (0.3 *. Float.min 1. st)) (List.init (b0 - a0 + 1) (fun k -> a0 + k))) fs.muscles
               else [])
              @ (if on Nerves then lines Nerves 0.4 fs.nerves else [])
              @ (if on Lungs then lines Lungs 0.4 fs.lungs else [])
              @
              if on Skin then
                (* claude: the skin, what the .mli exports: the private
                 * definitions shaded, the exported ones lit and barred (the
                 * author: "skin: exported") *)
                List.concat_map
                  (fun (a0, b0) ->
                    List.filter_map
                      (fun l -> Option.map (fun (x0, y0, w, h) -> rectangle (rgb 8 6 20) (w +. 16.) (h +. 0.5) |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.7) (box l))
                      (List.init (b0 - a0 + 1) (fun k -> a0 + k)))
                  fs.hidden
                @ List.filter_map (fun l -> Option.map (fun (x0, y0, _, h) -> rectangle (col Skin) 6. (Float.max 4. h) |> move (sx a (x0 -. 12.)) (sy a (y0 +. (h /. 2.)))) (box l)) fs.skin
              else [])
        (Map_skeleton.grounds t e)
  | None ->
      (* from afar: a file tinted by its muscles, a dot for its nerves and
       * one for its lungs, as big as they are many, its skin a frame *)
      (* claude: opti: 8 ms of a frame, not 30 files: a file's facts cost
       * from nothing to milliseconds, and 30 big ones made a frame of
       * half a second.
       * old: let budget = ref 30 in (facts_of ~budget, decr budget) *)
      let until = Sys.time () +. 0.008 in
      (* the muscles relative: the strongest sixth of the files known *)
      (* a file's strength: its definitions', weighed by their lines *)
      let strength (fs : Code_anatomy.facts) =
        let w, n = List.fold_left (fun (w, n) (a0, b0, st) -> let k = float_of_int (b0 - a0 + 1) in (w +. (st *. k), n +. k)) (0., 0.) fs.muscles in
        if n = 0. then 0. else w /. n
      in
      let all = Hashtbl.fold (fun _ fs acc -> strength fs :: acc) facts_cache [] |> List.sort compare |> Array.of_list in
      let cut = if Array.length all = 0 then 1. else all.(min (Array.length all - 1) (Array.length all * 85 / 100)) in
      Array.to_list t.placed
      |> List.concat_map (fun (p : entry Treemap.placed) ->
             match (p.node, clip c p.rect) with
             | File (_, _, e), Some (x0, y0, x1, y1) when not (Map_paint.outside t p) -> (
                 match facts_of t ~until e with
                 | None -> []
                 | Some fs ->
                     let x0 = float_of_int x0 and y0 = float_of_int y0 and x1 = float_of_int x1 and y1 = float_of_int y1 in
                     let w = x1 -. x0 and h = y1 -. y0 in
                     let strongest = strength fs in
                     let dot s k i =
                       if k = 0 then []
                       else
                         let r = Float.min (Float.min w h /. 3.) (2. +. Float.sqrt (float_of_int k)) in
                         [ circle (col s) r |> move (sx a (x0 +. 3. +. r +. (float_of_int i *. ((2. *. r) +. 2.)))) (sy a (y0 +. 3. +. r)) |> fade 0.9 ]
                     in
                     (* the strongest files only: the heavy lifters *)
                     (if on Muscles && strongest > cut && strongest > 0. then [ tint Muscles (Float.min 0.7 (0.25 +. (0.45 *. ((strongest -. cut) /. Float.max 0.01 cut)))) (x0, y0, w, h) ] else [])
                     @ (if on Nerves then dot Nerves (List.length fs.nerves) 0 else [])
                     @ (if on Lungs then dot Lungs (List.length fs.lungs) 1 else [])
                     @ if on Skin && fs.skin <> [] then List.map (fade 0.45) (frame a (col Skin) x0 y0 x1 y1 1.) else [])
             | _ -> [])

(* the atlas's key: the plates, the ones shown bright *)
(* claude: the plates' legend's corner, top right unless it hides more
 * bones there than elsewhere; legend_row_at's too *)
let legend_origin (c : camera) : float * float =
  let pw = float_of_int c.a.pw and ph = float_of_int c.a.ph in
  let x0, y0, _, _ = Map_skeleton.least_bones [ (pw -. 420., 36., 410., 150.); (pw -. 420., ph -. 160., 410., 150.); (10., ph -. 160., 410., 150.); (10., 36., 410., 150.) ] in
  (x0, y0)

let legend ?pointer (c : camera) : shape list =
  let a = c.a in
  let x0, y0 = legend_origin c in
  let row i s =
    let r, g, b = Code_anatomy.colour s in
    let on = List.mem s !Code_anatomy.shown in
    let y = y0 +. 22. +. (float_of_int i *. 20.) in
    let text = Printf.sprintf "%s  %s: %s" (Code_anatomy.key s) (Code_anatomy.name s) (Code_anatomy.meaning s) in
    [
      circle (rgb r g b) 6. |> move (sx a (x0 +. 14.)) (sy a y) |> fade (if on then 1. else 0.25);
      words (if on then ink else dim) text |> scale (14. /. words_font_size) |> move (sx a (x0 +. 30. +. (0.25 *. 14. *. float_of_int (String.length text)))) (sy a y);
    ]
  in
  (* claude: the row under the mouse, its plate explained in a card *)
  let card =
    match pointer with
    | Some (mx, my) when mx >= x0 && mx <= x0 +. 410. ->
        let i = int_of_float (Float.floor ((my -. y0 -. 12.) /. 20.)) in
        (match if i < 0 then None else List.nth_opt Code_anatomy.all i with
        | Some s ->
            let lines = List.concat_map (Map_names.wrap 62) (Code_anatomy.explain s) in
            let r, g, b = Code_anatomy.colour s in
            let w = 24. +. List.fold_left (fun m l -> Float.max m (text_width 14. l)) 0. lines and h = 16. +. (20. *. float_of_int (List.length lines)) in
            let cx0 = Float.max 8. (x0 -. w -. 10.) and cy0 = y0 +. 12. +. (float_of_int i *. 20.) in
            [ rectangle (rgb 18 16 36) w h |> move (sx a (cx0 +. (w /. 2.))) (sy a (cy0 +. (h /. 2.))) |> fade 0.96 ]
            @ frame a (rgb r g b) cx0 cy0 (cx0 +. w) (cy0 +. h) 1.5
            @ List.mapi (fun k l -> label a ink 14. (cx0 +. 12. +. (text_width 14. l /. 2.)) (cy0 +. 18. +. (20. *. float_of_int k)) l) lines
        | _ -> [])
    | _ -> []
  in
  (rectangle (rgb 18 16 36) 410. 150. |> move (sx a (x0 +. 205.)) (sy a (y0 +. 70.)) |> fade 0.92)
  :: (words ink "the X-ray (x)   hover: what it shows; click or 1-5: on, off" |> scale (12. /. words_font_size) |> move (sx a (x0 +. 170.)) (sy a (y0 +. 4.)))
  :: (List.concat (List.mapi row Code_anatomy.all) @ card)

(* claude: the X-ray's legend row under a pixel: a click toggles it *)
let legend_row_at (t : t) (c : camera) (px : float) (py : float) : Code_anatomy.system option =
  if not t.xray then None
  else
    let x0, y0 = legend_origin c in
    if px < x0 || px > x0 +. 410. || py < y0 || py > y0 +. 150. then None
    else
      let i = int_of_float (Float.floor ((py -. y0 -. 12.) /. 20.)) in
      if i < 0 then None else List.nth_opt Code_anatomy.all i
