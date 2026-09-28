(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Svg.mli *)

type node = { name : string; attributes : (string * string) list; children : node list }

(*****************************************************************************)
(* Reading XML *)
(*****************************************************************************)

let is_space (c : char) : bool = c = ' ' || c = '\n' || c = '\t' || c = '\r'

(* &amp; &lt; &gt; &quot; &apos; &#n; &#xn; in an attribute's value *)
let entities (s : string) : string =
  if not (String.contains s '&') then s
  else
    let b = Buffer.create (String.length s) in
    let n = String.length s in
    let rec go i =
      if i < n then
        if s.[i] = '&' then
          match String.index_from_opt s i ';' with
          | Some j when j - i <= 10 -> (
              let name = String.sub s (i + 1) (j - i - 1) in
              let code =
                match name with
                | "amp" -> Some 38 | "lt" -> Some 60 | "gt" -> Some 62 | "quot" -> Some 34 | "apos" -> Some 39
                | _ when String.length name > 1 && name.[0] = '#' ->
                    int_of_string_opt (if name.[1] = 'x' then "0" ^ String.sub name 1 (String.length name - 1) else String.sub name 1 (String.length name - 1))
                | _ -> None
              in
              match code with
              | Some c when c < 128 -> Buffer.add_char b (Char.chr c); go (j + 1)
              | Some c -> Buffer.add_utf_8_uchar b (Uchar.of_int c); go (j + 1)
              | None -> Buffer.add_char b '&'; go (i + 1))
          | _ -> Buffer.add_char b '&'; go (i + 1)
        else (Buffer.add_char b s.[i]; go (i + 1))
    in
    go 0;
    Buffer.contents b

(* "svg:path" is "path" *)
let local (name : string) : string =
  match String.index_opt name ':' with Some i when String.sub name 0 i = "svg" -> String.sub name (i + 1) (String.length name - i - 1) | _ -> name

let parse_xml (s : string) : node list =
  let n = String.length s in
  let pos = ref 0 in
  let starts_with p = !pos + String.length p <= n && String.sub s !pos (String.length p) = p in
  let skip_past p =
    let rec go i = if i + String.length p > n then n else if String.sub s i (String.length p) = p then i + String.length p else go (i + 1) in
    pos := go !pos
  in
  let skip_spaces () = while !pos < n && is_space s.[!pos] do incr pos done in
  let name () =
    let start = !pos in
    while !pos < n && not (is_space s.[!pos] || s.[!pos] = '>' || s.[!pos] = '/' || s.[!pos] = '=') do incr pos done;
    String.sub s start (!pos - start)
  in
  (* after "<name": the attributes, then ">" or "/>"; whether it is
   * self-closing *)
  let rec attributes acc =
    skip_spaces ();
    if !pos >= n then (List.rev acc, true)
    else if starts_with "/>" then (pos := !pos + 2; (List.rev acc, true))
    else if s.[!pos] = '>' then (incr pos; (List.rev acc, false))
    else
      let key = name () in
      skip_spaces ();
      if !pos < n && s.[!pos] = '=' then (
        incr pos;
        skip_spaces ();
        let value =
          if !pos < n && (s.[!pos] = '"' || s.[!pos] = '\'') then (
            let q = s.[!pos] in
            let start = !pos + 1 in
            let stop = match String.index_from_opt s start q with Some j -> j | None -> n in
            pos := min n (stop + 1);
            String.sub s start (stop - start))
          else name ()
        in
        attributes ((String.lowercase_ascii key, entities value) :: acc))
      else if key = "" then (incr pos; attributes acc)
      else attributes ((String.lowercase_ascii key, "") :: acc)
  in
  (* the nodes until "</" or the end *)
  let rec nodes acc =
    if !pos >= n then List.rev acc
    else if s.[!pos] <> '<' then (
      (match String.index_from_opt s !pos '<' with Some j -> pos := j | None -> pos := n);
      nodes acc)
    else if starts_with "<!--" then (skip_past "-->"; nodes acc)
    else if starts_with "<![CDATA[" then (skip_past "]]>"; nodes acc)
    else if starts_with "<?" then (skip_past "?>"; nodes acc)
    else if starts_with "<!" then (skip_past ">"; nodes acc)
    else if starts_with "</" then (skip_past ">"; List.rev acc)
    else (
      incr pos;
      let tag = local (String.lowercase_ascii (name ())) in
      let attributes, closed = attributes [] in
      let children = if closed then [] else nodes [] in
      nodes ({ name = tag; attributes; children } :: acc))
  in
  nodes []

let rec find_svg (nodes : node list) : node option =
  List.find_map (fun nd -> if nd.name = "svg" then Some nd else find_svg nd.children) nodes

let parse (s : string) : node option = find_svg (parse_xml s)

let sniff (bytes : string) : bool =
  let head = String.sub bytes 0 (min 1024 (String.length bytes)) in
  let rec from i = if i < String.length head && (is_space head.[i] || head.[i] = '\xef' || head.[i] = '\xbb' || head.[i] = '\xbf') then from (i + 1) else i in
  let i = from 0 in
  let rest = String.sub head i (String.length head - i) in
  let contains sub s =
    let n = String.length sub in
    let rec go k = k + n <= String.length s && (String.sub s k n = sub || go (k + 1)) in
    go 0
  in
  String.starts_with ~prefix:"<svg" rest || ((String.starts_with ~prefix:"<?xml" rest || String.starts_with ~prefix:"<!--" rest || String.starts_with ~prefix:"<!DOCTYPE svg" rest) && contains "<svg" rest)

(*****************************************************************************)
(* Numbers, colours, transforms *)
(*****************************************************************************)

(* the numbers of "M10-5.5.5e2,3": 10 -5.5 .5e2 3 -- SVG's grammar,
 * a sign or a second "." starting the next *)
let numbers (s : string) : float list =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let c = s.[i] in
      if (c >= '0' && c <= '9') || c = '.' || c = '-' || c = '+' then (
        let j = ref (i + 1) and dot = ref (c = '.') in
        let stop = ref false in
        while (not !stop) && !j < n do
          let d = s.[!j] in
          if d >= '0' && d <= '9' then incr j
          else if d = '.' && not !dot then (dot := true; incr j)
          else if (d = 'e' || d = 'E') && !j + 1 < n && (s.[!j + 1] = '-' || s.[!j + 1] = '+' || (s.[!j + 1] >= '0' && s.[!j + 1] <= '9')) then (
            j := !j + 2;
            dot := true)
          else stop := true
        done;
        go !j (match float_of_string_opt (String.sub s i (!j - i)) with Some f -> f :: acc | None -> acc))
      else go (i + 1) acc
  in
  go 0 []

let length (s : string) : float option =
  let s = String.trim s in
  if String.ends_with ~suffix:"%" s || String.ends_with ~suffix:"em" s then None
  else match numbers s with [ f ] -> Some f | _ -> None

type rgb = int * int * int

let names : (string * rgb) list =
  [ ("black", (0, 0, 0)); ("white", (255, 255, 255)); ("red", (255, 0, 0)); ("green", (0, 128, 0)); ("blue", (0, 0, 255));
    ("yellow", (255, 255, 0)); ("orange", (255, 165, 0)); ("gray", (128, 128, 128)); ("grey", (128, 128, 128));
    ("silver", (192, 192, 192)); ("purple", (128, 0, 128)); ("navy", (0, 0, 128)); ("teal", (0, 128, 128));
    ("maroon", (128, 0, 0)); ("lime", (0, 255, 0)); ("aqua", (0, 255, 255)); ("fuchsia", (255, 0, 255)); ("olive", (128, 128, 0)) ]

(* a paint: None for none (and transparent); a gradient drawn grey *)
let paint ~(current : rgb) (v : string) : rgb option =
  let v = String.trim (String.lowercase_ascii v) in
  let hex h = int_of_string_opt ("0x" ^ h) in
  match v with
  | "none" | "transparent" | "" -> None
  | "currentcolor" -> Some current
  | _ when String.length v = 4 && v.[0] = '#' -> (
      match (hex (String.make 2 v.[1]), hex (String.make 2 v.[2]), hex (String.make 2 v.[3])) with Some r, Some g, Some b -> Some (r, g, b) | _ -> None)
  | _ when String.length v = 7 && v.[0] = '#' -> (
      match (hex (String.sub v 1 2), hex (String.sub v 3 2), hex (String.sub v 5 2)) with Some r, Some g, Some b -> Some (r, g, b) | _ -> None)
  | _ when String.starts_with ~prefix:"rgb" v -> (
      match numbers v with r :: g :: b :: _ -> Some (int_of_float r, int_of_float g, int_of_float b) | _ -> None)
  | _ when String.starts_with ~prefix:"url(" v -> Some (160, 160, 160)
  | _ -> List.assoc_opt v names

(* "translate(10 5) scale(2)": the functions left to right, each
 * applied after the ones to its right *)
let transform (s : string) : Affine.t =
  let parts = String.split_on_char ')' s in
  List.fold_left
    (fun m part ->
      match String.index_opt part '(' with
      | None -> m
      | Some i ->
          let f = String.trim (String.sub part 0 i) |> fun f -> match String.rindex_opt f ',' with Some j -> String.trim (String.sub f (j + 1) (String.length f - j - 1)) | None -> f in
          let args = numbers (String.sub part (i + 1) (String.length part - i - 1)) in
          let t : Affine.t =
            match (f, args) with
            | "translate", [ x ] -> Affine.translate x 0.
            | "translate", x :: y :: _ -> Affine.translate x y
            | "scale", [ k ] -> Affine.scale k k
            | "scale", x :: y :: _ -> Affine.scale x y
            | "rotate", [ a ] -> Affine.rotate (a *. Float.pi /. 180.)
            | "rotate", a :: x :: y :: _ ->
                Affine.compose (Affine.translate x y) (Affine.compose (Affine.rotate (a *. Float.pi /. 180.)) (Affine.translate (-.x) (-.y)))
            | "matrix", [ a; b; c; d; e; f ] -> { a; b; c; d; tx = e; ty = f }
            | _ -> Affine.identity
          in
          Affine.compose m t)
    Affine.identity parts

(*****************************************************************************)
(* Shapes as contours *)
(*****************************************************************************)

type point = float * float

(* an elliptical arc, SVG's endpoint form (appendix F.6.5): its centre
 * and angles found, then points along it, in the user's space *)
let arc ((x1, y1) : point) ~(rx : float) ~(ry : float) ~(phi : float) ~(large : bool) ~(sweep : bool) ((x2, y2) : point) : point list =
  let rx = Float.abs rx and ry = Float.abs ry in
  if rx = 0. || ry = 0. then [ (x2, y2) ]
  else
    let cp = cos phi and sp = sin phi in
    let dx = (x1 -. x2) /. 2. and dy = (y1 -. y2) /. 2. in
    let x1' = (cp *. dx) +. (sp *. dy) and y1' = (-.sp *. dx) +. (cp *. dy) in
    (* radii too small: scaled up (F.6.6) *)
    let l = (x1' *. x1' /. (rx *. rx)) +. (y1' *. y1' /. (ry *. ry)) in
    let rx, ry = if l > 1. then (rx *. sqrt l, ry *. sqrt l) else (rx, ry) in
    let num = (rx *. rx *. ry *. ry) -. (rx *. rx *. y1' *. y1') -. (ry *. ry *. x1' *. x1') in
    let den = (rx *. rx *. y1' *. y1') +. (ry *. ry *. x1' *. x1') in
    let k = (if large = sweep then -1. else 1.) *. sqrt (Float.max 0. (num /. den)) in
    let cx' = k *. rx *. y1' /. ry and cy' = -.k *. ry *. x1' /. rx in
    let cx = (cp *. cx') -. (sp *. cy') +. ((x1 +. x2) /. 2.) and cy = (sp *. cx') +. (cp *. cy') +. ((y1 +. y2) /. 2.) in
    let angle (ux, uy) (vx, vy) = atan2 ((ux *. vy) -. (uy *. vx)) ((ux *. vx) +. (uy *. vy)) in
    let t1 = angle (1., 0.) ((x1' -. cx') /. rx, (y1' -. cy') /. ry) in
    let dt = angle ((x1' -. cx') /. rx, (y1' -. cy') /. ry) ((-.x1' -. cx') /. rx, (-.y1' -. cy') /. ry) in
    let dt = if (not sweep) && dt > 0. then dt -. (2. *. Float.pi) else if sweep && dt < 0. then dt +. (2. *. Float.pi) else dt in
    let steps = max 4 (int_of_float (Float.abs dt *. Float.max rx ry /. 2.)) |> min 128 in
    List.init steps (fun i ->
        let t = t1 +. (dt *. float_of_int (i + 1) /. float_of_int steps) in
        let x = rx *. cos t and y = ry *. sin t in
        ((cp *. x) -. (sp *. y) +. cx, (sp *. x) +. (cp *. y) +. cy))

(* a path's d: its subpaths, each a polyline in the picture's pixels
 * ([m] the user's space to them), and whether it was closed *)
let path (m : Affine.t) (d : string) : (point list * bool) list =
  let t = Affine.apply m in
  let subpaths = ref [] and current = ref [] and closed = ref false in
  let finish () =
    if List.length !current > 1 then subpaths := (List.rev !current, !closed) :: !subpaths;
    current := [];
    closed := false
  in
  let pos = ref (0., 0.) and start = ref (0., 0.) and last_ctrl = ref None in
  let emit p = current := t p :: !current in
  let line_to p = emit p; pos := p in
  let cubic c1 c2 p =
    (match Curve.flatten (t !pos) (t c1) (t c2) (t p) with _ :: rest -> current := List.rev_append rest !current | [] -> ());
    pos := p
  in
  (* the commands, each with the numbers after it *)
  let n = String.length d in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < n do
    let c = d.[!i] in
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then (
      if c <> 'e' && c <> 'E' then tokens := `Cmd c :: !tokens;
      incr i)
    else (
      let j = ref !i in
      while !j < n && not ((d.[!j] >= 'a' && d.[!j] <= 'z' && d.[!j] <> 'e') || (d.[!j] >= 'A' && d.[!j] <= 'Z' && d.[!j] <> 'E')) do incr j done;
      List.iter (fun f -> tokens := `Num f :: !tokens) (numbers (String.sub d !i (!j - !i)));
      i := !j)
  done;
  let rec run cmd args =
    let rel = cmd >= 'a' && cmd <= 'z' in
    let x0, y0 = !pos in
    let at (x, y) = if rel then (x0 +. x, y0 +. y) else (x, y) in
    let ctrl_prev = !last_ctrl in
    last_ctrl := None;
    match (Char.uppercase_ascii cmd, args) with
    | 'M', x :: y :: rest ->
        finish ();
        let p = at (x, y) in
        start := p;
        pos := p;
        emit p;
        (* the pairs after a moveto are linetos *)
        run (if rel then 'l' else 'L') rest
    | 'L', x :: y :: rest -> line_to (at (x, y)); run cmd rest
    | 'H', x :: rest -> line_to ((if rel then x0 +. x else x), y0); run cmd rest
    | 'V', y :: rest -> line_to (x0, if rel then y0 +. y else y); run cmd rest
    | 'C', x1 :: y1 :: x2 :: y2 :: x :: y :: rest ->
        let c2 = at (x2, y2) in
        cubic (at (x1, y1)) c2 (at (x, y));
        last_ctrl := Some (`C, c2);
        run cmd rest
    | 'S', x2 :: y2 :: x :: y :: rest ->
        let c1 = match ctrl_prev with Some (`C, (cx, cy)) -> ((2. *. x0) -. cx, (2. *. y0) -. cy) | _ -> (x0, y0) in
        let c2 = at (x2, y2) in
        cubic c1 c2 (at (x, y));
        last_ctrl := Some (`C, c2);
        run cmd rest
    | 'Q', x1 :: y1 :: x :: y :: rest ->
        let q = at (x1, y1) and p = at (x, y) in
        (* a quadratic as a cubic: its control points 2/3 of the way *)
        cubic (x0 +. (2. /. 3. *. (fst q -. x0)), y0 +. (2. /. 3. *. (snd q -. y0))) (fst p +. (2. /. 3. *. (fst q -. fst p)), snd p +. (2. /. 3. *. (snd q -. snd p))) p;
        last_ctrl := Some (`Q, q);
        run cmd rest
    | 'T', x :: y :: rest ->
        let q = match ctrl_prev with Some (`Q, (cx, cy)) -> ((2. *. x0) -. cx, (2. *. y0) -. cy) | _ -> (x0, y0) in
        let p = at (x, y) in
        cubic (x0 +. (2. /. 3. *. (fst q -. x0)), y0 +. (2. /. 3. *. (snd q -. y0))) (fst p +. (2. /. 3. *. (fst q -. fst p)), snd p +. (2. /. 3. *. (snd q -. snd p))) p;
        last_ctrl := Some (`Q, q);
        run cmd rest
    | 'A', rx :: ry :: rot :: large :: sweep :: x :: y :: rest ->
        let p = at (x, y) in
        List.iter emit (arc (x0, y0) ~rx ~ry ~phi:(rot *. Float.pi /. 180.) ~large:(large <> 0.) ~sweep:(sweep <> 0.) p);
        pos := p;
        run cmd rest
    | 'Z', _ ->
        closed := true;
        finish ();
        pos := !start;
        (* a path goes on from where it closed *)
        emit !start;
        args
    | _ -> args
  in
  let rec go tokens =
    match tokens with
    | `Cmd c :: rest ->
        let rec take acc ts = match ts with `Num f :: ts -> take (f :: acc) ts | _ -> (List.rev acc, ts) in
        let args, after = take [] rest in
        ignore (run c args);
        go after
    | `Num _ :: rest -> go rest
    | [] -> ()
  in
  go (List.rev !tokens);
  finish ();
  List.rev !subpaths

(* a shape's subpaths, in the picture's pixels *)
let shape (m : Affine.t) (nd : node) : (point list * bool) list =
  let num name = match List.assoc_opt name nd.attributes with Some v -> Option.value (length v) ~default:0. | None -> 0. in
  let t = Affine.apply m in
  let ellipse cx cy rx ry =
    let steps = 64 in
    [ (List.init steps (fun i -> let a = 2. *. Float.pi *. float_of_int i /. float_of_int steps in t (cx +. (rx *. cos a), cy +. (ry *. sin a))), true) ]
  in
  let points () =
    let rec pairs l = match l with x :: y :: rest -> (x, y) :: pairs rest | _ -> [] in
    List.map t (pairs (numbers (Option.value (List.assoc_opt "points" nd.attributes) ~default:"")))
  in
  match nd.name with
  | "path" -> path m (Option.value (List.assoc_opt "d" nd.attributes) ~default:"")
  | "rect" ->
      let x = num "x" and y = num "y" and w = num "width" and h = num "height" in
      if w <= 0. || h <= 0. then [] else [ (List.map t [ (x, y); (x +. w, y); (x +. w, y +. h); (x, y +. h) ], true) ]
  | "circle" -> let r = num "r" in if r <= 0. then [] else ellipse (num "cx") (num "cy") r r
  | "ellipse" -> ellipse (num "cx") (num "cy") (num "rx") (num "ry")
  | "line" -> [ ([ t (num "x1", num "y1"); t (num "x2", num "y2") ], false) ]
  | "polyline" -> [ (points (), false) ]
  | "polygon" -> [ (points (), true) ]
  | _ -> []

(*****************************************************************************)
(* Painting *)
(*****************************************************************************)

(* what is inherited down the tree *)
type style = {
  fill : rgb option;
  even_odd : bool;
  fill_opacity : float;
  stroke : rgb option;
  stroke_width : float;
  stroke_opacity : float;
  opacity : float;
  current : rgb;
}

(* a property: in style="...", else the attribute *)
let property (nd : node) (name : string) : string option =
  let from_style =
    match List.assoc_opt "style" nd.attributes with
    | Some s ->
        List.find_map
          (fun decl ->
            match String.index_opt decl ':' with
            | Some i when String.trim (String.sub decl 0 i) = name -> Some (String.trim (String.sub decl (i + 1) (String.length decl - i - 1)))
            | _ -> None)
          (String.split_on_char ';' s)
    | None -> None
  in
  match from_style with Some v -> Some v | None -> List.assoc_opt name nd.attributes

let styled (st : style) (nd : node) : style =
  let get name = property nd name in
  let num name default = match Option.bind (get name) (fun v -> match numbers v with [ f ] -> Some f | _ -> None) with Some f -> f | None -> default in
  let current = match Option.bind (get "color") (paint ~current:st.current) with Some c -> c | None -> st.current in
  {
    fill = (match get "fill" with Some v -> paint ~current v | None -> st.fill);
    even_odd = (match get "fill-rule" with Some v -> String.trim v = "evenodd" | None -> st.even_odd);
    fill_opacity = num "fill-opacity" st.fill_opacity;
    stroke = (match get "stroke" with Some v -> paint ~current v | None -> st.stroke);
    stroke_width = num "stroke-width" st.stroke_width;
    stroke_opacity = num "stroke-opacity" st.stroke_opacity;
    (* not inherited, but a group's multiplies what is in it *)
    opacity = st.opacity *. num "opacity" 1.;
    current;
  }

(* the picture being painted: premultiplied red, green, blue, alpha per
 * pixel, and the coverage framebuffer *)
type canvas = { w : int; h : int; acc : float array; fb : Framebuffer.t }

(* contours covered white on black (antialiased), then laid "over" the
 * picture in [rgb] at that coverage times [alpha] *)
let paint_contours (cv : canvas) ~(even_odd : bool) (contours : point list list) ((r, g, b) : rgb) (alpha : float) : unit =
  if contours <> [] && alpha > 0. then (
    Framebuffer.clear cv.fb ~rgb:0;
    Fill.polygons_aa ~rule:(if even_odd then Even_odd else Nonzero) cv.fb contours ~rgb:0xffffff ~alpha:1.;
    for y = 0 to cv.h - 1 do
      for x = 0 to cv.w - 1 do
        let coverage = float_of_int (Framebuffer.get_rgb cv.fb ~x ~y land 0xff) /. 255. in
        if coverage > 0. then (
          let a = coverage *. alpha in
          let i = 4 * ((y * cv.w) + x) in
          cv.acc.(i) <- (float_of_int r *. a) +. (cv.acc.(i) *. (1. -. a));
          cv.acc.(i + 1) <- (float_of_int g *. a) +. (cv.acc.(i + 1) *. (1. -. a));
          cv.acc.(i + 2) <- (float_of_int b *. a) +. (cv.acc.(i + 2) *. (1. -. a));
          cv.acc.(i + 3) <- a +. (cv.acc.(i + 3) *. (1. -. a)))
      done
    done)

let skipped = [ "defs"; "symbol"; "clippath"; "mask"; "lineargradient"; "radialgradient"; "pattern"; "title"; "desc"; "metadata"; "style"; "script"; "text"; "use"; "filter"; "marker"; "foreignobject" ]

let rec draw (cv : canvas) ~(scale : float) (m : Affine.t) (st : style) (nd : node) : unit =
  if not (List.mem (String.lowercase_ascii nd.name) skipped) && property nd "display" <> Some "none" && property nd "visibility" <> Some "hidden" then (
    let st = styled st nd in
    let m = match List.assoc_opt "transform" nd.attributes with Some tr -> Affine.compose m (transform tr) | None -> m in
    let subpaths = shape m nd in
    (match st.fill with
    | Some color when subpaths <> [] ->
        paint_contours cv ~even_odd:st.even_odd (List.map fst subpaths) color (st.fill_opacity *. st.opacity)
    | _ -> ());
    (match st.stroke with
    | Some color when subpaths <> [] && st.stroke_width > 0. ->
        let lines = List.map (fun (pts, closed) -> match pts with p :: _ when closed -> pts @ [ p ] | _ -> pts) subpaths in
        paint_contours cv ~even_odd:false (Stroke.contours lines ~width:(st.stroke_width *. scale)) color (st.stroke_opacity *. st.opacity)
    | _ -> ());
    List.iter (draw cv ~scale m st) nd.children)

let view_box (nd : node) : (float * float * float * float) option =
  match Option.map numbers (List.assoc_opt "viewbox" nd.attributes) with Some [ x; y; w; h ] when w > 0. && h > 0. -> Some (x, y, w, h) | _ -> None

let size (nd : node) : (float * float) option =
  let attr name = Option.bind (List.assoc_opt name nd.attributes) length in
  match (attr "width", attr "height", view_box nd) with
  | Some w, Some h, _ -> Some (w, h)
  | Some w, None, Some (_, _, vw, vh) -> Some (w, w *. vh /. vw)
  | None, Some h, Some (_, _, vw, vh) -> Some (h *. vw /. vh, h)
  | None, None, Some (_, _, vw, vh) -> Some (vw, vh)
  | _ -> None

let render ?(color = (0, 0, 0)) (nd : node) ~(width : int) ~(height : int) : Rgba_image.t =
  let w = max 1 width and h = max 1 height in
  let cv = { w; h; acc = Array.make (4 * w * h) 0.; fb = Framebuffer.create ~width:w ~height:h } in
  (* the viewBox onto the picture: scaled uniformly, centred *)
  let m, scale =
    match view_box nd with
    | Some (x, y, vw, vh) ->
        let k = Float.min (float_of_int w /. vw) (float_of_int h /. vh) in
        let tx = ((float_of_int w -. (vw *. k)) /. 2.) -. (x *. k) and ty = ((float_of_int h -. (vh *. k)) /. 2.) -. (y *. k) in
        (Affine.compose (Affine.translate tx ty) (Affine.scale k k), k)
    | None -> (Affine.identity, 1.)
  in
  let st = { fill = Some (0, 0, 0); even_odd = false; fill_opacity = 1.; stroke = None; stroke_width = 1.; stroke_opacity = 1.; opacity = 1.; current = color } in
  List.iter (draw cv ~scale m (styled st nd)) nd.children;
  let img = Rgba_image.create ~width:w ~height:h in
  for i = 0 to (w * h) - 1 do
    let a = cv.acc.((4 * i) + 3) in
    if a > 0. then (
      let un c = max 0 (min 255 (int_of_float (Float.round (c /. a)))) in
      img.rgba.{4 * i} <- un cv.acc.(4 * i);
      img.rgba.{(4 * i) + 1} <- un cv.acc.((4 * i) + 1);
      img.rgba.{(4 * i) + 2} <- un cv.acc.((4 * i) + 2);
      img.rgba.{(4 * i) + 3} <- max 0 (min 255 (int_of_float (Float.round (a *. 255.)))))
  done;
  img
