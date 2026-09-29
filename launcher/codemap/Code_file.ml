(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_file.mli *)

type t = {
  path : string;
  lines : Highlight_code.span list array;
  grid : Bytes.t;
  chars : Bytes.t;
  defs : (int * string * Highlight_code.category) list;
  marks : int list;
  names : Highlight_code.occurrence list array;
  uses : (int * int, Highlight_code.occurrence list) Hashtbl.t;
  definitions : Highlight_code.definition list;
  refs : Highlight_code.reference list array;
  opens : string list;
  includes : string list;
}

(* claude: 160 kept, the width of the lines' grid; [shown], the 80 a
 * column of the classic map draws (the ground draws what fits, a » where
 * a line goes on: the author's long lines, cut) *)
let cols = 160
let shown = 80
let trick = "the trick of this game"

let plain (src : string) : Highlight_code.span list array =
  String.split_on_char '\n' src |> List.map (fun l -> if l = "" then [] else [ { Highlight_code.col = 0; text = l; category = Normal } ]) |> Array.of_list

let make (path : string) (src : string) : t =
  (* claude: an ocamllex or ocamlyacc file too, mostly OCaml; plain if
   * OCaml's lexer gives up on it *)
  let ocaml = List.exists (Filename.check_suffix path) [ ".ml"; ".mli"; ".mll"; ".mly" ] in
  let c = List.exists (Filename.check_suffix path) [ ".c"; ".h" ] in
  (* claude: and assembly (Highlight_asm): an OS's entry points *)
  let asm = List.exists (Filename.check_suffix path) [ ".s"; ".S"; ".asm" ] in
  let plainly () : Highlight_code.analysis =
    { spans = plain src; occurrences = []; definitions = []; references = []; opens = []; includes = [] }
  in
  let an =
    if ocaml then (try Highlight_ml.analyze src with _ -> plainly ())
    else if c then (try Highlight_c.analyze src with _ -> plainly ())
    else if asm then (try Highlight_asm.analyze src with _ -> plainly ())
    else plainly ()
  in
  (* claude: a use bound to a prototype of the file (C's extern int
   * system_call(void);) whose body is elsewhere: a reference too, found
   * across the files (Linux 0.01's C calling its assembly; ~/ix's lesson:
   * a prototype counted as the definition hid the real one) *)
  let an =
    let bodies = List.filter_map (fun (d : Highlight_code.definition) -> if d.drank >= 3 then Some d.dname else None) an.definitions in
    let protos = List.filter (fun (d : Highlight_code.definition) -> d.drank = 2 && not (List.mem d.dname bodies)) an.definitions in
    if protos = [] then an
    else
      let extra =
        List.filter_map
          (fun (o : Highlight_code.occurrence) ->
            if (o.line, o.col) = o.bound_at then None
            else
              List.find_map
                (fun (d : Highlight_code.definition) ->
                  if (d.dline, d.dcol) = o.bound_at then
                    Some ({ rline = o.line; rcol = o.col; rlen = o.len; rpath = []; rname = d.dname; rspace = d.dspace; ropens = [] } : Highlight_code.reference)
                  else None)
                protos)
          an.occurrences
      in
      { an with references = an.references @ extra }
  in
  (* claude: an ocamllex file's entries (rule token = parse, and comment
   * = parse) and an ocamlyacc file's %start symbols: the functions the
   * generated module exports, so Lexer.token is found in its Lexer.mll
   * (the author, at ~/ix: languages/ml's CLI calling its Lexer.token,
   * resolved into languages/c's Lexer.ml) *)
  let generated =
    let lex = Filename.check_suffix path ".mll" and yacc = Filename.check_suffix path ".mly" in
    if not (lex || yacc) then []
    else
      String.split_on_char '\n' src
      |> List.mapi (fun y l -> (y, l))
      |> List.concat_map (fun (y, l) ->
             let words = List.filter (( <> ) "") (String.split_on_char ' ' (String.map (fun c -> if c = '\t' then ' ' else c) l)) in
             let at name = match String.index_opt l name.[0] with Some c -> c | None -> 0 in
             match words with
             | ("rule" | "and") :: name :: rest when lex && List.mem "parse" (rest @ []) || (lex && (match words with ("rule" | "and") :: _ :: _ -> String.length l > 0 && (l.[0] = 'r' || l.[0] = 'a') | _ -> false)) ->
                 let name = List.hd (String.split_on_char '(' name) in
                 [ ({ dname = name; dspace = Value; dline = y; dcol = at name; drank = 3 } : Highlight_code.definition) ]
             | "%start" :: names when yacc -> List.map (fun name -> ({ dname = name; dspace = Value; dline = y; dcol = at name; drank = 3 } : Highlight_code.definition)) names
             | _ -> [])
  in
  let an = if generated = [] then an else { an with definitions = an.definitions @ generated } in
  let lines = an.spans and occurrences = an.occurrences in
  let n = Array.length lines in
  let grid = Bytes.make (n * cols) '\000' in
  let chars = Bytes.make (n * cols) '\000' in
  let defs = ref [] in
  Array.iteri
    (fun y spans ->
      List.iter
        (fun (s : Highlight_code.span) ->
          let code = Char.chr (1 + Highlight_code.index s.category) in
          let k = ref 0 in
          while !k < String.length s.text do
            let c, len = Vga_font.decode s.text !k in
            let x = s.col + !k in
            if x < cols && s.text.[!k] <> ' ' && s.text.[!k] <> '\t' then begin
              Bytes.set grid ((y * cols) + x) code;
              Bytes.set chars ((y * cols) + x) (Char.chr c)
            end;
            k := !k + len
          done;
          match s.category with
          | Def_function | Def_value | Def_type | Def_module -> defs := (y, s.text, s.category) :: !defs
          | Comment_section when String.length s.text > 4 && s.text.[3] <> '*' ->
              (* a section's title: (* Model *) *)
              let t = String.trim (String.sub s.text 2 (String.length s.text - 4)) in
              defs := (y, t, s.category) :: !defs
          | _ -> ())
        spans)
    lines;
  (* claude: the lines saying the trick is here: [trick] in a comment,
   * not in quotes (a comment about the marker, or a string, is not one:
   * tinybox's own code has both) *)
  let says (text : string) : bool =
    let n = String.length trick in
    let rec at i =
      i + n <= String.length text && ((String.sub text i n = trick && (i = 0 || text.[i - 1] <> '"')) || at (i + 1))
    in
    at 0
  in
  let marks =
    List.filter_map
      (fun y ->
        if List.exists (fun (s : Highlight_code.span) -> (s.category = Comment || s.category = Comment_section) && says s.text) lines.(y)
        then Some y
        else None)
      (List.init n Fun.id)
  in
  (* claude: the names bound in a function, by line and by binding *)
  let names = Array.make n [] and uses = Hashtbl.create 64 in
  List.iter
    (fun (o : Highlight_code.occurrence) ->
      if o.line >= 0 && o.line < n then names.(o.line) <- o :: names.(o.line);
      Hashtbl.replace uses o.bound_at (o :: Option.value (Hashtbl.find_opt uses o.bound_at) ~default:[]))
    occurrences;
  (* claude: the names defined elsewhere, by line (level 3) *)
  let refs = Array.make n [] in
  List.iter (fun (r : Highlight_code.reference) -> if r.rline >= 0 && r.rline < n then refs.(r.rline) <- r :: refs.(r.rline)) an.references;
  (* the generated entries as definitions too, for anchors (def:token) *)
  List.iter (fun (d : Highlight_code.definition) -> if not (List.exists (fun (l, n, _) -> l = d.dline && n = d.dname) !defs) then defs := (d.dline, d.dname, Highlight_code.Def_function) :: !defs) generated;
  { path; lines; grid; chars; defs = List.rev !defs; marks; names; uses; definitions = an.definitions; refs; opens = an.opens; includes = an.includes }

let nlines (f : t) : int = Array.length f.lines

let at (f : t) (line : int) (col : int) : Highlight_code.category option =
  if line < 0 || line >= nlines f || col < 0 || col >= cols then None
  else
    let c = Char.code (Bytes.get f.grid ((line * cols) + col)) in
    if c = 0 then None else Some Highlight_code.all.(c - 1)

let modules_used = Code_deps.modules_used

let name_at (f : t) (line : int) (col : int) : Highlight_code.occurrence option =
  if line < 0 || line >= nlines f then None
  else List.find_opt (fun (o : Highlight_code.occurrence) -> col >= o.col && col < o.col + o.len) f.names.(line)

let ref_at (f : t) (line : int) (col : int) : Highlight_code.reference option =
  if line < 0 || line >= nlines f then None
  else List.find_opt (fun (r : Highlight_code.reference) -> col >= r.rcol && col < r.rcol + r.rlen) f.refs.(line)

let uses (f : t) (o : Highlight_code.occurrence) : Highlight_code.occurrence list =
  Option.value (Hashtbl.find_opt f.uses o.bound_at) ~default:[]
