(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_guide.mli *)

type rgb = int * int * int
type item = { at : string; say : string option; weight : int }

type file_note = {
  summary : string option;
  digest : string option;
  capitals : item list;
  important : item list;
  links : (string * string) list;
  related : string list;
}

type tour = { name : string; stops : item list }
type bone = { bat : string; bpath : string; banchor : string; role : string }
type joint = { jfrom : string; jto : string; jsay : string option }
type skeleton = { sname : string; sdir : string; bones : bone list; joints : joint list }
type view = { vname : string; files : string list; of_ : string option; with_ : string option }

(* claude: a mark: lines matching its rules, each lit in its colour *)
type rule = { text : string; is_ref : bool; colour : rgb; rsay : string option }
type mark = { mname : string; mdir : string; rules : rule list }

type dir_note = {
  dir : string;
  title : string option;
  summary : string option;
  colors : (string * rgb) list;
  subdirs : (string * string) list;
  notes : (string * file_note) list;
  tours : tour list;
  skeletons : skeleton list;
  views : view list;
  marks : mark list;
  nerves : rule list; (* claude: anatomy: nerves:, its inputs *)
  lungs : rule list; (* anatomy: lungs:, its I/O *)
}

type t = dir_note list

let empty = []

(*****************************************************************************)
(* Reading a config *)
(*****************************************************************************)

exception Bad of string

let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

(* an object's fields, each known, or the mistake *)
let fields (where : string) (known : string list) (v : Json.t) : (string * Json.t) list =
  match v with
  | Object fs ->
      List.iter (fun (k, _) -> if not (List.mem k known) then bad "%s: an unknown field %s (known: %s)" where k (String.concat ", " known)) fs;
      fs
  | _ -> bad "%s: an object expected" where

let str where = function Json.String s -> s | _ -> bad "%s: a string expected" where
let opt_str where fs k = Option.map (str (where ^ "." ^ k)) (List.assoc_opt k fs)
let list where f = function Json.Array vs -> List.mapi (fun i v -> f (Printf.sprintf "%s[%d]" where i) v) vs | _ -> bad "%s: an array expected" where
let opt_list where fs k f = match List.assoc_opt k fs with Some v -> list (where ^ "." ^ k) f v | None -> []

let item where v =
  let fs = fields where [ "at"; "say"; "weight" ] v in
  let at = match List.assoc_opt "at" fs with Some v -> str (where ^ ".at") v | None -> bad "%s: at, its anchor, missing" where in
  let weight =
    match List.assoc_opt "weight" fs with
    | None -> 1
    | Some (Number n) when Float.is_integer n && n >= 1. && n <= 3. -> int_of_float n
    | Some _ -> bad "%s.weight: 1, 2 or 3" where
  in
  { at; say = opt_str where fs "say"; weight }

let file_note where v =
  let fs = fields where [ "summary"; "digest"; "capitals"; "important"; "links"; "related" ] v in
  {
    summary = opt_str where fs "summary";
    digest = opt_str where fs "digest";
    capitals = opt_list where fs "capitals" item;
    important = opt_list where fs "important" item;
    links =
      opt_list where fs "links" (fun w v ->
          let fs = fields w [ "from"; "to" ] v in
          match (List.assoc_opt "from" fs, List.assoc_opt "to" fs) with
          | Some a, Some b -> (str (w ^ ".from") a, str (w ^ ".to") b)
          | _ -> bad "%s: from and to" w);
    related = opt_list where fs "related" str;
  }

(* a path of the config, from the root *)
let under (dir : string) (p : string) : string = if dir = "" then p else Jsonnet_parse.resolve ~from:(dir ^ "/.codemapconfig") p

let kinds = [ "def"; "type"; "module"; "section"; "comment"; "code"; "line"; "pattern" ]

let split (s : string) : string option * string =
  match String.index_opt s ':' with
  | Some i when not (List.mem (String.sub s 0 i) kinds) -> (Some (String.sub s 0 i), String.sub s (i + 1) (String.length s - i - 1))
  | _ -> (None, s)

(* an anatomy's rules: its [key]'s, { text or ref, say } *)
let sense (where : string) (fs : (string * Json.t) list) (key : string) : rule list =
  match List.assoc_opt "anatomy" fs with
  | None -> []
  | Some a ->
      let afs = fields (where ^ ".anatomy") [ "nerves"; "lungs" ] a in
      opt_list (where ^ ".anatomy") afs key (fun w v ->
          let fs = fields w [ "text"; "ref"; "say" ] v in
          let text, is_ref =
            match (opt_str w fs "text", opt_str w fs "ref") with
            | Some t, None when String.length t >= 2 -> (t, false)
            | None, Some r when String.length r >= 2 -> (r, true)
            | _ -> bad "%s: its text or ref, two characters at least" w
          in
          { text; is_ref; colour = (0, 0, 0); rsay = opt_str w fs "say" })

let of_json ~(dir : string) (v : Json.t) : (dir_note, string) result =
  let where = if dir = "" then ".codemapconfig" else dir ^ "/.codemapconfig" in
  match
    (* claude: marks: were layers: (the author renamed them, the name kept
     * for the layers to come, a map coloured by a measure): the old
     * name still read, the configs of other projects written before *)
    let v = match v with Json.Object kvs -> Json.Object (List.map (fun (k, x) -> ((if k = "layers" then "marks" else k), x)) kvs) | v -> v in
    let fs = fields where [ "title"; "summary"; "generated"; "colors"; "dirs"; "files"; "tours"; "skeletons"; "views"; "marks"; "anatomy" ] v in
    let obj k f = match List.assoc_opt k fs with Some (Json.Object kvs) -> List.map (fun (name, v) -> f (where ^ "." ^ k ^ "." ^ name) name v) kvs | Some _ -> bad "%s.%s: an object expected" where k | None -> [] in
    (match List.assoc_opt "generated" fs with Some g -> ignore (fields (where ^ ".generated") [ "by"; "on" ] g) | None -> ());
    {
      dir;
      title = opt_str where fs "title";
      summary = opt_str where fs "summary";
      colors =
        obj "colors" (fun w path v ->
            match Code_config.hex (str w v) with Some rgb -> (under dir path, rgb) | None -> bad "%s: %S is no #rrggbb" w (str w v));
      subdirs =
        obj "dirs" (fun w name v ->
            let fs = fields w [ "summary" ] v in
            (name, match opt_str w fs "summary" with Some s -> s | None -> bad "%s: its summary" w));
      (* claude: a file of a subdirectory is read only from its own
       * directory's config: named here, it would be ignored in silence *)
      notes = obj "files" (fun w name v -> if String.contains name '/' then bad "%s: %s is in a subdirectory: describe it in that directory's own .codemapconfig" w name else (name, file_note w v));
      tours =
        opt_list where fs "tours" (fun w v ->
            let fs = fields w [ "name"; "stops" ] v in
            { name = (match opt_str w fs "name" with Some n -> n | None -> bad "%s: its name" w); stops = opt_list w fs "stops" item });
      skeletons =
        opt_list where fs "skeletons" (fun w v ->
            let fs = fields w [ "name"; "bones"; "joints" ] v in
            let need k = match opt_str w fs k with Some s -> s | None -> bad "%s: its %s" w k in
            let bones =
              opt_list w fs "bones" (fun w v ->
                  let fs = fields w [ "at"; "role" ] v in
                  let at = match opt_str w fs "at" with Some s -> s | None -> bad "%s: at, its anchor" w in
                  let role = match opt_str w fs "role" with Some r -> r | None -> bad "%s: its role" w in
                  match split at with
                  | Some p, anchor -> { bat = at; bpath = under dir p; banchor = anchor; role }
                  (* a whole file or directory: its path, no anchor *)
                  | None, p ->
                      let p = if String.length p > 1 && p.[String.length p - 1] = '/' then String.sub p 0 (String.length p - 1) else p in
                      { bat = at; bpath = under dir p; banchor = ""; role })
            in
            let joints =
              opt_list w fs "joints" (fun w v ->
                  let fs = fields w [ "from"; "to"; "say" ] v in
                  let bone k = match opt_str w fs k with Some s when List.exists (fun b -> b.bat = s) bones -> s | Some s -> bad "%s.%s: %s is none of the bones" w k s | None -> bad "%s: its %s" w k in
                  { jfrom = bone "from"; jto = bone "to"; jsay = opt_str w fs "say" })
            in
            { sname = need "name"; sdir = dir; bones; joints });
      views =
        opt_list where fs "views" (fun w v ->
            let fs = fields w [ "name"; "files"; "of"; "with" ] v in
            {
              vname = (match opt_str w fs "name" with Some n -> n | None -> bad "%s: its name" w);
              files = opt_list w fs "files" str;
              of_ = opt_str w fs "of";
              with_ = opt_str w fs "with";
            });
      marks =
        opt_list where fs "marks" (fun w v ->
            let fs = fields w [ "name"; "rules" ] v in
            let rules =
              opt_list w fs "rules" (fun w v ->
                  let fs = fields w [ "text"; "ref"; "color"; "say" ] v in
                  (* claude: a text, or a name referred to (ref:, the code's
                   * references only, not comments' or strings' words) *)
                  let text, is_ref =
                    match (opt_str w fs "text", opt_str w fs "ref") with
                    | Some t, None when String.length t >= 2 -> (t, false)
                    | None, Some r when String.length r >= 2 -> (r, true)
                    | Some _, Some _ -> bad "%s: text or ref, not both" w
                    | _ -> bad "%s: its text or ref, two characters at least" w
                  in
                  let colour = match opt_str w fs "color" with Some c -> ( match Code_config.hex c with Some rgb -> rgb | None -> bad "%s: %S is no #rrggbb" w c) | None -> bad "%s: its color" w in
                  { text; is_ref; colour; rsay = opt_str w fs "say" })
            in
            { mname = (match opt_str w fs "name" with Some n -> n | None -> bad "%s: its name" w); mdir = dir; rules });
      (* claude: anatomy: { nerves: [rule], lungs: [rule] }, the rules
       * without a colour (the plate's) *)
      nerves = sense where fs "nerves";
      lungs = sense where fs "lungs";
    }
  with
  | d -> Ok d
  | exception Bad msg -> Error msg

let load ~(read : string -> string option) (paths : string list) : t * string list =
  List.fold_left
    (fun (ds, errs) path ->
      let dir = match Filename.dirname path with "." -> "" | d -> d in
      match read path with
      | None -> (ds, (path ^ ": cannot be read") :: errs)
      | Some text -> (
          match Jsonnet.eval ~read ~path text with
          | Error e -> (ds, e :: errs)
          | Ok v -> ( match of_json ~dir v with Ok d -> (d :: ds, errs) | Error e -> (ds, e :: errs))))
    ([], []) paths
  |> fun (ds, errs) -> (List.rev ds, List.rev errs)

(*****************************************************************************)
(* What the map asks *)
(*****************************************************************************)

let dirs (t : t) = t
let dir_of (path : string) = match Filename.dirname path with "." -> "" | d -> d
let title (t : t) = Option.bind (List.find_opt (fun d -> d.dir = "") t) (fun d -> d.title)

let dir_summary (t : t) (path : string) : string option =
  match List.find_opt (fun d -> d.dir = path) t with
  | Some { summary = Some s; _ } -> Some s
  | _ -> Option.bind (List.find_opt (fun d -> d.dir = dir_of path) t) (fun d -> List.assoc_opt (Filename.basename path) d.subdirs)

let file_note (t : t) (path : string) : file_note option =
  Option.bind (List.find_opt (fun d -> d.dir = dir_of path) t) (fun d -> List.assoc_opt (Filename.basename path) d.notes)

let colours (t : t) = List.concat_map (fun d -> d.colors) t
let marks (t : t) : mark list = List.concat_map (fun d -> d.marks) t

(* claude: the anatomy rules applying to a file: its configs' and its
 * ancestors' *)
let senses (t : t) (path : string) : rule list * rule list =
  let over d = d.dir = "" || String.length path > String.length d.dir && String.sub path 0 (String.length d.dir + 1) = d.dir ^ "/" in
  let ds = List.filter over t in
  (List.concat_map (fun d -> d.nerves) ds, List.concat_map (fun d -> d.lungs) ds)

(* claude: a path of a config from the root, a directory's final slash
 * dropped *)
let rooted (dir : string) (p : string) : string =
  let p = under dir p in
  if String.length p > 1 && p.[String.length p - 1] = '/' then String.sub p 0 (String.length p - 1) else p

let views (t : t) : view list =
  List.concat_map (fun d -> List.map (fun v -> { v with files = List.map (rooted d.dir) v.files; of_ = Option.map (rooted d.dir) v.of_ }) d.views) t

let tours (t : t) : tour list =
  List.concat_map
    (fun d ->
      List.map
        (fun tr ->
          { tr with stops = List.map (fun (i : item) -> match split i.at with Some p, anchor -> { i with at = rooted d.dir p ^ ":" ^ anchor } | None, _ -> i) tr.stops })
        d.tours)
    t
let skeletons_of (t : t) (path : string) : skeleton list =
  List.concat_map (fun d -> List.filter (fun s -> List.exists (fun b -> b.bpath = path) s.bones) d.skeletons) t

let capitals (t : t) = List.concat_map (fun d -> List.concat_map (fun (name, n) -> List.map (fun i -> (under d.dir name, i)) n.capitals) d.notes) t

(*****************************************************************************)
(* Anchors *)
(*****************************************************************************)

(* a line's characters, as the grid keeps them (a space a 0) *)
let line_text (f : Code_file.t) (l : int) : string =
  String.map (fun c -> if c = '\000' then ' ' else c) (Bytes.sub_string f.chars (l * Code_file.cols) Code_file.cols)

let contains (s : string) (sub : string) : int option =
  let n = String.length s and m = String.length sub in
  let rec go i = if i > n - m then None else if String.sub s i m = sub then Some i else go (i + 1) in
  if m = 0 then None else go 0

(* claude: a syncweb marker's chunk (principia's configs anchor 392
 * capitals at comment:"function [[namec]]", the marker above it): the
 * first line of code under it, the markers and blank lines skipped;
 * another comment, itself *)
let below_marker (f : Code_file.t) (l : int) : int =
  let blank l = String.trim (line_text f l) = "" in
  let rec go k = if k < Code_file.nlines f && (Code_file.syncweb_marker f k || blank k) then go (k + 1) else k in
  if Code_file.syncweb_marker f l then (match go l with k when k < Code_file.nlines f -> k | _ -> l) else l

(* claude: what an anchor names, to show: its words without the kind
 * and the quotes, and a syncweb chunk's name without its kind and
 * brackets, comment:"struct [[Window]]" as Window (the author: not
 * "struct [[Window]]" on the map) *)
let anchor_name (at : string) : string =
  let what = snd (split at) in
  let what = match String.index_opt what ':' with Some i when List.mem (String.sub what 0 i) kinds -> String.sub what (i + 1) (String.length what - i - 1) | _ -> what in
  let n = String.length what in
  let what = if n >= 2 && what.[0] = '"' && what.[n - 1] = '"' then String.sub what 1 (n - 2) else what in
  let rec find_from s sub i = if i + String.length sub > String.length s then None else if String.sub s i (String.length sub) = sub then Some i else find_from s sub (i + 1) in
  match find_from what "[[" 0 with
  | Some a -> ( match find_from what "]]" (a + 2) with Some b -> String.sub what (a + 2) (b - a - 2) | None -> what)
  | None -> what

let find (f : Code_file.t) (anchor : string) : (int, string) result =
  let kind, what = match String.index_opt anchor ':' with Some i -> (String.sub anchor 0 i, String.sub anchor (i + 1) (String.length anchor - i - 1)) | None -> ("", anchor) in
  let unquote s = let n = String.length s in if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s in
  (* claude: a name defined twice (a C prototype, then its body): the
   * body's line, the definition of rank 3 (the Linux 0.01 pass: def:
   * landed on forward declarations) *)
  let def cats =
    match List.filter (fun (_, name, cat) -> name = what && List.mem cat cats) f.defs with
    | [] -> Error (Printf.sprintf "%s: no %s %s" f.path kind what)
    | [ (l, _, _) ] -> Ok l
    | (l0, _, _) :: _ as all -> (
        let body = List.find_opt (fun (l, _, _) -> List.exists (fun (d : Highlight_code.definition) -> d.dline = l && d.drank >= 3 && (d.dname = what || "_" ^ d.dname = what)) f.definitions) all in
        match body with Some (l, _, _) -> Ok l | None -> Ok l0)
  in
  match kind with
  | "def" -> def [ Def_function; Def_value ]
  | "type" -> def [ Def_type ]
  | "module" -> def [ Def_module ]
  | "section" -> def [ Comment_section ]
  | "line" -> (
      match int_of_string_opt what with
      | Some n when n >= 1 && n <= Code_file.nlines f -> Ok (n - 1)
      | _ -> Error (Printf.sprintf "%s: no line %s" f.path what))
  | "comment" -> (
      let words = unquote what in
      let rec go l =
        if l >= Code_file.nlines f then Error (Printf.sprintf "%s: no comment saying %S" f.path words)
        else
          match contains (line_text f l) words with
          | Some c when (match Code_file.at f l c with Some (Comment | Comment_section) -> true | _ -> false) -> Ok (below_marker f l)
          | _ -> go (l + 1)
      in
      go 0)
  (* claude: code:"words", the first line of code containing them (not a
   * comment): the tricky lines 1991 C left uncommented *)
  | "code" -> (
      let words = unquote what in
      let rec go l =
        if l >= Code_file.nlines f then Error (Printf.sprintf "%s: no code saying %S" f.path words)
        else
          match contains (line_text f l) words with
          | Some c when (match Code_file.at f l c with Some (Comment | Comment_section) -> false | _ -> true) -> Ok l
          | _ -> go (l + 1)
      in
      go 0)
  | "pattern" -> Error "pattern: anchors are to come (plan_codemap_v2.md, Marks)"
  | k -> Error (Printf.sprintf "%S: an anchor is %s:..." anchor (String.concat ", " (List.filter (( <> ) k) kinds)))

let digest (text : string) : string = String.sub (Digest.to_hex (Digest.string text)) 0 12

(*****************************************************************************)
(* The checker *)
(*****************************************************************************)

let check (t : t) ~(file : string -> (Code_file.t * string) option) ~(exists : string -> bool) : (string, string) result list =
  let out = ref [] in
  let err fmt = Printf.ksprintf (fun s -> out := Error s :: !out) fmt in
  let warn fmt = Printf.ksprintf (fun s -> out := Ok s :: !out) fmt in
  (* an anchor in [path]'s file *)
  let anchor where path a =
    match file path with
    | None -> err "%s: %s is not a source here" where path
    | Some (f, _) -> ( match find f a with Ok _ -> () | Error e -> err "%s: %s" where e)
  in
  (* an anchor with maybe a path first, from the config's directory, or
   * from [default] *)
  let anchored where d default a = match split a with Some p, a -> anchor where (under d.dir p) a | None, a -> anchor where default a in
  List.iter
    (fun d ->
      let conf = if d.dir = "" then ".codemapconfig" else d.dir ^ "/.codemapconfig" in
      List.iter
        (fun (name, n) ->
          let path = under d.dir name in
          let where = Printf.sprintf "%s: %s" conf name in
          match file path with
          | None -> err "%s: no such source" where
          | Some (_, text) ->
              (match n.digest with
              | Some dg when dg <> digest text -> warn "%s: changed since it was described (digest now %s)" where (digest text)
              | None -> warn "%s: no digest (now %s)" where (digest text)
              | _ -> ());
              List.iter (fun (i : item) -> anchored where d path i.at) (n.capitals @ n.important);
              List.iter (fun (a, b) -> anchored where d path a; anchored where d path b) n.links;
              List.iter (fun r -> if not (exists (under d.dir r)) then err "%s: related %s not found" where r) n.related)
        d.notes;
      List.iter
        (fun tr ->
          List.iter
            (fun (i : item) ->
              match split i.at with
              | Some p, a -> anchor (Printf.sprintf "%s: tour %S" conf tr.name) (under d.dir p) a
              | None, _ -> err "%s: tour %S: %s: a stop names its file" conf tr.name i.at)
            tr.stops)
        d.tours;
      List.iter
        (fun s ->
          List.iter
            (fun b ->
              let where = Printf.sprintf "%s: skeleton %S" conf s.sname in
              if b.banchor = "" then (if not (exists b.bpath) then err "%s: %s not found" where b.bpath) else anchor where b.bpath b.banchor)
            s.bones)
        d.skeletons;
      List.iter (fun (name, _) -> if not (exists (under d.dir name)) then err "%s: dirs: %s not found" conf name) d.subdirs;
      List.iter
        (fun v ->
          List.iter (fun p -> if not (exists (under d.dir p)) then err "%s: view %S: %s not found" conf v.vname p) v.files;
          Option.iter (fun p -> if not (exists (under d.dir p)) then err "%s: view %S: %s not found" conf v.vname p) v.of_;
          match v.with_ with Some ("users" | "uses" | "both" | "related") | None -> () | Some w -> err "%s: view %S: with: users, uses, both or related, not %s" conf v.vname w)
        d.views)
    t;
  List.rev !out
