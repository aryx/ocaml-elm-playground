(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_anatomy.mli *)

type system = Skeleton | Blood | Muscles | Nerves | Lungs | Skin

let all = [ Skeleton; Blood; Muscles; Nerves; Lungs; Skin ]

let name = function
  | Skeleton -> "skeleton"
  | Blood -> "blood"
  | Muscles -> "muscles"
  | Nerves -> "nerves"
  | Lungs -> "lungs"
  | Skin -> "skin"

let colour = function
  | Skeleton -> (245, 232, 200)
  | Blood -> (225, 45, 70)
  | Muscles -> (235, 120, 80)
  | Nerves -> (250, 220, 80)
  | Lungs -> (110, 195, 250)
  | Skin -> (240, 170, 200)

let meaning = function
  | Skeleton -> "the architecture, its parts' roles"
  | Blood -> "the data flowing between the parts"
  | Muscles -> "the loop-heavy code, the work"
  | Nerves -> "the inputs: keyboard, mouse, events"
  | Lungs -> "the I/O: files, network, console"
  | Skin -> "exported: what the .mli shows, the rest shaded"

(* claude: what a plate marks, how it is found, what to look for: its
 * legend row's card (the author: "a way to understand what those plates
 * are by hovering on it") *)
let explain = function
  | Skeleton ->
      [ "The bones: the few definitions the rest hangs on, and the joints between them.";
        "Written in the configs (a role per bone), or derived from the code: its capitals, its most used definitions.";
        "Look for: how the parts connect; x shows the next skeleton." ]
  | Blood ->
      [ "What flows along the skeleton's joints: pulses moving from one bone to the next.";
        "Found from the joints' direction: from the user to the used.";
        "Look for: which way the data goes, and where it loops back." ]
  | Muscles ->
      [ "Where the work is: definitions dense with loops (for, while, List.iter, fold...).";
        "Found by counting loop words per line of each definition: the denser, the redder.";
        "Look for: the inner loops, where time is spent: a rasterizer's, a solver's." ]
  | Nerves ->
      [ "Where the program senses its user: keyboard, mouse, events, touches.";
        "Found by the words: keyboard, mouse, key, click, pressed, event...";
        "Look for: where input enters and which definitions react to it." ]
  | Lungs ->
      [ "Where the program breathes with the world: files, network, console, processes.";
        "Found by the words: open_in, read, write, socket, print, Unix., Cap....";
        "Look for: the edges of the program, what may fail or block." ]
  | Skin ->
      [ "What a module shows the others: the definitions its .mli exports.";
        "Found from the .mli: exported definitions barred, the private ones shaded.";
        "Look for: the surface to learn first; what is only inside." ]

let key = function Skeleton -> "1" | Blood -> "2" | Muscles -> "3" | Nerves -> "4" | Lungs -> "5" | Skin -> "6"

let shown = ref [ Skeleton ]
let toggle s = shown := if List.mem s !shown then List.filter (( <> ) s) !shown else s :: !shown

(*****************************************************************************)
(* The words *)
(*****************************************************************************)

let nerve_words =
  [ "keyboard"; "mouse"; "mclick"; "mdown"; "mwheel"; "mdouble"; "kspace"; "kup"; "kdown"; "kleft"; "kright"; "to_x"; "to_y";
    "Keyboard."; "Sub."; "Cmd."; "on_key"; "pressed"; "typed" ]

let lung_words =
  [ "Cap."; "In_channel."; "Out_channel."; "Unix."; "Sys.readdir"; "Sys.file_exists"; "Sys.getenv"; "Sys.argv"; "open_in"; "open_out";
    "print_string"; "print_endline"; "prerr_endline"; "Printf.printf"; "Printf.eprintf"; "Http"; "Tcp."; "Udp."; "Websocket";
    "Download."; "Playground_platform."; "Random.self_init"; "fopen"; "fread"; "fwrite"; "printf("; "syscall" ]

let is_ident c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\''

(* a line's characters, as the grid keeps them (a space a 0) *)
let line_text (f : Code_file.t) (l : int) : string =
  String.map (fun c -> if c = '\000' then ' ' else c) (Bytes.sub_string f.chars (l * Code_file.cols) Code_file.cols)

(* [word] in [s] at [i], not in the middle of a name, in code *)
let found_at (f : Code_file.t) (l : int) (s : string) (i : int) (word : string) : bool =
  let n = String.length word in
  i + n <= String.length s
  && String.sub s i n = word
  && (i = 0 || not (is_ident s.[i - 1]))
  && (match Code_file.at f l i with Some (Comment | Comment_section | String) -> false | _ -> true)

let line_has (f : Code_file.t) (l : int) (words : string list) : bool =
  let s = line_text f l in
  let n = String.length s in
  let rec at i = i < n && (List.exists (found_at f l s i) words || at (i + 1)) in
  at 0

(*****************************************************************************)
(* The facts *)
(*****************************************************************************)

type facts = { nerves : int list; lungs : int list; muscles : (int * int * float) list; skin : int list; hidden : (int * int) list }

let loop_words = [ "for"; "while"; "List.iter"; "List.map"; "List.fold_left"; "List.fold_right"; "List.filter"; "List.concat_map"; "Array.iter"; "Array.iteri"; "Array.map"; "Array.init"; "Array.fold_left"; "Hashtbl.iter"; "Seq." ]

let facts (f : Code_file.t) ~(public : string list option) : facts =
  let n = Code_file.nlines f in
  let lines words = List.filter (fun l -> line_has f l words) (List.init n Fun.id) in
  (* the top-level definitions, each to the next *)
  let heads =
    List.filter_map (fun (l, name, (cat : Highlight_code.category)) -> match cat with Def_function | Def_value | Def_type | Def_module -> Some (l, name) | _ -> None) f.defs
    |> List.sort_uniq compare
  in
  let rec extents = function
    | (l, name) :: ((l', _) :: _ as rest) -> (l, l' - 1, name) :: extents rest
    | [ (l, name) ] -> [ (l, n - 1, name) ]
    | [] -> []
  in
  let defs = extents heads in
  let muscles =
    List.map
      (fun (a, b, _) ->
        let loops = List.length (List.filter (fun l -> line_has f l loop_words) (List.init (b - a + 1) (fun k -> a + k))) in
        (* work is loops, not size: a definition as dense with them as a
         * rasterizer's inner function (one line in four) makes 1 *)
        (a, b, 4. *. float_of_int loops /. float_of_int (b - a + 1 + 8)))
      defs
  in
  let skin = match public with None -> [] | Some names -> List.filter_map (fun (a, _, name) -> if List.mem name names then Some a else None) defs in
  (* claude: what the .mli does not show: the private definitions' lines *)
  let hidden = match public with None -> [] | Some names -> List.filter_map (fun (a, b, name) -> if List.mem name names then None else Some (a, b)) defs in
  { nerves = lines nerve_words; lungs = lines lung_words; muscles; skin; hidden }

let public_names (f : Code_file.t) : string list =
  List.filter_map (fun (_, name, (cat : Highlight_code.category)) -> match cat with Def_function | Def_value | Def_type | Def_module -> Some name | _ -> None) f.defs
