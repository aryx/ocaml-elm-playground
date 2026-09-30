(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_roles.mli *)

type category =
  | Generated
  | Test
  | Example
  | Third_party
  | Architecture
  | Os
  | Parsing
  | Network
  | Graphics
  | Audio
  | Storage
  | Security
  | Utils
  | Entry
  | Interface
  | Plain

let all = [ Generated; Test; Example; Third_party; Architecture; Os; Parsing; Network; Graphics; Audio; Storage; Security; Utils; Entry; Interface; Plain ]

let name = function
  | Generated -> "generated"
  | Test -> "tests"
  | Example -> "examples"
  | Third_party -> "third party"
  | Architecture -> "per CPU"
  | Os -> "per OS"
  | Parsing -> "parsing"
  | Network -> "network"
  | Graphics -> "graphics, UI"
  | Audio -> "audio"
  | Storage -> "storage"
  | Security -> "security"
  | Utils -> "utilities"
  | Entry -> "entry points"
  | Interface -> "interfaces"
  | Plain -> "the rest"

let colour = function
  | Generated -> (120, 130, 160)
  | Test -> (80, 200, 120)
  | Example -> (160, 225, 150)
  | Third_party -> (140, 140, 140)
  | Architecture -> (225, 90, 60)
  | Os -> (240, 150, 60)
  | Parsing -> (195, 120, 235)
  | Network -> (70, 170, 245)
  | Graphics -> (240, 205, 70)
  | Audio -> (240, 110, 180)
  | Storage -> (170, 120, 70)
  | Security -> (205, 45, 60)
  | Utils -> (100, 215, 215)
  | Entry -> (250, 250, 250)
  | Interface -> (155, 155, 235)
  | Plain -> (90, 90, 100)

(*****************************************************************************)
(* Words *)
(*****************************************************************************)

(* a component's words: at its punctuation, then at a small letter
 * followed by a capital, and at a capital followed by a capital and a
 * small letter (HTTPServer: http, server); lowercased *)
let split_words (s : string) : string list =
  let is_alnum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') in
  let is_lower c = c >= 'a' && c <= 'z' and is_upper c = c >= 'A' && c <= 'Z' in
  let pieces = String.split_on_char ' ' (String.map (fun c -> if is_alnum c then c else ' ') s) |> List.filter (fun p -> p <> "") in
  List.concat_map
    (fun p ->
      let n = String.length p in
      let cuts = ref [] in
      for i = 1 to n - 1 do
        if (is_lower p.[i - 1] && is_upper p.[i]) || (i + 1 < n && is_upper p.[i - 1] && is_upper p.[i] && is_lower p.[i + 1]) then cuts := i :: !cuts
      done;
      let rec go from = function c :: rest -> String.sub p from (c - from) :: go c rest | [] -> [ String.sub p from (n - from) ] in
      List.map String.lowercase_ascii (go 0 (List.rev !cuts)))
    pieces

let words (path : string) : string list =
  let parts = String.split_on_char '/' path |> List.filter (fun p -> p <> "") in
  let n = List.length parts in
  List.concat
    (List.mapi
       (fun i part ->
         let stem, ext =
           if i = n - 1 then
             match String.rindex_opt part '.' with Some k when k > 0 -> (String.sub part 0 k, [ String.lowercase_ascii (String.sub part (k + 1) (String.length part - k - 1)) ]) | _ -> (part, [])
           else (part, [])
         in
         (* the component whole too: amd64, x86_64 *)
         let whole = String.lowercase_ascii stem in
         let ws = split_words stem in
         (if List.mem whole ws then ws else whole :: ws) @ ext)
       parts)

(*****************************************************************************)
(* The categories' words *)
(*****************************************************************************)

let test_words = [ "test"; "tests"; "testing"; "testsuite"; "unittest"; "unittests"; "spec"; "specs"; "bench"; "benchmark"; "benchmarks"; "regression"; "fixture"; "fixtures"; "golden" ]
let example_words = [ "example"; "examples"; "demo"; "demos"; "sample"; "samples"; "tutorial"; "tutorials" ]
let third_party_words = [ "vendor"; "vendors"; "vendored"; "external"; "extern"; "contrib"; "thirdparty"; "3rdparty"; "third_party" ]

let arch_words =
  [ "386"; "i386"; "x86"; "x86_64"; "amd64"; "x64"; "arm"; "arm64"; "aarch64"; "mips"; "riscv"; "ppc"; "powerpc"; "sparc"; "m68k"; "alpha"; "ia64"; "s390"; "omap"; "bcm"; "raspi"; "arch" ]

let os_words = [ "unix"; "linux"; "macos"; "osx"; "darwin"; "win32"; "win64"; "w32"; "plan9"; "posix"; "freebsd"; "netbsd"; "openbsd"; "bsd"; "cygwin"; "msdos" ]
let parsing_words = [ "parse"; "parser"; "parsers"; "parsing"; "lexer"; "lexers"; "lex"; "scanner"; "grammar"; "tokens"; "tokenizer"; "yacc"; "ast" ]

let network_words =
  [ "net"; "network"; "networking"; "netcode"; "http"; "https"; "tcp"; "udp"; "ip"; "ipv4"; "ipv6"; "dns"; "dhcp"; "ftp"; "ssh"; "smtp"; "imap"; "pop3"; "socket"; "sockets";
    "websocket"; "url"; "uri"; "irc"; "ndb" ]

let graphics_words =
  [ "gui"; "ui"; "draw"; "drawing"; "render"; "renderer"; "rendering"; "screen"; "display"; "graphics"; "gfx"; "image"; "images"; "font"; "fonts"; "paint"; "canvas"; "pixel";
    "pixels"; "sprite"; "sprites"; "texture"; "textures"; "raster"; "rasterizer"; "vga"; "window"; "windows"; "widget"; "widgets" ]

let audio_words = [ "audio"; "sound"; "sounds"; "sfx"; "music"; "synth"; "synthesis"; "midi"; "wav"; "mp3"; "mixer"; "voice"; "voices"; "tracker" ]
let storage_words = [ "db"; "database"; "databases"; "sql"; "sqlite"; "storage"; "store"; "fs"; "filesystem"; "filesystems"; "disk"; "disks"; "bdb"; "cache" ]

let security_words =
  [ "security"; "crypto"; "cryptography"; "auth"; "authentication"; "tls"; "ssl"; "cert"; "certs"; "x509"; "rsa"; "aes"; "sha"; "password"; "passwords"; "secret"; "secrets"; "factotum" ]

let utils_words = [ "util"; "utils"; "utility"; "utilities"; "common"; "commons"; "helper"; "helpers"; "misc" ]

(*****************************************************************************)
(* A file's category *)
(*****************************************************************************)

let ext (p : string) : string = match String.rindex_opt p '.' with Some k -> String.sub p (k + 1) (String.length p - k - 1) | None -> ""
let stem (p : string) : string = match String.rindex_opt p '.' with Some k -> String.sub p 0 k | None -> p

let categories ~(links : (string * string * int) list) (paths : string list) : (string, category) Hashtbl.t =
  let exists = Hashtbl.create 1024 in
  List.iter (fun p -> Hashtbl.replace exists p ()) paths;
  let beside p e = Hashtbl.mem exists (stem p ^ "." ^ e) in
  let ins = Hashtbl.create 1024 and outs = Hashtbl.create 1024 in
  List.iter (fun (a, b, _) -> if a <> b then begin Hashtbl.replace outs a (); Hashtbl.replace ins b () end) links;
  let tbl = Hashtbl.create 1024 in
  List.iter
    (fun p ->
      let ws = words p in
      let has l = List.exists (fun w -> List.mem w l) ws in
      let base_words = words (Filename.basename p) in
      let e = ext p in
      let evidence = function
        | Generated ->
            (e = "ml" && (beside p "mll" || beside p "mly")) || (e = "mli" && beside p "mly") || (e = "c" && (beside p "y" || beside p "l"))
            || Filename.basename p = "y.tab.c" || Filename.basename p = "y.tab.h" || List.mem "generated" ws
        (* a test's words anywhere; unit_ and test_ files by their first word *)
        | Test -> has test_words || (match base_words with ("unit" | "test") :: _ -> true | _ -> false) || List.mem "t" (String.split_on_char '/' (Filename.dirname p))
        | Example -> has example_words
        | Third_party -> has third_party_words
        | Architecture -> List.mem e [ "s"; "S"; "asm" ] || has arch_words || List.mem "pc" (String.split_on_char '/' (Filename.dirname p))
        | Os -> has os_words
        | Parsing -> List.mem e [ "mll"; "mly"; "y"; "l" ] || has parsing_words
        | Network -> has network_words
        | Graphics -> has graphics_words
        | Audio -> has audio_words
        | Storage -> has storage_words
        | Security -> has security_words
        | Utils -> has utils_words
        (* used by no file, using some: a program's main, a game, a tool *)
        | Entry -> (not (Hashtbl.mem ins p)) && Hashtbl.mem outs p && not (List.mem e [ "mli"; "h" ])
        | Interface -> List.mem e [ "mli"; "h"; "hpp" ]
        | Plain -> true
      in
      Hashtbl.replace tbl p (List.find evidence all))
    paths;
  tbl
