(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_url.mli *)

let parse (s : string) : Url.t = match Url.parse s with Ok u -> u | Error e -> Alcotest.fail e

(* RFC 3986 section 5.4.1, "Normal Examples", from the base below *)
let normal =
  [
    ("g:h", "g:h");
    ("g", "http://a/b/c/g");
    ("./g", "http://a/b/c/g");
    ("g/", "http://a/b/c/g/");
    ("/g", "http://a/g");
    ("//g", "http://g");
    ("?y", "http://a/b/c/d;p?y");
    ("g?y", "http://a/b/c/g?y");
    ("#s", "http://a/b/c/d;p?q#s");
    ("g#s", "http://a/b/c/g#s");
    ("g?y#s", "http://a/b/c/g?y#s");
    (";x", "http://a/b/c/;x");
    ("g;x", "http://a/b/c/g;x");
    ("g;x?y#s", "http://a/b/c/g;x?y#s");
    ("", "http://a/b/c/d;p?q");
    (".", "http://a/b/c/");
    ("./", "http://a/b/c/");
    ("..", "http://a/b/");
    ("../", "http://a/b/");
    ("../g", "http://a/b/g");
    ("../..", "http://a/");
    ("../../", "http://a/");
    ("../../g", "http://a/g");
  ]

(* section 5.4.2, "Abnormal Examples" *)
let abnormal =
  [
    ("../../../g", "http://a/g");
    ("../../../../g", "http://a/g");
    ("/./g", "http://a/g");
    ("/../g", "http://a/g");
    ("g.", "http://a/b/c/g.");
    (".g", "http://a/b/c/.g");
    ("g..", "http://a/b/c/g..");
    ("..g", "http://a/b/c/..g");
    ("./../g", "http://a/b/g");
    ("./g/.", "http://a/b/c/g/");
    ("g/./h", "http://a/b/c/g/h");
    ("g/../h", "http://a/b/c/h");
    ("g;x=1/./y", "http://a/b/c/g;x=1/y");
    ("g;x=1/../y", "http://a/b/c/y");
    ("g?y/./x", "http://a/b/c/g?y/./x");
    ("g?y/../x", "http://a/b/c/g?y/../x");
    ("g#s/./x", "http://a/b/c/g#s/./x");
    ("g#s/../x", "http://a/b/c/g#s/../x");
    ("http:g", "http:g");
  ]

let check_resolutions (count : int) (examples : (string * string) list) () =
  let base = parse "http://a/b/c/d;p?q" in
  Alcotest.(check int) "the RFC's count" count (List.length examples);
  List.iter
    (fun (reference, expected) ->
      Alcotest.(check string) reference expected (Url.to_string (Url.resolve base (parse reference))))
    examples

(* appendix B's regexp, in Str's syntax (its groups written \\( \\),
 * its literal '?' \\?): groups 2, 4, 5, 7 and 9 are the five parts *)
let appendix_b = Str.regexp {|^\(\([^:/?#]+\):\)?\(//\([^/?#]*\)\)?\([^?#]*\)\(\?\([^#]*\)\)?\(#\(.*\)\)?|}

let by_the_regexp (s : string) : string option * string option * string * string option * string option =
  assert (Str.string_match appendix_b s 0);
  let group n = try Some (Str.matched_group n s) with Not_found -> None in
  (group 2, group 4, Option.value ~default:"" (group 5), group 7, group 9)

let by_parse (s : string) : string option * string option * string * string option * string option =
  let u = parse s in
  let authority (a : Url.authority) =
    let s = Url.to_string { scheme = None; authority = Some a; path = ""; query = None; fragment = None } in
    String.sub s 2 (String.length s - 2)
  in
  (u.scheme, Option.map authority u.authority, u.path, u.query, u.fragment)

let tests =
  Testo.categorize "Url"
    [
      Testo.create "the five parts of the diagram" (fun () ->
          let u = parse "http://elm-lang.org:80/images/turtle.gif?size=96#top" in
          Alcotest.(check (option string)) "scheme" (Some "http") u.scheme;
          Alcotest.(check (option string)) "host" (Some "elm-lang.org") (Option.map (fun (a : Url.authority) -> a.host) u.authority);
          Alcotest.(check (option int)) "port" (Some 80) (Url.port u);
          Alcotest.(check string) "path" "/images/turtle.gif" u.path;
          Alcotest.(check (option string)) "query" (Some "size=96") u.query;
          Alcotest.(check (option string)) "fragment" (Some "top") u.fragment;
          Alcotest.(check string) "the request target, no fragment" "/images/turtle.gif?size=96" (Url.request_target u));
      Testo.create "absent is not empty" (fun () ->
          Alcotest.(check (option string)) "\"?\" has an empty query" (Some "") (parse "?").query;
          Alcotest.(check (option string)) "\"\" has none" None (parse "").query);
      Testo.create "the default ports, and a host in brackets" (fun () ->
          Alcotest.(check (option int)) "http" (Some 80) (Url.port (parse "http://a/"));
          Alcotest.(check (option int)) "https" (Some 443) (Url.port (parse "HTTPS://A/"));
          Alcotest.(check (option int)) "an empty port is the default" (Some 80) (Url.port (parse "http://a:/"));
          Alcotest.(check (option int)) "IPv6" (Some 8001) (Url.port (parse "http://[::1]:8001/"));
          Alcotest.(check string) "an empty path asks for /" "/" (Url.request_target (parse "http://a")));
      Testo.create "a bad port" (fun () ->
          Alcotest.(check bool) "refused" true (Result.is_error (Url.parse "http://a:http/")));
      Testo.create "the hand-written parse agrees with appendix B's regexp" (fun () ->
          let parts = Alcotest.(pair (pair (option string) (option string)) (triple string (option string) (option string))) in
          let flat ((a, b, c, d, e) : _ * _ * _ * _ * _) = ((a, b), (c, d, e)) in
          List.iter
            (fun s -> Alcotest.check parts s (flat (by_the_regexp s)) (flat (by_parse s)))
            ([ "http://elm-lang.org:80/images/turtle.gif?size=96#top"; "http://user@a:8001/b?c#d"; "a:b:c"; "/p:q"; "./a:b";
               "http:"; "//"; "?#"; "#?"; "g?y#s?t"; "mailto:pad@example.com"; "urn:isbn:0451450523"; "http://[::1]:8001/x" ]
            @ List.map fst (normal @ abnormal)));
      Testo.create "back to the same string" (fun () ->
          List.iter
            (fun s -> Alcotest.(check string) s s (Url.to_string (parse s)))
            [ "http://user@a:8001/b/c?q#f"; "g:h"; "//g"; "?"; "#"; ""; "../g;x?y#s"; "mailto:pad@example.com" ]);
      Testo.create "remove_dot_segments, the RFC's two traces" (fun () ->
          Alcotest.(check string) "absolute" "/a/g" (Url.remove_dot_segments "/a/b/c/./../../g");
          Alcotest.(check string) "relative" "mid/6" (Url.remove_dot_segments "mid/content=5/../6"));
      Testo.create "RFC 3986 5.4.1, the 23 normal examples" (check_resolutions 23 normal);
      Testo.create "RFC 3986 5.4.2, the 19 abnormal examples" (check_resolutions 19 abnormal);
    ]
