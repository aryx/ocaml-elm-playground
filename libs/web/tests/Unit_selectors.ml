(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_selectors.mli *)

let one (s : string) : Selectors.complex = match Selectors.parse_string s with Some [ c ] -> c | _ -> Alcotest.fail ("not one selector: " ^ s)

(* the ids of the elements [sel] matches, in document order *)
let matching (sel : string) (html : string) : string list =
  let sels = match Selectors.parse_string sel with Some l -> l | None -> Alcotest.fail ("not a selector: " ^ sel) in
  let found = ref [] in
  let rec go ancestors (e : Dom.element) =
    if List.exists (fun s -> Selectors.matches s ancestors e) sels then Option.iter (fun id -> found := id :: !found) (Dom.attribute "id" e);
    List.iter (fun (n : Dom.node) -> match n with Element c -> go (e :: ancestors) c | Text _ -> ()) e.children
  in
  go [] (Html_tree.of_string html);
  List.rev !found

let check what sel html expected = Alcotest.(check (list string)) (what ^ ": " ^ sel) expected (matching sel html)

let page =
  "<div id=d1 class=\"box main\"><ul id=u1><li id=a class=item>a<li id=b class=\"item done\">b<li id=c>c</ul>"
  ^ "<p id=p1>x <a id=l1 href=\"https://x.org/a.pdf\" lang=en-US>l</a></p></div><p id=p2>y</p>"

let tests =
  Testo.categorize "Selectors"
    [
      Testo.create "the worked example: specificity" (fun () ->
          Alcotest.(check (list (triple int int int))) "ul > li.item:not(.done), #nav a:hover, *"
            [ (0, 2, 2); (1, 1, 1); (0, 0, 0) ]
            (List.map (fun s -> Selectors.specificity (one s)) [ "ul > li.item:not(.done)"; "#nav a:hover"; "*" ]));
      Testo.create "what is not understood: refused" (fun () ->
          Alcotest.(check bool) "a:unknown" true (Selectors.parse_string "a:unknown" = None);
          Alcotest.(check bool) "a dangling >" true (Selectors.parse_string "ul >" = None);
          Alcotest.(check string) "written back" "ul > li.item:not(.done)" (Selectors.to_string (one "ul>li.item:not( .done )")));
      Testo.create "combinators" (fun () ->
          check "descendant" "div li" page [ "a"; "b"; "c" ];
          check "child: li is not the div's" "div > li" page [];
          check "backtracking: the nearest div fails, one further up matches" ".main p a" page [ "l1" ];
          check "next sibling" "#a + li" page [ "b" ];
          check "later siblings" "#a ~ li" page [ "b"; "c" ];
          check "a group" "#c, #p2" page [ "c"; "p2" ]);
      Testo.create "attributes" (fun () ->
          check "[href]" "[href]" page [ "l1" ];
          check "^= and $=" "a[href^=\"https:\"][href$=\".pdf\"]" page [ "l1" ];
          check "~= one of the words" "[class~=done]" page [ "b" ];
          check "|= a language" "[lang|=en]" page [ "l1" ];
          check "*= contains, case ignored" "[href*=\"X.ORG\" i]" page [ "l1" ];
          Alcotest.(check bool) "an unquoted value is one identifier" true (Selectors.parse_string "[href*=X.ORG]" = None));
      Testo.create "pseudo-classes" (fun () ->
          check "the notes' selector" "ul > li.item:not(.done)" page [ "a" ];
          check ":first-child, :last-child" "li:first-child, li:last-child" page [ "a"; "c" ];
          check ":nth-child(2n+1)" "li:nth-child(odd)" page [ "a"; "c" ];
          check ":link" "a:link" page [ "l1" ];
          check ":hover never, the page static" "a:hover" page []);
      Testo.create "a pseudo-element's rule is its box's" (fun () ->
          Alcotest.(check (option string)) "p::before" (Some "before") (Selectors.pseudo_element (one "p::before"));
          Alcotest.(check (option string)) "p" None (Selectors.pseudo_element (one "p")));
    ]
