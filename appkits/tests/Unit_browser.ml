(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_browser.mli *)

(* a page read with no pictures, nothing visited, 976 wide *)
let page (url : string) (html : string) : Browser_page.t =
  Browser_page.read
    { extensions = false; css = false; width = 976.; breaker = Html_layout.greedy; visited = (fun _ -> false); picture = (fun _ -> None) }
    url 200 (Some "text/html") html

let element (p : Browser_page.t) (name : string) : Dom.element =
  List.find (fun e -> Dom.attribute "name" e = Some name) (Dom.find_all "input" p.tree)

let tests =
  Testo.categorize "Browser"
    [
      Testo.create "the history: the worked example" (fun () ->
          let h = Browser_history.(visit "B" (visit "A" empty)) in
          (* at C, A and B behind *)
          let b, h = Option.get (Browser_history.back "C" h) in
          Alcotest.(check string) "Back: B" "B" b;
          Alcotest.(check (pair (list string) (list string))) "A behind, C ahead" ([ "A" ], [ "C" ]) (h.behind, h.ahead);
          let h = Browser_history.visit "B" h in
          (* at D now *)
          Alcotest.(check (pair (list string) (list string))) "visit D: C is gone" ([ "B"; "A" ], []) (h.behind, h.ahead);
          Alcotest.(check bool) "no forward" true (Browser_history.forward "D" h = None));
      Testo.create "URLs: resolved, split" (fun () ->
          Alcotest.(check string)
            "relative" "http://info.cern.ch/hypertext/WWW/Help.html#people"
            (Browser_url.resolve "http://info.cern.ch/hypertext/WWW/TheProject.html" "Help.html#people");
          Alcotest.(check (pair string (option string))) "the fragment" ("a", Some "b") (Browser_url.split_fragment "a#b");
          Alcotest.(check (pair string (option string))) "the query" ("a", Some "q=1") (Browser_url.split_query "a?q=1"));
      Testo.create "a form sent: GET into the URL, POST into the body" (fun () ->
          let p = page "http://a/dir/page.html" "<form action=search><input name=q value=\"caf&eacute; au lait\"></form><form action=/order method=post><input name=n value=2></form>" in
          let get, post = match p.forms with [ g; o ] -> (g, o) | _ -> Alcotest.fail "two forms" in
          Alcotest.(check (pair string (option (pair string string))))
            "GET" ("http://a/dir/search?q=caf%C3%A9+au+lait", None)
            (Browser_forms.submission p get ~submitter:None);
          Alcotest.(check (pair string (option (pair string string))))
            "POST" ("http://a/order", Some ("application/x-www-form-urlencoded", "n=2"))
            (Browser_forms.submission p post ~submitter:None));
      Testo.create "clicks: a radio button is the one of its name" (fun () ->
          let p = page "about:x" "<form><input type=radio name=r value=1 checked><input type=radio name=r value=2 id=two></form>" in
          let radios = Dom.find_all "input" p.tree in
          let second = List.nth radios 1 in
          match Browser_forms.click p second with
          | Changed p ->
              Alcotest.(check (list bool)) "the first unchecked, the second checked" [ false; true ]
                (List.map (fun e -> (Browser_page.value_of p e).checked) radios)
          | _ -> Alcotest.fail "a change");
      Testo.create "keys: typed, Backspace, Return sends" (fun () ->
          let p = page "about:x" "<form action=about:echo><input name=q></form>" in
          let q = element p "q" in
          let p = Browser_forms.typed p q "hi!" in
          (match Browser_forms.key p q "backspace" with
          | Changed p' -> Alcotest.(check string) "hi" "hi" (Browser_page.value_of p' q).text
          | _ -> Alcotest.fail "a change");
          match Browser_forms.key p q "return" with
          | Submit { url; post = None; _ } -> Alcotest.(check string) "sent" "about:echo?q=hi%21" url
          | _ -> Alcotest.fail "a submission");
    ]
