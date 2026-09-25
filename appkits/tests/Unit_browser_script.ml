(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_browser_script.mli *)

(* a page's scripts run *)
let page (html : string) : Browser_script.t =
  let t = Browser_script.create (Html_tree.of_string html) in
  Browser_script.run_scripts t;
  t

(* the frozen body, blank text and scripts left out, as indented lines *)
let body (t : Browser_script.t) : string list =
  let root = Dom.without_blank_text (Browser_script.tree t) in
  match Dom.find_all "body" root with
  | [ b ] ->
      List.concat_map
        (fun (n : Dom.node) ->
          match n with
          | Element e when e.name = "script" -> []
          | Element e -> Dom.to_lines e
          | Text s -> [ Printf.sprintf "%S" s ])
        b.children
  | _ -> Alcotest.fail "no body"

(* a script's value, as the console shows it *)
let value (t : Browser_script.t) (s : string) : string =
  match Browser_script.eval t s with Ok v -> Js_value.display v | Error e -> "error: " ^ e.message

let check_body what t expected = Alcotest.(check (list string)) what expected (body t)

let tests =
  Testo.categorize "Browser_script"
    [
      Testo.create "the worked example: a text changed" (fun () ->
          let t = Browser_script.create (Html_tree.of_string "<p id=x>a</p><script>document.getElementById(\"x\").textContent = \"b\"</script>") in
          Alcotest.(check bool) "not changed before the scripts" false (Browser_script.changed t);
          Browser_script.run_scripts t;
          Alcotest.(check bool) "changed after" true (Browser_script.changed t);
          check_body "the text" t [ "p id=\"x\""; "  \"b\"" ];
          Alcotest.(check bool) "frozen: not changed any more" false (Browser_script.changed t));
      Testo.create "the worked example: a hundred items, one task" (fun () ->
          let t =
            page
              "<ul id=list></ul><script>\nconst list = document.getElementById(\"list\");\nfor (let i = 0; i < 100; i++) {\n  const li = document.createElement(\"li\");\n  li.textContent = \"item \" + i;\n  list.appendChild(li);\n}\n</script>"
          in
          Alcotest.(check int) "100 li" 100 (List.length (Dom.find_all "li" (Browser_script.tree t)));
          Alcotest.(check string) "the last" "item 99" (value t "document.querySelector(\"ul\").lastChild.textContent"));
      Testo.create "innerHTML, written and read" (fun () ->
          let t = page "<p id=x>a</p>" in
          ignore (value t "document.getElementById(\"x\").innerHTML = \"<b>3</b> & more\"");
          check_body "parsed" t [ "p id=\"x\""; "  b"; "    \"3\""; "  \" & more\"" ];
          Alcotest.(check string) "read back, escaped" "<b>3</b> &amp; more" (value t "document.getElementById(\"x\").innerHTML"));
      Testo.create "selectors: Css's, descendants and classes" (fun () ->
          let t = page "<ul><li class=done>a<li>b</ul><ol><li class=done>c</ol>" in
          Alcotest.(check string) "ul li.done: one" "1" (value t "document.querySelectorAll(\"ul li.done\").length");
          Alcotest.(check string) ".done: the first" "a" (value t "document.querySelector(\".done\").textContent");
          Alcotest.(check string) "within an element" "c" (value t "document.querySelector(\"ol\").querySelector(\"li\").textContent");
          Alcotest.(check string) "none" "null" (value t "document.querySelector(\"table\")"));
      Testo.create "style and class" (fun () ->
          let t = page "<p id=x>a</p>" in
          ignore (value t "const p = document.getElementById(\"x\"); p.style.color = \"red\"; p.style.backgroundColor = \"blue\"; p.className = \"done\"");
          check_body "the attributes" t [ "p id=\"x\" style=\"color: red; background-color: blue\" class=\"done\""; "  \"a\"" ];
          Alcotest.(check string) "read back" "blue" (value t "p.style.backgroundColor"));
      Testo.create "an element, one host object" (fun () ->
          let t = page "<p id=x>a</p>" in
          Alcotest.(check string) "===" "true" (value t "document.getElementById(\"x\") === document.getElementById(\"x\")");
          Alcotest.(check string) "an expando kept" "true" (value t "document.getElementById(\"x\").seen = true; document.body.firstElementChild.seen"));
      Testo.create "the tree edited" (fun () ->
          let t = page "<ul id=a><li>1<li>2</ul><ul id=b></ul>" in
          ignore (value t "const a = document.getElementById(\"a\"), b = document.getElementById(\"b\"); b.appendChild(a.firstChild)");
          check_body "moved" t [ "ul id=\"a\""; "  li"; "    \"2\""; "ul id=\"b\""; "  li"; "    \"1\"" ];
          ignore (value t "const li = document.createElement(\"li\"); li.textContent = \"0\"; b.insertBefore(li, b.firstChild); a.removeChild(a.firstChild)");
          check_body "inserted, removed" t [ "ul id=\"a\""; "ul id=\"b\""; "  li"; "    \"0\""; "  li"; "    \"1\"" ];
          Alcotest.(check string) "never inside itself" "error: HierarchyRequestError: The new child element contains the parent."
            (value t "b.firstChild.appendChild(b)"));
      Testo.create "the title" (fun () ->
          let t = page "<title>A</title><p>x" in
          ignore (value t "document.title = document.title + \"B\"");
          Alcotest.(check string) "AB" "AB" (Dom.text_content (List.hd (Dom.find_all "title" (Browser_script.tree t)))));
      Testo.create "Netscape's attributes kept through the copy" (fun () ->
          let t = page "<body bgcolor=white><center>x</center>" in
          let root = Browser_script.tree t in
          let b = List.hd (Dom.find_all "body" root) and c = List.hd (Dom.find_all "center" root) in
          Alcotest.(check (option string)) "bgcolor still an extension" (Some "white") (Dom.attribute ~extensions:true "bgcolor" b);
          Alcotest.(check (option string)) "not a core attribute" None (Dom.attribute "bgcolor" b);
          Alcotest.(check bool) "center still Netscape's" true (c.origin = Dtd.Netscape));
      Testo.create "errors to the console, the next script still run" (fun () ->
          let t = page "<script>\nx.y\n</script><script>console.log(\"next\", [1])</script>" in
          Alcotest.(check (list string)) "the console" [ "Uncaught ReferenceError: x is not defined (line 2)"; "next [1]" ] (Browser_script.console t));
    ]
