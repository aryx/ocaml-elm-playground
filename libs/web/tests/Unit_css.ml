(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_css.mli *)

(* the winning value of [property] for each element named [name] *)
let values (sheet : string) (html : string) (name : string) (property : string) : string option list =
  let root = Html_tree.of_string html in
  let style = Css.cascade (Css.parse sheet) root in
  List.map (fun e -> List.assoc_opt property (style e)) (Dom.find_all name root)

let tests =
  Testo.categorize "Css"
    [
      Testo.create "the worked example" (fun () ->
          let sheet = "p { color: black } .intro { color: green } p.intro { color: red } #top { color: blue }" in
          Alcotest.(check (list (option string)))
            "p, p.intro, #top, style=" [ Some "black"; Some "red"; Some "blue"; Some "gray" ]
            (values sheet "<p>a<p class=intro>b<p class=intro id=top>c<p id=top style=\"color: gray\">d" "p" "color"));
      Testo.create "specificity: (ids, classes, names)" (fun () ->
          let spec s = Css.specificity (List.hd (Css.parse (s ^ " {}"))).selector in
          Alcotest.(check (list (triple int int int)))
            "p, .intro, p.intro, #top, ul li, *" [ (0, 0, 1); (0, 1, 0); (0, 1, 1); (1, 0, 0); (0, 0, 2); (0, 0, 0) ]
            (List.map spec [ "p"; ".intro"; "p.intro"; "#top"; "ul li"; "*" ]));
      Testo.create "equal specificity: the later wins" (fun () ->
          Alcotest.(check (list (option string))) "blue" [ Some "blue" ] (values "p { color: red } p { color: blue }" "<p>x" "p" "color"));
      Testo.create "parsing: groups, comments, @-rules, what does not parse" (fun () ->
          let sheet = Css.parse "/* a comment */ h1, h2 { color: red; margin: 0 } @media print { p { color: black } } a:link { color: blue } em { }" in
          Alcotest.(check (list string)) "h1 and h2, and em; the @media block and a:link skipped" [ "h1"; "h2"; "em" ]
            (List.map (fun (r : Css.rule) -> match List.rev r.selector with s :: _ -> Option.value s.name ~default:"*" | [] -> "") sheet);
          Alcotest.(check (list (pair string string))) "declarations" [ ("color", "red"); ("margin", "0") ] (List.hd sheet).declarations;
          Alcotest.(check (list (pair string string))) "style=" [ ("color", "red"); ("font-size", "2em") ]
            (Css.declarations " COLOR : red;font-size:2em; ;bad"));
      Testo.create "descendants, at any depth" (fun () ->
          let html = "<ul><li>a<ol><li>b</ol></ul><ol><li>c</ol>" in
          Alcotest.(check (list (option string))) "ul li: a and b, not c" [ Some "grey"; Some "grey"; None ]
            (values "ul li { color: grey }" html "li" "color");
          Alcotest.(check (list (option string))) "ul ol li: b only" [ None; Some "red"; None ]
            (values "ul ol li { color: red }" html "li" "color"));
      Testo.create "a class is one of the words" (fun () ->
          Alcotest.(check (list (option string))) "class=\"a note\"" [ Some "red" ]
            (values ".note { color: red }" "<p class=\"a note\">x" "p" "color"));
    ]
