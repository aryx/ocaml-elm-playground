(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_cascade.mli *)

let media : Cascade.media = { width = 976.; height = 800. }

(* the computed style of the element of id [id], [css] the page's sheet *)
let style (css : string) (html : string) (id : string) : Computed.t =
  let root = Html_tree.of_string html in
  let styles = Computed.styles media [ { origin = Author; rules = Css_syntax.parse_stylesheet css } ] root in
  let rec find (e : Dom.element) =
    if Dom.attribute "id" e = Some id then Some e else List.find_map (fun (n : Dom.node) -> match n with Element c -> find c | Text _ -> None) e.children
  in
  match find root with Some e -> styles e | None -> Alcotest.fail ("no element " ^ id)

let rgb (c : Css_values.color) = (c.r, c.g, c.b)
let color = Alcotest.(triple int int int)
let near = Alcotest.float 1e-6

let px (s : Computed.size) : float = match s with Len l -> Css_values.resolve l 0. | Auto -> nan
let top (a, _, _, _) = a
let right (_, b, _, _) = b
let left (_, _, _, d) = d
let query s = Cascade.media_matches media (Css_syntax.components_of s)

let tests =
  Testo.categorize "Cascade"
    [
      Testo.create "the worked example: the cascade's order" (fun () ->
          Alcotest.check color "green: !important beats a higher specificity" (0, 128, 0)
            (rgb (style "p { color: black } .x { color: green !important } #a { color: red }" "<p id=a class=x>" "a").color);
          Alcotest.check color "style= beats #a" (0, 0, 255) (rgb (style "#a { color: red }" "<p id=a style=\"color: blue\">" "a").color));
      Testo.create "the worked example: var()" (fun () ->
          let css = ":root { --accent: #36c; --gap: 8px } a { color: var(--accent); margin: 0 var(--gap) } .dark { --accent: #9cf }" in
          let html = "<p><a id=x href=/>x</a></p><div class=dark><a id=y href=/>y</a></div>" in
          let x = style css html "x" in
          Alcotest.check color "#36c" (0x33, 0x66, 0xcc) (rgb x.color);
          Alcotest.check near "margin 0 8px: the left" 8. (px (left x.margin));
          Alcotest.check color "inside .dark, the nearer --accent" (0x99, 0xcc, 0xff) (rgb (style css html "y").color);
          Alcotest.check color "a fallback" (255, 0, 0) (rgb (style "p { color: var(--nope, red) }" "<p id=p>" "p").color);
          Alcotest.check color "no value, no fallback: as if absent (inherited)" (0, 0, 0) (rgb (style "p { color: var(--nope) }" "<p id=p>" "p").color));
      Testo.create "the worked example: calc()" (fun () ->
          let s = style "div { width: calc(100% - 2em) }" "<div id=d>" "d" in
          match s.width with
          | Len l ->
              Alcotest.(check (pair near near)) "{ px = -32; pct = 100 }" (-32., 100.) (l.px, l.pct);
              Alcotest.check near "in a block 600 wide: 568" 568. (Css_values.resolve l 600.)
          | Auto -> Alcotest.fail "auto");
      Testo.create "media queries, in a window 976 wide" (fun () ->
          Alcotest.(check (list bool)) "max-width 800, min-width 600, print, not print, screen and (min-width: 1000px), print or screen"
            [ false; true; false; true; false; true ]
            (List.map query [ "(max-width: 800px)"; "(min-width: 600px)"; "print"; "not print"; "screen and (min-width: 1000px)"; "print, screen" ]);
          Alcotest.check color "a rule for phones asleep" (0, 0, 0)
            (rgb (style "@media (max-width: 800px) { p { color: red } }" "<p id=p>" "p").color));
      Testo.create "the browser's own sheet" (fun () ->
          let h1 = style "" "<h1 id=h>x</h1>" "h" in
          Alcotest.(check (pair near bool)) "h1: 2em, bold" (32., true) (h1.font_size, h1.bold);
          Alcotest.check near "its margin, .67em of its own size" 21.44 (px (top h1.margin));
          Alcotest.check near "body's margin" 8. (px (left (style "" "<body id=b>" "b").margin));
          Alcotest.(check bool) "li a list item" true ((style "" "<ul><li id=l>x</ul>" "l").display = List_item);
          Alcotest.(check bool) "a script hidden" true ((style "" "<p><script id=h>x</script>" "h").display = Display_none);
          Alcotest.check color "a link's blue" (0, 0, 0xee) (rgb (style "" "<a id=a href=x>l</a>" "a").color));
      Testo.create "inheritance, and its keywords" (fun () ->
          let css = "div { color: red; margin: 10px; border: 2px solid } p { margin: inherit } .i { color: initial }" in
          let html = "<div><p id=p>x</p><span id=s class=i>y</span></div>" in
          let p = style css html "p" in
          Alcotest.check color "colour inherited" (255, 0, 0) (rgb p.color);
          Alcotest.check near "margin, when asked (inherit)" 10. (px (top p.margin));
          Alcotest.check near "the border not inherited" 0. (let a, _, _, _ = p.border_width in a);
          Alcotest.check color "initial: black" (0, 0, 0) (rgb (style css html "s").color));
      Testo.create "shorthands" (fun () ->
          let s = style "div { margin: 1px 2px 3px; padding: 5% ; border: 1px solid #ccc; font: italic bold 12px/1.5 monospace }" "<div id=d>" "d" in
          Alcotest.(check (list near)) "margin: top, right, bottom, left" [ 1.; 2.; 3.; 2. ] (let a, b, c, d = s.margin in List.map px [ a; b; c; d ]);
          Alcotest.check near "padding 5%" 5. (let a, _, _, _ = s.padding in a.pct);
          Alcotest.(check (pair near color)) "border 1px #ccc" (1., (0xcc, 0xcc, 0xcc)) (let a, _, _, _ = s.border_width and c, _, _, _ = s.border_color in (a, rgb c));
          Alcotest.(check bool) "font: italic bold monospace" true (s.italic && s.bold && s.family = Monospace);
          Alcotest.check near "12px" 12. s.font_size;
          Alcotest.check near "border without a style: none" 0. (let a, _, _, _ = (style "div { border: 1px red }" "<div id=d>" "d").border_width in a));
      Testo.create "colours" (fun () ->
          let c s = rgb (style ("p { color: " ^ s ^ " }") "<p id=p>" "p").color in
          Alcotest.(check (list color)) "#f00, rgb with spaces, hsl, a name"
            [ (255, 0, 0); (10, 20, 30); (255, 0, 0); (0x66, 0x33, 0x99) ]
            (List.map c [ "#f00"; "rgb(10 20 30 / 50%)"; "hsl(0, 100%, 50%)"; "rebeccapurple" ]));
      Testo.create "overflow's two values, opacity" (fun () ->
          Alcotest.(check bool) "hidden auto clips" true (style "#a { overflow: hidden auto }" "<div id=a>" "a").overflow_hidden;
          Alcotest.(check bool) "visible does not" false (style "#a { overflow: visible }" "<div id=a>" "a").overflow_hidden;
          Alcotest.(check bool) "opacity: 0 is not shown" false (style "#a { opacity: 0 }" "<input id=a>" "a").visible;
          Alcotest.(check bool) "opacity: 0.5 is" true (style "#a { opacity: 0.5 }" "<input id=a>" "a").visible);
    ]
