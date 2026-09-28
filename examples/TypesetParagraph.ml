(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A paragraph, and where to break it: the typesetting example
 * (plan_gui_teaching.md, phase 9; the breakers are appkits/typeset).
 *
 * Not a word processor, and it is worth saying why, because it looks
 * like one. Its shape is TeX's rather than Word's: the text as typed on
 * one side, the typeset page on the other, and one idea in between --
 *
 *   **where to break a paragraph into lines.**
 *
 * -- which is Knuth's, and which Word never used: Word breaks its lines
 * greedily, as browsers do. The optimal breaker is TeX's (and Adobe
 * InDesign's "paragraph composer"). TinyWord, when it exists, is
 * the other thing: a page you edit in place, with formatting.
 *
 * The left pane is the text, as typed. The right one is the page:
 * every paragraph justified -- both edges straight -- which means each
 * line's spaces stretch or shrink to fill it, and *where* the lines
 * break decides how far. Two ways, switchable, with the numbers on the
 * screen (this repository's third principle, the simple version
 * beside the better one):
 *
 *   greedy        as many words as fit, then the next line -- what
 *                 every browser and most editors do, and blind to what
 *                 it does to the line after
 *   Knuth-Plass   the whole paragraph scored, and the best set of
 *                 breaks found by dynamic programming (Donald Knuth and
 *                 Michael Plass, 1981: why TeX's paragraphs look the
 *                 way they do)
 *
 * Each line's ratio -- how far its spaces had to give -- is written in
 * the margin, and a line stretched past its glue's comfort is marked:
 * those are the "rivers" a typesetter's eye goes straight to. Drag the
 * measure slider and watch where they appear under greedy and where
 * the optimal breaker has already moved a word to avoid them. The
 * line at the bottom is both scores for the whole page.
 *
 * Paragraphs are TeX's: a blank line ends one, and a single newline is
 * only a space. A paragraph starting with "# " is a heading, set
 * larger and left ragged -- the one style there is.
 *
 * What it uses: appkits/typeset (Linebreak, both breakers),
 * gui/Text_edit through the text area, gui/Layout, and the playground
 * widgets (a dropdown, a slider, labels). What it does not use:
 * appkits/document (there is nothing to save to), gui/Grid.
 *
 * Set in a monospaced face, like a typewriter, one character to a
 * cell. That is not a matter of taste: it makes every width exact, so
 * the right edge is straight to the pixel and the only thing that
 * differs between the two breakers is the breaking. A real word
 * processor asks the font how wide each word is; the algorithm would
 * not change by a line.
 *
 * What it deliberately does not do: editing *in* the typeset page --
 * a caret inside justified text is the hard part of WYSIWYG, and it is
 * why Bravo (Xerox PARC, 1974), the first editor to do it, is a
 * landmark; hyphenation (Liang's patterns, TeX's other half, which
 * give the breaker more places to break and make rivers rarer still);
 * pages, and the breaking of *them*; and any style but the heading.
 *
 * Exercises: hyphenation of one long word by hand (a penalty item at
 * each allowed break) and watch the rivers go; a proportional face
 * with real widths from graphics/font's Hershey tables, and check the
 * breaks change only where widths did; the caret in the page, which is
 * the one exercise here that is a research project's worth.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  (* the text as typed: gui/Text_edit's piece table, which carries its
     own caret, selection and history *)
  text : Text_edit.t;
  (* 0 greedy, 1 Knuth-Plass *)
  breaker : int;
  (* how wide the page's lines are, in characters *)
  measure : number;
}

let opening =
  "# Breaking paragraphs into lines\n\n\
   A paragraph of justified text has straight edges on both sides, so the \
   spaces of every line are stretched or shrunk until the line fills the \
   measure exactly. Where the lines break decides how far.\n\n\
   The obvious way fills each line with as many words as fit and moves \
   on, which is best for that line and blind to the next. Knuth and Plass \
   scored the whole paragraph instead, and chose the breaks that made the \
   sum of the lines' badness least: rivers of white disappear, and a \
   reader sees an even page without knowing why.\n\n\
   Drag the measure and watch the margin."

let initial = { text = Text_edit.of_string opening; breaker = 1; measure = 38. }
let breakers = [ "greedy"; "Knuth-Plass" ]

(*****************************************************************************)
(* The page *)
(*****************************************************************************)

let body = 15. (* the body text's size *)
let heading = 22.
let char_w size = Widget.text_width ~size "x"
let line_h size = size *. 1.55

(* TeX's own proportions for a space, in cmr10: stretch half of it,
 * shrink a third *)
let params_for ~measure size =
  let w = char_w size in
  { Linebreak.measure = measure *. w; space = w; stretch = w *. 0.5; shrink = w /. 3. }

type block = Heading of string | Paragraph of Linebreak.word array

(* TeX's paragraphs: a blank line ends one, a single newline is a space *)
let blocks_of (text : string) : block list =
  let words_of size s =
    String.split_on_char ' ' (String.map (fun c -> if c = '\n' then ' ' else c) s)
    |> List.filter (fun w -> w <> "")
    |> List.map (fun w -> { Linebreak.text = w; width = char_w size *. float_of_int (String.length w) })
    |> Array.of_list
  in
  let rec split acc cur = function
    | [] -> List.rev (if cur = [] then acc else String.concat "\n" (List.rev cur) :: acc)
    | "" :: rest -> split (if cur = [] then acc else String.concat "\n" (List.rev cur) :: acc) [] rest
    | line :: rest -> split acc (line :: cur) rest
  in
  split [] [] (String.split_on_char '\n' text)
  |> List.map (fun para ->
         if String.length para > 2 && String.sub para 0 2 = "# " then
           Heading (String.sub para 2 (String.length para - 2))
         else Paragraph (words_of body para))

let break_with breaker p words =
  if breaker = 0 then Linebreak.greedy p words else Linebreak.optimal p words

(* both scores for the whole page, whichever is showing *)
let scores ~measure text =
  let p = params_for ~measure body in
  blocks_of (Text_edit.to_string text)
  |> List.fold_left
       (fun (g, o) -> function
         | Heading _ -> (g, o)
         | Paragraph ws ->
             ( g +. Linebreak.total (Linebreak.greedy p ws),
               o +. Linebreak.total (Linebreak.optimal p ws) ))
       (0., 0.)

(* the page, set: every character in its own cell, the words placed
 * where the breaker's spacing puts them *)
let set_page (b : Widget.box) ~breaker ~measure text =
  let th = Gui.theme () in
  let left = Widget.left b +. 50. in
  let draw_word size color x y w =
    List.mapi
      (fun i c -> words color (String.make 1 c) |> scale (size /. words_font_size)
                  |> move (x +. ((float_of_int i +. 0.5) *. char_w size)) y)
      (List.init (String.length w) (String.get w))
  in
  let y = ref (Widget.top b -. 30.) in
  let out = ref [] in
  blocks_of (Text_edit.to_string text)
  |> List.iter (fun block ->
         (match block with
         | Heading s ->
             (* a heading is set larger, and left ragged: it is one
                line of its own, not a paragraph to justify *)
             out := draw_word heading th.text left !y s @ !out;
             y := !y -. line_h heading
         | Paragraph ws ->
             let p = params_for ~measure body in
             let lines = break_with breaker p ws in
             List.iter
               (fun (l : Linebreak.line) ->
                 let gap = Linebreak.spacing p l in
                 let x = ref left in
                 for k = l.first to l.last do
                   out := draw_word body th.text !x !y ws.(k).text @ !out;
                   x := !x +. ws.(k).width +. gap
                 done;
                 (* the margin: how far this line's spaces had to give,
                    and a mark on the ones stretched past comfort *)
                 let loose = l.ratio > 1. && Float.abs l.ratio <> infinity in
                 out :=
                   (words (if loose then red else rgb 150 150 150)
                      (if l.last = Array.length ws - 1 then ""
                       else if Float.abs l.ratio = infinity then "!"
                       else Printf.sprintf "%.2f" l.ratio)
                   |> scale (11. /. words_font_size)
                   |> move (left +. p.measure +. 30.) !y)
                   :: (if loose then
                         [ rectangle red 4. (line_h body *. 0.7) |> move (left -. 16.) !y ]
                       else [])
                   @ !out;
                 y := !y -. line_h body)
               lines);
         y := !y -. (line_h body *. 0.6));
  List.rev !out

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type slot = Breaker_label | Breaker | Measure_label | Measure | Source | Page | Status

let panel =
  Layout.(
    pad 30.
      (column ~gap:12.
         [
           row ~gap:12.
             [
               leaf Breaker_label (Gui.label_size "breaking:");
               leaf Breaker (Gui.menu_size breakers);
               space 30.;
               leaf Measure_label (Gui.label_size "measure: 00");
               leaf Measure (Gui.slider_size ());
             ];
           (* the two panes fill the height: in a row, a child keeps the
              height it asked for unless it is stretched *)
           expand
             (row ~gap:20.
                [ stretch (leaf Source (340., 560.)); expand (stretch (leaf Page (560., 560.))) ]);
           leaf Status (Gui.label_size "Knuth-Plass 000000   greedy 0000000   x100 better");
         ]))

let places computer = Layout.arrange (Gui.area computer) panel

let update computer model =
  let at = places computer in
  let box slot : Widget.box = List.assoc slot at in
  let text = Gui.text_area_in computer (box Source) model.text in
  Gui.label_in computer (box Breaker_label) "breaking:";
  Gui.label_in computer (box Measure_label) (Printf.sprintf "measure: %.0f" model.measure);
  let measure = Float.round (Gui.slider_in computer (box Measure) ~from:16. ~to_:44. model.measure) in
  let g, o = scores ~measure text in
  Gui.label_in computer (box Status)
    (Printf.sprintf "Knuth-Plass %.0f   greedy %.0f   x%.0f better" o g (if o > 0. then g /. o else 1.));
  (* asked for last: its items are painted over what is below *)
  let breaker = Gui.menu_in computer (box Breaker) breakers model.breaker in
  { text; breaker; measure }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view computer model =
  let s = computer.screen in
  let th = Gui.theme () in
  let page : Widget.box = List.assoc Page (places computer) in
  [ rectangle th.background s.width s.height; rectangle white page.w page.h |> move page.x page.y ]
  @ set_page page ~breaker:model.breaker ~measure:model.measure model.text
  @ Gui.draw ()

let app = game view update initial
let main = Playground_platform.run_app app
