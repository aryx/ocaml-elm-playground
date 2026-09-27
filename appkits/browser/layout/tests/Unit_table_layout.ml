(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_table_layout.mli *)

let near = Alcotest.float 1e-6

let table (html : string) : Dom.element = List.hd (Dom.find_all "table" (Html_tree.of_string html))

let tests =
  Testo.categorize "Table_layout"
    [
      Testo.create "the worked example" (fun () ->
          (* column 1: min 60 ("tomato"), max 110; column 2: 40 and 40 *)
          let columns = [| (60., 110.); (40., 40.) |] in
          let widths room = Array.to_list (Table_layout.widths ~room ~fixed:false columns) in
          Alcotest.(check (list near)) "200: every column at its max" [ 110.; 40. ] (widths 200.);
          Alcotest.(check (list near)) "120: 20 shared as 50 to 0" [ 80.; 40. ] (widths 120.);
          Alcotest.(check (list near)) "90: every column at its min" [ 60.; 40. ] (widths 90.));
      Testo.create "a table that says its width takes it all" (fun () ->
          Alcotest.(check (list near)) "300, shared as the maxes" [ 200.; 100. ]
            (Array.to_list (Table_layout.widths ~room:300. ~fixed:true [| (10., 20.); (5., 10.) |])));
      Testo.create "the grid: a cell of two columns" (fun () ->
          let cells, n = Table_layout.grid (table "<table><tr><td colspan=2>a<td>b<tr><td>c<td>d<td>e</table>") in
          Alcotest.(check int) "three columns" 3 n;
          Alcotest.(check (list (triple int int int)))
            "row, column, span" [ (0, 0, 2); (0, 2, 1); (1, 0, 1); (1, 1, 1); (1, 2, 1) ]
            (List.map (fun (c : Table_layout.cell) -> (c.row, c.column, c.span)) cells));
      Testo.create "the grid: through tbody" (fun () ->
          let cells, n = Table_layout.grid (table "<table><caption>x</caption><tbody><tr><th>a<td>b</tbody></table>") in
          Alcotest.(check (pair int int)) "two cells, two columns" (2, 2) (List.length cells, n);
          Alcotest.(check bool) "the caption" true (Table_layout.caption (table "<table><caption>x</caption></table>") <> None));
      Testo.create "a wide cell of two columns: the rest spread" (fun () ->
          let cells, n = Table_layout.grid (table "<table><tr><td colspan=2>a<tr><td>b<td>c</table>") in
          let sizes = [ (30., 50.); (10., 10.); (10., 10.) ] in
          (* the two columns give 10 + 2 + 10 = 22 of the 30 and 50 the
           * wide cell needs: 4 and 14 more each *)
          let columns = Table_layout.columns n (List.combine cells sizes) ~spacing:2. in
          Alcotest.(check (list (pair near near))) "14 and 24 each" [ (14., 24.); (14., 24.) ] (Array.to_list columns));
    ]
