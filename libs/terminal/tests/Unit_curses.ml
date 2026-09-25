(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_curses.mli *)

let check = Alcotest.(check string)

(* a random screen: some words in some colours, maybe a cursor (and
   which) *)
let random_screen (st : Random.State.t) ~rows ~cols : Curses.t * (int * int) option =
  let colors = [| Vt.Default; Red; Green; Blue; Yellow |] in
  let t = ref (Curses.create ~rows ~cols) in
  for _ = 1 to Random.State.int st 12 do
    let attrs =
      { Vt.plain with fg = colors.(Random.State.int st 5); bold = Random.State.bool st; reverse = Random.State.int st 5 = 0 }
    in
    let word = String.init (1 + Random.State.int st 6) (fun _ -> Char.chr (Char.code 'a' + Random.State.int st 26)) in
    t := Curses.put ~attrs (Random.State.int st rows) (Random.State.int st cols) word !t
  done;
  let cursor = if Random.State.bool st then Some (Random.State.int st rows, Random.State.int st cols) else None in
  (Curses.cursor cursor !t, cursor)

let same_cells (vt : Vt.t) (t : Curses.t) : bool =
  let ok = ref true in
  for r = 0 to Curses.rows t - 1 do
    for c = 0 to Curses.cols t - 1 do
      if Vt.cell vt r c <> Curses.cell t r c then ok := false
    done
  done;
  !ok

let tests =
  Testo.categorize "Curses"
    [
      Testo.create "the worked example: CAT HAT to BAT HOT, the gap sent again" (fun () ->
          let before = Curses.create ~rows:1 ~cols:10 |> Curses.put 0 0 "CAT HAT" in
          let after = Curses.create ~rows:1 ~cols:10 |> Curses.put 0 0 "BAT HOT" in
          let bytes = Curses.refresh ~before after in
          check "bytes" "\x1b[1;1HBAT HO" bytes;
          Alcotest.(check int) "12" 12 (String.length bytes));
      Testo.create "a far change: a move rather than the gap" (fun () ->
          let before = Curses.create ~rows:2 ~cols:20 |> Curses.put 0 0 "A" in
          let after = before |> Curses.put 0 0 "B" |> Curses.put 0 15 "C" in
          check "bytes" "\x1b[1;1HB\x1b[1;16HC" (Curses.refresh ~before after));
      Testo.create "nothing changed, nothing sent; colours set and reset" (fun () ->
          let t = Curses.create ~rows:3 ~cols:10 |> Curses.put 1 1 "HI" |> Curses.cursor (Some (2, 0)) in
          check "none" "" (Curses.refresh ~before:t t);
          let red = { Vt.plain with fg = Vt.Red } in
          check "red" "\x1b[1;1H\x1b[0;31mX\x1b[0m" (Curses.refresh ~before:(Curses.create ~rows:1 ~cols:5) (Curses.create ~rows:1 ~cols:5 |> Curses.put ~attrs:red 0 0 "X")));
      Testo.create "the cursor: hidden, shown and moved" (fun () ->
          let t = Curses.create ~rows:2 ~cols:5 in
          check "shown" "\x1b[2;3H\x1b[?25h" (Curses.refresh ~before:t (Curses.cursor (Some (1, 2)) t));
          check "hidden" "\x1b[?25l" (Curses.refresh ~before:(Curses.cursor (Some (1, 2)) t) t));
      Testo.create "a VT100 shows what curses meant, on random screens" (fun () ->
          let st = Random.State.make [| 42 |] in
          for _ = 1 to 200 do
            let rows = 1 + Random.State.int st 6 and cols = 1 + Random.State.int st 20 in
            let a, _ = random_screen st ~rows ~cols and b, cursor = random_screen st ~rows ~cols in
            let vt = Vt.feed (Vt.feed (Vt.create ~rows ~cols) (Curses.redraw a)) (Curses.refresh ~before:a b) in
            if not (same_cells vt b) then
              Alcotest.failf "screen differs:\n%s\n---\n%s" (String.concat "\n" (Vt.text vt)) (String.concat "\n" (Curses.text b));
            (match cursor with
            | Some rc ->
                Alcotest.(check bool) "shown" true (Vt.cursor_visible vt);
                Alcotest.(check (pair int int)) "there" rc (Vt.cursor vt)
            | None -> Alcotest.(check bool) "hidden" false (Vt.cursor_visible vt));
            check "and then nothing to send" "" (Curses.refresh ~before:b b)
          done);
    ]
