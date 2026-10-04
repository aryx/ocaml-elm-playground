(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Corpus.mli *)

type t = {
  learn : string list;
  held : string list;
  test : string list;
}

let split ?(seed = 42) (words : string list) : t =
  let words = Array.of_list words in
  let state = Lehmer.make seed in
  (* Fisher and Yates: each place, from the last, swapped with one at
   * or before it *)
  for i = Array.length words - 1 downto 1 do
    let j = Lehmer.int state (i + 1) in
    let x = words.(i) in
    words.(i) <- words.(j);
    words.(j) <- x
  done;
  let n = Array.length words in
  let part a b = Array.to_list (Array.sub words a (b - a)) in
  { learn = part 0 (n * 8 / 10); held = part (n * 8 / 10) (n * 9 / 10); test = part (n * 9 / 10) n }

let sizes (c : t) : int * int * int = (List.length c.learn, List.length c.held, List.length c.test)
