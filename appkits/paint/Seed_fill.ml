(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let area b x y =
  let w = Bitmap.width b and h = Bitmap.height b in
  let mask = Bitmap.create ~width:w ~height:h in
  if x >= 0 && y >= 0 && x < w && y < h then begin
    let colour = Bitmap.get b x y in
    (* a dot still to be filled: of the area's colour, not yet marked *)
    let open_ x y = x >= 0 && x < w && y >= 0 && y < h && Bitmap.get b x y = colour && not (Bitmap.get mask x y) in
    let seeds = Stack.create () in
    Stack.push (x, y) seeds;
    while not (Stack.is_empty seeds) do
      let x, y = Stack.pop seeds in
      if open_ x y then begin
        (* the span through the seed, wall to wall *)
        let rec left l = if open_ (l - 1) y then left (l - 1) else l in
        let rec right r = if open_ (r + 1) y then right (r + 1) else r in
        let l = left x and r = right x in
        for i = l to r do
          Bitmap.set mask i y true
        done;
        (* one seed per run of open dots just above and just below *)
        List.iter
          (fun y ->
            let i = ref l in
            while !i <= r do
              if open_ !i y then begin
                Stack.push (!i, y) seeds;
                while !i <= r && open_ !i y do
                  incr i
                done
              end
              else incr i
            done)
          [ y - 1; y + 1 ]
      end
    done
  end;
  mask

let fill b p x y =
  let mask = area b x y in
  for j = 0 to Bitmap.height b - 1 do
    for i = 0 to Bitmap.width b - 1 do
      if Bitmap.get mask i j then Paint.dot b p i j
    done
  done
