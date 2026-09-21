(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

type part = {
  kind : string;
  height : float -> float;
  natural : (float * float) option;
  draw : Widget.box -> active:bool -> shape list;
  input : computer -> Widget.box -> part;
  menu : string list;
  command : string -> part;
  save : unit -> string;
}

type registry = (string * (string -> part)) list

let rec placeholder ~kind text =
  {
    kind;
    height = (fun _ -> 50.);
    natural = None;
    draw =
      (fun (b : Widget.box) ~active:_ ->
        [
          rectangle (rgb 225 225 225) b.w b.h |> move b.x b.y;
          words (rgb 90 90 90) (Printf.sprintf "a part of kind %S, which nothing here can show (%d bytes, kept)" kind (String.length text))
          |> move b.x b.y;
        ]);
    input = (fun _ _ -> placeholder ~kind text);
    menu = [];
    command = (fun _ -> placeholder ~kind text);
    (* exactly what it was given: not understood, not lost *)
    save = (fun () -> text);
  }

let load registry ~kind text =
  match List.assoc_opt kind registry with Some load -> load text | None -> placeholder ~kind text

(*****************************************************************************)
(* Scaling *)
(*****************************************************************************)

let fitted_height ~scaled p w =
  match p.natural with Some (nw, nh) when scaled && nw > 0. -> nh *. w /. nw | _ -> p.height w

(* the scale, and where the scaled drawing's centre goes: as big as the
   box allows, against its top-left *)
let fit p (b : Widget.box) =
  match p.natural with
  | Some (nw, nh) when nw > 0. && nh > 0. ->
      let s = Float.min (b.w /. nw) (b.h /. nh) in
      Some (nw, nh, s, Widget.left b +. (nw *. s /. 2.), Widget.top b -. (nh *. s /. 2.))
  | _ -> None

(* the part's natural box, around the origin: what it draws into and
   takes the mouse in, before the scaling *)
let natural_box nw nh : Widget.box = { Widget.x = 0.; y = 0.; w = nw; h = nh }

let draw_in ~scaled p b ~active =
  match (scaled, fit p b) with
  | true, Some (nw, nh, s, cx, cy) -> [ group (p.draw (natural_box nw nh) ~active) |> scale s |> move cx cy ]
  | _ -> p.draw b ~active

let input_in ~scaled p (computer : computer) b =
  match (scaled, fit p b) with
  | true, Some (nw, nh, s, cx, cy) ->
      let m = computer.mouse in
      let mouse = { m with mx = (m.mx -. cx) /. s; my = (m.my -. cy) /. s } in
      p.input { computer with mouse } (natural_box nw nh)
  | _ -> p.input computer b
