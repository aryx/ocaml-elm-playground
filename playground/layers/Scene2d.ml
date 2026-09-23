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

(* See Scene2d.mli *)

type 'scene t = {
  scene : 'scene;
  elapsed : number;
  frames : int;
  last : number option;
  keys : keyboard;
  before : keyboard;
}

let start (scene : 'scene) : 'scene t =
  let keys = initial_computer.keyboard in
  { scene; elapsed = 0.; frames = 0; last = None; keys; before = keys }

let update (computer : computer) (t : 'scene t) : 'scene t =
  let (Time now) = computer.time in
  let dt = match t.last with None -> 0. | Some last -> now -. last in
  { t with
    elapsed = t.elapsed +. dt;
    frames = t.frames + 1;
    last = Some now;
    keys = computer.keyboard;
    before = t.keys }

(* claude: before = keys: whatever is down now counts as already down in
 * the new scene *)
let go (scene : 'scene) (t : 'scene t) : 'scene t =
  { t with scene; elapsed = 0.; frames = 0; before = t.keys }

let pressed (key : keyboard -> bool) (t : 'scene t) : bool = key t.keys && not (key t.before)

let blink (period : number) (t : 'scene t) (shapes : shape list) : shape list =
  if Float.rem t.elapsed period < period /. 2. then shapes else []
