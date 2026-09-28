(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Backprop.mli *)

type example = float array * float array
type grads = (Matrix.t * Matrix.t) list

let one_loss (got : float array) (want : float array) : float =
  let s = ref 0. in
  Array.iteri (fun i a -> let d = a -. want.(i) in s := !s +. (0.5 *. d *. d)) got;
  !s

let loss (net : Net.t) (examples : example list) : float =
  match examples with
  | [] -> 0.
  | _ ->
      let total = List.fold_left (fun s (x, y) -> s +. one_loss (Net.forward net x) y) 0. examples in
      total /. float_of_int (List.length examples)

(* the backward pass. [delta] is dL/dz at a layer: what the loss makes
 * of that layer's pre-activation. It starts at the output as
 * (a - y) * f'(z) and is carried back through W^T at every step *)
let gradient (net : Net.t) ((x, y) : example) : grads =
  let pass = Net.forward_pass net x in
  let steps = Array.of_list pass.steps and layers = Array.of_list net in
  let n = Array.length layers in
  let grads = Array.make n (Matrix.create 0 0, Matrix.create 0 0) in
  let delta = ref (Matrix.create 0 0) in
  for l = n - 1 downto 0 do
    let (z, a) = steps.(l) in
    let f = layers.(l).f in
    let slopes = Matrix.init a.rows 1 (fun i _ -> Net.slope f ~z:(Matrix.get z i 0) ~a:(Matrix.get a i 0)) in
    let from_above =
      if l = n - 1 then
        (* the loss's own derivative: a - y *)
        Matrix.init a.rows 1 (fun i _ -> Matrix.get a i 0 -. y.(i))
      else
        (* what the layer above makes of this one's outputs *)
        Matrix.mul (Matrix.transpose layers.(l + 1).w) !delta
    in
    delta := Matrix.times from_above slopes;
    (* dL/dW is the delta against what came in; dL/db is the delta *)
    let below = if l = 0 then pass.input else snd steps.(l - 1) in
    grads.(l) <- (Matrix.mul !delta (Matrix.transpose below), !delta)
  done;
  Array.to_list grads

let zeros (net : Net.t) : grads =
  List.map (fun (l : Net.layer) -> (Matrix.create l.w.rows l.w.cols, Matrix.create l.b.rows 1)) net

let add_grads (a : grads) (b : grads) : grads =
  List.map2 (fun (aw, ab) (bw, bb) -> (Matrix.add aw bw, Matrix.add ab bb)) a b

let over (net : Net.t) (examples : example list) : grads =
  match examples with
  | [] -> zeros net
  | _ ->
      let sum = List.fold_left (fun acc e -> add_grads acc (gradient net e)) (zeros net) examples in
      let k = 1. /. float_of_int (List.length examples) in
      List.map (fun (dw, db) -> (Matrix.scale k dw, Matrix.scale k db)) sum

let step ~(rate : float) (net : Net.t) (grads : grads) : Net.t =
  List.map2
    (fun (l : Net.layer) (dw, db) ->
      { l with w = Matrix.sub l.w (Matrix.scale rate dw); b = Matrix.sub l.b (Matrix.scale rate db) })
    net grads

let learn ?(rate = 0.5) (net : Net.t) (examples : example list) : Net.t = step ~rate net (over net examples)

(* the same gradient the slow, obvious way: move one weight a little
 * each way and see what the loss does *)
let numeric ?(epsilon = 1e-5) (net : Net.t) (examples : example list) : grads =
  let nudged (which : int) (field : [ `W | `B ]) (i : int) (by : float) : Net.t =
    List.mapi
      (fun l (layer : Net.layer) ->
        if l <> which then layer
        else
          match field with
          | `W ->
              let w = { layer.w with data = Array.copy layer.w.data } in
              w.data.(i) <- w.data.(i) +. by;
              { layer with w }
          | `B ->
              let b = { layer.b with data = Array.copy layer.b.data } in
              b.data.(i) <- b.data.(i) +. by;
              { layer with b })
      net
  in
  let slope which field i =
    (loss (nudged which field i epsilon) examples -. loss (nudged which field i (-.epsilon)) examples)
    /. (2. *. epsilon)
  in
  List.mapi
    (fun l (layer : Net.layer) ->
      ( { layer.w with data = Array.init (Array.length layer.w.data) (fun i -> slope l `W i) },
        { layer.b with data = Array.init (Array.length layer.b.data) (fun i -> slope l `B i) } ))
    net

let magnitudes (grads : grads) : float list =
  List.map (fun ((dw : Matrix.t), _) -> sqrt (Array.fold_left (fun s v -> s +. (v *. v)) 0. dw.data)) grads
