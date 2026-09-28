(* What the synthesizers show of their sound: its spectrum, its wave. *)

(* [segment c w a b]: a line from [a] to [b], [w] wide *)
val segment : Playground.color -> float -> float * float -> float * float -> Playground.shape

(* [spectrum ~at ~size ~color ~back samples]: the spectrum of the
   samples, 20 Hz to 20 kHz on a log axis in 90 bars, -80 to 0 dB *)
val spectrum :
  at:float * float -> size:float * float -> color:Playground.color -> back:Playground.color -> Signal.t -> Playground.shape list

(* [scope ~at ~size ~points ~color ~back ?gain samples]: the last 1024
   samples' wave in [points] points, scaled to its peak, or by [gain]
   and clipped *)
val scope :
  at:float * float ->
  size:float * float ->
  points:int ->
  color:Playground.color ->
  back:Playground.color ->
  ?gain:float ->
  Signal.t ->
  Playground.shape list
