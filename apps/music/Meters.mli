(* What the synthesizers show of their sound: its spectrum, its wave. *)

(* [segment c w a b]: a line from [a] to [b], [w] wide *)
val segment : Playground.color -> float -> float * float -> float * float -> Playground.shape

(* [spectrum ~at ~size ~color ~back ?bars samples]: the spectrum of the
   samples, 20 Hz to 20 kHz on a log axis in [bars] bars (90), -80 to
   0 dB *)
val spectrum :
  at:float * float ->
  size:float * float ->
  color:Playground.color ->
  back:Playground.color ->
  ?bars:int ->
  Signal.t ->
  Playground.shape list

(* [scope ~at ~size ~points ~color ~back ?window ?gain samples]: the
   last [window] samples' wave (1024) in [points] points, scaled to its
   peak, or by [gain] and clipped *)
val scope :
  at:float * float ->
  size:float * float ->
  points:int ->
  color:Playground.color ->
  back:Playground.color ->
  ?window:int ->
  ?gain:float ->
  Signal.t ->
  Playground.shape list
