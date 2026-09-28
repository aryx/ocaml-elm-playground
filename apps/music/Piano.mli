(* The keyboard every synthesizer here draws: keys to play with the
 * mouse, and the letters of the computer's keyboard, several at once
 * (a chord):
 *
 *    w e   t y u          the black keys
 *   a s d f g h j k       the white keys, from C
 *
 * z and x an octave down and up. The mouse plays the key under it,
 * gliding from key to key while held -- when it was pressed on a key:
 * a press elsewhere (a drawbar dragged down over the keys) plays
 * nothing. *)

(* where it is and how it looks *)
type look = {
  keys : int; (* 25: two octaves and a C *)
  left : float; (* the first white key's left edge *)
  top : float;
  white_width : float;
  white_height : float;
  black_height : float;
  letters_from : int; (* the key the letter a plays: 0, or 12 when the keys start an octave under it *)
  velocity : float;
  octaves : int * int; (* the lowest and the highest z and x reach *)
  white_key : Playground.color;
  black_key : Playground.color;
  letter_on_white : Playground.color; (* on a black key, white *)
  letter_scale : float;
  letter_lift : float; (* above the key's bottom *)
}

type t

(* [octave]: the letter a's C, C4 at 4 *)
val initial : octave:int -> t
val octave : t -> int

(* the note a key plays: the letter a's C is octave + 1 in MIDI's
   numbering (C4 = 60) *)
val note : look -> t -> int -> int

(* the key under the mouse, a black key first: it's on top *)
val key_at : look -> float -> float -> int option

(* the letters, z and x, and the mouse, played on the instrument *)
val update : look -> Playground.computer -> t -> Instrument.t -> t

(* the keys, those down lit *)
val view : look -> Playground.computer -> t -> lit:Playground.color -> Playground.shape list
