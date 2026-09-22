(* Playing cards: what the card games share -- the 52 cards, a deal, and
   a card drawn. Used by TinySolitaire and TinyFreeCell.

   The cards are numbered as Microsoft's FreeCell numbers them, rank
   first, then the suit in the order clubs, diamonds, hearts, spades:

     index:   0   1   2   3   4   5   6   7  ...  48  49  50  51
     card:   AC  AD  AH  AS  2C  2D  2H  2S  ...  KC  KD  KH  KS

   so a card's rank is index / 4 + 1 and its suit index mod 4. *)

open Playground

type suit = Clubs | Diamonds | Hearts | Spades

(* [rank]: 1 (the ace) to 13 (the king) *)
type card = { rank : int; suit : suit }

(* hearts and diamonds; the games that alternate colors ask this *)
val red : card -> bool

(* [of_index i]: card [i] of the numbering above, e.g. of_index 0 = the
   ace of clubs, of_index 51 = the king of spades *)
val of_index : int -> card

(* [name c]: the two letters players write, rank then suit, "T" for
   ten: "AC", "TD", "QS" *)
val name : card -> string

(* the 52 cards, in the numbering's order *)
val deck : card list

(* [deal n]: the 52 cards of Microsoft's deal number [n], in the order
   they are dealt. Windows' FreeCell (1991, Jim Horne's port of Paul
   Alfille's 1978 game) numbered its deals 1 to 32000, so that players
   could name a deal and compare their play, and every deal is the
   same on every computer: a deal is a linear congruential generator,
   C's rand() of Microsoft's C library, seeded with the number,

     state := (state * 214013 + 2531011) mod 2^31
     rand   = state / 2^16                     (15 bits, 0 to 32767)

   choosing which of the cards still in the deck is dealt next:

     cards = [0 .. 51]
     while cards is not empty:
       j = rand() mod (the number of cards left)
       swap card j with the last card, and deal the last card

   FreeCell deals them row by row onto 8 columns, so deal 1 is (the
   worked example, checked by the kit's tests):

     JD 2D 9H JC 5D 7H 7C 5H
     KD KC 9S 5S AD QC KH 3H
     2S KS 9D QD JS AS AH 3C
     4C 5C TS QH 4H AC 4D 7S
     3S TD 4S TH 8H 2C JH 7D
     6D 8S 8D QS 6C 3D 8C TC
     6S 9C 2H 6H *)
val deal : int -> card list

(* a card's size on the screen, and the offset between two cards fanned
   down a column: [fanned] face up, [stacked] face down *)
val width : number
val height : number
val fanned : number
val stacked : number

(* [face c]: the card face up, its center at (0, 0): the rank and the
   suit side by side in the top left corner (all that shows of a card
   fanned in a column), and a big suit in the middle. The four
   suits are drawn as shapes, not characters (a font need not have
   them): a diamond is a square turned 45 degrees, a heart two circles
   over a triangle pointing down, a spade the same upside down on a
   stem, a club three circles on a stem. *)
val face : card -> shape

(* the back of a card, and the outline of an empty place *)
val back : shape
val slot : shape

(* [suit_shape s size]: the suit alone, [size] across, in its own color
   or in [color] (e.g. the mark of an empty foundation) *)
val suit_shape : ?color:color -> suit -> number -> shape

(* [under (x, y) (mx, my)]: whether the point (mx, my) is on the card
   whose center is at (x, y) *)
val under : number * number -> number * number -> bool
