(* Bright_stars: the stars the constellations are drawn with.

   About 150 stars: every one brighter than magnitude 2 or so, and the
   fainter ones the figures need (the Little Dipper's handle). A star is
   known by its Bayer designation (Johann Bayer, Uranometria, 1603): a
   Greek letter, roughly in order of brightness, and the constellation's
   Latin genitive, abbreviated -- alpha Orionis, "alf Ori" in the ASCII
   of the catalogues -- and, for the brightest, by its proper name,
   mostly Arabic (Betelgeuse, yad al-jauza, the hand of Orion).

   The magnitude is Hipparchus' scale (about 130 BC), made exact by
   Norman Pogson (1856): each step is 2.512 times fainter, five steps a
   hundred times, 6 the faintest the eye sees under a dark sky; the
   brightest go below zero (Sirius -1.46). The spectral class is the
   Harvard sequence O B A F G K M (Annie Jump Cannon, 1901), hot and
   blue to cool and red, the star's colour: Rigel B, blue-white;
   Betelgeuse M, orange.

   Positions are J2000 (Celestial.precess moves them to a date), from
   the Yale Bright Star Catalogue and Hipparcos, rounded to the second;
   proper motions are left out (Arcturus, the fastest here, moves a
   degree in 2000 years). Magnitudes of variable stars (Betelgeuse,
   Mira) are typical ones.

   The figures are lines between stars, a convention and not a fact --
   the IAU (1928) fixed only the constellations' borders -- these being
   close to the usual Western ones (H. A. Rey's, "The Stars", 1952, more
   or less).

   References: Dorrit Hoffleit and Wayne Warren, "The Bright Star
   Catalogue" (5th ed., 1991); Mitchell Charity, "What color are the
   stars?" (2001), for the colours. *)

type star = {
  id : string; (* "alf Ori" *)
  name : string; (* "Betelgeuse", or "" *)
  pos : Celestial.equatorial; (* J2000 *)
  mag : float;
  spectral : char; (* 'O' .. 'M' *)
}

val stars : star list

(* [find id]: the star of a Bayer designation *)
val find : string -> star option

(* a constellation's name and its lines, as pairs of Bayer designations *)
type figure = { constellation : string; lines : (string * string) list }

val figures : figure list

(* the colour of a spectral class, as (r, g, b) in 0-255 *)
val color : char -> int * int * int
