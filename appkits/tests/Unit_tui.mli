(* appkits/tui's programs, driven by events: Snake moving on ticks (a
 * move per tenth of a second, several for a slow frame), turning but
 * never back, crashing into the wall, q ending it; and its screen sent
 * as a few bytes a move *)
val tests : Testo.t list
