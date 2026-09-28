(* Same inputs, same model, bit for bit: the property lockstep
 * networking stands on (notes_networking.md section 8), checked the way
 * a second computer would, by the models' checksums (Checksum.mli).
 * Tetris, its randomness in the model (Playground.pick): two games
 * with one seed and the same keys stay identical, whatever the clock
 * says in their Ticks; another seed, another game. *)
val tests : Testo.t list
