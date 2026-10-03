(* Physics.simulate's small steps and Physics.grouped: a stick 2 thick
 * that lands on its end is thrown away with one step a tick, and
 * lands with four; a tick's fall with four steps is 0.625 of one
 * step's; a push lasts all the steps; two bodies of a group pass
 * through each other, and still land on the floor *)
val tests : Testo.t list
