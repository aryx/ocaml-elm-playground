(* Rollback over Sim_net: the .mli's worked example -- two
 * and three peers, 1,000 ticks, under latency, jitter, loss and
 * duplication, every confirmed model the one of the same inputs on one
 * machine; at 100 ms, full speed where lockstep runs at half, paid in
 * replays; a disagreement injected at tick 500 caught by the checksums
 * of confirmed models *)
val tests : Testo.t list
