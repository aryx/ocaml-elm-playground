(* Lockstep over Sim_net: the .mli's worked example -- two
 * and three peers, 1,000 ticks, under latency, jitter, loss and
 * duplication, all computing the model of the same inputs applied on
 * one machine; stalls or not by the latency; a disagreement injected
 * at tick 500 caught at the next checksum; garbage dropped *)
val tests : Testo.t list
