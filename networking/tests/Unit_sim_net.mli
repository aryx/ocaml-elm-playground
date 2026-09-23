(* networking/Sim_net: the .mli's worked example as laws -- loss within
 * three standard deviations, delays between latency and latency +
 * jitter and their mean in the middle, no reordering without jitter,
 * some with it -- and a seed is a network: the same seed, the same
 * packets *)
val tests : Testo.t list
