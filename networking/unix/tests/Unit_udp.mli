(* networking/unix/Udp: datagrams between a host and a player over
 * localhost, the host learning its player from the first one; a third
 * sender ignored; and Lockstep over real sockets, 300 ticks, both peers
 * computing the model of the same inputs applied on one machine *)
val tests : Testo.t list
