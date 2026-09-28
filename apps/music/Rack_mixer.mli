(* Reason's Mixer 14:2 as a rack device (Rack_device.mli): 14 stereo
 * channels into two, each with its level, pan, mute and one aux send;
 * the send's return added to the sum; the master.
 *
 *   ch 1..14 --level, pan, mute--+--------------------+--master--> Master Out (16)
 *        |                      |  sum              |
 *        +--aux--> Aux Send (14) ... an effect ... Aux Return (15)
 *
 * In two stages, "sends" (the channels in, the sum and the aux send
 * out) and "master" (the return in, the master out): a send and its
 * return pass through an effect between them without a loop.
 *
 * Its knobs, by name: "ch1.level" .. "ch14.level" (0 to 1, 0.7 at
 * first), "chN.pan" (-1 left to 1 right), "chN.aux" (0 to 1), "chN.mute"
 * (0 or 1), "master" (0 to 1). Ours, and said so: one aux (Reason has
 * four), no EQ, no solo; the pan a balance (a side turned down, the
 * other left). *)

val channels : int (* 14 *)
val create : unit -> Rack_device.t

(* the jacks: channel k (0 to 13) in, then these *)
val aux_send : int
val aux_return : int
val master_out : int
