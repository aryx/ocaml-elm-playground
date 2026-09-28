(* Clock: the time you read on a wall.

   An animation counts seconds from wherever it likes; a clock must
   count them from the Unix epoch, 1970-01-01 at 00:00 in Greenwich
   (UTC), and then say what they are *here*. A day is 86400 of them
   (leap seconds are not counted by Unix time, so every day is), and
   "here" is an offset, the minutes the local clocks are ahead of UTC:

        seconds since the epoch           1790253735.
      + offset * 60                        + 120 * 60     (Paris, summer)
      = local seconds                     1790260935.
        / 86400 -> day 20720 (Civil: 2026-09-24, a Thursday)
        mod 86400 -> 52935 s = 14:42:15

   The offset is not the zone: Paris is +60 in winter and +120 in
   summer, and which one applies at an instant is the platform's to say
   (the tz database is out of scope, plan_pim.md). Some are not whole
   hours: India is +330, Nepal +345.

   No clock is read in here (the README's principle 5): the seconds and
   the offset are arguments, and only an app, given its [computer], has
   the time. *)

type time_of_day = { hour : int (* 0-23 *); minute : int (* 0-59 *); second : float (* [0, 60) *) }

val seconds_per_day : int

(* [split ~offset t]: the local day number (Civil's) and time of day
 * of [t], seconds since the epoch, [offset] minutes east of UTC. Before
 * the epoch too: -1. is day -1 (1969-12-31) at 23:59:59. *)
val split : offset:int -> float -> int * time_of_day

(* the same with the day as a date *)
val local : offset:int -> float -> Civil.date * time_of_day

(* [of_local ~offset date tod]: the inverse, back to seconds since the
 * epoch *)
val of_local : offset:int -> Civil.date -> time_of_day -> float

(* "14:42:15", the seconds truncated; [~seconds:false] for "14:42" *)
val to_string : ?seconds:bool -> time_of_day -> string

(* "+02:00", "-04:00", "+05:30": the offset as ISO 8601 writes it *)
val offset_to_string : int -> string
