(* Civil: the Gregorian calendar computed, not looked up.

   A computer counts time as one number (seconds, or days, since
   1970-01-01, the Unix epoch); people count it in years, months and
   days. This module goes between the two for days: a date is a day
   number, and a day number is a date, for any year, with no table.

   The trick (Howard Hinnant's, below) is to start the year in March.
   February, the month whose length varies, is then the year's last,
   and the leap day at the very end of it can't shift any other month:

       month   Mar Apr May Jun Jul Aug Sep Oct Nov Dec Jan Feb
       mp        0   1   2   3   4   5   6   7   8   9  10  11
       length   31  30  31  30  31  31  30  31  30  31  31  28|29
       starts    0  31  61  92 122 153 184 214 245 275 306 337
                 = (153 * mp + 2) / 5, a line through the 30s and 31s

   And the years come in eras of 400, each exactly 146097 days (97 of
   its years leap: every 4th, but not every 100th, but every 400th --
   the Gregorian rule, 1582), so a day number is an era, a year in the
   era, a day in the year, a month, a day.

   Worked example: 2026-09-24 is day 20720 (2026-01-01 is 20454, and
   September 24 is the year's 267th day), and a Thursday, as 1970-01-01
   (day 0) was.

   The simple version, beside it: Zeller's congruence (1882), the
   weekday from the date alone, with January and February counted as
   months 13 and 14 of the year before -- the same March trick, 140
   years earlier. The tests check the two agree on every day of 800
   years.

   Dates here are proleptic Gregorian: the rule applied to years
   before 1582 too, and year 0 exists (it is 1 BC). The calendar
   people actually used before the switch is Julian.mli's.

   References: Howard Hinnant, "chrono-Compatible Low-Level Date
   Algorithms" (2013); Christian Zeller, "Kalender-Formeln", Acta
   Mathematica 9 (1887, first published 1882). *)

type date = { year : int; month : int (* 1-12 *); day : int (* 1-31 *) }

(* every 4th year, but not every 100th, but every 400th: 2000 yes,
 * 1900 no, 2024 yes *)
val is_leap_year : int -> bool

(* [days_in_month year month]: 28 to 31 *)
val days_in_month : int -> int -> int

(* [is_valid d]: the month is 1-12 and the day exists in it
 * (2000-02-29 yes, 1900-02-29 no) *)
val is_valid : date -> bool

(* the date's day number: days since 1970-01-01, negative before it.
 * The date is assumed valid. *)
val days_from_civil : date -> int

(* the day number's date, the inverse of [days_from_civil] *)
val civil_from_days : int -> date

(* [weekday days]: 0 for Sunday to 6 for Saturday (as Unix's tm_wday
 * and JavaScript's getDay count), from a day number *)
val weekday : int -> int

(* the same, from the date, by Zeller's congruence *)
val zeller : date -> int

(* [add_months d n]: [n] months later (or earlier), the day kept when
 * the month has it and clamped otherwise: 2024-01-31 plus one month is
 * 2024-02-29. For a calendar's "next month" arrow. *)
val add_months : date -> int -> date

(* "January" .. "December", for 1-12 *)
val month_name : int -> string

(* "Sunday" .. "Saturday", for 0-6 *)
val weekday_name : int -> string

(* "2026-09-24", ISO 8601's order, the one that sorts *)
val to_string : date -> string
