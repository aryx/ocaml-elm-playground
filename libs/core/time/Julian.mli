(* Julian: the calendar before the Gregorian one, and the switch.

   Julius Caesar's calendar (46 BC) has a leap year every 4th year, no
   exception: a year of 365.25 days, 11 minutes too long. By 1582 the
   spring equinox had drifted 10 days, and Pope Gregory XIII's reform
   dropped them (and the leap years of 1700, 1800 and 1900, but not
   2000: Civil.mli's rule). Not everyone switched at once: Catholic
   Europe in October 1582, Britain and its colonies only in September
   1752, 11 days by then -- which is why Unix's cal shows

          September 1752
       Su Mo Tu We Th Fr Sa
              1  2 14 15 16
       17 18 19 20 21 22 23
       24 25 26 27 28 29 30

   Wednesday the 2nd (Julian) was followed by Thursday the 14th
   (Gregorian): the days of the week went on, only their numbers
   jumped.

   The day numbers are Civil's (days since 1970-01-01), the same for
   both calendars: a day is a day, only its name differs. The Julian
   dates are computed as Civil's are, a year starting in March, with
   eras of 4 years (1461 days) instead of 400.

   Worked examples: Julian 1582-10-04 and Gregorian 1582-10-15 are
   consecutive days (-141428 and -141427). The astronomers' Julian day
   number (Joseph Scaliger, 1583; nothing to do with the calendar but
   the name, from his father Julius) counts days from Julian -4712-01-01
   (4713 BC): 2000-01-01 is day 2451545.

   Reference: Edward Graham Richards, "Mapping Time: The Calendar and
   its History" (1998). *)

(* every 4th year: 1900 yes (not in Civil) *)
val is_leap_year : int -> bool

val days_in_month : int -> int -> int

(* the day number of a Julian date, and back *)
val days_from_julian : Civil.date -> int
val julian_from_days : int -> Civil.date

(* The switch: the first Gregorian day, as a day number; the day
 * before it is Julian. *)
type switch = int

(* Britain and its colonies, 1752-09-14 *)
val england : switch

(* Rome, and Catholic Europe, 1582-10-15 *)
val rome : switch

(* [of_days switch n]: the date people wrote on day [n] -- Julian before
 * the switch, Gregorian from it *)
val of_days : switch -> int -> Civil.date

(* [to_days switch d]: the day number of a date as people wrote it; a
 * date the switch skipped (1752-09-05) is None *)
val to_days : switch -> Civil.date -> int option

(* [month switch year month]: the day numbers of the month's days, in
 * order: 30 for September 1752 in most places, 19 in England *)
val month : switch -> int -> int -> int list

(* the astronomers' Julian day number of a day number (the one starting
 * at noon on that day) *)
val julian_day_number : int -> int
