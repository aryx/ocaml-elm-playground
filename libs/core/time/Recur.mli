(* Recur: a repeating event as a rule, not a list.

   "Every other week on Monday, Wednesday and Friday until Christmas"
   is a rule; its dates are computed when a calendar shows a window
   of days, and never stored -- a rule with no end has infinitely
   many. This is iCalendar's RRULE (RFC 5545, 3.3.10), its common
   subset; the text form ("FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE,FR") is
   Ics.mli's business, this module is what it means.

   A rule cuts time into periods (days, weeks, months or years, every
   [interval]th one from the start's) and picks days in each:

       FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE,FR, from Monday 1997-09-01

       week of   Sep 1       Sep 8     Sep 15      Sep 22    Sep 29
                 M . W . F   (skip)    M . W . F   (skip)    M . W . F
                 1   3   5             15  17  19            29 ...

   The picking, per frequency:
   - DAILY: the period's day;
   - WEEKLY: the [by_day] weekdays (the start's if none), the week
     beginning on [week_start] (Monday unless WKST says otherwise);
   - MONTHLY: the [by_month_day] days (the start's day if none), -1 the
     last, -3 the third-to-last; a month without the day is skipped
     (the 31st: no February, no April);
   - YEARLY: the start's month and day (a 29 February: leap years only).

   Then the occurrences before the start are dropped, and the rule ends
   after [count] of them, or after [until], or never.

   Worked examples: RFC 5545's own (3.8.5.3), in the tests, e.g. the
   one above: Sep 1, 3, 5, 15, 17, 19, 29, ... until Dec 22.

   Left out (a rule using them is refused by Ics.rule_of_string rather
   than misread): BYDAY with an ordinal (the 1st Friday, 1FR), BYMONTH,
   BYYEARDAY, BYWEEKNO, BYSETPOS, and the sub-day frequencies
   (HOURLY...).

   Reference: RFC 5545, "Internet Calendaring and Scheduling Core
   Object Specification (iCalendar)" (Bernard Desruisseaux, 2009). *)

type freq = Daily | Weekly | Monthly | Yearly

(* the last occurrence allowed: a day, or a day and a time (seconds
 * after midnight), compared with the occurrence's own time *)
type until = Until_date of Civil.date | Until_time of Civil.date * int

type rule = {
  freq : freq;
  interval : int; (* every [interval]th period, 1 or more *)
  by_day : int list; (* weekdays, 0 Sunday .. 6 Saturday (WEEKLY) *)
  by_month_day : int list; (* 1..31 or -31..-1 (MONTHLY) *)
  week_start : int; (* a weekday, 1 (Monday) by default *)
  count : int option;
  until : until option;
}

(* [make freq]: every period, no end *)
val make : freq -> rule

(* [occurrences ?at rule ~start ~from ~upto]: the days the event
 * happens on between [from] and [upto] (both included), in order.
 * [start] is the first one (DTSTART), [at] its time of day in seconds
 * (0 by default), which only [Until_time] looks at. Computed from
 * [start], since [count] counts from there. *)
val occurrences :
  ?at:int -> rule -> start:Civil.date -> from:Civil.date -> upto:Civil.date -> Civil.date list
