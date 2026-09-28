(* Money: amounts in whole cents, never in floats.
 *
 * The first thing to know about a program that keeps money: a float
 * cannot hold 0.10. Binary fractions are sums of halves, quarters,
 * eighths..., and a tenth is not such a sum, so it is rounded, and the
 * roundings add up:
 *
 *   0.1 +. 0.2 = 0.30000000000000004      (floats)
 *   10 + 20    = 30                        (cents)
 *
 * A checkbook that is off by a cent after a thousand additions is
 * wrong, and the person reconciling it will find the cent. So an
 * amount is an int of cents: addition is exact, and only the screen
 * puts a point two digits from the right. COBOL's decimal arithmetic
 * (1959) was the same answer, built into the language.
 *
 * And the other thing a check needs: the amount in words, what the
 * bank reads when the figures are unclear (and what makes changing
 * 100 into 900 harder). Worked example, checked by the tests:
 *
 *   123.45  ->  One hundred twenty-three and 45/100
 *)

type cents = int

(* "1,234.56", "-42.17", "$42", "42.5": Some cents; a third decimal,
 * or anything that is not a number, None *)
val of_string : string -> cents option

(* 123456 -> "1,234.56", -4217 -> "-42.17" *)
val to_string : cents -> string

(* the amount in words as a check spells it: the dollars in words, the
 * cents as a fraction of 100 -- 0.07 is "Zero and 07/100"; a negative
 * amount is written as its size *)
val words : cents -> string
