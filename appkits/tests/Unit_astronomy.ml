(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits: Celestial, Ephemeris, Bright_stars, Stereographic *)

let t = Testo.create

(* degrees, to a tolerance in degrees *)
let check_deg (what : string) ~(tol : float) (expected : float) (actual : float) =
  let d = Float.rem (actual -. expected +. 540.) 360. -. 180. in
  if Float.abs d > tol then Alcotest.failf "%s: expected %.5f deg, got %.5f (off by %.5f)" what expected actual d

let ra_deg (p : Celestial.equatorial) = Celestial.to_degrees p.ra
let dec_deg (p : Celestial.equatorial) = Celestial.to_degrees p.dec

(* Meeus, examples 12.a and 12.b *)
let test_sidereal () =
  check_deg "GMST 1987-04-10 0h" ~tol:1e-6
    (Celestial.to_degrees (Celestial.hours 13. 10. 46.3668))
    (Celestial.to_degrees (Celestial.gmst 2446895.5));
  check_deg "GMST 1987-04-10 19:21" ~tol:1e-5
    (Celestial.to_degrees (Celestial.hours 8. 34. 57.0896))
    (Celestial.to_degrees (Celestial.gmst 2446896.30625))

(* the Unix epoch and J2000 as Julian dates *)
let test_julian_date () =
  Alcotest.(check (float 1e-9)) "epoch" 2440587.5 (Celestial.julian_date 0.);
  (* 2000-01-01 at noon: day 10957 since 1970 (Civil), and a half *)
  Alcotest.(check (float 1e-9)) "J2000" Celestial.j2000 (Celestial.julian_date ((10957. +. 0.5) *. 86400.))

(* Meeus, example 21.b, without theta Persei's proper motion *)
let test_precession () =
  let p0 : Celestial.equatorial = { ra = Celestial.hours 2. 44. 11.986; dec = Celestial.dms 1 49. 13. 42.48 } in
  let p = Celestial.precess 2462088.69 p0 in
  (* 1 arcsec is 0.00028 deg *)
  check_deg "RA" ~tol:0.0003 (Celestial.to_degrees (Celestial.hours 2. 46. 10.343)) (ra_deg p);
  check_deg "Dec" ~tol:0.0003 (Celestial.to_degrees (Celestial.dms 1 49. 20. 57.12)) (dec_deg p);
  (* and the pyramids' pole star: Thuban within a degree of the pole
   * around 2800 BC (Lieske's polynomials stretched to 48 centuries),
   * Polaris 25 degrees away *)
  let jd = Celestial.julian_date (-150_500_000_000.) in
  let thuban = Option.get (Bright_stars.find "alf Dra") and polaris = Option.get (Bright_stars.find "alf UMi") in
  let dec (st : Bright_stars.star) = dec_deg (Celestial.precess jd st.pos) in
  if dec thuban < 88.5 then Alcotest.failf "Thuban at %.2f deg" (dec thuban);
  if dec polaris > 70. then Alcotest.failf "Polaris at %.2f deg" (dec polaris)

(* Meeus, example 13.b: Venus from Washington (38 55 17 N, 77 03 56 W)
 * on 1987-04-10 at 19:21 UT, at RA 23h09m16.641s Dec -6 43 11.61,
 * is 15.1249 deg high at 68.0337 deg west of south *)
let test_horizontal () =
  let lat = Celestial.dms 1 38. 55. 17. and lon = Celestial.dms (-1) 77. 3. 56. in
  let lst = Celestial.lst 2446896.30625 ~lon in
  let h =
    Celestial.to_horizontal ~lat ~lst { ra = Celestial.hours 23. 9. 16.641; dec = Celestial.dms (-1) 6. 43. 11.61 }
  in
  check_deg "altitude" ~tol:0.001 15.1249 (Celestial.to_degrees h.alt);
  check_deg "azimuth" ~tol:0.001 (180. +. 68.0337) (Celestial.to_degrees h.az)

(* Kepler's equation, Meeus example 30.a: e = 0.1, M = 5 deg *)
let test_kepler () =
  let e = Ephemeris.kepler ~e:0.1 (Celestial.degrees 5.) in
  check_deg "E" ~tol:1e-6 5.554589 (Celestial.to_degrees e)

(* Meeus, examples 25.a and 33.a (apparent positions: our mean ones
 * differ by the nutation and the aberration, about 20 arcsec) *)
let test_sun_venus () =
  let jd = 2448908.5 in
  let sun = Celestial.precess jd (Ephemeris.sun jd) in
  check_deg "Sun RA" ~tol:0.02 (Celestial.to_degrees (Celestial.hours 13. 13. 31.4)) (ra_deg sun);
  check_deg "Sun Dec" ~tol:0.02 (Celestial.to_degrees (Celestial.dms (-1) 7. 47. 6.)) (dec_deg sun);
  let jd = 2448976.5 in
  let venus = Celestial.precess jd (Ephemeris.planet Venus jd) in
  check_deg "Venus RA" ~tol:0.03 (Celestial.to_degrees (Celestial.hours 21. 4. 41.454)) (ra_deg venus);
  check_deg "Venus Dec" ~tol:0.03 (Celestial.to_degrees (Celestial.dms (-1) 18. 53. 16.84)) (dec_deg venus)

(* Meeus, example 47.a, to the low precision formulas' 0.3 deg *)
let test_moon () =
  let lon, lat = Ephemeris.moon_ecliptic 2448724.5 in
  check_deg "longitude" ~tol:0.3 133.162655 (Celestial.to_degrees lon);
  check_deg "latitude" ~tol:0.3 (-3.229126) (Celestial.to_degrees lat);
  (* the full moon of 2000-01-21 (a total eclipse, at 4:44 UT), and the
   * new moon of 2000-01-06 (18:14 UT) *)
  let lit t =
    let jd = Celestial.julian_date t in
    Ephemeris.illuminated ~sun:(Celestial.precess jd (Ephemeris.sun jd)) (Ephemeris.moon jd)
  in
  let full = lit ((10957. +. 20.) *. 86400. +. (4.73 *. 3600.)) and fresh = lit ((10957. +. 5.) *. 86400. +. (18.23 *. 3600.)) in
  if full < 0.99 then Alcotest.failf "full moon lit %.3f" full;
  if fresh > 0.01 then Alcotest.failf "new moon lit %.3f" fresh

(* every figure's line joins two stars of the catalogue *)
let test_figures () =
  List.iter
    (fun (f : Bright_stars.figure) ->
      List.iter
        (fun (a, b) ->
          List.iter
            (fun id -> if Bright_stars.find id = None then Alcotest.failf "%s: no star %s" f.constellation id)
            [ a; b ])
        f.lines)
    Bright_stars.figures

(* the stereographic projection: 90 deg away lands at 2, and the horizon
 * circle passes through the horizon's points *)
let test_stereographic () =
  let view : Celestial.horizontal = { az = Celestial.degrees 180.; alt = Celestial.degrees 30. } in
  let x, y = Option.get (Stereographic.project ~view { az = Celestial.degrees 180.; alt = Celestial.degrees 120. }) in
  Alcotest.(check (float 1e-9)) "x" 0. x;
  Alcotest.(check (float 1e-9)) "90 deg up" 2. y;
  let cy, r = Stereographic.horizon view.alt in
  List.iter
    (fun az ->
      match Stereographic.project ~view { az = Celestial.degrees az; alt = 0. } with
      | Some (x, y) -> Alcotest.(check (float 1e-9)) (Printf.sprintf "horizon at %.0f" az) r (Float.hypot x (y -. cy))
      | None -> ())
    [ 90.; 135.; 180.; 225.; 270.; 330. ]

let tests =
  [ t "astronomy: sidereal time" test_sidereal;
    t "astronomy: julian date" test_julian_date;
    t "astronomy: precession, and the pyramids' pole star" test_precession;
    t "astronomy: altitude and azimuth" test_horizontal;
    t "astronomy: Kepler's equation" test_kepler;
    t "astronomy: the Sun and Venus" test_sun_venus;
    t "astronomy: the Moon and its phases" test_moon;
    t "astronomy: the figures' stars" test_figures;
    t "astronomy: the stereographic projection" test_stereographic;
  ]
