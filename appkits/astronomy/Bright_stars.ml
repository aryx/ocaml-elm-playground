(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Bright_stars.mli *)

type star = { id : string; name : string; pos : Celestial.equatorial; mag : float; spectral : char }

(* [s id name (h, m, s) (sign, d, m, s) mag spectral]: a catalogue's
 * line, RA in hours and Dec in degrees as it prints them *)
let s id name (rh, rm, rs) (sign, dd, dm, ds) mag spectral : star =
  { id; name; pos = { ra = Celestial.hours rh rm rs; dec = Celestial.dms sign dd dm ds }; mag; spectral }

(* by constellation, west to east more or less *)
let stars : star list =
  [ (* Andromeda, Pegasus, Cassiopeia *)
    s "alf And" "Alpheratz" (0., 8., 23.3) (1, 29., 5., 26.) 2.06 'B';
    s "del And" "" (0., 39., 19.7) (1, 30., 51., 40.) 3.27 'K';
    s "bet And" "Mirach" (1., 9., 43.9) (1, 35., 37., 14.) 2.05 'M';
    s "gam And" "Almach" (2., 3., 54.0) (1, 42., 19., 47.) 2.10 'K';
    s "alf Peg" "Markab" (23., 4., 45.7) (1, 15., 12., 19.) 2.49 'B';
    s "bet Peg" "Scheat" (23., 3., 46.5) (1, 28., 4., 58.) 2.42 'M';
    s "gam Peg" "Algenib" (0., 13., 14.2) (1, 15., 11., 1.) 2.83 'B';
    s "eps Peg" "Enif" (21., 44., 11.2) (1, 9., 52., 30.) 2.38 'K';
    s "zet Peg" "Homam" (22., 41., 27.7) (1, 10., 49., 53.) 3.40 'B';
    s "the Peg" "Biham" (22., 10., 12.0) (1, 6., 11., 52.) 3.53 'A';
    s "bet Cas" "Caph" (0., 9., 10.7) (1, 59., 8., 59.) 2.27 'F';
    s "alf Cas" "Schedar" (0., 40., 30.4) (1, 56., 32., 14.) 2.24 'K';
    s "gam Cas" "" (0., 56., 42.5) (1, 60., 43., 0.) 2.47 'B';
    s "del Cas" "Ruchbah" (1., 25., 48.9) (1, 60., 14., 7.) 2.68 'A';
    s "eps Cas" "Segin" (1., 54., 23.7) (1, 63., 40., 12.) 3.37 'B';
    (* Aries, Cetus, Eridanus *)
    s "alf Ari" "Hamal" (2., 7., 10.4) (1, 23., 27., 45.) 2.00 'K';
    s "bet Ari" "Sheratan" (1., 54., 38.4) (1, 20., 48., 29.) 2.64 'A';
    s "bet Cet" "Diphda" (0., 43., 35.4) (-1, 17., 59., 12.) 2.04 'K';
    s "omi Cet" "Mira" (2., 19., 20.8) (-1, 2., 58., 39.) 3.0 'M';
    s "alf Eri" "Achernar" (1., 37., 42.8) (-1, 57., 14., 12.) 0.46 'B';
    (* Perseus, Taurus, Auriga *)
    s "alf Per" "Mirfak" (3., 24., 19.4) (1, 49., 51., 40.) 1.79 'F';
    s "bet Per" "Algol" (3., 8., 10.1) (1, 40., 57., 20.) 2.12 'B';
    s "gam Per" "" (3., 4., 47.8) (1, 53., 30., 23.) 2.93 'G';
    s "del Per" "" (3., 42., 55.5) (1, 47., 47., 15.) 3.01 'B';
    s "eps Per" "" (3., 57., 51.2) (1, 40., 0., 37.) 2.89 'B';
    s "zet Per" "" (3., 54., 7.9) (1, 31., 53., 1.) 2.85 'B';
    s "alf Tau" "Aldebaran" (4., 35., 55.2) (1, 16., 30., 33.) 0.86 'K';
    s "bet Tau" "Elnath" (5., 26., 17.5) (1, 28., 36., 27.) 1.65 'B';
    s "zet Tau" "" (5., 37., 38.7) (1, 21., 8., 33.) 3.00 'B';
    s "gam Tau" "" (4., 19., 47.6) (1, 15., 37., 39.) 3.65 'K';
    s "eps Tau" "" (4., 28., 37.0) (1, 19., 10., 50.) 3.53 'K';
    s "eta Tau" "Pleiades" (3., 47., 29.1) (1, 24., 6., 18.) 2.87 'B';
    s "alf Aur" "Capella" (5., 16., 41.4) (1, 45., 59., 53.) 0.08 'G';
    s "bet Aur" "Menkalinan" (5., 59., 31.7) (1, 44., 56., 51.) 1.90 'A';
    s "the Aur" "" (5., 59., 43.3) (1, 37., 12., 45.) 2.62 'A';
    s "iot Aur" "" (4., 56., 59.6) (1, 33., 9., 58.) 2.69 'K';
    (* Orion and his dogs *)
    s "alf Ori" "Betelgeuse" (5., 55., 10.3) (1, 7., 24., 25.) 0.50 'M';
    s "bet Ori" "Rigel" (5., 14., 32.3) (-1, 8., 12., 6.) 0.13 'B';
    s "gam Ori" "Bellatrix" (5., 25., 7.9) (1, 6., 20., 59.) 1.64 'B';
    s "del Ori" "Mintaka" (5., 32., 0.4) (-1, 0., 17., 57.) 2.23 'O';
    s "eps Ori" "Alnilam" (5., 36., 12.8) (-1, 1., 12., 7.) 1.69 'B';
    s "zet Ori" "Alnitak" (5., 40., 45.5) (-1, 1., 56., 34.) 1.77 'O';
    s "kap Ori" "Saiph" (5., 47., 45.4) (-1, 9., 40., 11.) 2.09 'B';
    s "lam Ori" "Meissa" (5., 35., 8.3) (1, 9., 56., 3.) 3.39 'O';
    s "alf CMa" "Sirius" (6., 45., 8.9) (-1, 16., 42., 58.) (-1.46) 'A';
    s "bet CMa" "Mirzam" (6., 22., 42.0) (-1, 17., 57., 22.) 1.98 'B';
    s "eps CMa" "Adhara" (6., 58., 37.5) (-1, 28., 58., 20.) 1.50 'B';
    s "del CMa" "Wezen" (7., 8., 23.5) (-1, 26., 23., 36.) 1.84 'F';
    s "eta CMa" "Aludra" (7., 24., 5.7) (-1, 29., 18., 11.) 2.45 'B';
    s "alf CMi" "Procyon" (7., 39., 18.1) (1, 5., 13., 30.) 0.34 'F';
    s "bet CMi" "Gomeisa" (7., 27., 9.0) (1, 8., 17., 22.) 2.89 'B';
    (* Gemini *)
    s "alf Gem" "Castor" (7., 34., 36.0) (1, 31., 53., 18.) 1.58 'A';
    s "bet Gem" "Pollux" (7., 45., 18.9) (1, 28., 1., 34.) 1.14 'K';
    s "gam Gem" "Alhena" (6., 37., 42.7) (1, 16., 23., 57.) 1.93 'A';
    s "mu Gem" "" (6., 22., 57.6) (1, 22., 30., 49.) 2.88 'M';
    s "eps Gem" "" (6., 43., 55.9) (1, 25., 7., 52.) 2.98 'G';
    s "del Gem" "" (7., 20., 7.4) (1, 21., 58., 56.) 3.53 'F';
    (* the southern ship, and its false cross *)
    s "alf Car" "Canopus" (6., 23., 57.1) (-1, 52., 41., 45.) (-0.74) 'A';
    s "bet Car" "Miaplacidus" (9., 13., 12.0) (-1, 69., 43., 2.) 1.68 'A';
    s "eps Car" "Avior" (8., 22., 30.8) (-1, 59., 30., 34.) 1.86 'K';
    s "iot Car" "" (9., 17., 5.4) (-1, 59., 16., 31.) 2.25 'A';
    s "gam Vel" "" (8., 9., 32.0) (-1, 47., 20., 12.) 1.83 'O';
    s "del Vel" "" (8., 44., 42.2) (-1, 54., 42., 30.) 1.96 'A';
    s "lam Vel" "Suhail" (9., 7., 59.8) (-1, 43., 25., 57.) 2.21 'K';
    s "kap Vel" "" (9., 22., 6.8) (-1, 55., 0., 38.) 2.47 'B';
    s "zet Pup" "Naos" (8., 3., 35.0) (-1, 40., 0., 12.) 2.25 'O';
    (* Leo, Hydra *)
    s "alf Leo" "Regulus" (10., 8., 22.3) (1, 11., 58., 2.) 1.35 'B';
    s "gam Leo" "Algieba" (10., 19., 58.4) (1, 19., 50., 29.) 2.08 'K';
    s "bet Leo" "Denebola" (11., 49., 3.6) (1, 14., 34., 19.) 2.14 'A';
    s "del Leo" "Zosma" (11., 14., 6.5) (1, 20., 31., 25.) 2.56 'A';
    s "eps Leo" "" (9., 45., 51.1) (1, 23., 46., 27.) 2.98 'G';
    s "eta Leo" "" (10., 7., 19.9) (1, 16., 45., 45.) 3.48 'A';
    s "the Leo" "Chertan" (11., 14., 14.4) (1, 15., 25., 46.) 3.33 'A';
    s "zet Leo" "" (10., 16., 41.4) (1, 23., 25., 2.) 3.44 'F';
    s "mu Leo" "" (9., 52., 45.8) (1, 26., 0., 25.) 3.88 'K';
    s "alf Hya" "Alphard" (9., 27., 35.2) (-1, 8., 39., 31.) 1.98 'K';
    (* the Great Bear, the Little Bear, the Dragon *)
    s "alf UMa" "Dubhe" (11., 3., 43.7) (1, 61., 45., 3.) 1.79 'K';
    s "bet UMa" "Merak" (11., 1., 50.5) (1, 56., 22., 57.) 2.37 'A';
    s "gam UMa" "Phecda" (11., 53., 49.8) (1, 53., 41., 41.) 2.44 'A';
    s "del UMa" "Megrez" (12., 15., 25.6) (1, 57., 1., 57.) 3.31 'A';
    s "eps UMa" "Alioth" (12., 54., 1.7) (1, 55., 57., 35.) 1.77 'A';
    s "zet UMa" "Mizar" (13., 23., 55.5) (1, 54., 55., 31.) 2.27 'A';
    s "eta UMa" "Alkaid" (13., 47., 32.4) (1, 49., 18., 48.) 1.86 'B';
    s "alf UMi" "Polaris" (2., 31., 49.1) (1, 89., 15., 51.) 1.98 'F';
    s "bet UMi" "Kochab" (14., 50., 42.3) (1, 74., 9., 20.) 2.08 'K';
    s "gam UMi" "Pherkad" (15., 20., 43.7) (1, 71., 50., 2.) 3.05 'A';
    s "del UMi" "Yildun" (17., 32., 12.9) (1, 86., 35., 11.) 4.35 'A';
    s "eps UMi" "" (16., 45., 58.2) (1, 82., 2., 14.) 4.21 'G';
    s "zet UMi" "" (15., 44., 3.5) (1, 77., 47., 40.) 4.32 'A';
    s "eta UMi" "" (16., 17., 30.3) (1, 75., 45., 19.) 4.95 'F';
    s "alf Dra" "Thuban" (14., 4., 23.3) (1, 64., 22., 33.) 3.65 'A';
    s "gam Dra" "Eltanin" (17., 56., 36.4) (1, 51., 29., 20.) 2.23 'K';
    s "bet Dra" "Rastaban" (17., 30., 25.9) (1, 52., 18., 5.) 2.79 'G';
    s "eta Dra" "" (16., 23., 59.5) (1, 61., 30., 51.) 2.74 'G';
    (* Virgo, Corvus, Bootes, the Crown *)
    s "alf Vir" "Spica" (13., 25., 11.6) (-1, 11., 9., 41.) 0.97 'B';
    s "gam Vir" "Porrima" (12., 41., 39.6) (-1, 1., 26., 58.) 2.74 'F';
    s "del Vir" "" (12., 55., 36.2) (1, 3., 23., 51.) 3.38 'M';
    s "eps Vir" "Vindemiatrix" (13., 2., 10.6) (1, 10., 57., 33.) 2.85 'G';
    s "gam Crv" "Gienah" (12., 15., 48.4) (-1, 17., 32., 31.) 2.59 'B';
    s "bet Crv" "Kraz" (12., 34., 23.2) (-1, 23., 23., 48.) 2.65 'G';
    s "del Crv" "Algorab" (12., 29., 51.9) (-1, 16., 30., 56.) 2.95 'B';
    s "eps Crv" "" (12., 10., 7.5) (-1, 22., 37., 11.) 3.00 'K';
    s "alf Boo" "Arcturus" (14., 15., 39.7) (1, 19., 10., 57.) (-0.05) 'K';
    s "eps Boo" "Izar" (14., 44., 59.2) (1, 27., 4., 27.) 2.37 'K';
    s "eta Boo" "Muphrid" (13., 54., 41.1) (1, 18., 23., 52.) 2.68 'G';
    s "gam Boo" "Seginus" (14., 32., 4.7) (1, 38., 18., 30.) 3.03 'A';
    s "bet Boo" "Nekkar" (15., 1., 56.8) (1, 40., 23., 26.) 3.50 'G';
    s "del Boo" "" (15., 15., 30.2) (1, 33., 18., 53.) 3.47 'G';
    s "alf CrB" "Alphecca" (15., 34., 41.3) (1, 26., 42., 53.) 2.23 'A';
    (* the Southern Cross and the Centaur *)
    s "alf Cru" "Acrux" (12., 26., 35.9) (-1, 63., 5., 57.) 0.77 'B';
    s "bet Cru" "Mimosa" (12., 47., 43.3) (-1, 59., 41., 19.) 1.25 'B';
    s "gam Cru" "Gacrux" (12., 31., 9.9) (-1, 57., 6., 48.) 1.59 'M';
    s "del Cru" "Imai" (12., 15., 8.7) (-1, 58., 44., 56.) 2.79 'B';
    s "alf Cen" "Rigil Kentaurus" (14., 39., 36.5) (-1, 60., 50., 2.) (-0.27) 'G';
    s "bet Cen" "Hadar" (14., 3., 49.4) (-1, 60., 22., 23.) 0.61 'B';
    (* Libra, Scorpius, Ophiuchus *)
    s "alf Lib" "Zubenelgenubi" (14., 50., 52.7) (-1, 16., 2., 30.) 2.75 'A';
    s "bet Lib" "Zubeneschamali" (15., 17., 0.4) (-1, 9., 22., 59.) 2.61 'B';
    s "alf Sco" "Antares" (16., 29., 24.5) (-1, 26., 25., 55.) 1.06 'M';
    s "bet Sco" "Acrab" (16., 5., 26.2) (-1, 19., 48., 20.) 2.62 'B';
    s "del Sco" "Dschubba" (16., 0., 20.0) (-1, 22., 37., 18.) 2.29 'B';
    s "pi Sco" "" (15., 58., 51.1) (-1, 26., 6., 51.) 2.89 'B';
    s "sig Sco" "" (16., 21., 11.3) (-1, 25., 35., 34.) 2.89 'B';
    s "tau Sco" "" (16., 35., 53.0) (-1, 28., 12., 58.) 2.82 'B';
    s "eps Sco" "" (16., 50., 9.8) (-1, 34., 17., 36.) 2.29 'K';
    s "mu1 Sco" "" (16., 51., 52.2) (-1, 38., 2., 51.) 3.04 'B';
    s "zet2 Sco" "" (16., 54., 35.0) (-1, 42., 21., 41.) 3.62 'K';
    s "eta Sco" "" (17., 12., 9.2) (-1, 43., 14., 21.) 3.33 'F';
    s "the Sco" "Sargas" (17., 37., 19.1) (-1, 42., 59., 52.) 1.86 'F';
    s "iot1 Sco" "" (17., 47., 35.1) (-1, 40., 7., 37.) 2.99 'F';
    s "kap Sco" "" (17., 42., 29.3) (-1, 39., 1., 48.) 2.39 'B';
    s "lam Sco" "Shaula" (17., 33., 36.5) (-1, 37., 6., 14.) 1.62 'B';
    s "alf Oph" "Rasalhague" (17., 34., 56.1) (1, 12., 33., 36.) 2.08 'A';
    s "alf TrA" "Atria" (16., 48., 39.9) (-1, 69., 1., 40.) 1.91 'K';
    (* the summer triangle: the Lyre, the Swan, the Eagle *)
    s "alf Lyr" "Vega" (18., 36., 56.3) (1, 38., 47., 1.) 0.03 'A';
    s "zet1 Lyr" "" (18., 44., 46.4) (1, 37., 36., 18.) 4.36 'A';
    s "bet Lyr" "Sheliak" (18., 50., 4.8) (1, 33., 21., 46.) 3.52 'B';
    s "gam Lyr" "Sulafat" (18., 58., 56.6) (1, 32., 41., 22.) 3.25 'B';
    s "del2 Lyr" "" (18., 54., 30.3) (1, 36., 53., 55.) 4.30 'M';
    s "alf Cyg" "Deneb" (20., 41., 25.9) (1, 45., 16., 49.) 1.25 'A';
    s "gam Cyg" "Sadr" (20., 22., 13.7) (1, 40., 15., 24.) 2.23 'F';
    s "eps Cyg" "Aljanah" (20., 46., 12.7) (1, 33., 58., 13.) 2.48 'K';
    s "del Cyg" "" (19., 44., 58.5) (1, 45., 7., 51.) 2.87 'B';
    s "bet Cyg" "Albireo" (19., 30., 43.3) (1, 27., 57., 35.) 3.08 'K';
    s "alf Aql" "Altair" (19., 50., 47.0) (1, 8., 52., 6.) 0.76 'A';
    s "gam Aql" "Tarazed" (19., 46., 15.6) (1, 10., 36., 48.) 2.72 'K';
    s "bet Aql" "Alshain" (19., 55., 18.8) (1, 6., 24., 24.) 3.71 'G';
    s "zet Aql" "" (19., 5., 24.6) (1, 13., 51., 48.) 2.99 'A';
    s "del Aql" "" (19., 25., 29.9) (1, 3., 6., 53.) 3.36 'F';
    s "lam Aql" "" (19., 6., 14.9) (-1, 4., 52., 57.) 3.43 'B';
    s "the Aql" "" (20., 11., 18.3) (-1, 0., 49., 17.) 3.23 'B';
    (* the Archer's teapot, and the far south *)
    s "eps Sgr" "Kaus Australis" (18., 24., 10.3) (-1, 34., 23., 5.) 1.85 'B';
    s "sig Sgr" "Nunki" (18., 55., 15.9) (-1, 26., 17., 48.) 2.05 'B';
    s "zet Sgr" "Ascella" (19., 2., 36.7) (-1, 29., 52., 48.) 2.60 'A';
    s "del Sgr" "Kaus Media" (18., 20., 59.6) (-1, 29., 49., 41.) 2.70 'K';
    s "lam Sgr" "Kaus Borealis" (18., 27., 58.2) (-1, 25., 25., 18.) 2.81 'K';
    s "gam Sgr" "Alnasl" (18., 5., 48.5) (-1, 30., 25., 27.) 2.99 'K';
    s "phi Sgr" "" (18., 45., 39.4) (-1, 26., 59., 27.) 3.17 'B';
    s "tau Sgr" "" (19., 6., 56.4) (-1, 27., 40., 13.) 3.32 'K';
    s "alf Pav" "Peacock" (20., 25., 38.9) (-1, 56., 44., 6.) 1.94 'B';
    s "alf Gru" "Alnair" (22., 8., 13.9) (-1, 46., 57., 40.) 1.74 'B';
    s "alf PsA" "Fomalhaut" (22., 57., 39.0) (-1, 29., 37., 20.) 1.16 'A' ]

let find (id : string) : star option = List.find_opt (fun (st : star) -> st.id = id) stars

type figure = { constellation : string; lines : (string * string) list }

(* [chain ["a"; "b"; "c"]]: the lines a-b and b-c *)
let rec chain (ids : string list) : (string * string) list =
  match ids with a :: (b :: _ as rest) -> (a, b) :: chain rest | _ -> []

let figures : figure list =
  [ { constellation = "Orion";
      lines =
        chain [ "alf Ori"; "zet Ori"; "eps Ori"; "del Ori"; "gam Ori" ]
        @ [ ("alf Ori", "gam Ori"); ("zet Ori", "kap Ori"); ("del Ori", "bet Ori"); ("lam Ori", "alf Ori"); ("lam Ori", "gam Ori") ] };
    { constellation = "Canis Major";
      lines = [ ("bet CMa", "alf CMa"); ("alf CMa", "del CMa"); ("del CMa", "eps CMa"); ("del CMa", "eta CMa") ] };
    { constellation = "Canis Minor"; lines = [ ("alf CMi", "bet CMi") ] };
    { constellation = "Gemini";
      lines = [ ("alf Gem", "bet Gem"); ("alf Gem", "eps Gem"); ("eps Gem", "mu Gem"); ("bet Gem", "del Gem"); ("del Gem", "gam Gem") ] };
    { constellation = "Taurus";
      lines = [ ("gam Tau", "alf Tau"); ("gam Tau", "eps Tau"); ("alf Tau", "zet Tau"); ("eps Tau", "bet Tau") ] };
    { constellation = "Auriga"; lines = chain [ "alf Aur"; "bet Aur"; "the Aur"; "bet Tau"; "iot Aur"; "alf Aur" ] };
    { constellation = "Perseus";
      lines = chain [ "gam Per"; "alf Per"; "del Per"; "eps Per"; "zet Per" ] @ [ ("alf Per", "bet Per") ] };
    { constellation = "Cassiopeia"; lines = chain [ "bet Cas"; "alf Cas"; "gam Cas"; "del Cas"; "eps Cas" ] };
    { constellation = "Andromeda"; lines = chain [ "alf And"; "del And"; "bet And"; "gam And" ] };
    { constellation = "Pegasus";
      lines = chain [ "alf Peg"; "bet Peg"; "alf And"; "gam Peg"; "alf Peg" ] @ chain [ "alf Peg"; "zet Peg"; "the Peg"; "eps Peg" ] };
    { constellation = "Aries"; lines = [ ("alf Ari", "bet Ari") ] };
    { constellation = "Ursa Major";
      lines = chain [ "alf UMa"; "bet UMa"; "gam UMa"; "del UMa"; "alf UMa" ] @ chain [ "del UMa"; "eps UMa"; "zet UMa"; "eta UMa" ] };
    { constellation = "Ursa Minor";
      lines = chain [ "alf UMi"; "del UMi"; "eps UMi"; "zet UMi"; "bet UMi"; "gam UMi"; "eta UMi"; "zet UMi" ] };
    { constellation = "Draco"; lines = [ ("gam Dra", "bet Dra"); ("bet Dra", "eta Dra"); ("eta Dra", "alf Dra") ] };
    { constellation = "Leo";
      lines =
        chain [ "alf Leo"; "eta Leo"; "gam Leo"; "zet Leo"; "mu Leo"; "eps Leo" ]
        @ chain [ "gam Leo"; "del Leo"; "bet Leo"; "the Leo"; "alf Leo" ] @ [ ("del Leo", "the Leo") ] };
    { constellation = "Virgo"; lines = chain [ "alf Vir"; "gam Vir"; "del Vir"; "eps Vir" ] };
    { constellation = "Corvus"; lines = chain [ "gam Crv"; "del Crv"; "bet Crv"; "eps Crv"; "gam Crv" ] };
    { constellation = "Bootes";
      lines = chain [ "alf Boo"; "eps Boo"; "del Boo"; "bet Boo"; "gam Boo"; "alf Boo" ] @ [ ("alf Boo", "eta Boo") ] };
    { constellation = "Crux"; lines = [ ("alf Cru", "gam Cru"); ("bet Cru", "del Cru") ] };
    { constellation = "Libra"; lines = [ ("alf Lib", "bet Lib") ] };
    { constellation = "Scorpius";
      lines =
        chain [ "bet Sco"; "del Sco"; "pi Sco" ]
        @ chain
            [ "del Sco"; "sig Sco"; "alf Sco"; "tau Sco"; "eps Sco"; "mu1 Sco"; "zet2 Sco"; "eta Sco"; "the Sco"; "iot1 Sco";
              "kap Sco"; "lam Sco" ] };
    { constellation = "Sagittarius";
      lines =
        chain [ "gam Sgr"; "del Sgr"; "lam Sgr"; "phi Sgr"; "zet Sgr"; "eps Sgr"; "gam Sgr" ]
        @ [ ("del Sgr", "eps Sgr"); ("phi Sgr", "del Sgr"); ("phi Sgr", "sig Sgr"); ("sig Sgr", "tau Sgr"); ("tau Sgr", "zet Sgr") ] };
    { constellation = "Lyra"; lines = chain [ "alf Lyr"; "zet1 Lyr"; "bet Lyr"; "gam Lyr"; "del2 Lyr"; "zet1 Lyr" ] };
    { constellation = "Cygnus"; lines = chain [ "alf Cyg"; "gam Cyg"; "bet Cyg" ] @ chain [ "del Cyg"; "gam Cyg"; "eps Cyg" ] };
    { constellation = "Aquila";
      lines = chain [ "gam Aql"; "alf Aql"; "bet Aql"; "the Aql" ] @ [ ("alf Aql", "del Aql"); ("del Aql", "lam Aql"); ("del Aql", "zet Aql") ] } ]

(* Mitchell Charity's colours, a typical star of each class seen by a
 * camera balanced for the Sun's white *)
let color (c : char) : int * int * int =
  match c with
  | 'O' -> (155, 176, 255)
  | 'B' -> (170, 191, 255)
  | 'A' -> (202, 215, 255)
  | 'F' -> (248, 247, 255)
  | 'G' -> (255, 244, 234)
  | 'K' -> (255, 210, 161)
  | 'M' -> (255, 204, 111)
  | _ -> (255, 255, 255)
