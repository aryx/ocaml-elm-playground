(* Stereographic: the sky's sphere drawn on a flat screen.

   No map of a sphere keeps both its angles and its areas; the
   stereographic projection (Hipparchus', the astrolabe's) keeps the
   angles, so constellations keep their shapes, and every circle on the
   sky stays a circle on the screen -- the horizon included. It projects
   from the point opposite the one you look at, onto the plane touching
   the sphere where you look:

                    plane: (x, y)
            --------+----------*----       a point at the angle th
                    |         /            from the view's centre lands
                    |        /             2 tan (th / 2) from it:
                (centre)    /              1 at 53 deg, 2 at 90 deg,
                    |    th/               and to infinity at 180,
                    |     /                the projection's pole
                    |    /
                    |   /
                    |  /
                    | /
                    |/  (the pole, opposite the centre)

   The view is a direction (azimuth, altitude); the screen's up is
   towards the zenith, its right towards increasing azimuth. A whole
   dome, the view straight up, is a disk of radius 2: the planetarium's
   own projection.

   The horizon (altitude 0) of a view at altitude [a] > 0 is the circle
   of centre (0, 2 cot a) and radius 2 / sin a: the sky inside, the
   ground outside. Stellarium's default projection (2001), and the
   planispheres'. *)

(* [project ~view p]: where [p] lands on the plane, [None] too near the
 * projection's pole (more than about 150 deg from the view) *)
val project : view:Celestial.horizontal -> Celestial.horizontal -> (float * float) option

(* [horizon view_alt]: the horizon's centre (0, y) and radius, for a
 * view at altitude [view_alt] > 0 *)
val horizon : float -> float * float
