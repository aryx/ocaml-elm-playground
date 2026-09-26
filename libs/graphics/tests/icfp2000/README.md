# The ICFP 2000 ray tracer's pictures

`fib_2000.png` is `fib.ppm`, the picture the author's entry to the ICFP
Programming Contest 2000 (Cornell and Bell Labs: a ray tracer for GML)
rendered from the task's `fib.gml`, converted losslessly to PNG (the
entry's folder, `~/Dropbox/Downloads/icfp-raytrace-2020/delay/fib.ppm`,
dated 2003 on disk; the folder's name says 2020, the contest was 2000).

`Unit_raytrace.ml` renders the same scene with today's ray tracer
(`graphics/3d/raytrace/`, `examples/PovrayFib.ml`) and compares: a
regression test twenty-six years long. Measured when it was added
(2026-09-26): 99.96% of the pixels the same, all within 1.
