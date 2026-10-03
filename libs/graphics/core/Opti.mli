(* The switch between optimized code and the original, simpler code it
 * replaced, which is kept, runnable, next to it.
 *
 * Optimizations make code faster but harder to read. For teaching, the
 * simple version explains the idea, the optimized one shows the
 * craft, and switching between them while a game runs (the "o" key,
 * see playground/platforms/software/Playground_platform.ml) shows what each
 * optimization buys, on the fps counter. The measured numbers are in
 * notes_opti.md.
 *
 * Code checking [enabled] (search for "Opti.enabled"):
 * - Framebuffer.plot: write the pixel directly, not through fill_span;
 * - Fill.polygons_aa: sparse coverage cells, not a coverage array
 *   updated pixel by pixel (Fill.polygons_aa_simple);
 * - Blit.draw: forward differencing and inlined samplers, not a matrix
 *   product and a sampling function per pixel (Blit.draw_simple);
 * - Pixelate.nearest: a block's rows after its first copied whole, not
 *   pixel by pixel (Pixelate.nearest_simple).
 *
 * claude: and tinybox's code map (launcher/codemap/, flag opti=off;
 * timed by launcher/codemap/bench/codemap_bench.exe, OPTI=off):
 * - Code_names.resolve_include: an include among the files of its base
 *   name, each resolved once (resolve_include_simple: every C file);
 * - Codemap.map_of: the uses counted once for all the maps of the same
 *   sources (the simple way: each map its own, Code_map_base.rank_of);
 * - Code_anatomy.line_has: words tried where one can start, compared in
 *   place (line_has_simple);
 * - Map_names.capitals and unit_ties: what does not depend on the camera
 *   kept (capitals_chosen, ties_of); placed_of and entry_of: a table by
 *   path (placed_of_simple, entry_of_simple: a scan). *)

(* true: use the optimized versions (the default) *)
val enabled : bool ref
