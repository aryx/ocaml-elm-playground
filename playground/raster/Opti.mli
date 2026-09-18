(* The switch between optimized code and the original, simpler code it
 * replaced, which is kept, runnable, next to it.
 *
 * Optimizations make code faster but harder to read. For teaching, the
 * simple version explains the idea, the optimized one shows the
 * craft, and switching between them while a game runs (the "o" key,
 * see playground/software/Playground_platform.ml) shows what each
 * optimization buys, on the fps counter. The measured numbers are in
 * docs/claude_notes/notes_opti.md.
 *
 * Code checking [enabled] (search for "Opti.enabled"):
 * - Framebuffer.plot: write the pixel directly, not through fill_span;
 * - Fill.polygons_aa: sparse coverage cells, not a coverage array
 *   updated pixel by pixel (Fill.polygons_aa_simple). *)

(* true: use the optimized versions (the default) *)
val enabled : bool ref
