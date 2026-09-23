(* Keeping the meshes of unchanged shapes on the GPU from one frame to
 * the next: the retained-mode half of the GPU backends.
 *
 * The playground's API is immediate mode: view3d describes the whole
 * scene again every frame. For a GPU backend, that means turning every
 * face into vertex data and uploading it every frame, even when
 * nothing moved; only the camera changes from frame to frame, and it's
 * a single matrix. For a world of thousands of blocks this CPU work
 * and upload take seconds per frame, while the GPU would draw the
 * same data again in milliseconds. Retained mode is the classic fix
 * (OpenGL's own history: glBegin/glEnd, then display lists, then
 * vertex buffers; pyglet's Batch in the original Python Minecraft):
 * build a mesh once, keep it in GPU memory, and on each frame only ask
 * the GPU to draw it again.
 *
 *     frame 1                 frame 2                 frame 3
 *   view3d: [A; B]          view3d: [A; B]          view3d: [A; C]
 *   A: build + upload       A: draw again           A: draw again
 *   B: build + upload       B: draw again           C: build + upload
 *                                                   B: freed (not seen)
 *
 * Which shapes are unchanged is the app's promise, through
 * Playground3d.cached3d, which gives each such shape a unique id (like
 * Elm's Html.lazy, which relies on reference identity). This module is
 * only the bookkeeping, generic in what a mesh is (each GPU backend has
 * its own: buffer handles and vertex counts) and knowing nothing of GL
 * or of shapes: an id -> mesh table where each frame marks the ids it
 * draws, and a sweep at the end of the frame frees the meshes of the
 * ids no frame asked for since the previous sweep. So the app never
 * frees anything: a shape it stops returning (e.g. a chunk of a world
 * rebuilt after an edit) just stops being drawn, and its GPU memory is
 * given back at the end of that frame. See
 * docs/claude_notes/plan_opengl_perf.md and notes_opengl.md, section 6.
 *
 * References:
 *  - retained vs. immediate mode, the two schools of the 1980s: PHIGS
 *    (ANSI 1988, ISO 9592 1989) kept a scene database ("structures") on the
 *    graphics system's side and re-drew it; SGI's IRIS GL, OpenGL's
 *    ancestor, had the program send everything again each frame, and
 *    won; retained mode came back inside it as display lists;
 *  - M. Segal, K. Akeley, "The OpenGL Graphics System: A Specification,
 *    Version 1.0", 1992, chapter 5.4, display lists: commands recorded
 *    once, stored by the server (the GPU side), executed again by
 *    glCallList; and version 1.5, 2003 (from the ARB_vertex_buffer_object
 *    extension), buffer objects with their usage hints (STATIC_DRAW:
 *    "specified once and used many times"), what the backends use;
 *  - E. Czaplicki, "Blazing Fast HTML: Virtual DOM in Elm", 2014
 *    (elm-lang.org/news/blazing-fast-html): Html.lazy, the same deal as
 *    cached3d -- the app promises a value doesn't change, the renderer
 *    skips it, by reference identity. *)

type 'mesh t

val create : unit -> 'mesh t

(* [find_or_build cache id build]: the mesh of [id], built with [build]
 * the first time; either way [id] is marked as used this frame *)
val find_or_build : 'mesh t -> int -> (unit -> 'mesh) -> 'mesh

(* [sweep cache ~free]: at the end of a frame, [free] the meshes whose
 * ids weren't used since the previous sweep, and forget them *)
val sweep : 'mesh t -> free:('mesh -> unit) -> unit

(* What the frame before the last sweep did, for the -debug stats line:
 * meshes kept (live), built and freed *)
type stats = { live : int; built : int; freed : int }

val stats : 'mesh t -> stats
