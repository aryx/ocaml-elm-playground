(* A golden scene: a program run to a frame, the frame its golden one
 * (tests/*/golden/, compared by Testutil_golden). The scenes of the 2D
 * rasterizer are Scenes_2d's, the 3D one's Scenes_3d's: data, apart
 * from the tests, since tinybox's menu reads them too (a program's
 * screenshot in its grid, the input its preview plays). Four kinds: *)

(* A scene: an executable, from the project's root and without its .exe
 * (e.g. "examples/software/Cubes3d"), the debug keys to press ("" for none),
 * and the frame to compare. Its golden frame is golden/<name>.png, with
 * <name> the executable's basename, plus "_" and the keys if any (e.g.
 * golden/Cubes3d_bf.png). *)
type scene = string * string * int

(* A scene played with game keys: an executable, a label, the frame to
 * compare, and the -script giving the keys held over the frames (see
 * playground/platforms/native_common/Input_script.mli), e.g. ("games/platform/
 * software/TinyMario", "jump", 60, "right:1-60,up:20-25"). Its golden frame is
 * golden/<basename>_<label>.png (e.g. golden/TinyMario_jump.png). *)
type scripted = string * string * int * string

(* A scene started with flags (see Playground.flags): an executable, a
 * label, the frame to compare, and the flags, e.g.
 * ("games/platform/software/TinyMario", "shapes", 5, [ "artwork=shapes" ])
 * -- the other look of a game that has two. Its golden frame is
 * golden/<basename>_<label>.png, as a scripted scene's is. *)
type flagged = string * string * int * string list

(* claude: a scene both played and flagged: an executable, a label, the
 * frame, the -script and the flags, e.g. a juiced game's hit with
 * juice=engine (Juice.mode). Its golden frame is
 * golden/<basename>_<label>.png, as the others'. *)
type scripted_flagged = string * string * int * string * string list
