(* The Hammond B-3's panel as a part (Component.mli), the office's idea
 * for music: the same panel in every host -- TinyHammond full screen,
 * TinyReface's YC face scaled into the case, a rack's device
 * (plan_tiny_reason.md).
 *
 * The part closes over the voice: the sound lives with the mixer, not
 * in a model (Instrument.mli), so the voice's patch is the truth, read
 * each frame and set back when a control moves -- a host may change it
 * too (a preset, TinyHammond's space bar for the Leslie).
 *
 *   +-----------------------------------------------------------+
 *   | DRAWBARS                        PERCUSSION                 |
 *   | 16' 5 1/3' 8' 4' ...            ON SOFT FAST 3RD  VIBRATO  |
 *   |  |   |   |  |                   LESLIE                     |
 *   |  #   #   #  #   REGISTRATION    ON FAST     CLICK VOLUME   |
 *   +-----------------------------------------------------------+
 *
 * The drawbars pull down with the mouse, 0 (in) to 8 (all the way
 * out); the tabs, the rockers, the rotary switch click; the knobs turn
 * by dragging. *)

val kind : string

(* its size of its own, 1000 x 470, to be scaled to a host's room *)
val natural : float * float

val make : Voice_hammond.t -> Component.part

(* a part saved: the voice given the patch [text] was saved from *)
val load : Voice_hammond.t -> string -> Component.part
