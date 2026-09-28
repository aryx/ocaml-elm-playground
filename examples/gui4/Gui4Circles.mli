(* 7GUIs 6, Circle Drawer, four ways: a canvas where a click makes a
 * circle, the one under the mouse filled grey; a right click on it
 * opens a menu whose "Adjust diameter..." opens a dialog, a slider that
 * changes the circle *as it moves*; and Undo and Redo -- of which the
 * whole of one adjustment, however long the slider was dragged, is one
 * step, taken when the dialog is closed.
 *
 * The task where the architectures stop being a matter of taste: the
 * state is a list that grows, a selection that follows the mouse, a
 * popup, a dialog with a live value, and a history whose steps are not
 * the program's events. With callbacks, the value before the dialog
 * opened must be kept somewhere for the close to find, and every
 * callback that changes anything must repaint the canvas; MVC
 * repaints on every change of the model; MVU draws the canvas from the
 * model like everything else; immediate mode asks the canvas what the
 * mouse did and then draws what it now knows. *)

val make : Theme.t -> Widget.box -> Gui4.architecture -> Gui4.runner
