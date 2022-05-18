val input_poll : unit -> unit
(** Polls for event queue the latest key press. May exit the program.*)

val poll_key_option : unit -> Sdlkeycode.t option
(** [poll_key_option _] is the option of the keycode of the last key
    press, or None if no key pressed*)

val pop_key_option : unit -> Sdlkeycode.t option
(** [pop_key_option _] is the option of the keycode of the last key
    press, or None if no key pressed. Subsequent calls will return None
    until a new key press (held keys are ignored)*)

val sleep : float -> unit -> unit
(** [sleep f _] sleeps for [f] seconds then calls [input_poll _]*)

(*****************************************************************)
(***************    Game Control Keys     *********************)
(*****************************************************************)
type control_key =
  | Up
  | Down
  | Left
  | Right
  | Action
  | Back
  | Start
  | Select
  | Save
  | Debug
  | NoKey

val ( => ) : Sdlkeycode.t -> control_key -> bool
(** [k => c] checks if the keycode [k] is equivalent to [c]*)

val get_ctrl_key : Sdlkeycode.t -> control_key
(** [get_ctrl_key k] is the control key corresponding to [k]*)

val get_ctrl_option : Sdlkeycode.t option -> control_key option
(** [get_ctrl_key k] is Some control key corresponding to [k], or None
    if [k] is None*)
