val input_poll : unit -> unit
(** Polls for event queue the latest key press. MAY EXIT THE PROGRAM*)

val poll_key_option : unit -> Sdlkeycode.t option
(** [poll_key_option _] is the option of the keycode of the last key
    press, or None if no key pressed*)

val pop_key_option : unit -> Sdlkeycode.t option
(** [pop_key_option _] is the option of the keycode of the last key
    press, or None if no key pressed. Subsequent calls will return None
    until a new key press (held keys are ignored)*)

val sleep : float -> unit -> unit
