(** The executor of the battle

    This module orchestrates the gameplay in battle*)

val run_tick : char -> bool
(** [run_tick c] runs a new tick of the battle. [c] represents the last
    character that the user is pressing. The return value indicates
    whether the battle has finished.*)

val draw : unit -> unit
(** [draw _] redraws the scene on the window. It may be called from
    other modules if the window has been forcibly refreshed.*)
