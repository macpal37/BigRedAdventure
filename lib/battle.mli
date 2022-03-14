(** The executor of the battle

    This module orchestrates the gameplay in battle*)

val run_tick : char option -> unit
(** [run_tick c] runs a new tick of the battle. [c] represents the last
    character that the user is pressing.*)
