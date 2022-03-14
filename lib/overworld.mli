(** The executor of the overworld

    This module orchestrates the gameplay in the overworld*)

val run_tick : char option -> unit
(** [run_tick c] runs a new tick of the overworld. [c] represents the
    last character that the user is pressing.*)
