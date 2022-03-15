(** The executor of the menu *)

val run_tick : char option -> unit
(** [run_tick c] runs a new tick of the menu. [c] represents the last
    character that the user is pressing.*)
