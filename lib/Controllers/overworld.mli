(** The executor of the overworld

    This module orchestrates the gameplay in the overworld*)

val tile_dpi : int

val run_tick : unit -> unit
(** [run_tick _] runs a new tick of the overworld.*)
