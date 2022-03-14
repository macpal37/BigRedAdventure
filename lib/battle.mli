(** The executor of the battle

    This module orchestrates the gameplay in battle*)

val init1 : unit -> unit
val adhoc_test1 : unit -> unit
val run_combat : unit -> unit
val start_up : unit -> unit
val update_health : Creature.creature -> int -> unit -> unit
