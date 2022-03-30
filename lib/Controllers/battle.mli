open Draw
(** The executor of the battle

    This module orchestrates the gameplay in battle*)

val start_battle : Creature.creature -> unit
val update_health : Creature.creature -> int -> unit -> unit
val draw_combat_commands : int -> bool -> unit -> unit

val animate_faint : sprite -> bool -> unit -> unit
(** [animate_faint sprite (_)] performs the fainting animation of the
    creature [sprite] *)

val run_tick : unit -> unit
