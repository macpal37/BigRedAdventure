open Draw
(** The executor of the battle

    This module orchestrates the gameplay in battle*)

val start_battle : unit -> unit
val update_health : Creature.creature -> int -> unit -> unit
val draw_combat_commands : int -> bool -> unit -> unit
val draw_exp_bar : int -> int -> int -> unit -> unit

val animate_faint : sprite -> bool -> unit -> unit
(** [animate_faint sprite (_)] performs the fainting animation of the
    creature [sprite] *)

val draw_health_bar : int -> int -> int -> bool -> unit -> unit
(** [draw_health_bar max_hp before_current_hp after_current_hp is_player(_)]
    performs the health bar animation of either the enemy or the player
    depending on the [is_player] boolean *)

val run_tick : unit -> unit
