open Draw
(** The executor of the battle

    This module orchestrates the gameplay in battle*)

<<<<<<< HEAD
val init1 : unit -> unit
val adhoc_test1 : unit -> unit
val run_combat : unit -> unit
val start_up : unit -> unit
val update_health : Creature.creature -> int -> unit -> unit
val draw_combat_commands : int -> bool -> unit -> unit
val draw_exp_bar : int -> int -> int -> unit -> unit

val animate_faint : sprite -> bool -> unit -> unit
(** [animate_faint sprite (_)] performs the fainting animation of the
    creature [sprite] *)

val damage_render : sprite -> bool -> unit -> unit
(** [damage_render sprite is_player (_)] performs the damage animation
    of either the enemy or the player depedning on the [is_player]
    boolean *)

val draw_health_bar : int -> int -> int -> bool -> unit -> unit
(** [draw_health_bar max_hp before_current_hp after_current_hp is_player(_)]
    performs the health bar animation of either the enemy or the player
    depending on the [is_player] boolean *)
=======
val run_tick : char option -> unit
(** [run_tick c] runs a new tick of the battle. [c] represents the last
    character that the user is pressing.*)
>>>>>>> a21fbf28f7efa1e77ec87b15afde95fbbfd2748f
