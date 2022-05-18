(** The executor of the battle This module graphically orchestrates the
    gameplay in battle.*)

val start_wild_battle : Creature.creature -> unit
(** [start_wild_battle c ] initializes a battle controller with the
    creature [c] as the opponent*)

val start_trainer_battle : Creature.creature list -> unit
(** [start_wild_battle c ] initializes a battle controller with the
    creature [c] as the opponent*)

val load_assets : unit -> unit
(** [load_assets ()] loads all the assets for the menu. *)
